(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

(** This file defines the datatypes used as internal states by the
    tactic monad, and specialises the [Logic_monad] to these types. It should
    not be used directly. Consider using {!Proofview} instead. *)

(** {6 Forest builder for traces} *)

module TraceBuilder : sig
  (** The intent is that an ['a forest] is a list of messages of type
      ['a]. But messages can stand for a list of more precise
      messages, hence the structure is organised as a tree. *)
  type 'a tree = Node of 'a * 'a forest
  and 'a forest = 'a tree list

  (** To build a trace incrementally, we use an intermediary data
      structure on which we can define an S-expression like language
      (like a simplified xml except the closing tags do not carry a
      name). *)
  type 'a incr

  val init : 'a incr

  (** [open_ a] opens a tag with name [a]. *)
  val open_ : 'a -> 'a incr -> 'a incr

  (** [close] closes the last open tag. It is the responsibility of
      the user to close all the tags. *)
  val close : 'a incr -> 'a incr

  (** [leaf] creates an empty tag with name [a]. *)
  val leaf : 'a -> 'a incr -> 'a incr

  val finalize : 'a incr -> 'a forest
end

(** {6 State types} *)

(** Info trace. *)
module Info : sig
  open TraceBuilder

  type deferred_id

  val new_deferred_id : unit -> deferred_id

  (** We typically label nodes of the trace with messages to
      print. But we don't want to compute the result. *)
  type lazy_msg = unit -> Pp.t

  type tactic_kind =
    | Primitive of Pp.t
    | Builtin of Pp.t
    | Alias of Pp.t
    | ML of Pp.t
    [@@deriving yojson { variants = `Adjacent ("type", "s") }]

  type saved_proofview = Evd.evar_map * Evar.t list

  val empty_saved_proofview : saved_proofview

  (** The type of the tags for [Info]. *)
  type tag =
    | TagDeferredContents of deferred_id
    | TagDeferredPlaceholder of deferred_id
    | TagDispatch of saved_proofview (** A call to [tclDISPATCHGEN], [tclEXTEND], [Proofview.Goal.enter], or [Ftactic.enter]. *)
    | TagDispatchBranch (** A marker to delimit an individual branch of [TagDispatch]. *)
    | TagTactic of saved_proofview * saved_proofview ref * tactic_kind * lazy_msg (** A tactic call. *)
    | TagMessage of lazy_msg (** A message by [TacId]. *)

  type state = tag incr
  type pretrace = tag forest

  type trace =
    | Sequence of trace list (** A sequence. *)
    | Dispatch of saved_proofview * trace list (** A call to [tclDISPATCHGEN], [tclEXTEND], [Proofview.Goal.enter], or [Ftactic.enter]. *)
    | Tactic of saved_proofview * saved_proofview * tactic_kind * lazy_msg * trace (** A tactic call, with its execution detailed. *)
    | Message of lazy_msg (** A message by [TacId]. *)

  val finish : pretrace -> trace

  val compress : trace -> trace

  val print : trace -> Pp.t

  (** [collapse n t] flattens the first [n] levels of [Tactic] in an
      info trace, effectively forgetting about the [n] top level of
      names (if there are fewer, the last name is kept). *)
  val collapse : int -> trace -> trace
end

module StateStore : Store.S
type goal = Evar.t
type goal_with_state
val drop_state : goal_with_state -> goal
val get_state : goal_with_state -> StateStore.t
val goal_with_state : goal -> StateStore.t -> goal_with_state
val with_empty_state : goal -> goal_with_state
val map_goal_with_state : (goal -> goal) -> goal_with_state -> goal_with_state

(** Type of proof views: current [evar_map] together with the list of
    focused goals, locally shelved goals and globally shelved goals. *)
type proofview = {
  solution : Evd.evar_map;
  comb : goal_with_state list;
}

(** {6 Instantiation of the logic monad} *)

module P : sig
  type s = proofview * Environ.env

  (** Status (safe/unsafe) * given up *)
  type w = bool

  val wunit : w
  val wprod : w -> w -> w

  (** Recording info trace (true) or not. *)
  type e = { trace: bool; name : Names.Id.t; poly : bool }

  type u = Info.state

  val uunit : u
end

module Logical : module type of Logic_monad.Logical(P)


(** {6 Lenses to access to components of the states} *)

module type State = sig
  type t
  val get : t Logical.t
  val set : t -> unit Logical.t
  val modify : (t->t) -> unit Logical.t
end
module type Reader = sig
  type t
  val get : t Logical.t
end

module type Writer = sig
  type t
  val put : t -> unit Logical.t
end

(** Lens to the [proofview]. *)
module Pv : State with type t := proofview

(** Lens to the [evar_map] of the proofview. *)
module Solution : State with type t := Evd.evar_map

(** Lens to the list of focused goals. *)
module Comb : State with type t = goal_with_state list

(** Lens to the global environment. *)
module Env : State with type t := Environ.env

(** Lens to the tactic status ([true] if safe, [false] if unsafe) *)
module Status : Writer with type t := bool

(** Lens and utilities pertaining to the info trace *)
module InfoL : sig
  (** [record_trace true t] behaves like [t] and computes its Info trace.
      [record_trace false t] behaves like [t], but does not compute its Info trace. *)
  val with_recording : bool -> 'a Logical.t -> 'a Logical.t

  val update : (Info.state -> Info.state) -> unit Logical.t

  val leaf : Info.tag -> unit Logical.t

  (** [tag a t] opens tag [a], runs [t] then closes the tag. *)
  val tag : Info.tag -> 'a Logical.t -> 'a Logical.t
end
