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

(** {6 State types} *)

(** Info trace. *)
module Info : sig
  type context = Environ.env * Evd.evar_map

  (** The type of the tags for [Info]. *)
  type 'a tag =
    | Seq of 'a tag list
    | Dispatch of 'a tag list (** A call to [tclDISPATCHGEN], [tclEXTEND] or [Proofview.Goal.enter] *)
    | DispatchBranch of 'a tag  (** A special marker to delimit individual branch of a dispatch. *)
    | Tactic of 'a * 'a tag (** A tactic call *)
    | Message of 'a (** A simple message *)
    | Body of 'a tag
    | Barrier of 'a tag
    | Context of context * 'a tag

  type pretrace = (context -> Pp.t) tag

  type stack

  val init : stack
  val push : pretrace -> stack -> stack
  val pop : stack -> stack
  val finalize : stack -> pretrace

  (** We typically label nodes of the trace with messages to
      print. But we don't want to compute the result. *)
  type trace = (unit -> Pp.t) tag

  val give_contexts : pretrace -> pretrace

  val apply_contexts : pretrace -> trace

  val compress : trace -> trace

  val print : trace -> Pp.t

  (** [collapse n t] flattens the first [n] levels of [Tactic] in an
      info trace, effectively forgetting about the [n] top level of
      names (if there are fewer, the last name is kept). *)
  val collapse : int -> trace -> trace

  type 'a event =
    | EventSeq of 'a event list
    | EventDispatch of 'a event list
    | EventTactic of 'a * 'a event
    | EventMessage of string
    [@@deriving yojson]

  val printed : ((unit -> Pp.t) -> 'a) -> trace -> 'a event
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

  type u = Info.stack

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
  (** [record_trace t] behaves like [t] and compute its [info] trace. *)
  val record_trace : 'a Logical.t -> 'a Logical.t

  val update : (Info.stack -> Info.stack) -> unit Logical.t

  val leaf : Info.pretrace -> unit Logical.t

  (** [tag a t] opens tag [a], runs [t] then closes the tag. *)
  val tag : Info.pretrace -> 'a Logical.t -> 'a Logical.t

  (* val update : (Info.state -> Info.state) -> unit Logical.t

  val open_ : (Info.context -> Pp.t) Info.tag -> unit Logical.t
  val close : unit Logical.t
  val leaf : (Info.context -> Pp.t) Info.tag -> unit Logical.t

  (** [tag a t] opens tag [a], runs [t] then closes the tag. *)
  val tag : (Info.context -> Pp.t) Info.tag -> 'a Logical.t -> 'a Logical.t *)
end
