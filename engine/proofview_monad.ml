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
    tactic monad, and specialises the [Logic_monad] to these type. *)

(** {6 Forest builder for traces} *)

module TraceBuilder = struct
  (** The intent is that an ['a forest] is a list of messages of type
      ['a]. But messages can stand for a list of more precise
      messages, hence the structure is organised as a tree. *)
  type 'a tree = Node of 'a * 'a forest
  and 'a forest = 'a tree list

  (** To build a trace incrementally, we use an intermediary data
      structure on which we can define an S-expression like language
      (like a simplified xml except the closing tags do not carry a
      name). Note that nodes are built from right to left in ['a
      incr], the result is mirrored when returning so that in the
      exposed interface, the forest is read from left to right.

      Concretely, we want to add a new tree to a forest: and we are
      building it by adding new trees to the left of its left-most
      subtrees which is built the same way. *)
  type 'a incr = {head : 'a forest; opened : 'a tree list}

  (** S-expression like language as ['a incr] transformers. It is the
      responsibility of the library builder not to use [close] when no
      tag is open. *)

  let init = {head = []; opened = []}

  let open_ a {head; opened} = {head; opened = Node (a, []) :: opened}
  let close {head; opened} =
    match opened with
    | [a] -> {head = a :: head; opened = []}
    | a :: Node(b, f) :: opened -> {head; opened = Node (b, a :: f) :: opened}
    | [] -> assert false
  let leaf a s = close (open_ a s)

  (** Returning a forest. It is the responsibility of the library
      builder to close all the tags. *)
  (* spiwack: I may want to close the tags instead, to deal with
    interruptions. *)
  let rec mirror_tree (Node (a, f)) = Node (a, mirror_forest f)
  and mirror_forest f = f |> List.rev_map mirror_tree

  let finalize = function
    | {head; opened = []} -> mirror_forest head
    | {head; opened = _ :: _} -> assert false
end

(** {6 State types} *)

(** Info trace. *)
module Info = struct
  open TraceBuilder

  (** We typically label nodes of the trace with messages to
      print. But we don't want to compute the result. *)
  type lazy_msg = unit -> Pp.t

  (** The type of the tags for [Info]. *)
  type tag =
    | TagDispatch (** A call to [tclDISPATCHGEN], [tclEXTEND] or [Proofview.Goal.enter]. *)
    | TagDispatchBranch (** A marker to delimit an individual branch of [TagDispatch]. *)
    | TagTactic of lazy_msg (** A tactic call. *)
    | TagMessage of lazy_msg (** A message by [TacId]. *)

  type state = tag incr
  type pretrace = tag forest

  type trace =
    | Sequence of trace list (** A sequence. *)
    | Dispatch of trace list (** A call to [tclDISPATCHGEN], [tclEXTEND] or [Proofview.Goal.enter]. *)
    | Tactic of lazy_msg * trace (** A tactic call, with its execution detailed. *)
    | Message of lazy_msg (** A message by [TacId]. *)

  let rec finish_tree = function
    | Node (TagDispatch, brs) ->
      Dispatch (
        brs |> List.map (function
          | Node (TagDispatchBranch, f) -> finish f
          | _ -> assert false
        )
      )
    | Node (TagDispatchBranch, _) -> assert false
    | Node (TagTactic m, f) -> Tactic (m, finish f)
    | Node (TagMessage m, []) -> Message m
    | Node (TagMessage _, _) -> assert false
  and finish f = Sequence (f |> List.map finish_tree)

  let rec compress = function
    | Sequence brs ->
      let brs = (
        brs
          |> List.map compress
          |> List.map (function
              | Sequence brs -> brs
              | b -> [b]
            )
          |> List.flatten
      ) in
      (match brs with
      | [c] -> c
      | brs -> Sequence brs)
    | Dispatch [c] -> compress c
    | Dispatch brs ->
      let brs = brs |> List.map compress in
      let all_empty = brs |> List.for_all (fun e -> match e with Sequence [] -> true | _ -> false) in
      if all_empty then Sequence [] else Dispatch brs
    | Tactic (m, c) -> Tactic (m, compress c)
    | Message m -> Message m

  let rec print = let open Pp in function
    | Sequence brs ->
      let sep () = str ";" ++ spc () in
      if brs = []
      then str "idtac"
      else brs |> Pp.prlist_with_sep sep print
    | Dispatch brs ->
      let sep () = spc () ++ str "|" ++ spc () in
      if brs = []
      then str "idtac"
      else str "[>" ++ spc () ++ (brs |> Pp.prlist_with_sep sep print) ++ spc () ++ str "]"
    | Tactic (m, _) -> m ()
    | Message m -> str "(* " ++ m () ++ str " *)"

  let rec collapse n = function
    | Sequence brs -> Sequence (brs |> List.map (collapse n))
    | Dispatch brs -> Dispatch (brs |> List.map (collapse n))
    | Tactic (m, c) -> if n > 0 then collapse (n - 1) c else Tactic (m, Sequence [])
    | Message m -> Message m
end

module Event = struct
  type 'a t =
    | Sequence of {elements : 'a t list}
    | Dispatch of {branches : 'a t list}
    | Tactic of {tactic : 'a; details : 'a t}
    | Message of {message : string}
    [@@deriving yojson { variants = `Internal "type" }]

  let rec of_trace f = function
    | Info.Sequence brs -> Sequence {elements = brs |> List.map (of_trace f)}
    | Info.Dispatch brs -> Dispatch {branches = brs |> List.map (of_trace f)}
    | Info.Tactic (m, c) -> Tactic {tactic = f m; details = of_trace f c}
    | Info.Message m -> Message {message = m () |> Pp.simple_string_of_ppcmds}
end

module StateStore = Store.Make()

(* let (set_state, get_state) = StateDyn.Easy.make_dyn "goal_state" *)

type goal = Evar.t
type goal_with_state = Evar.t * StateStore.t

let drop_state = fst
let get_state = snd
let goal_with_state g s = (g, s)
let with_empty_state g = (g, StateStore.empty)
let map_goal_with_state f (g, s) = (f g, s)

(** Type of proof views: current [evar_map] together with the list of
    focused goals. *)
type proofview = {
  solution : Evd.evar_map;
  comb : goal_with_state list;
}

(** {6 Instantiation of the logic monad} *)

(** Parameters of the logic monads *)
module P = struct

  type s = proofview * Environ.env

  (** Recording info trace (true) or not. *)
  type e = { trace: bool; name : Names.Id.t; poly : bool }

  (** Status (safe/unsafe) * shelved goals * given up *)
  type w = bool

  let wunit = true
  let wprod b1 b2 = b1 && b2

  type u = Info.state

  let uunit = TraceBuilder.init

end

module Logical = Logic_monad.Logical(P)


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

module Pv : State with type t := proofview = struct
  let get = Logical.(map fst get)
  let set p = Logical.modify (fun (_,e) -> (p,e))
  let modify f= Logical.modify (fun (p,e) -> (f p,e))
end

module Solution : State with type t := Evd.evar_map = struct
  let get = Logical.map (fun {solution} -> solution) Pv.get
  let set s = Pv.modify (fun pv -> { pv with solution = s })
  let modify f = Pv.modify (fun pv -> { pv with solution = f pv.solution })
end

module Comb : State with type t = goal_with_state list = struct
    (* spiwack: I don't know why I cannot substitute ([:=]) [t] with a type expression. *)
  type t = goal_with_state list
  let get = Logical.map (fun {comb} -> comb) Pv.get
  let set c = Pv.modify (fun pv -> { pv with comb = c })
  let modify f = Pv.modify (fun pv -> { pv with comb = f pv.comb })
end

module Env : State with type t := Environ.env = struct
  let get = Logical.(map snd get)
  let set e = Logical.modify (fun (p,_) -> (p,e))
  let modify f = Logical.modify (fun (p,e) -> (p,f e))
end

module Status : Writer with type t := bool = struct
  let put s = Logical.put s
end

(** Lens and utilities pertaining to the info trace *)
module InfoL = struct
  let recording = Logical.(map (fun {P.trace} -> trace) current)

  let record_trace t =
    Logical.(
      current >>= fun s ->
      local {s with P.trace = true} t
    )

  let raw_update = Logical.update
  let update f =
    let open Logical in
    recording >>= fun r ->
    if r then
      raw_update f
    else
      return ()

  let leaf a =
    let open Logical in
    recording >>= fun r ->
    if r then
      raw_update (TraceBuilder.leaf a)
    else
      return ()

  let tag a t =
    let open Logical in
    recording >>= fun r ->
    if r then (
      raw_update (TraceBuilder.open_ a) >>
      t >>= fun a ->
      raw_update TraceBuilder.close >>
      return a
    ) else
      t
end
