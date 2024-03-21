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

(** {6 State types} *)

(** Info trace. *)
module Info = struct
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

  type stack = pretrace list

  let init = [Seq []]
  let push b s = b :: s
  let append a b =
    match a, b with
    | Dispatch _, Seq [] -> Seq [a]
    | DispatchBranch c, Dispatch brs -> Dispatch (brs @ [c])
    | DispatchBranch _, _ -> assert false
    | _, Seq [] -> a
    | _, Seq brs -> Seq (brs @ [a])
    | _, Dispatch _ -> assert false
    | _, _ -> Seq [b; a]
  let pop s =
    match s with
    | DispatchBranch c :: Dispatch brs :: s' -> Dispatch (brs @ [c]) :: s'
    | DispatchBranch _ :: _ :: _ -> assert false
    | a :: Seq brs :: s' -> Seq (brs @ [a]) :: s'
    | _ :: Dispatch _ :: _ -> assert false
    | a :: DispatchBranch c :: s' -> DispatchBranch (append a c) :: s'
    | a :: Tactic (m, c) :: s' -> Tactic (m, append a c) :: s'
    | _ :: Message _ :: _ -> assert false
    | a :: Body c :: s' -> Body (append a c) :: s'
    | a :: Barrier c :: s' -> Barrier (append a c) :: s'
    | a :: Context (context, c) :: s' -> Context (context, append a c) :: s'
    | [_] | [] -> assert false
  let finalize s =
    match s with
    | [b] -> b
    | _ -> assert false

  (** We typically label nodes of [Trace.tree] with messages to
      print. But we don't want to compute the result. *)
  type trace = (unit -> Pp.t) tag

  let rec give_contexts reconstruct = function
    | Seq brs ->
      let brs, given = brs |> List.map (give_contexts reconstruct) |> List.split in
      Seq brs, given |> List.exists Fun.id
    | Dispatch brs ->
      let brs, given = brs |> List.map (give_contexts reconstruct) |> List.split in
      Dispatch brs, given |> List.exists Fun.id
    | Tactic (m, c) ->
      let reconstruct b = reconstruct (Tactic (m, b)) in
      let c, given = give_contexts reconstruct c in
      (if given then c else Tactic (m, c)), given
    | Message m -> Message m, false
    | Body c -> reconstruct c, true
    | Barrier c ->
      let c, _given = give_contexts Fun.id c in
      c, false
    | Context (context, c) ->
      let c, given = give_contexts reconstruct c in
      Context (context, c), given
    | DispatchBranch _ -> assert false
  let give_contexts b =
    let b, _given = give_contexts Fun.id b in
    b

  let rec apply_contexts context = function
    | Seq brs -> Seq (brs |> List.map (apply_contexts context))
    | Dispatch brs -> Dispatch (brs |> List.map (apply_contexts context))
    | Tactic (m, c) -> Tactic ((fun () -> m (Option.get context)), apply_contexts context c)
    | Message m -> Message (fun () -> m (Option.get context))
    | Context (context, c) -> apply_contexts (Some context) c
    | DispatchBranch _ | Body _ | Barrier _ -> assert false
  let apply_contexts = apply_contexts None

  let rec compress = function
    | Seq [c] -> compress c
    | Seq brs ->
      Seq (
        brs
          |> List.map compress
          |> List.map (function
              | Seq brs -> brs
              | b -> [b]
            )
          |> List.flatten
      )
    | Dispatch [c] -> compress c
    | Dispatch brs ->
      let brs = brs |> List.map compress in
      let all_empty = brs |> List.for_all (fun e -> match e with Seq [] -> true | _ -> false) in
      if all_empty then Seq [] else Dispatch brs
    | Tactic (m, c) -> Tactic (m, compress c)
    | Message m -> Message m
    | DispatchBranch _ | Body _ | Barrier _ | Context _ -> assert false

  let rec print = let open Pp in function
    | Seq brs ->
      let sep () = spc () ++ str ";" ++ spc () in
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
    | DispatchBranch _ | Body _ | Barrier _ | Context _ -> assert false

  let rec collapse n = function
    | Seq brs -> Seq (brs |> List.map (collapse n))
    | Dispatch brs -> Dispatch (brs |> List.map (collapse n))
    | Tactic (m, c) -> if n > 0 then collapse (n - 1) c else Tactic (m, Seq [])
    | Message m -> Message m
    | DispatchBranch _ | Body _ | Barrier _ | Context _ -> assert false

  type 'a event =
    | EventSeq of 'a event list
    | EventDispatch of 'a event list
    | EventTactic of 'a * 'a event
    | EventMessage of string
    [@@deriving yojson]

  let rec printed f = function
    | Seq brs -> EventSeq (brs |> List.map (printed f))
    | Dispatch brs -> EventDispatch (brs |> List.map (printed f))
    | Tactic (m, c) -> EventTactic (f m, printed f c)
    | Message m -> EventMessage (m () |> Pp.single_line_string_of_ppcmds)
    | DispatchBranch _ | Body _ | Barrier _ | Context _ -> assert false
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

  type u = Info.stack

  let uunit = Info.init

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
      raw_update (Info.push a) >>
      raw_update Info.pop
    else
      return ()

  let tag a t =
    let open Logical in
    recording >>= fun r ->
    if r then (
      raw_update (Info.push a) >>
      t >>= fun a ->
      raw_update Info.pop >>
      return a
    ) else
      t
end
