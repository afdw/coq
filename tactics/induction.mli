(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

open Names
open EConstr
open Evd
open Tactypes
open Locus
open Tactics

(** {6 Elimination tactics. } *)

(*
   The general form of an induction principle is the following:

   forall prm1 prm2 ... prmp,                          (induction parameters)
   forall Q1...,(Qi:Ti_1 -> Ti_2 ->...-> Ti_ni),...Qq, (predicates)
   branch1, branch2, ... , branchr,                    (branches of the principle)
   forall (x1:Ti_1) (x2:Ti_2) ... (xni:Ti_ni),         (induction arguments)
   (HI: I prm1..prmp x1...xni)                         (optional main induction arg)
   -> (Qi x1...xni HI        (f prm1...prmp x1...xni)).(conclusion)
                   ^^        ^^^^^^^^^^^^^^^^^^^^^^^^
               optional                optional
               even if HI      argument added if principle
             present above   generated by functional induction
             [indarg]          [farg]

  HI is not present when the induction principle does not come directly from an
  inductive type (like when it is generated by functional induction for
  example). HI is present otherwise BUT may not appear in the conclusion
  (dependent principle). HI and (f...) cannot be both present.

  Principles taken from functional induction have the final (f...).
*)

(** [rel_contexts] and [rel_declaration] actually contain triples, and
   lists are actually in reverse order to fit [compose_prod]. *)
type elim_scheme = {
  elimt: types;
  indref: GlobRef.t option;
  params: rel_context;      (** (prm1,tprm1);(prm2,tprm2)...(prmp,tprmp) *)
  nparams: int;               (** number of parameters *)
  predicates: rel_context;  (** (Qq, (Tq_1 -> Tq_2 ->...-> Tq_nq)), (Q1,...) *)
  npredicates: int;           (** Number of predicates *)
  branches: rel_context;    (** branchr,...,branch1 *)
  nbranches: int;             (** Number of branches *)
  args: rel_context;        (** (xni, Ti_ni) ... (x1, Ti_1) *)
  nargs: int;                 (** number of arguments *)
  indarg: rel_declaration option;  (** Some (H,I prm1..prmp x1...xni)
                                                 if HI is in premisses, None otherwise *)
  concl: types;               (** Qi x1...xni HI (f...), HI and (f...)
                                  are optional and mutually exclusive *)
  indarg_in_concl: bool;      (** true if HI appears at the end of conclusion *)
  farg_in_concl: bool;        (** true if (f...) appears at the end of conclusion *)
}

val compute_elim_sig : evar_map -> types -> elim_scheme

val induction : evars_flag -> clear_flag -> constr -> or_and_intro_pattern option ->
  constr with_bindings option -> unit Proofview.tactic

val destruct : evars_flag -> clear_flag -> constr -> or_and_intro_pattern option ->
  constr with_bindings option -> unit Proofview.tactic

(** {6 Generic case analysis / induction tactics. } *)

(** Implements user-level "destruct" and "induction" *)

val induction_destruct : rec_flag -> evars_flag ->
  (named_delayed_open_constr_with_bindings Tactics.destruction_arg
   * (intro_pattern_naming option * or_and_intro_pattern option)
   * clause option) list *
  constr with_bindings option -> unit Proofview.tactic
