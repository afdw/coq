(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

open Util
open Names
open EConstr
open Genarg
open Geninterp
open Tactypes

(** Coercions from highest level generic arguments to actual data used by Ltac
    interpretation. Those functions examinate dynamic types and try to return
    something sensible according to the object content. *)

exception CannotCoerceTo of string
(** Exception raised whenever a coercion failed. *)

(** {5 High-level access to values}

  The [of_*] functions cast a given argument into a value. The [to_*] do the
  converse, and return [None] if there is a type mismatch.

*)

module Value :
sig
  type t = Val.t

  val of_constr : constr -> t
  val to_constr : t -> constr option
  val of_uconstr : Ltac_pretype.closed_glob_constr -> t
  val to_uconstr : t -> Ltac_pretype.closed_glob_constr option
  val of_int : int -> t
  val to_int : t -> int option
  val of_ident : Id.t -> t
  val to_ident : t -> Id.t option
  val to_list : t -> t list option
  val to_option : t -> t option option
  val to_pair : t -> (t * t) option
  val cast : 'a typed_abstract_argument_type -> Geninterp.Val.t -> 'a
end

(** {5 Coercion functions} *)

val in_gen : 'a typed_abstract_argument_type -> 'a -> Value.t
val out_gen : 'a typed_abstract_argument_type -> Value.t -> 'a

val coerce_to_constr_context : Value.t -> Constr_matching.context

val coerce_var_to_ident : bool -> Environ.env -> Evd.evar_map -> Value.t -> Id.t

val coerce_to_ident_not_fresh : Evd.evar_map -> Value.t -> Id.t

val coerce_to_intro_pattern : Evd.evar_map -> Value.t -> delayed_open_constr intro_pattern_expr

val coerce_to_intro_pattern_naming :
  Evd.evar_map -> Value.t -> Namegen.intro_pattern_naming_expr

val coerce_to_hint_base : Value.t -> string

val coerce_to_int : Value.t -> int

val coerce_to_constr : Environ.env -> Value.t -> Ltac_pretype.constr_under_binders

val coerce_to_uconstr : Value.t -> Ltac_pretype.closed_glob_constr

val coerce_to_closed_constr : Environ.env -> Value.t -> constr

val coerce_to_evaluable_ref :
  Environ.env -> Evd.evar_map -> Value.t -> Evaluable.t

val coerce_to_constr_list : Environ.env -> Value.t -> constr list

val coerce_to_intro_pattern_list :
  ?loc:Loc.t -> Evd.evar_map -> Value.t -> Tacexpr.intro_patterns

val coerce_to_hyp : Environ.env -> Evd.evar_map -> Value.t -> Id.t

val coerce_to_hyp_list : Environ.env -> Evd.evar_map -> Value.t -> Id.t list

val coerce_to_reference : Evd.evar_map -> Value.t -> GlobRef.t

val coerce_to_quantified_hypothesis : Evd.evar_map -> Value.t -> quantified_hypothesis

val coerce_to_decl_or_quant_hyp : Evd.evar_map -> Value.t -> quantified_hypothesis

val coerce_to_int_list : Value.t -> int list

(** {5 Missing generic arguments} *)

val wit_constr_context : (Empty.t, Empty.t, Constr_matching.context) genarg_type

val wit_constr_under_binders : (Empty.t, Empty.t, Ltac_pretype.constr_under_binders) genarg_type

val wit_value : (Val.t, Val.t, Val.t) genarg_type

val wit_value_of_corrent_type : (Val.t, Val.t, Val.t) genarg_type

val error_ltac_variable : ?loc:Loc.t -> Id.t ->
  (Environ.env * Evd.evar_map) option -> Value.t -> string -> 'a

(** Abstract application, to print ltac functions *)
type appl =
  | UnnamedAppl (** For generic applications: nothing is printed *)
  | GlbAppl of (Names.KerName.t * Val.t list) list
       (** For calls to global constants, some may alias other. *)

type tacvalue =
  | VFun of appl * Tacexpr.ltac_trace * Loc.t option * Val.t Id.Map.t *
      Name.t list * Tacexpr.glob_tactic_expr
  | VRec of Val.t Id.Map.t ref * Tacexpr.glob_tactic_expr

val wit_tacvalue : (Empty.t, tacvalue, tacvalue) Genarg.genarg_type

val wit_atomic_tactic : (Empty.t, Empty.t, Tacexpr.atomic_tactic_expr) genarg_type

val pr_value : (Environ.env * Evd.evar_map) option -> Geninterp.Val.t -> Pp.t

(** {5 Late args} *)

type late_arg

val new_late_arg : unit -> late_arg

type late_args_map

val f_late_args_map : late_args_map Evd.Store.field

val retrieve_raw_late_arg : late_arg -> raw_generic_argument option Proofview.tactic
val populate_raw_late_arg : late_arg -> raw_generic_argument option -> unit Proofview.tactic
val wrap_populate_raw_late_arg : late_arg -> raw_generic_argument option -> 'a Proofview.tactic -> 'a Proofview.tactic

val retrieve_glob_late_arg : late_arg -> glob_generic_argument option Proofview.tactic
val populate_glob_late_arg : late_arg -> glob_generic_argument option -> unit Proofview.tactic
val wrap_populate_glob_late_arg : late_arg -> glob_generic_argument option -> 'a Proofview.tactic -> 'a Proofview.tactic

val wrap_keep_late_args : 'a Proofview.tactic -> 'a Proofview.tactic

val wit_late_arg : (late_arg * raw_generic_argument option, late_arg * glob_generic_argument option, Empty.t) genarg_type

val glob_late_arg_tac_arg : ?isquot:string -> ?default:glob_generic_argument -> late_arg -> Tacexpr.glob_tactic_arg
val glob_late_arg_tac : ?isquot:string -> ?default:glob_generic_argument -> late_arg -> Tacexpr.glob_tactic_expr

(** {5 Printed args} *)

val wit_printed_arg : (Proofview_monad.Info.lazy_msg, Proofview_monad.Info.lazy_msg, Proofview_monad.Info.lazy_msg) genarg_type

val glob_printed_arg_tac_arg : ?isquot:string -> Proofview_monad.Info.lazy_msg -> Tacexpr.glob_tactic_arg
val glob_printed_arg_tac : ?isquot:string -> Proofview_monad.Info.lazy_msg -> Tacexpr.glob_tactic_expr
