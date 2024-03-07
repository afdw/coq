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
open Redexpr
open Constrexpr

val print_constr :
  Environ.env ->
  Evd.evar_map ->
  EConstr.t ->
  Constrextern.PrintingVariants.t

val print_constr_expr :
  Environ.env ->
  Evd.evar_map ->
  Constrexpr.constr_expr ->
  Constrextern.PrintingVariants.t

module Step : sig
  type kind =
    | Tactic of {
        goal_selector : string;
        tactic_raw : string;
        tactic : Constrextern.PrintingVariants.t;
        event : Proof.Event.t;
      }
    | StartSubproof
    | EndSubproof
    | Bullet of {bullet : Proof_bullet.t}
    [@@deriving yojson { variants = `Internal "type" }]

  type t = {
    goals_before : Proof.Goal.t list;
    goals_after : Proof.Goal.t list;
    kind : kind;
  } [@@deriving yojson { variants = `Internal "type" }]
end

val current_name : Names.Id.t option ref
val current_type : EConstr.t option ref
val current_steps : Step.t list ref

val record_step : Proof.t -> Proof.t -> Step.kind -> unit

module Declaration : sig
  type outcome =
    | Admitted
    | Proved
    | Exact
    | Abort
    | Fail
    [@@deriving yojson { variants = `Internal "type" }]

  type kind =
    | Inductive
    | Constructor of {ind_path : Libnames.full_path}
    | Assumption
    | Definition of {
        value : Constrextern.PrintingVariants.t;
        equations : Constrextern.PrintingVariants.t list;
      }
    | Interactive of {
        steps : Step.t list;
        outcome : outcome;
      }
    [@@deriving yojson { variants = `Internal "type" }]

  type t = {
    path : Libnames.full_path;
    type_ : Constrextern.PrintingVariants.t;
    kind : kind;
  } [@@deriving yojson { variants = `Internal "type" }]
end

val declarations : Declaration.t list ref

val end_proof : Declaration.outcome -> unit

val compute_equations :
  Environ.env -> Evd.evar_map -> EConstr.rel_context ->
  EConstr.t -> EConstr.t -> EConstr.t -> Evd.evar_map * EConstr.t list

(** {6 Definitions/Let} *)

val interp_definition
  :  program_mode:bool
  -> Environ.env
  -> Evd.evar_map
  -> Constrintern.internalization_env
  -> Constrexpr.local_binder_expr list
  -> red_expr option
  -> constr_expr
  -> constr_expr option
  -> Evd.evar_map * (EConstr.t * EConstr.t option) * Impargs.manual_implicits

val do_definition
  :  ?hook:Declare.Hook.t
  -> name:Id.t
  -> ?scope:Locality.definition_scope
  -> ?clearbody:bool
  -> poly:bool
  -> ?typing_flags:Declarations.typing_flags
  -> kind:Decls.definition_object_kind
  -> ?using:Vernacexpr.section_subset_expr
  -> ?user_warns:UserWarn.t
  -> universe_decl_expr option
  -> local_binder_expr list
  -> red_expr option
  -> constr_expr
  -> constr_expr option
  -> unit

val do_definition_program
  :  ?hook:Declare.Hook.t
  -> pm:Declare.OblState.t
  -> name:Id.t
  -> scope:Locality.definition_scope
  -> ?clearbody:bool
  -> poly:bool
  -> ?typing_flags:Declarations.typing_flags
  -> kind:Decls.logical_kind
  -> ?using:Vernacexpr.section_subset_expr
  -> ?user_warns:UserWarn.t
  -> universe_decl_expr option
  -> local_binder_expr list
  -> red_expr option
  -> constr_expr
  -> constr_expr option
  -> Declare.OblState.t
