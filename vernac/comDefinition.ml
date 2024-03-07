(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

open Pp
open Util
open Redexpr
open Constrintern

let print_constr env sigma e = Constrextern.PrintingVariants.make (fun () -> e |> Printer.pr_leconstr_env env sigma)

let print_constr_expr env sigma e = Constrextern.PrintingVariants.make (fun () -> e |> Ppconstr.pr_lconstr_expr env sigma)

let () = Proof.print_constr_hook := print_constr

module Step = struct
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

let current_name = ref None
let current_type = ref None
let current_steps = ref []

let record_step proof_before proof_after kind =
  let Proof.{sigma = sigma_before; goals = goals_before; _} = proof_before |> Proof.data in
  let Proof.{sigma = sigma_after; goals = goals_after; _} = proof_after |> Proof.data in
  current_steps := !current_steps @ [Step.{
    goals_before = goals_before |> List.map (Proof.Goal.make sigma_before);
    goals_after = goals_after |> List.map (Proof.Goal.make sigma_after);
    kind = kind;
  }]

module Declaration = struct
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

let declarations = ref []

let end_proof outcome =
  if !Flags.tracing_interactive then
    Feedback.msg_info Pp.(str "Recorded steps:" ++ spc () ++ int (!current_steps |> List.length));
  if !Flags.tracing && !current_type <> None then begin
    let env = Global.env () in
    let sigma = Evd.from_env env in
    declarations := !declarations @ [Declaration.{
      path = Libnames.make_path (Global.current_dirpath ()) (!current_name |> Option.get);
      type_ = !current_type |> Option.get |> print_constr env sigma;
      kind = Interactive {
        steps = !current_steps;
        outcome;
      };
    }]
  end;
  current_name := None;
  current_type := None;
  current_steps := []

let rec compute_equations
    (env : Environ.env) (sigma : Evd.evar_map) (ctx : EConstr.rel_context)
    (type_ : EConstr.t) (left : EConstr.t) (right : EConstr.t) : Evd.evar_map * EConstr.t list =
  match EConstr.kind sigma type_, EConstr.kind sigma right with
  | Prod (na_1, t_1, type'), Lambda (na_2, t_2, right') when
      Termops.eq_constr env sigma t_1 t_2 ->
    let na = na_2 |> Context.map_annot (fun n -> if Names.Name.is_anonymous n then na_1.binder_name else n) in
    compute_equations
      env sigma (Context.Rel.Declaration.LocalAssum (na, t_2) :: ctx)
      type' (EConstr.mkApp (left |> EConstr.Vars.lift 1, [|EConstr.mkRel 1|])) right'
  | _, LetIn (na, b, t, right') ->
    compute_equations
      env sigma (Context.Rel.Declaration.LocalDef (na, b, t) :: ctx)
      (type_ |> EConstr.Vars.lift 1) (left |> EConstr.Vars.lift 1) right'
  | _, Case (ci, u, params, _, _, c, brs) when
      EConstr.isRel sigma c &&
      let n = EConstr.destRel sigma c in
      let (_mib, mip) = Inductive.lookup_mind_specif env ci.ci_ind in
      params |> Array.for_all (EConstr.Vars.noccur_between sigma 1 (n - 1)) &&
      mip.mind_nrealargs = 0 ->
    let n = EConstr.destRel sigma c in
    let (_mib, mip) = Inductive.lookup_mind_specif env ci.ci_ind in
    let unlifted_params = params |> Array.map (EConstr.Vars.substl (List.make n EConstr.mkProp)) in
    brs |> Array.fold_left_i (fun contructor_i (sigma, equations) (nas, right') ->
      let (constructor_ctx, _) = mip.mind_nf_lc.(contructor_i) in
      let contructor_realdecls_count = mip.mind_consnrealdecls.(contructor_i) in
      let (realargs_ctx, params_ctx) = constructor_ctx |> List.chop contructor_realdecls_count in
      let (realargs_ctx, params_ctx) = (realargs_ctx |> EConstr.of_rel_context, params_ctx |> EConstr.of_rel_context) in
      let params_subst = EConstr.Vars.subst_of_rel_context_instance params_ctx unlifted_params in
      let params_substituted_realargs_ctx = realargs_ctx |> EConstr.Vars.substl_rel_context params_subst in
      let named_realargs_ctx =
        List.combine params_substituted_realargs_ctx (nas |> Array.rev_to_list) |> List.map (fun (declaration, na) ->
          declaration |> Context.Rel.Declaration.set_name na.Context.binder_name
        ) in
      let lifted_params = unlifted_params |> Array.map (EConstr.Vars.lift contructor_realdecls_count) in
      let self =
        EConstr.mkApp (
          EConstr.mkConstructU ((ci.ci_ind, contructor_i + 1), u),
          Array.append
            lifted_params
            (params_substituted_realargs_ctx |> Context.Rel.instance EConstr.mkRel 0)
        ) in
      let (ctx_1, ctx_2) = ctx |> List.chop (n - 1) in
      let substitued_ctx_1 =
        ctx_1
          |> EConstr.Vars.liftn_rel_context contructor_realdecls_count 2
          |> EConstr.Vars.substl_rel_context [self] in
      let ctx_2_tail = List.tl ctx_2 in
      let new_ctx = substitued_ctx_1 @ named_realargs_ctx @ ctx_2_tail in
      let subs =
        Esubst.subs_liftn (n - 1) (
          Esubst.subs_cons self (
            Esubst.subs_shft (contructor_realdecls_count,
              Esubst.subs_id (List.length ctx_1)
            )
          )
        ) in
      let new_type = type_ |> EConstr.Vars.esubst EConstr.Vars.lift subs in
      let new_left = left |> EConstr.Vars.esubst EConstr.Vars.lift subs in
      let new_right' =
        right'
          |> EConstr.Vars.esubst EConstr.Vars.lift (Esubst.subs_liftn contructor_realdecls_count subs)
          |> EConstr.Vars.substl (List.init contructor_realdecls_count (fun i -> EConstr.mkRel (n + i))) in
      let sigma, new_equations = compute_equations env sigma new_ctx new_type new_left new_right' in
      sigma, equations @ new_equations
    ) (sigma, [])
  | _, Fix (([|n|], 0), ([|_|], [|_|], [|right'|])) ->
    compute_equations env sigma ctx type_ left (right' |> EConstr.Vars.subst1 left)
  | _, _ ->
    let sigma, eq = Evd.fresh_global env sigma (Coqlib.lib_ref "core.eq.type") in
    let sigma, refl = Evd.fresh_global env sigma (Coqlib.lib_ref "core.eq.refl") in
    let sigma, type_ = Evarsolve.refresh_universes (Some false) env sigma type_ in
    let sigma, left = Evarsolve.refresh_universes (Some false) env sigma left in
    let sigma, right = Evarsolve.refresh_universes (Some false) env sigma right in
    let applied_eq = EConstr.mkApp (eq, [|type_; left; right|]) in
    let applied_refl = EConstr.mkApp (refl, [|type_; left|]) in
    let equation = EConstr.it_mkProd_or_LetIn applied_eq ctx in
    let sigma =
      try Typing.check env sigma equation EConstr.mkProp
      with Pretype_errors.PretypeError (_, _, Pretype_errors.CantApplyBadTypeExplained (_, Pretype_errors.UnifUnivInconsistency _)) -> sigma in
    let _proof = EConstr.it_mkLambda_or_LetIn applied_refl ctx in
    (* let sigma = Typing.check env sigma proof equation in *)
    sigma, [equation]

(* Commands of the interface: Constant definitions *)

let red_constant_body red_opt env sigma body = match red_opt with
  | None -> sigma, body
  | Some red ->
    let red, _ = reduction_of_red_expr env red in
    red env sigma body

let warn_implicits_in_term =
  CWarnings.create ~name:"implicits-in-term" ~category:CWarnings.CoreCategories.implicits
         (fun () ->
          strbrk "Implicit arguments declaration relies on type." ++ spc () ++
            strbrk "Discarding incompatible declaration in term.")

let check_imps ~impsty ~impsbody =
  let rec aux impsty impsbody =
    match impsty, impsbody with
    | a1 :: impsty, a2 :: impsbody ->
      let () = match a1.CAst.v, a2.CAst.v with
        | None , None | Some _, None -> ()
        | Some (_,b1) , Some (_,b2) ->
          if not ((b1:bool) = b2) then warn_implicits_in_term ?loc:a2.CAst.loc ()
        | None, Some _ -> warn_implicits_in_term ?loc:a2.CAst.loc ()
      in
      aux impsty impsbody
    | _ :: _, [] | [], _ :: _ -> (* Information only on one side *) ()
    | [], [] -> () in
  aux impsty impsbody

let protect_pattern_in_binder bl c ctypopt =
  (* We turn "Definition d binders := body : typ" into *)
  (* "Definition d := fun binders => body:type" *)
  (* This is a hack while waiting for LocalPattern in regular environments *)
  if List.exists (function Constrexpr.CLocalPattern _ -> true | _ -> false) bl
  then
    let t = match ctypopt with
      | None -> CAst.make ?loc:c.CAst.loc (Constrexpr.CHole (None))
      | Some t -> t in
    let loc = Loc.merge_opt c.CAst.loc t.CAst.loc in
    let c = CAst.make ?loc @@ Constrexpr.CCast (c, Some Constr.DEFAULTcast, t) in
    let loc = match List.hd bl with
      | Constrexpr.CLocalAssum (a::_,_,_,_) | Constrexpr.CLocalDef (a,_,_,_) -> a.CAst.loc
      | Constrexpr.CLocalPattern {CAst.loc} -> loc
      | Constrexpr.CLocalAssum ([],_,_,_) -> assert false in
    let apply_under_binders f env evd c =
      let rec aux env evd c =
        let open Constr in
        let open EConstr in
        let open Context.Rel.Declaration in
        match kind evd c with
        | Lambda (x,t,c)  ->
          let evd,c = aux (push_rel (LocalAssum (x,t)) env) evd c in
          evd, mkLambda (x,t,c)
        | LetIn (x,b,t,c) ->
          let evd,c = aux (push_rel (LocalDef (x,b,t)) env) evd c in
          evd, mkLetIn (x,t,b,c)
        | Case (ci,u,pms,p,iv,a,bl) ->
          let (ci, p, iv, a, bl) = EConstr.expand_case env evd (ci, u, pms, p, iv, a, bl) in
          let evd,bl = Array.fold_left_map (aux env) evd bl in
          evd, mkCase (EConstr.contract_case env evd (ci, p, iv, a, bl))
        | Cast (c,_,_)    -> f env evd c  (* we remove the cast we had set *)
        (* This last case may happen when reaching the proof of an
           impossible case, as when pattern-matching on a vector of length 1 *)
        | _ -> (evd,c) in
      aux env evd c in
    ([], Constrexpr_ops.mkLambdaCN ?loc:(Loc.merge_opt loc c.CAst.loc) bl c, None, apply_under_binders)
  else
    (bl, c, ctypopt, fun f env evd c -> f env evd c)

let interp_definition ~program_mode env evd impl_env bl red_option c ctypopt =
  let flags = Pretyping.{ all_no_fail_flags with program_mode } in
  let (bl, c, ctypopt, apply_under_binders) = protect_pattern_in_binder bl c ctypopt in
  (* Build the parameters *)
  let evd, (impls, ((env_bl, ctx), imps1)) = interp_context_evars ~program_mode ~impl_env env evd bl in
  (* Build the type *)
  let evd, tyopt = Option.fold_left_map
      (interp_type_evars_impls ~flags ~impls env_bl)
      evd ctypopt
  in
  (* Build the body, and merge implicits from parameters and from type/body *)
  let evd, c, imps, tyopt =
    match tyopt with
    | None ->
      let evd, (c, impsbody) = interp_constr_evars_impls ~program_mode ~impls env_bl evd c in
      evd, c, imps1@impsbody, None
    | Some (ty, impsty) ->
      let evd, (c, impsbody) = interp_casted_constr_evars_impls ~program_mode ~impls env_bl evd c ty in
      check_imps ~impsty ~impsbody;
      evd, c, imps1@impsty, Some ty
  in
  (* Do the reduction *)
  let evd, c = apply_under_binders (red_constant_body red_option) env_bl evd c in

  (* Declare the definition *)
  let c = EConstr.it_mkLambda_or_LetIn c ctx in
  let tyopt = Option.map (fun ty -> EConstr.it_mkProd_or_LetIn ty ctx) tyopt in
  evd, (c, tyopt), imps

let do_definition ?hook ~name ?scope ?clearbody ~poly ?typing_flags ~kind ?using ?user_warns udecl bl red_option c ctypopt =
  let program_mode = false in
  let env = Global.env() in
  let env = Environ.update_typing_flags ?typing_flags env in
  (* Explicitly bound universes and constraints *)
  let evd, udecl = interp_univ_decl_opt env udecl in
  let evd, (body, types), impargs =
    interp_definition ~program_mode env evd empty_internalization_env bl red_option c ctypopt
  in
  let kind = Decls.IsDefinition kind in
  let cinfo = Declare.CInfo.make ~name ~impargs ~typ:types () in
  let info = Declare.Info.make ?scope ?clearbody ~kind ?hook ~udecl ~poly ?typing_flags ?user_warns () in
  let ref : Names.GlobRef.t =
    Declare.declare_definition ~info ~cinfo ~opaque:false ~body ?using evd
  in
  let env = Global.env () in
  let evd, c = Evd.fresh_global env evd ref in
  let type_ = types |> Option.default (Retyping.get_type_of env evd c) in
  let evd, equations = compute_equations env evd [] type_ c body in
  let equations =
    match ref with
    | ConstRef x when Names.Constant.to_string x = "Coq.Init.Logic.not" ->
      equations |> List.map (
        Termops.replace_term evd
          c
          (EConstr.mkVar (Names.Id.of_string_soft "~"))
      )
    | ConstRef x when Names.Constant.to_string x = "Coq.Init.Logic.iff" ->
      equations |> List.map (
        Termops.replace_term evd
          (EConstr.mkApp (c, [|EConstr.mkRel 0; EConstr.mkRel (-1)|]))
          (EConstr.mkApp (EConstr.mkRel 0, [|(EConstr.mkVar (Names.Id.of_string_soft "<->")); EConstr.mkRel (-1)|]))
      )
    | _ -> equations in
  declarations := !declarations @ [Declaration.{
    path = Nametab.path_of_global ref;
    type_ = print_constr env evd type_;
    kind = Definition {
      value = print_constr env evd body;
      equations = equations |> List.map (print_constr env evd);
    };
  }];
  ()

let do_definition_program ?hook ~pm ~name ~scope ?clearbody ~poly ?typing_flags ~kind ?using ?user_warns udecl bl red_option c ctypopt =
  let () = if not poly then udecl |> Option.iter (fun udecl ->
      if not udecl.UState.univdecl_extensible_instance
      || not udecl.UState.univdecl_extensible_constraints
      then
        CErrors.user_err
          Pp.(str "Non extensible universe declaration not supported \
                   with monomorphic Program Definition."))
  in
  let env = Global.env() in
  let env = Environ.update_typing_flags ?typing_flags env in
  (* Explicitly bound universes and constraints *)
  let evd, udecl = interp_univ_decl_opt env udecl in
  let evd, (body, types), impargs =
    interp_definition ~program_mode:true env evd empty_internalization_env bl red_option c ctypopt
  in
  let term, typ, uctx, obls = Declare.Obls.prepare_obligation ~name ~body ~types evd in
  let pm, _ =
    let cinfo = Declare.CInfo.make ~name ~typ ~impargs () in
    let info = Declare.Info.make ~udecl ~scope ?clearbody ~poly ~kind ?hook ?typing_flags ?user_warns () in
    Declare.Obls.add_definition ~pm ~cinfo ~info ~term ~uctx ?using obls
  in
  pm
