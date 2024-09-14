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
open Constr
open EConstr
open Namegen
open Tactypes
open Genarg
open Stdarg
open Tacarg
open Geninterp
open Pp
open Proofview.Notations

exception CannotCoerceTo of string

let base_val_typ wit =
  match val_tag (topwit wit) with Val.Base t -> t | _ -> CErrors.anomaly (Pp.str "Not a base val.")

let (wit_constr_context : (Empty.t, Empty.t, Constr_matching.context) Genarg.genarg_type) =
  let wit = Genarg.create_arg "constr_context" in
  let () = register_val0 wit None in
  let pr env sigma lev c : Pp.t = Printer.pr_econstr_n_env env sigma lev (Constr_matching.repr_context c) in
  let () = Genprint.register_val_print0 (base_val_typ wit) (Pptactic.make_constr_printer pr) in
  wit

(* includes idents known to be bound and references *)
let (wit_constr_under_binders : (Empty.t, Empty.t, Ltac_pretype.constr_under_binders) Genarg.genarg_type) =
  let wit = Genarg.create_arg "constr_under_binders" in
  let () = register_val0 wit None in
  let () = Genprint.register_val_print0 (base_val_typ wit)
             (fun c ->
               Genprint.TopPrinterNeedsContext (fun env sigma -> Printer.pr_constr_under_binders_env env sigma c)) in
  wit

let (wit_value : (Val.t, Val.t, Val.t) genarg_type) =
  let wit = Genarg.create_arg "value" in
  register_val0 wit None;
  Genprint.register_print0
    wit
    (Genprint.generic_val_print %> Genprint.printer_result_of_top_printer_result)
    (Genprint.generic_val_print %> Genprint.printer_result_of_top_printer_result)
    Genprint.generic_val_print;
  wit

let (wit_value_of_corrent_type : (Val.t, Val.t, Val.t) genarg_type) =
  let wit = Genarg.create_arg "value_of_correct_type" in
  register_val0 wit None;
  Genprint.register_print0
    wit
    (Genprint.generic_val_print %> Genprint.printer_result_of_top_printer_result)
    (Genprint.generic_val_print %> Genprint.printer_result_of_top_printer_result)
    Genprint.generic_val_print;
  wit

let rec project_tag : type a. a Val.tag -> Val.t -> a option = fun t (Dyn (typ, x) as v) ->
  match t with
  | Val.Base t -> (
    let Val.Dyn (t', x) = v in
    match Val.eq t t' with
    | Some Refl -> Some x
    | None -> None
  )
  | Val.List t -> (
    match Val.eq typ Val.typ_list with
    | Some Refl ->
      let l = x |> List.map (project_tag t) in
      if l |> List.for_all Option.has_some
      then Some (l |> List.map Option.get)
      else None
    | None -> None
  )
  | Val.Opt t -> (
    match Val.eq typ Val.typ_opt with
    | Some Refl -> (
      match x with
      | Some y -> (
        match project_tag t y with
        | Some z -> Some (Some z)
        | None -> None
      )
      | None -> Some None
    )
    | None -> None
  )
  | Val.Pair (t1, t2) -> (
    match Val.eq typ Val.typ_pair with
    | Some Refl -> (
      let (y, z) = x in
      match project_tag t1 y, project_tag t2 z with
      | Some y', Some z' -> Some (y', z')
      | _, _ -> None
    )
    | None -> None
  )

let project : type a. a typed_abstract_argument_type -> Val.t -> a option = fun wit v ->
  project_tag (val_tag wit) v

(** All the types considered here are base types *)
let val_tag wit = match val_tag wit with
| Val.Base t -> t
| _ -> assert false

let has_type : type a. Val.t -> a typed_abstract_argument_type -> bool = fun v wit ->
  let Val.Dyn (t, _) = v in
  match Val.eq t (val_tag wit) with
  | None -> false
  | Some Refl -> true

let prj : type a. a Val.typ -> Val.t -> a option = fun t v ->
  let Val.Dyn (t', x) = v in
  match Val.eq t t' with
  | None -> None
  | Some Refl -> Some x

let in_gen wit v = Val.Dyn (val_tag wit, v)
let out_gen wit v = match prj (val_tag wit) v with None -> assert false | Some x -> x

module Value =
struct

type t = Val.t

let of_constr c = in_gen (topwit wit_constr) c

let to_constr v =
  if has_type v (topwit wit_constr) then
    let c = out_gen (topwit wit_constr) v in
    Some c
  else if has_type v (topwit wit_constr_under_binders) then
    let vars, c = out_gen (topwit wit_constr_under_binders) v in
    match vars with [] -> Some c | _ -> None
  else None

let of_uconstr c = in_gen (topwit wit_uconstr) c

let to_uconstr v =
  if has_type v (topwit wit_uconstr) then
    Some (out_gen (topwit wit_uconstr) v)
  else None

let of_int i = in_gen (topwit wit_int) i

let to_int v =
  if has_type v (topwit wit_int) then
    Some (out_gen (topwit wit_int) v)
  else None

let of_ident id = in_gen (topwit wit_ident) id

let to_ident v =
  if has_type v (topwit wit_ident) then
    Some (out_gen (topwit wit_ident) v)
  else None

let to_list v = prj Val.typ_list v

let to_option v = prj Val.typ_opt v

let to_pair v = prj Val.typ_pair v

let cast_error wit v =
  let pr_v =
    let env = Global.env () in
    let sigma = Evd.from_env env in
    Pptactic.pr_value ~context:(env, sigma) Pptactic.ltop v in
  let Val.Dyn (tag, _) = v in
  let tag = Val.pr tag in
  CErrors.user_err (str "Type error: value " ++ pr_v ++ str " is a " ++ tag
    ++ str " while type " ++ Val.pr wit ++ str " was expected.")

let unbox wit v ans = match ans with
| None -> cast_error wit v
| Some x -> x

let rec prj : type a. a Val.tag -> Val.t -> a = fun tag v -> match tag with
| Val.List tag -> List.map (fun v -> prj tag v) (unbox Val.typ_list v (to_list v))
| Val.Opt tag -> Option.map (fun v -> prj tag v) (unbox Val.typ_opt v (to_option v))
| Val.Pair (tag1, tag2) ->
  let (x, y) = unbox Val.typ_pair v (to_pair v) in
  (prj tag1 x, prj tag2 y)
| Val.Base t ->
  let Val.Dyn (t', x) = v in
  match Val.eq t t' with
  | None -> cast_error t v
  | Some Refl -> x
let rec tag_of_arg : type a b c. (a, b, c) genarg_type -> c Val.tag = fun wit -> match wit with
| ExtraArg _ -> Geninterp.val_tag (topwit wit)
| ListArg t -> Val.List (tag_of_arg t)
| OptArg t -> Val.Opt (tag_of_arg t)
| PairArg (t1, t2) -> Val.Pair (tag_of_arg t1, tag_of_arg t2)

let val_cast arg v = prj (tag_of_arg arg) v

let cast (Topwit wit) v = val_cast wit v

end

let is_variable env id =
  Id.List.mem id (Termops.ids_of_named_context (Environ.named_context env))

(* Transforms an id into a constr if possible, or fails with Not_found *)
let constr_of_id env id =
  EConstr.mkVar (let _ = Environ.lookup_named id env in id)

(* Gives the constr corresponding to a Constr_context tactic_arg *)
let coerce_to_constr_context v =
  if has_type v (topwit wit_constr_context) then
    out_gen (topwit wit_constr_context) v
  else raise (CannotCoerceTo "a term context")

let is_intro_pattern v =
  if has_type v (topwit wit_intro_pattern) then
    Some (out_gen (topwit wit_intro_pattern) v).CAst.v
  else
    None

(* Interprets an identifier which must be fresh *)
let coerce_var_to_ident fresh env sigma v =
  let fail () = raise (CannotCoerceTo "a fresh identifier") in
  match is_intro_pattern v with
  | Some (IntroNaming (IntroIdentifier id)) -> id
  | Some _ -> fail ()
  | None ->
  if has_type v (topwit wit_intro_pattern) then
    match out_gen (topwit wit_intro_pattern) v with
    | { CAst.v=IntroNaming (IntroIdentifier id)} -> id
    | _ -> fail ()
  else if has_type v (topwit wit_hyp) then
    out_gen (topwit wit_hyp) v
  else match Value.to_constr v with
  | None -> fail ()
  | Some c ->
    (* We need it fresh for intro e.g. in "Tac H = clear H; intro H" *)
    if isVar sigma c && not (fresh && is_variable env (destVar sigma c)) then
      destVar sigma c
    else fail ()


(* Interprets, if possible, a constr to an identifier which may not
   be fresh but suitable to be given to the fresh tactic. Works for
   vars, constants, inductive, constructors and sorts. *)
let coerce_to_ident_not_fresh sigma v =
let id_of_name = function
  | Name.Anonymous -> Id.of_string "x"
  | Name.Name x -> x in
  let fail () = raise (CannotCoerceTo "an identifier") in
  match is_intro_pattern v with
  | Some (IntroNaming (IntroIdentifier id)) -> id
  | Some _ -> fail ()
  | None ->
  if has_type v (topwit wit_hyp) then
    out_gen (topwit wit_hyp) v
  else
    match Value.to_constr v with
    | None -> fail ()
    | Some c ->
       match EConstr.kind sigma c with
       | Var id -> id
       | Meta m -> id_of_name (Evd.meta_name sigma m)
       | Evar (kn,_) ->
        begin match Evd.evar_ident kn sigma with
        | None -> fail ()
        | Some id -> id
        end
       | Const (cst,_) -> Label.to_id (Constant.label cst)
       | Construct (cstr,_) ->
          let ref = GlobRef.ConstructRef cstr in
          let basename = Nametab.basename_of_global ref in
          basename
       | Ind (ind,_) ->
          let ref = GlobRef.IndRef ind in
          let basename = Nametab.basename_of_global ref in
          basename
       | Sort s ->
          begin
            match ESorts.kind sigma s with
            | Sorts.SProp -> Label.to_id (Label.make "SProp")
            | Sorts.Prop -> Label.to_id (Label.make "Prop")
            | Sorts.Set -> Label.to_id (Label.make "Set")
            | Sorts.Type _ | Sorts.QSort _ -> Label.to_id (Label.make "Type")
          end
       | _ -> fail()


let coerce_to_intro_pattern sigma v =
  match is_intro_pattern v with
  | Some pat -> pat
  | None ->
  if has_type v (topwit wit_hyp) then
    let id = out_gen (topwit wit_hyp) v in
    IntroNaming (IntroIdentifier id)
  else match Value.to_constr v with
  | Some c when isVar sigma c ->
      (* This happens e.g. in definitions like "Tac H = clear H; intro H" *)
      (* but also in "destruct H as (H,H')" *)
      IntroNaming (IntroIdentifier (destVar sigma c))
  | _ -> raise (CannotCoerceTo "an introduction pattern")

let coerce_to_intro_pattern_naming sigma v =
  match coerce_to_intro_pattern sigma v with
  | IntroNaming pat -> pat
  | _ -> raise (CannotCoerceTo "a naming introduction pattern")

let coerce_to_hint_base v =
  match is_intro_pattern v with
  | Some (IntroNaming (IntroIdentifier id)) -> Id.to_string id
  | Some _ | None -> raise (CannotCoerceTo "a hint base name")

let coerce_to_int v =
  if has_type v (topwit wit_int) then
    out_gen (topwit wit_int) v
  else raise (CannotCoerceTo "an integer")

let coerce_to_constr env v =
  let fail () = raise (CannotCoerceTo "a term") in
  match is_intro_pattern v with
  | Some (IntroNaming (IntroIdentifier id)) ->
      (try ([], constr_of_id env id) with Not_found -> fail ())
  | Some _ -> fail ()
  | None ->
  if has_type v (topwit wit_constr) then
    let c = out_gen (topwit wit_constr) v in
    ([], c)
  else if has_type v (topwit wit_constr_under_binders) then
    out_gen (topwit wit_constr_under_binders) v
  else if has_type v (topwit wit_hyp) then
    let id = out_gen (topwit wit_hyp) v in
    (try [], constr_of_id env id with Not_found -> fail ())
  else fail ()

let coerce_to_uconstr v =
  if has_type v (topwit wit_uconstr) then
    out_gen (topwit wit_uconstr) v
  else
    raise (CannotCoerceTo "an untyped term")

let coerce_to_closed_constr env v =
  let ids,c = coerce_to_constr env v in
  let () = if not (List.is_empty ids) then raise (CannotCoerceTo "a term") in
  c

let coerce_to_evaluable_ref env sigma v =
  let open Evaluable in
  let fail () = raise (CannotCoerceTo "an evaluable reference") in
  let ev =
  match is_intro_pattern v with
  | Some (IntroNaming (IntroIdentifier id)) when is_variable env id -> EvalVarRef id
  | Some _ -> fail ()
  | None ->
  if has_type v (topwit wit_hyp) then
    let id = out_gen (topwit wit_hyp) v in
    if Id.List.mem id (Termops.ids_of_context env) then EvalVarRef id
    else fail ()
  else if has_type v (topwit wit_ref) then
    let open GlobRef in
    let r = out_gen (topwit wit_ref) v in
    match r with
    | VarRef var -> EvalVarRef var
    | ConstRef c -> EvalConstRef c
    | IndRef _ | ConstructRef _ -> fail ()
  else if has_type v (topwit wit_smart_global) then
    let open GlobRef in
    let r = out_gen (topwit wit_smart_global) v in
    match r with
    | VarRef var -> EvalVarRef var
    | ConstRef c -> EvalConstRef c
    | IndRef _ | ConstructRef _ -> fail ()
  else
    match Value.to_constr v with
    | Some c when isConst sigma c -> EvalConstRef (fst (destConst sigma c))
    | Some c when isVar sigma c -> EvalVarRef (destVar sigma c)
    | _ -> fail ()
  in if Tacred.is_evaluable env ev then ev else fail ()

let coerce_to_constr_list env v =
  let v = Value.to_list v in
  match v with
  | Some l ->
    let map v = coerce_to_closed_constr env v in
    List.map map l
  | None -> raise (CannotCoerceTo "a term list")

let coerce_to_intro_pattern_list ?loc sigma v =
  match Value.to_list v with
  | None -> raise (CannotCoerceTo "an intro pattern list")
  | Some l ->
    let map v = CAst.make ?loc @@ coerce_to_intro_pattern sigma v in
    List.map map l

let coerce_to_hyp env sigma v =
  let fail () = raise (CannotCoerceTo "a variable") in
  match is_intro_pattern v with
  | Some (IntroNaming (IntroIdentifier id)) when is_variable env id -> id
  | Some _ -> fail ()
  | None ->
  if has_type v (topwit wit_hyp) then
    let id = out_gen (topwit wit_hyp) v in
    if is_variable env id then id else fail ()
  else match Value.to_constr v with
  | Some c when isVar sigma c -> destVar sigma c
  | _ -> fail ()

let coerce_to_hyp_list env sigma v =
  let v = Value.to_list v in
  match v with
  | Some l ->
    let map n = coerce_to_hyp env sigma n in
    List.map map l
  | None -> raise (CannotCoerceTo "a variable list")

(* Interprets a qualified name *)
let coerce_to_reference sigma v =
  match Value.to_constr v with
  | Some c ->
    begin
      try fst (EConstr.destRef sigma c)
      with DestKO -> raise (CannotCoerceTo "a reference")
    end
  | None -> raise (CannotCoerceTo "a reference")

(* Quantified named or numbered hypothesis or hypothesis in context *)
(* (as in Inversion) *)
let coerce_to_quantified_hypothesis sigma v =
  match is_intro_pattern v with
  | Some (IntroNaming (IntroIdentifier id)) -> NamedHyp (CAst.make id)
  | Some _ -> raise (CannotCoerceTo "a quantified hypothesis")
  | None ->
  if has_type v (topwit wit_hyp) then
    let id = out_gen (topwit wit_hyp) v in
    NamedHyp (CAst.make id)
  else if has_type v (topwit wit_int) then
    AnonHyp (out_gen (topwit wit_int) v)
  else match Value.to_constr v with
  | Some c when isVar sigma c -> NamedHyp (CAst.make @@ destVar sigma c)
  | _ -> raise (CannotCoerceTo "a quantified hypothesis")

(* Quantified named or numbered hypothesis or hypothesis in context *)
(* (as in Inversion) *)
let coerce_to_decl_or_quant_hyp sigma v =
  if has_type v (topwit wit_int) then
    AnonHyp (out_gen (topwit wit_int) v)
  else
    try coerce_to_quantified_hypothesis sigma v
    with CannotCoerceTo _ ->
      raise (CannotCoerceTo "a declared or quantified hypothesis")

let coerce_to_int_list v =
  match Value.to_list v with
  | None -> raise (CannotCoerceTo "an int list")
  | Some l ->
    List.map coerce_to_int l

(** Abstract application, to print ltac functions *)
type appl =
  | UnnamedAppl (** For generic applications: nothing is printed *)
  | GlbAppl of (Names.KerName.t * Val.t list) list
       (** For calls to global constants, some may alias other. *)

let pr_argument_type arg =
  let Val.Dyn (tag, _) = arg in
  Val.pr tag

(** TODO: unify printing of generic Ltac values in case of coercion failure. *)

(* Displays a value *)
let pr_value env v =
  let ppty = spc() ++ str "of type" ++ spc () ++ pr_argument_type v in
  let pr_with_env pr =
    match env with
    | Some (env,sigma) -> pr env sigma ++ ppty
    | None -> str "a value" ++ ppty
  in
  let open Genprint in
  match generic_val_print v with
  | TopPrinterBasic pr -> pr () ++ ppty
  | TopPrinterNeedsContext pr -> pr_with_env pr
  | TopPrinterNeedsContextAndLevel { default_already_surrounded; printer } ->
     pr_with_env (fun env sigma -> printer env sigma default_already_surrounded)

(* Values for interpretation *)
type tacvalue =
  | VFun of
      appl *
      Tacexpr.ltac_trace *
      Loc.t option * (* when executing a global Ltac function: the location where this function was called *)
      Val.t Id.Map.t * (* closure *)
      Name.t list * (* binders *)
      Tacexpr.glob_tactic_expr (* body *)
  | VRec of Val.t Id.Map.t ref * Tacexpr.glob_tactic_expr

let (wit_tacvalue : (Empty.t, tacvalue, tacvalue) Genarg.genarg_type) =
  let wit = Genarg.create_arg "tacvalue" in
  register_val0 wit None;
  let pr_lfun env sigma lfun =
    Id.Map.bindings lfun
      |> List.map_i (fun i (name, value) ->
        hov 2 (
          Id.print name ++ str " :" ++ spc () ++ pr_argument_type value ++ str " :=" ++ spc () ++
          (let open Genprint in
          match generic_val_print value with
          | TopPrinterBasic pr -> pr ()
          | TopPrinterNeedsContext pr -> pr env sigma
          | TopPrinterNeedsContextAndLevel { default_already_surrounded; printer } ->
            printer env sigma default_already_surrounded) ++
          (if i = (Id.Map.cardinal lfun) - 1 then mt () else spc() ++ str "and")
        )
      ) 0
      |> prlist_with_sep (fun () -> spc ()) Fun.id in
  let seen_lfuns = ref [] in
  let pr v =
    Genprint.TopPrinterNeedsContext (fun env sigma ->
      match v with
      | VFun (a, _, loc, lfun, lvar, body) ->
        let tac = if lvar = [] then body else CAst.make ?loc (Tacexpr.TacFun (lvar, body)) in
        if Id.Map.is_empty lfun then
          Pptactic.pr_glob_tactic env sigma tac
        else
          hv 2 (
            h (Pptactic.pr_glob_tactic env sigma tac ++ spc () ++ str "where") ++ spc () ++
            pr_lfun env sigma lfun
          )
      | VRec (lfun, tac) ->
        hv 2 (
          h (Pptactic.pr_glob_tactic env sigma tac ++ spc () ++ str "where rec") ++ spc () ++
          match !seen_lfuns |> List.find_index (fun lfun' -> lfun' == lfun) with
          | None ->
            Flags.with_modified_ref seen_lfuns (fun seen_lfuns -> lfun :: seen_lfuns) (fun () ->
              pr_lfun env sigma !lfun
            ) ()
          | Some i ->
            str "<tactic closure on level " ++ int i ++ str ">"
        )
    ) in
  Genprint.register_print0 wit Empty.abort (pr %> Genprint.printer_result_of_top_printer_result) pr;
  wit

let wit_atomic_tactic : (Empty.t, Empty.t, Tacexpr.atomic_tactic_expr) genarg_type =
  let wit = Genarg.create_arg "atomic_tactic" in
  let () = register_val0 wit None in
  Pptactic.declare_extra_genarg_pprule_with_level
    wit
    (fun env sigma _ _ _ n -> Empty.abort)
    (fun env sigma _ _ _ n -> Empty.abort)
    (fun env sigma _ _ _ n tac ->
      Pptactic.pr_atomic_tactic env sigma tac
    )
    Ppconstr.ltop Ppconstr.lsimpleconstr;
  wit

exception CoercionError of Id.t * (Environ.env * Evd.evar_map) option * Val.t * string

let () = CErrors.register_handler begin function
| CoercionError (id, env, v, s) ->
  Some (str "Ltac variable " ++ Id.print id ++
   strbrk " is bound to" ++ spc () ++ pr_value env v ++ spc () ++
   strbrk "which cannot be coerced to " ++ str s ++ str".")
| _ -> None
end

let error_ltac_variable ?loc id env v s =
  Loc.raise ?loc (CoercionError (id, env, v, s))

(** {5 Late args} *)

type late_args_map = raw_generic_argument LateArgMap.t * glob_generic_argument LateArgMap.t

let f_late_args_map : late_args_map Evd.Store.field = Evd.Store.field "late_args_map"

let retrieve_raw_late_arg late_arg =
  Proofview.tclEVARMAP >>= fun sigma ->
  let store = Evd.get_extra_data sigma in
  let late_args_map = Evd.Store.get store f_late_args_map |> Option.default (LateArgMap.empty, LateArgMap.empty) in
  Proofview.tclUNIT (fst late_args_map |> LateArgMap.find_opt late_arg)

let populate_raw_late_arg late_arg v =
  Proofview.tclEVARMAP >>= fun sigma ->
  let store = Evd.get_extra_data sigma in
  let late_args_map = Evd.Store.get store f_late_args_map |> Option.default (LateArgMap.empty, LateArgMap.empty) in
  let late_args_map = (
    (match v with
    | None -> fst late_args_map |> LateArgMap.remove late_arg
    | Some v -> fst late_args_map |> LateArgMap.add late_arg v),
    snd late_args_map
  ) in
  let store = Evd.Store.set store f_late_args_map late_args_map in
  let sigma = Evd.set_extra_data store sigma in
  Proofview.Unsafe.tclEVARS sigma

let wrap_populate_raw_late_arg late_arg v tac =
  retrieve_raw_late_arg late_arg >>= fun initial ->
  populate_raw_late_arg late_arg v <*>
  tac >>= fun r ->
  populate_raw_late_arg late_arg initial <*>
  Proofview.tclUNIT r

let retrieve_glob_late_arg late_arg =
  Proofview.tclEVARMAP >>= fun sigma ->
  let store = Evd.get_extra_data sigma in
  let late_args_map = Evd.Store.get store f_late_args_map |> Option.default (LateArgMap.empty, LateArgMap.empty) in
  Proofview.tclUNIT (snd late_args_map |> LateArgMap.find_opt late_arg)

let populate_glob_late_arg late_arg v =
  Proofview.tclEVARMAP >>= fun sigma ->
  let store = Evd.get_extra_data sigma in
  let late_args_map = Evd.Store.get store f_late_args_map |> Option.default (LateArgMap.empty, LateArgMap.empty) in
  let late_args_map = (
    fst late_args_map,
    (match v with
    | None -> snd late_args_map |> LateArgMap.remove late_arg
    | Some v -> snd late_args_map |> LateArgMap.add late_arg v)
  ) in
  let store = Evd.Store.set store f_late_args_map late_args_map in
  let sigma = Evd.set_extra_data store sigma in
  (* Printf.eprintf "!! %d %d\n" (Obj.magic late_arg) (snd late_args_map |> LateArgMap.cardinal); *)
  Proofview.Unsafe.tclEVARS sigma

let wrap_populate_glob_late_arg late_arg v tac =
  retrieve_glob_late_arg late_arg >>= fun initial ->
  populate_glob_late_arg late_arg v <*>
  tac >>= fun r ->
  populate_glob_late_arg late_arg initial <*>
  Proofview.tclUNIT r

let wrap_keep_late_args t =
  Proofview.tclEVARMAP >>= fun sigma ->
  let store = Evd.get_extra_data sigma in
  let late_args_map = Evd.Store.get store f_late_args_map |> Option.default (LateArgMap.empty, LateArgMap.empty) in
  t >>= fun r ->
  let store = Evd.Store.set store f_late_args_map late_args_map in
  let sigma = Evd.set_extra_data store sigma in
  Proofview.Unsafe.tclEVARS sigma <*>
  Proofview.tclUNIT r

let set_glob_late_arg sigma late_arg v =
  let store = Evd.get_extra_data sigma in
  let late_args_map = Evd.Store.get store f_late_args_map |> Option.default (LateArgMap.empty, LateArgMap.empty) in
  let late_args_map = (
    fst late_args_map,
    (match v with
    | None -> snd late_args_map |> LateArgMap.remove late_arg
    | Some v -> snd late_args_map |> LateArgMap.add late_arg v)
  ) in
  let store = Evd.Store.set store f_late_args_map late_args_map in
  let sigma = Evd.set_extra_data store sigma in
  (* Printf.eprintf "!! %d %d\n" (Obj.magic late_arg) (snd late_args_map |> LateArgMap.cardinal); *)
  sigma

let () =
  let () = register_val0 wit_late_arg None in
  Pptactic.declare_extra_genarg_pprule_with_level
    wit_late_arg
    (fun env sigma _ _ _ n (late_arg, default) ->
      let store = Evd.get_extra_data sigma in
      let late_args_map = Evd.Store.get store f_late_args_map |> Option.default (LateArgMap.empty, LateArgMap.empty) in
      Pputils.pr_raw_generic env sigma (Some n) (Option.append (fst late_args_map |> LateArgMap.find_opt late_arg) default |> Option.get)
    )
    (fun env sigma _ _ _ n (late_arg, default) ->
      let store = Evd.get_extra_data sigma in
      let late_args_map = Evd.Store.get store f_late_args_map |> Option.default (LateArgMap.empty, LateArgMap.empty) in
      (* Printf.eprintf "?? %d %d\n" (Obj.magic late_arg) (snd late_args_map |> LateArgMap.cardinal); *)
      Pputils.pr_glb_generic env sigma (Some n) (Option.append (snd late_args_map |> LateArgMap.find_opt late_arg) default |> Option.get)
    )
    (fun env sigma _ _ _ n -> Empty.abort)
    Ppconstr.ltop Ppconstr.lsimpleconstr

let glob_late_arg_tac_arg ?isquot ?default late_arg =
  Tacexpr.TacGeneric (isquot, GenArg (Glbwit wit_late_arg, (late_arg, default)))

let glob_late_arg_tac ?isquot ?default late_arg =
  CAst.make (Tacexpr.TacArg (glob_late_arg_tac_arg ?isquot ?default late_arg))

let prepare_named_delayed_open wit ?default delayed_open =
  Either.Left (
    (new_late_arg (), default),
    fun late_arg env sigma ->
      let (sigma, a) = delayed_open env sigma in
      let sigma = set_glob_late_arg sigma late_arg (Some (GenArg (glbwit wit_value, in_gen (topwit wit) (Either.Right a)))) in
      (sigma, a)
  )

let prepare_named_delayed_open_1 wit ?default delayed_open =
  let late_arg = new_late_arg () in
  fun x ->
    Either.Left (
      (late_arg, default),
      fun late_arg env sigma ->
        let (sigma, a) = delayed_open x env sigma in
        let sigma = set_glob_late_arg sigma late_arg (Some (GenArg (glbwit wit_value, in_gen (topwit wit) (Either.Right a)))) in
        (sigma, a)
    )

let clone_named_delayed_open named_delayed_open =
  match named_delayed_open with
  | Either.Left ((_, default), f) -> Either.Left ((new_late_arg (), default), f)
  | Either.Right x -> Either.Right x

let clone_destruction_arg = function
  | clear, Tactics.ElimOnConstr c -> clear, Tactics.ElimOnConstr (clone_named_delayed_open c)
  | clear, Tactics.ElimOnIdent n -> clear, Tactics.ElimOnIdent n
  | clear, Tactics.ElimOnAnonHyp n -> clear, Tactics.ElimOnAnonHyp n

let rec clone_intro_pattern x =
  x |> CAst.map (function
    | IntroAction p -> IntroAction (clone_intro_pattern_action p)
    | IntroNaming _ | IntroForthcoming _ as x -> x
  )

and clone_intro_pattern_action = let open CAst in function
  | IntroApplyOn ({loc; v = t},pat) ->
    IntroApplyOn (make ?loc @@ clone_named_delayed_open t, clone_intro_pattern pat)
  | IntroOrAndPattern l ->
    IntroOrAndPattern (clone_intro_or_and_pattern l)
  | IntroInjection l -> IntroInjection (List.map clone_intro_pattern l)
  | IntroWildcard | IntroRewrite _ as x -> x

and clone_intro_or_and_pattern = function
  | IntroAndPattern l ->
    IntroAndPattern (List.map (clone_intro_pattern) l)
  | IntroOrPattern ll ->
    IntroOrPattern (List.map (List.map (clone_intro_pattern)) ll)


let rec clone val_ =
  let Val.Dyn (typ, x) = val_ in
  match Val.eq typ Val.typ_list with
  | Some Refl -> Val.Dyn (typ, x |> List.map clone)
  | None ->
    match Val.eq typ Val.typ_opt with
    | Some Refl -> Val.Dyn (typ, x |> Option.map clone)
    | None ->
      match Val.eq typ Val.typ_pair with
      | Some Refl -> Val.Dyn (typ, (fst x |> clone, snd x |> clone))
      | None ->
        match prj (val_tag (Topwit wit_bindings)) val_ with
        | Some y -> Val.Dyn (val_tag (Topwit wit_bindings), clone_named_delayed_open y)
        | None ->
          match prj (val_tag (Topwit wit_constr_with_bindings)) val_ with
          | Some y -> Val.Dyn (val_tag (Topwit wit_constr_with_bindings), clone_named_delayed_open y)
          | None ->
            match prj (val_tag (Topwit wit_open_constr_with_bindings)) val_ with
            | Some y -> Val.Dyn (val_tag (Topwit wit_open_constr_with_bindings), clone_named_delayed_open y)
            | None ->
              match prj (val_tag (Topwit wit_destruction_arg)) val_ with
              | Some y -> Val.Dyn (val_tag (Topwit wit_destruction_arg), clone_destruction_arg y)
              | None ->
                match prj (val_tag (Topwit wit_intro_pattern)) val_ with
                | Some y -> Val.Dyn (val_tag (Topwit wit_intro_pattern), clone_intro_pattern y)
                | None -> val_

let debug_deref = CDebug.create ~name:"deref" ()

let _ =
  let f : type l. Environ.env -> Evd.evar_map -> argument_type option -> l generic_argument -> l generic_argument option = fun env sigma argument_type ->
    let store = Evd.get_extra_data sigma in
    let late_args_map = Evd.Store.get store f_late_args_map |> Option.default (LateArgMap.empty, LateArgMap.empty) in
    function (GenArg (wit, arg) as x) -> match wit with
    | Rawwit (ExtraArg tag) when Genarg.has_type x (Rawwit wit_late_arg) -> (
      let (late_arg, default) = Genarg.out_gen (Rawwit wit_late_arg) x in
      match fst late_args_map |> LateArgMap.find_opt late_arg with
      | Some v -> Some v
      | None -> default
    )
    | Rawwit (ExtraArg tag) when Genarg.has_type x (Rawwit wit_tactic) -> (
      let tac = Genarg.out_gen (Rawwit wit_tactic) x in
      match tac.v with
      | TacArg (TacGeneric (_, v)) -> Some v
      | _ -> None
    )
    | Rawwit (ExtraArg tag) when Genarg.has_type x (Rawwit wit_value) -> (
      let (Dyn (typ, y) as val_) = Genarg.out_gen (Rawwit wit_value) x in
      match argument_type with
      | Some (ArgumentType argument_type) -> (
        match prj (val_tag (Topwit argument_type)) val_ with
        | Some z ->
          debug_deref (fun () -> Pp.str "prj!");
          Some (Genarg.in_gen (Rawwit wit_value_of_corrent_type) val_)
        | None -> None
      )
      | None ->
        match Val.eq typ Val.typ_list with
        | Some Refl -> Some (Genarg.in_gen (Rawwit (ListArg wit_value)) y)
        | None ->
          match Val.eq typ Val.typ_opt with
          | Some Refl -> Some (Genarg.in_gen (Rawwit (OptArg wit_value)) y)
          | None ->
            match Val.eq typ Val.typ_pair with
            | Some Refl -> Some (Genarg.in_gen (Rawwit (PairArg (wit_value, wit_value))) y)
            | None -> None
    )
    | Glbwit (ExtraArg tag) when Genarg.has_type x (Glbwit wit_late_arg) -> (
      let (late_arg, default) = Genarg.out_gen (Glbwit wit_late_arg) x in
      match snd late_args_map |> LateArgMap.find_opt late_arg with
      | Some v -> Some v
      | None -> default
    )
    | Glbwit (ExtraArg tag) when Genarg.has_type x (Glbwit wit_tactic) -> (
      let tac = Genarg.out_gen (Glbwit wit_tactic) x in
      match tac.v with
      | TacArg (TacGeneric (_, v)) -> Some v
      | _ ->
        let env = Global.env () in
        let sigma = Evd.from_env env in
        debug_deref (fun () -> Pp.str "unknown tactic: " ++ Pptactic.pr_glob_tactic env sigma tac);
        None
    )
    | Glbwit (ExtraArg tag) when Genarg.has_type x (Glbwit wit_value) -> (
      let (Dyn (typ, y) as val_) = Genarg.out_gen (Glbwit wit_value) x in
      match argument_type with
      | Some (ArgumentType argument_type) -> (
        match project (Topwit argument_type) val_ with
        | Some z ->
          debug_deref (fun () -> Pp.str "prj!");
          Some (Genarg.in_gen (Glbwit wit_value_of_corrent_type) val_)
        | None -> None
      )
      | None ->
        match Val.eq typ Val.typ_list with
        | Some Refl -> Some (Genarg.in_gen (Glbwit (ListArg wit_value)) y)
        | None ->
          match Val.eq typ Val.typ_opt with
          | Some Refl -> Some (Genarg.in_gen (Glbwit (OptArg wit_value)) y)
          | None ->
            match Val.eq typ Val.typ_pair with
            | Some Refl -> Some (Genarg.in_gen (Glbwit (PairArg (wit_value, wit_value))) y)
            | None -> None
    )
    | _ -> None in
  Pptactic.current_dereffer := {
    deref = fun env sigma argument_type x ->
      debug_deref (fun () -> Genarg.pr_argument_type (Genarg.genarg_tag x));
      match f env sigma argument_type x with
      | Some v -> !Pptactic.current_dereffer.deref env sigma argument_type v
      | None -> debug_deref (fun () -> Pp.str "==="); x
  }

(** {5 Printed args} *)

let () =
  let () = register_val0 wit_printed_arg None in
  Pptactic.declare_extra_genarg_pprule
    wit_printed_arg
    (fun env sigma _ _ _ msg -> msg ())
    (fun env sigma _ _ _ msg -> msg ())
    (fun env sigma _ _ _ msg -> msg ())

let glob_printed_arg_tac_arg ?isquot msg =
  Tacexpr.TacGeneric (isquot, GenArg (Glbwit wit_printed_arg, msg))

let glob_printed_arg_tac ?isquot msg =
  CAst.make (Tacexpr.TacArg (glob_printed_arg_tac_arg ?isquot msg))
