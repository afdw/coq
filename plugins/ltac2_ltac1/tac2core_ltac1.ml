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
open Pp
open Names
open Genarg
open Ltac2_plugin
open Tac2val
open Tac2ffi
open Tac2env
open Tac2expr
open Proofview.Notations
open Tac2externals

let ltac2_ltac1_plugin = "coq-core.plugins.ltac2_ltac1"

let pname ?(plugin=ltac2_ltac1_plugin) s = { mltac_plugin = plugin; mltac_tactic = s }

let define ?plugin s = define (pname ?plugin s)

let return x = Proofview.tclUNIT x

(** Ltac1 in Ltac2 *)

let ltac1 = Tac2ffi.repr_ext Tac2ffi.val_ltac1
let of_ltac1 v = Tac2ffi.of_ext Tac2ffi.val_ltac1 v

let () =
  define "ltac1_ref" (list ident @-> ret ltac1) @@ fun ids ->
  let open Ltac_plugin in
  let r =
    match ids with
    | [] -> raise Not_found
    | _ :: _ as ids ->
      let (id, path) = List.sep_last ids in
      let path = DirPath.make (List.rev path) in
      let fp = Libnames.make_path path id in
      if Tacenv.exists_tactic fp then
        List.hd (Tacenv.locate_extended_all_tactic (Libnames.qualid_of_path fp))
      else raise Not_found
  in
  Tacinterp.Value.of_closure (Tacinterp.default_ist ()) (Tacenv.interp_ltac r)

let () =
  define "ltac1_run" (ltac1 @-> tac unit) @@ fun v ->
  let open Ltac_plugin in
  Tacinterp.tactic_of_value (Tacinterp.default_ist ()) v

let () =
  define "ltac1_apply" (ltac1 @-> list ltac1 @-> closure @-> tac unit) @@ fun f args k ->
  let open Ltac_plugin in
  let open Tacexpr in
  let open Locus in
  let k ret =
    Proofview.Trace.tag_deferred_contents ret.Proofview.Tagged.deferred_id (
      Proofview.tclIGNORE (Tac2val.apply k [Tac2ffi.of_ext val_ltac1 ret.Proofview.Tagged.v])
    )
  in
  let fold arg (i, vars, lfun) =
    let id = Id.of_string ("x" ^ string_of_int i) in
    let x = Reference (ArgVar CAst.(make id)) in
    (succ i, x :: vars, Id.ObservableMap.add id arg lfun)
  in
  let (_, args, lfun) = List.fold_right fold args (0, [], Id.Map.empty) in
  let lfun = Id.ObservableMap.add (Id.of_string "F") f lfun in
  let ist = { (Tacinterp.default_ist ()) with Tacinterp.lfun = lfun; } in
  let tac = CAst.make @@ TacArg (TacCall (CAst.make (ArgVar CAst.(make @@ Id.of_string "F"),args))) in
  Tacinterp.val_interp ist tac k

let () =
  define "ltac1_of_int" (int @-> ret ltac1)
    Ltac_plugin.Tacinterp.Value.of_int

let () =
  define "ltac1_to_int" (ltac1 @-> ret (option int))
    Ltac_plugin.Tacinterp.Value.to_int

let () =
  define "ltac1_of_constr" (constr @-> ret ltac1)
    Ltac_plugin.Tacinterp.Value.of_constr

let () =
  define "ltac1_to_constr" (ltac1 @-> ret (option constr))
    Ltac_plugin.Tacinterp.Value.to_constr

let () =
  define "ltac1_of_preterm" (preterm @-> ret ltac1)
    Ltac_plugin.Taccoerce.Value.of_uconstr

let () =
  define "ltac1_to_preterm" (ltac1 @-> ret (option preterm))
    Ltac_plugin.Taccoerce.Value.to_uconstr

let () =
  define "ltac1_of_ident" (ident @-> ret ltac1)
    Ltac_plugin.Taccoerce.Value.of_ident

let () =
  define "ltac1_to_ident" (ltac1 @-> ret (option ident))
    Ltac_plugin.Taccoerce.Value.to_ident

let () =
  define "ltac1_of_list" (list ltac1 @-> ret ltac1) @@ fun l ->
  Geninterp.Val.(inject (Base typ_list) l)

let () =
  define "ltac1_to_list" (ltac1 @-> ret (option (list ltac1)))
    Ltac_plugin.Tacinterp.Value.to_list

let () =
  define "ltac1_tag_name" (ltac1 @-> ret string) @@ fun (Dyn (tag,_)) ->
  Geninterp.Val.repr tag

let gtypref kn = GTypRef (Other kn, [])

open Tac2core.Core

let core_prefix path n = KerName.make path (Label.of_id (Id.of_string_soft n))
let ltac1_core n = core_prefix Tac2env.ltac1_prefix n
let t_ltac1 = ltac1_core "t"

let () =
  let intern self ist (ids, tac) =
    let map { CAst.v = id } = id in
    let ids = List.map map ids in
    (* Prevent inner calls to Ltac2 values *)
    let extra = Tac2intern.drop_ltac2_env ist.Genintern.extra in
    let ltacvars = List.fold_right Id.Set.add ids ist.Genintern.ltacvars in
    let ist = { ist with Genintern.extra; ltacvars } in
    let _, tac = Genintern.intern Ltac_plugin.Tacarg.wit_tactic ist tac in
    let fold ty _ = GTypArrow (gtypref t_ltac1, ty) in
    let ty = List.fold_left fold (gtypref t_unit) ids in
    GlbVal (ids, tac), ty
  in
  let interp _ (ids, tac) =
    let clos args =
      let add lfun id v =
        let v = Tac2ffi.to_ext val_ltac1 v in
        Id.ObservableMap.add id v lfun
      in
      let lfun = List.fold_left2 add Id.Map.empty ids args in
      let ist = { env_ist = Id.Map.empty } in
      let lfun = Tac2interp.set_env ist lfun in
      let ist = Ltac_plugin.Tacinterp.default_ist () in
      let ist = { ist with Geninterp.lfun = lfun } in
      let tac = (Ltac_plugin.Tacinterp.eval_tactic_ist ist tac : unit Proofview.tactic) in
      tac >>= fun () ->
      return v_unit
    in
    let len = List.length ids in
    if Int.equal len 0 then
      clos []
    else
      return (Tac2ffi.of_closure (Tac2val.abstract len clos))
  in
  let subst s (ids, tac) = (ids, Gensubst.substitute Ltac_plugin.Tacarg.wit_tactic s tac) in
  let print env sigma (ids, tac) =
    let ids =
      if List.is_empty ids then mt ()
      else pr_sequence Id.print ids ++ spc () ++ str "|-" ++ spc ()
    in
    str "ltac1:(" ++ ids ++ Ltac_plugin.Pptactic.pr_glob_tactic env sigma tac ++ str ")"
  in
  let raw_print env sigma (ids, tac) =
    let ids =
      if List.is_empty ids then mt ()
      else pr_sequence (fun id -> Id.print id.CAst.v) ids ++ spc () ++ str "|-" ++ spc ()
    in
    str "ltac1:(" ++ ids ++ Ltac_plugin.Pptactic.pr_raw_tactic env sigma tac ++ str ")"
  in
  let obj = {
    ml_intern = intern;
    ml_subst = subst;
    ml_interp = interp;
    ml_print = print;
    ml_raw_print = raw_print;
  } in
  define_ml_object Tac2quote_ltac1.wit_ltac1 obj

let () =
  let open Ltac_plugin in
  let intern self ist (ids, tac) =
    let map { CAst.v = id } = id in
    let ids = List.map map ids in
    (* Prevent inner calls to Ltac2 values *)
    let extra = Tac2intern.drop_ltac2_env ist.Genintern.extra in
    let ltacvars = List.fold_right Id.Set.add ids ist.Genintern.ltacvars in
    let ist = { ist with Genintern.extra; ltacvars } in
    let _, tac = Genintern.intern Ltac_plugin.Tacarg.wit_tactic ist tac in
    let fold ty _ = GTypArrow (gtypref t_ltac1, ty) in
    let ty = List.fold_left fold (gtypref t_ltac1) ids in
    GlbVal (ids, tac), ty
  in
  let interp _ (ids, tac) =
    let clos args =
      let add lfun id v =
        let v = Tac2ffi.to_ext val_ltac1 v in
        Id.ObservableMap.add id v lfun
      in
      let lfun = List.fold_left2 add Id.Map.empty ids args in
      let ist = { env_ist = Id.Map.empty } in
      let lfun = Tac2interp.set_env ist lfun in
      let ist = Ltac_plugin.Tacinterp.default_ist () in
      let ist = { ist with Geninterp.lfun = lfun } in
      return (Tac2ffi.of_ext val_ltac1 (Tacinterp.Value.of_closure ist tac))
    in
    let len = List.length ids in
    if Int.equal len 0 then
      clos []
    else
      return (Tac2ffi.of_closure (Tac2val.abstract len clos))
  in
  let subst s (ids, tac) = (ids, Gensubst.substitute Tacarg.wit_tactic s tac) in
  let print env sigma (ids, tac) =
    let ids =
      if List.is_empty ids then mt ()
      else pr_sequence Id.print ids ++ str " |- "
    in
    str "ltac1val:(" ++ ids++ Ltac_plugin.Pptactic.pr_glob_tactic env sigma tac ++ str ")"
  in
  let raw_print env sigma (ids, tac) =
    let ids =
      if List.is_empty ids then mt ()
      else pr_sequence (fun id -> Id.print id.CAst.v) ids ++ spc () ++ str "|-" ++ spc ()
    in
    str "ltac1val:(" ++ ids ++ Ltac_plugin.Pptactic.pr_raw_tactic env sigma tac ++ str ")"
  in
  let obj = {
    ml_intern = intern;
    ml_subst = subst;
    ml_interp = interp;
    ml_print = print;
    ml_raw_print = raw_print;
  } in
  define_ml_object Tac2quote_ltac1.wit_ltac1val obj

(** Ltac2 in Ltac1 *)

let () =
  let create name wit =
    let e = Tac2entries.Pltac.tac2expr_in_env in
    let inject (loc, v) = Ltac_plugin.Tacexpr.TacGeneric (Some name, in_gen (rawwit wit) v) in
    Ltac_plugin.Tacentries.create_ltac_quotation ~plugin:ltac2_ltac1_plugin name inject (e, None)
  in
  let () = create "ltac2" wit_ltac2in1 in
  let () = create "ltac2val" wit_ltac2in1_val in
  ()

(* Ltac1 runtime representation of Ltac2 closures. *)
let typ_ltac2 : valexpr Geninterp.Val.typ =
  Geninterp.Val.create "ltac2:ltac2_eval"

let () = Genprint.register_val_print0 typ_ltac2 (fun v ->
    TopPrinterBasic (fun () -> Pp.str "<ltac2 closure>"))

let cast_typ (type a) (tag : a Geninterp.Val.typ) (v : Geninterp.Val.t) : a =
  let Geninterp.Val.Dyn (tag', v) = v in
  match Geninterp.Val.eq tag tag' with
  | None -> assert false
  | Some Refl -> v

let () =
  let open Ltac_plugin in
  (* This is a hack similar to Tacentries.ml_val_tactic_extend *)
  let intern_fun _ e = Empty.abort e in
  let subst_fun s v = v in
  let () = Genintern.register_intern0 wit_ltac2_val intern_fun in
  let () = Gensubst.register_subst0 wit_ltac2_val subst_fun in
  (* These are bound names and not relevant *)
  let tac_id = Id.of_string "F" in
  let arg_id = Id.of_string "X" in
  let interp_fun deferred_id ist () =
    let tac = cast_typ typ_ltac2 @@ Id.ObservableMap.get tac_id ist.Tacinterp.lfun in
    let arg = Id.ObservableMap.get arg_id ist.Tacinterp.lfun in
    let tac = Tac2ffi.to_closure tac in
    Tac2val.apply tac [of_ltac1 arg] >>= fun ans ->
    let ans = Tac2ffi.to_ext val_ltac1 ans in
    Ftactic.return (Geninterp.TaggedVal.make deferred_id ans)
  in
  let () = Geninterp.register_interp0 wit_ltac2_val interp_fun in
  define "ltac1_lambda" (valexpr @-> ret ltac1) @@ fun f ->
  let body = Tacexpr.TacGeneric (Some ltac2_ltac1_plugin, in_gen (glbwit wit_ltac2_val) ()) in
  let clos = CAst.make (Tacexpr.TacFun ([Name arg_id], CAst.make (Tacexpr.TacArg body))) in
  let f = Geninterp.Val.inject (Geninterp.Val.Base typ_ltac2) f in
  let lfun = Id.ObservableMap.singleton tac_id f in
  let ist = { (Tacinterp.default_ist ()) with Tacinterp.lfun } in
  Tacinterp.Value.of_closure ist clos

let ltac2_eval =
  let open Ltac_plugin in
  let ml_name = {
    Tacexpr.mltac_plugin = ltac2_ltac1_plugin;
    mltac_tactic = "ltac2_eval";
  } in
  let eval_fun args ist = match args with
  | [] -> assert false
  | tac :: args ->
    (* By convention the first argument is the tactic being applied, the rest
      being the arguments it should be fed with *)
    let tac = cast_typ typ_ltac2 tac in
    let tac = Tac2ffi.to_closure tac in
    let args = List.map (fun arg -> Tac2ffi.of_ext val_ltac1 arg) args in
    Proofview.tclIGNORE (Tac2val.apply tac args)
  in
  let () = Tacenv.register_ml_tactic ml_name [|eval_fun|] in
  { Tacexpr.mltac_name = ml_name; mltac_index = 0 }

let () =
  let open Ltac_plugin in
  let open Tacinterp in
  let interp deferred_id ist (ids, tac) = match ids with
  | [] ->
    (* Evaluate the Ltac2 quotation eagerly *)
    let idtac = Value.of_closure { ist with lfun = Id.Map.empty }
        (CAst.make (Tacexpr.TacId [])) in
    let ist = { env_ist = Id.Map.empty } in
    Tac2interp.interp ist tac >>= fun v ->
    let v = idtac in
    Ftactic.return (Geninterp.TaggedVal.make deferred_id v)
  | _ :: _ ->
    (* Return a closure [@f := {blob} |- fun ids => ltac2_eval(f, ids) ] *)
    (* This name cannot clash with Ltac2 variables which are all lowercase *)
    let self_id = Id.of_string "F" in
    let nas = List.map (fun id -> Name id) ids in
    let mk_arg id = Tacexpr.Reference (Locus.ArgVar (CAst.make id)) in
    let args = List.map mk_arg ids in
    let clos = CAst.make (Tacexpr.TacFun
        (nas, CAst.make (Tacexpr.TacML (ltac2_eval, mk_arg self_id :: args)))) in
    let self = GTacFun (List.map (fun id -> Name id) ids, tac) in
    let self = Tac2interp.interp_value { env_ist = Id.Map.empty } self in
    let self = Geninterp.Val.inject (Geninterp.Val.Base typ_ltac2) self in
    let ist = { ist with lfun = Id.ObservableMap.singleton self_id self } in
    Ftactic.return (Geninterp.TaggedVal.make deferred_id (Value.of_closure ist clos))
  in
  Geninterp.register_interp0 wit_ltac2in1 interp

let () =
  let interp deferred_id ist tac =
    let ist = { env_ist = Id.Map.empty } in
    Tac2interp.interp ist tac >>= fun v ->
    let v = repr_to ltac1 v in
    Ftactic.return (Geninterp.TaggedVal.make deferred_id v)
  in
  Geninterp.register_interp0 wit_ltac2in1_val interp
