(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

(* We register this handler for lower-level toplevel loading code *)
let () = CErrors.register_handler (function
    | Symtable.Error e ->
      Some (Pp.str (Format.asprintf "%a" Symtable.report_error e))
    | _ ->
      None
  )

(* Another bit of text is printed in the [include_utilities] file, so the default one is not need *)
let () = Clflags.noversion := true

let load_module fmt name =
  if not ((Topdirs.load_file [@ocaml.warning "-3"]) fmt name) then
    CErrors.user_err Pp.(str ("Could not load plugin " ^ name))

let load_plugin fmt ps =
  match Mltop.PluginSpec.repr ps with
  | (Some file, _)  ->
    let file = file ^ ".cma" in
    load_module fmt file
  | (None, lib ) ->
    Topfind.load_deeply [lib]

let ml_loop fmt ?init_file () =
  let init_file = ref init_file in
  Toploop.add_hook (fun event ->
    if event = Toploop.After_setup then begin
      match !init_file with
      | None -> ()
      | Some f ->
        init_file := None; (* Run the initialization file only once *)
        ignore (Toploop.use_silently fmt (Toploop.File f))
    end
  );
  try
    Toploop.loop fmt
  with Compenv.Exit_with_status(0) -> ()

let drop_setup () =
  let ppf = Format.std_formatter in
  Mltop.set_top
    { load_plugin = load_plugin ppf
    ; load_module = load_module ppf
    ; add_dir = Topdirs.dir_directory
    ; ml_loop = ml_loop ppf
    }

(* Main coqtop initialization *)
let _ =
  drop_setup ();
  Coqtop.(start_coq coqtop_toplevel)
