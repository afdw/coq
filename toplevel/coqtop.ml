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
open Coqargs

(** This file provides generic support for Coq executables + specific
    support for the coqtop executable *)

let () = at_exit flush_all

let get_version ~boot =
  if boot then Coq_config.version else
  try
    let env = Boot.Env.init () in
    let revision = Boot.Env.revision env |> Boot.Path.to_string in
    let ch = open_in revision in
    let ver = input_line ch in
    let rev = input_line ch in
    let () = close_in ch in
    Printf.sprintf "%s (%s)" ver rev
  with _ -> Coq_config.version

let print_header ~boot () =
  Feedback.msg_info (str "Welcome to Coq " ++ str (get_version ~boot));
  flush_all ()


(******************************************************************************)
(* Fatal Errors                                                               *)
(******************************************************************************)

(** Prints info which is either an error or an anomaly and then exits
    with the appropriate error code *)
let fatal_error_exn exn =
  Topfmt.(in_phase ~phase:Initialization print_err_exn exn);
  flush_all ();
  let exit_code =
    if (CErrors.is_anomaly exn) then 129 else 1
  in
  exit exit_code

type ('a,'b) custom_toplevel =
  { parse_extra : string list -> 'a * string list
  ; usage : Boot.Usage.specific_usage
  ; init_extra : 'a -> Coqargs.injection_command list -> opts:Coqargs.t -> 'b
  ; initial_args : Coqargs.t
  ; run : 'a -> opts:Coqargs.t -> 'b -> unit
  }

(** Main init routine *)
let init_toplevel { parse_extra; init_extra; usage; initial_args } =
  Coqinit.init_ocaml ();
  let opts, customopts = Coqinit.parse_arguments ~parse_extra ~usage ~initial_args () in
  Stm.init_process (snd customopts);
  let injections = Coqinit.init_runtime opts in
  (* This state will be shared by all the documents *)
  Stm.init_core ();
  let customstate = init_extra ~opts customopts injections in
  opts, customopts, customstate

module Trace = struct
  type t = {
    sub_filenames : string list;
    declarations : Vernacentries.Declaration.t list;
  } [@@deriving yojson { variants = `Internal "type" }]
end

let start_coq custom =
  Flags.tracing_interactive := Sys.getenv_opt "TRACING_INTERACTIVE" |> Option.has_some;
  Flags.tracing_file := Sys.getenv_opt "TRACING_FILE";
  Flags.tracing := !Flags.tracing_interactive || (!Flags.tracing_file |> Option.has_some);
  Flags.tracing_split := Sys.getenv_opt "TRACING_SPLIT" |> Option.has_some;
  Flags.tracing_compress := Sys.getenv_opt "TRACING_COMPRESS" |> Option.has_some;
  Flags.tracing_low_level := Sys.getenv_opt "TRACING_LOW_LEVEL" |> Option.has_some;
  Flags.tracing_no_event := Sys.getenv_opt "TRACING_NO_EVENT" |> Option.has_some;
  at_exit (fun () ->
    if !Vernacentries.current_name |> Option.has_some then
      Vernacentries.end_proof Vernacentries.Declaration.Fail;
    if !Flags.tracing_interactive then
      Feedback.msg_info Pp.(str "Collected declarations:" ++ spc () ++ int (!Vernacentries.declarations |> List.length));
    match !Flags.tracing_file with
    | None -> ()
    | Some filename ->
      Util.modify_file_with_lock ~filename ~f:(fun old_contents ->
        let trace =
          if old_contents = "" then
            {
              Trace.sub_filenames = [];
              Trace.declarations = [];
            }
          else
            let old_contents =
              if !Flags.tracing_compress && not !Flags.tracing_split
              then Zstd.decompress (Zstd.get_decompressed_size old_contents) old_contents
              else old_contents in
            match old_contents |> Yojson.Safe.from_string |> Trace.of_yojson with
            | Ok trace -> trace
            | Error error -> raise (Failure error) in
        let trace =
          if !Flags.tracing_split then
            let sub_filename =
              Printf.sprintf "%s-%d-%s%s"
                (Filename.chop_suffix filename ".json")
                (List.length trace.sub_filenames)
                !Flags.tracing_sub_suffix
                (if !Flags.tracing_compress then ".json.zst" else ".json") in
            let sub_trace = {
              Trace.sub_filenames = [];
              Trace.declarations = !Vernacentries.declarations;
            } in
            let sub_contents = (sub_trace |> Trace.to_yojson |> Yojson.Safe.pretty_to_string) ^ "\n" in
            let sub_contents =
              if !Flags.tracing_compress
              then Zstd.compress ~level:15 sub_contents
              else sub_contents in
            Out_channel.with_open_text sub_filename (fun oc -> output_string oc sub_contents);
            {
              trace with
              Trace.sub_filenames = trace.sub_filenames @ [Filename.basename sub_filename];
            }
          else
            {
              trace with
              Trace.declarations = trace.declarations @ !Vernacentries.declarations;
            } in
        let new_contents = (trace |> Trace.to_yojson |> Yojson.Safe.pretty_to_string) ^ "\n" in
        let new_contents =
          if !Flags.tracing_compress && not !Flags.tracing_split
          then Zstd.compress  ~level:15 new_contents
          else new_contents in
        new_contents
      )
  );

  let init_feeder = Feedback.add_feeder Coqloop.coqloop_feed in
  (* Init phase *)
  let opts, custom_opts, state =
    try init_toplevel custom
    with any ->
      flush_all();
      fatal_error_exn any in
  Feedback.del_feeder init_feeder;
  (* Run phase *)
  custom.run ~opts custom_opts state

(** ****************************************)
(** Specific support for coqtop executable *)

let ltac_debug_answer = let open DebugHook.Answer in function
    | Prompt prompt ->
      (* No newline *)
      Format.fprintf !Topfmt.err_ft "@[%a@]%!" Pp.pp_with prompt
    | Goal g ->
      Format.fprintf !Topfmt.err_ft "@[%a@]@\n%!" Pp.pp_with g
    | Output o ->
      Format.fprintf !Topfmt.err_ft "@[%a@]@\n%!" Pp.pp_with o
    | Init ->
      Format.fprintf !Topfmt.err_ft "@[%a@]@\n%!" Pp.pp_with (str "Init")
    | Stack _
    | Vars _ -> CErrors.anomaly (str "ltac_debug_answer: unsupported Answer type")

let ltac_debug_parse () =
  let open DebugHook in
  let act =
    try Action.parse (read_line ())
    with End_of_file -> Ok Action.Interrupt
  in
  match act with
  | Ok act -> act
  | Error error -> ltac_debug_answer (Answer.Output (str error)); Action.Failed

type query = PrintTags | PrintModUid of string list
type run_mode = Interactive | Batch | Query of query

type toplevel_options = {
  run_mode : run_mode;
  color_mode : Colors.color;
}

let init_document opts stm_options injections =
  (* Coq init process, phase 3: Stm initialization, backtracking state.

     It is essential that the module system is in a consistent
     state before we take the first snapshot. This was not
     guaranteed in the past, but now is thanks to the STM API.
  *)
  (* Next line allows loading .vos files when in interactive mode *)
  Flags.load_vos_libraries := true;
  let open Vernac.State in
  let doc, sid =
    Stm.(new_doc
           { doc_type = Interactive opts.config.logic.toplevel_name;
             injections;
           }) in
  { doc; sid; proof = None; time = Option.map Vernac.make_time_output opts.config.time }

let init_toploop opts stm_opts injections =
  let state = init_document opts stm_opts injections in
  let state = Load.load_init_vernaculars opts ~state in
  state

let coqtop_init ({ run_mode; color_mode }, async_opts) injections ~opts =
  if run_mode != Interactive then Flags.quiet := true;
  Colors.init_color (if opts.config.print_emacs then `EMACS else color_mode);
  Flags.if_verbose (print_header ~boot:opts.pre.boot) ();
  DebugHook.Intf.(set
    { read_cmd = ltac_debug_parse
    ; submit_answer = ltac_debug_answer
    ; isTerminal = true
    });
  init_toploop opts async_opts injections

let coqtop_parse_extra extras =
  let rec parse_extra run_mode  = function
  | "-batch" :: rest -> parse_extra Batch  rest
  | "-print-mod-uid" :: rest -> Query (PrintModUid rest), []
  |   x :: rest ->
    let run_mode, rest = parse_extra run_mode rest in run_mode, x :: rest
  | [] -> run_mode, [] in
  let run_mode, extras = parse_extra Interactive extras in
  let color_mode, extras = Colors.parse_extra_colors extras in
  let async_opts, extras = Stmargs.parse_args ~init:Stm.AsyncOpts.default_opts extras in
  ({ run_mode; color_mode}, async_opts), extras

let fix_windows_dirsep s =
  if Sys.win32 then Str.(global_replace (regexp "\\(.\\)\\") "\\1/" s)
  else s

let get_native_name s =
  (* We ignore even critical errors because this mode has to be super silent *)
  try
    fix_windows_dirsep @@
    Filename.(List.fold_left concat (dirname s)
                [ !Nativelib.output_dir
                ; Library.native_name_from_filename s
                ])
  with _ -> ""

let coqtop_run ({ run_mode; color_mode },_) ~opts state =
  match run_mode with
  | Interactive -> Coqloop.run ~opts ~state;
  | Query PrintTags -> Colors.print_style_tags color_mode; exit 0
  | Query (PrintModUid sl) ->
      let s = String.concat " " (List.map get_native_name sl) in
      print_endline s;
      exit 0
  | Batch -> exit 0

let coqtop_specific_usage = Boot.Usage.{
  executable_name = "coqtop";
  extra_args = "";
  extra_options = "\n\
coqtop specific options:\n\
\n  -batch                 batch mode (exits after interpretation of command line)\
\n"
}

let coqtop_toplevel =
  { parse_extra = coqtop_parse_extra
  ; usage = coqtop_specific_usage
  ; init_extra = coqtop_init
  ; run = coqtop_run
  ; initial_args = Coqargs.default
  }
