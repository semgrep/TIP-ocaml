open Common
(* Provides the 'Arg', 'Cmd', 'Manpage', and 'Term' modules. *)
open Cmdliner

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

type conf = {
  targets: Fpath.t list;
  action: action_kind option;
  logging_level: Logs.level option;
}
and action_kind =
  | DumpAST
  | DumpIL
  | DumpCFG
  | DumpTypes
[@@deriving show]

let _default: conf = {
    targets = [];
    action = None;
    logging_level = None;
}

type exit_code = int

(*************************************************************************)
(* Command-line flags *)
(*************************************************************************)

(* ------------------------------------------------------------------ *)
(* "Verbosity options" (mutually exclusive) *)
(* ------------------------------------------------------------------ *)
let o_verbose : bool Term.t =
  let info =
    Arg.info [ "v"; "verbose" ]
      ~doc:
        {|Show more details about what analysis are running, which files
failed to parse, etc.
|}
  in
  Arg.value (Arg.flag info)

let o_debug : bool Term.t =
  let info =
    Arg.info [ "debug" ]
      ~doc:{|All of --verbose, but with additional debugging information.|}
  in
  Arg.value (Arg.flag info)

(* ------------------------------------------------------------------ *)
(* Positional arguments *)
(* ------------------------------------------------------------------ *)

let o_targets : string list Term.t =
  let info =
    Arg.info [] ~docv:"TARGETS" ~doc:{|Files to be analyzed by otip.|}
  in
  Arg.value (Arg.pos_all Arg.string ([]) info)

(* ------------------------------------------------------------------ *)
(* Alternate modes (actions) *)
(* ------------------------------------------------------------------ *)

let o_dump_ast : bool Term.t =
  let info =
    Arg.info [ "dump-ast" ] ~doc:{|Dump the AST of the target.|}
  in
  Arg.value (Arg.flag info)

let o_dump_il : bool Term.t =
  let info =
    Arg.info [ "dump-il" ] ~doc:{|Dump the normalized AST (IL) of the target.|}
  in
  Arg.value (Arg.flag info)

let o_dump_cfg : bool Term.t =
  let info =
    Arg.info [ "dump-cfg" ] ~doc:{|Dump the CFG of the target.|}
  in
  Arg.value (Arg.flag info)

let o_dump_types : bool Term.t =
  let info =
    Arg.info [ "dump-types" ] ~doc:{|Dump the type assignments of the target.|}
  in
  Arg.value (Arg.flag info)

(*****************************************************************************)
(* Turn argv into a conf *)
(*****************************************************************************)

let cmdline_term : conf Term.t =
  (* !The parameters must be in alphabetic orders to match the order
   * of the corresponding '$ o_xx $' further below! *)
  let combine debug dump_ast dump_cfg dump_il dump_types targets verbose =
    let targets = targets |> Common.map Fpath.v in
    let action = 
      match dump_ast, dump_il, dump_cfg, dump_types with
      | false, false, false, false -> None
      | true, false, false, false -> Some DumpAST
      | false, true, false, false -> Some DumpIL
      | false, false, true, false -> Some DumpCFG
      | false, false, false, true -> Some DumpTypes
      | _else_ -> 
          failwith "mutually exclusive options --dump-ast/--dump-cfg/..."
    in
    let logging_level =
      match (verbose, debug) with
      | false, false -> Some Logs.Warning
      | true, false -> Some Logs.Info
      | false, true -> Some Logs.Debug
      | _else_ ->
           failwith "mutually exclusive options --verbose/--debug"
    in
    { targets; 
      action;
      logging_level;
    }
  in
  (* Term defines 'const' but also the '$' operator *)
  Term.(
    (* !the o_xxx must be in alphabetic orders to match the parameters of
     * combine above! *)
    const combine $ o_debug $ o_dump_ast $ o_dump_cfg $ o_dump_il 
     $o_dump_types $ o_targets $ o_verbose
  )

let doc = "run analysis on TIP files"

let man : Manpage.block list =
  [
    `S Manpage.s_description;
    `P
      "Run analysis on TIP files.";
    `P "To get started quickly, run";
    `Pre "otip --dump-ast foo.tip";
    `P "For more information about TIP, see https://cs.au.dk/~amoeller/spa/";
  ]

let cmdline_info : Cmd.info = Cmd.info "otip" ~doc ~man

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* Small wrapper around Cmdliner.Cmd.eval_value. 
 * mostly a copy-paste of osemgrep/CLI_Common.eval_value
*)
let eval_value ~argv cmd =
  (* the ~catch:false is to let non-cmdliner exn to bubble up *)
  match Cmd.eval_value ~catch:false ~argv cmd with
  (* 124 is the magic number chosen by Cmdliner for those errors, see
   * otip --help 'EXIT STATUS' section, which is autogenerated by Cmdliner
   *)
  | Error (`Term | `Parse) -> raise (UnixExit 124)
  (* this should never happen, because of the ~catch:false above *)
  | Error `Exn -> assert false
  | Ok ok -> (
      match ok with
      | `Ok config -> config
      | `Version
      | `Help ->
          raise (UnixExit 0))

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse_argv (argv : string array) : conf =
  let cmd : conf Cmd.t = Cmd.v cmdline_info cmdline_term in
  eval_value ~argv cmd

let run (conf : conf) : exit_code =
  Logs_helpers.setup_logging ~force_color:true ~level:conf.logging_level;
  Logs.info (fun m -> m "conf = %s" (show_conf conf));
  (match conf.action, conf.targets with
  | Some DumpAST, [path] -> Actions.dump_ast path
  | Some DumpIL, [path] -> Actions.dump_il path
  | Some DumpCFG, [path] -> Actions.dump_cfg path
  | Some DumpTypes, [path] -> Actions.dump_types path
  | _else_ -> failwith "Unsupported action. Run with `--help` to see supported actions"
  );
  0

let main (argv: string array) : exit_code =
  let conf = parse_argv argv in
  run conf
