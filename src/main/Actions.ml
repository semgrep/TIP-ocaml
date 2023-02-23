(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Dumpers *)
(*****************************************************************************)

let dump_ast path =
  let ast = Parse_tip.parse_program path in
  Logs.app (fun m -> m "%s" (AST.show_program ast))

let dump_il path =
  let ast = Parse_tip.parse_program path in
  let il = Normalizing.program ast in
  Logs.app (fun m -> m "%s" (IL.show_program il))

let dump_cfg _path =
  failwith "TODO"
