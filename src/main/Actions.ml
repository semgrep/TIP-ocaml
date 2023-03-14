(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Dumpers *)
(*****************************************************************************)

let dump_types path =
  let ast = Parse_tip.parse_program path in
  let type_ = Typing.typecheck ast in
  Logs.app (fun m -> m "%s" (AST.show_type_ type_))

let dump_ast path =
  let ast = Parse_tip.parse_program path in
  Logs.app (fun m -> m "%s" (AST.show_program ast))

let dump_il path =
  let ast = Parse_tip.parse_program path in
  let il = Normalizing.program ast in
  Logs.app (fun m -> m "%s" (IL.show_program il))

let dump_cfg path =
  let ast = Parse_tip.parse_program path in
  let il = Normalizing.program ast in
  il |> List.iter (fun f ->
    let g = CFG_build.cfg_of_fun f in
    CFG.display_cfg g
  )

