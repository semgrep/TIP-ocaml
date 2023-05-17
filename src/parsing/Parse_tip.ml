module Flag = Flag_parsing
module TH = Token_helpers
module PS = Parsing_stat

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Error diagnostic  *)
(*****************************************************************************)
let error_msg_tok tok = Parsing_helpers.error_message_info (TH.info_of_tok tok)

(*****************************************************************************)
(* Lexing only *)
(*****************************************************************************)
let tokens path =
  Parsing_helpers.tokenize_all_and_adjust_pos 
    (Parsing_helpers.File path)
    Lexer_tip.token 
    TH.visitor_info_of_tok
    TH.is_eof
  [@@profiling]

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)
let parse_program path =
  (* this can also raise exn, TODO? catch those too? *)
  let toks = tokens path in
  let tr, lexer, lexbuf_fake =
    Parsing_helpers.mk_lexer_for_yacc toks TH.is_comment
  in
  try
    (* -------------------------------------------------- *)
    (* Call parser *)
    (* -------------------------------------------------- *)
     Parser_tip.program lexer lexbuf_fake
  with
  | Parsing.Parse_error ->
      let cur = tr.Parsing_helpers.current in
      Logs.err (fun m -> m "parse error = %s" (error_msg_tok cur));
      raise (Parsing_error.Syntax_error (TH.info_of_tok cur))
[@@profiling]
