open Common
module Flag = Flag_parsing
module TH = Token_helpers
module PI = Parse_info
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
let tokens file =
  let token = Lexer_tip.token in
  Parsing_helpers.tokenize_all_and_adjust_pos file token TH.visitor_info_of_tok
    TH.is_eof
  [@@profiling]

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)
let parse filename =
  let stat = Parsing_stat.default_stat filename in
  let toks = tokens filename in

  let tr, lexer, lexbuf_fake =
    Parsing_helpers.mk_lexer_for_yacc toks TH.is_comment
  in

  try
    (* -------------------------------------------------- *)
    (* Call parser *)
    (* -------------------------------------------------- *)
    let xs =
      Profiling.profile_code "Parser_tip.program" (fun () ->
          Parser_tip.program lexer lexbuf_fake)
    in
    { Parsing_result.ast = xs; tokens = toks; stat }
  with
  | Parsing.Parse_error ->
      let cur = tr.Parsing_helpers.current in
      if not !Flag.error_recovery then
        raise (PI.Parsing_error (TH.info_of_tok cur));

      if !Flag.show_parsing_error then (
        pr2 ("parse error \n = " ^ error_msg_tok cur);
        let filelines = Common2.cat_array filename in
        let checkpoint2 = Common.cat filename |> List.length in
        let line_error = TH.line_of_tok cur in
        Parsing_helpers.print_bad line_error (0, checkpoint2) filelines);

      stat.PS.error_line_count <- stat.PS.total_line_count;
      { Parsing_result.ast = []; tokens = toks; stat }
  [@@profiling]

let parse_program path =
  let res = parse (Fpath.to_string path) in
  res.Parsing_result.ast
