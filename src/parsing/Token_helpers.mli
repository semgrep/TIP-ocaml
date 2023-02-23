val visitor_info_of_tok :
  (Parse_info.t -> Parse_info.t) -> Parser_tip.token -> Parser_tip.token

val info_of_tok: Parser_tip.token -> Parse_info.t

val line_of_tok: Parser_tip.token -> int

val is_eof: Parser_tip.token -> bool
val is_comment: Parser_tip.token -> bool

