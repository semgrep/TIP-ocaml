val visitor_info_of_tok :
  (Tok.t -> Tok.t) -> Parser_tip.token -> Parser_tip.token

val info_of_tok: Parser_tip.token -> Tok.t

val line_of_tok: Parser_tip.token -> int

val is_eof: Parser_tip.token -> bool
val is_comment: Parser_tip.token -> bool

