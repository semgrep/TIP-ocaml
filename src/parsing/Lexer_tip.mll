{
open Parser_tip

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(* shortcuts *)
let tok = Lexing.lexeme
let tokinfo = Parse_info.tokinfo

}
(*****************************************************************************)
(* Regexps aliases *)
(*****************************************************************************)

let letter = ['A'-'Z' 'a'-'z']
let digit  = ['0'-'9']

let ident = letter+

let newline = '\n'
let space = [' ' '\t']

(*****************************************************************************)
(* Rule token *)
(*****************************************************************************)
rule token = parse

  (* ----------------------------------------------------------------------- *)
  (* spacing/comments *)
  (* ----------------------------------------------------------------------- *)
  | newline { token lexbuf }
  | space+ { token lexbuf }

  (* ----------------------------------------------------------------------- *)
  (* symbols *)
  (* ----------------------------------------------------------------------- *)
  | "," { TComma(tokinfo lexbuf) }
  | ";" { TSemiColon(tokinfo lexbuf) }
  | ":" { TColon(tokinfo lexbuf) }
  | "." { TDot(tokinfo lexbuf) }
  | "(" { TOParen(tokinfo lexbuf) }  | ")" { TCParen(tokinfo lexbuf) }
  | "{" { TOBrace(tokinfo lexbuf) } | "}" { TCBrace(tokinfo lexbuf) }
  
  | "+" { TPlus(tokinfo lexbuf) }  | "-" { TMinus(tokinfo lexbuf) }
  | "*" { TStar(tokinfo lexbuf) }
  | "&" { TAmpersand(tokinfo lexbuf) }
  | "/" { TDiv(tokinfo lexbuf) }
  | "<" { TLt(tokinfo lexbuf) }  | ">" { TGt(tokinfo lexbuf) }
  | "==" { TEqEq (tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* Keywords and ident *)
  (* ----------------------------------------------------------------------- *)
  | "input" { Tinput (tokinfo lexbuf) }
  | "output" { Toutput (tokinfo lexbuf) }
  | "if" { Tif (tokinfo lexbuf) }
  | "else" { Telse (tokinfo lexbuf) }
  | "while" { Twhile (tokinfo lexbuf) }
  | "var" { Tvar (tokinfo lexbuf) }
  | "return" { Treturn (tokinfo lexbuf) }
  | "alloc" { Talloc (tokinfo lexbuf) }
  | "null" { Tnull (tokinfo lexbuf) }

  (* not in original TIP *)
  | "true" { TBool (true, tokinfo lexbuf) }
  | "false" { TBool (false, tokinfo lexbuf) }

  | ident as s { TIdent (s, tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* Constant *)
  (* ----------------------------------------------------------------------- *)

  | ('-'? digit+) as s { TInt (int_of_string s, tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* eof *)
  (* ----------------------------------------------------------------------- *)

  | eof { EOF (tokinfo lexbuf) }

  | _ { failwith ("unrecognised symbol, in token rule:" ^ tok lexbuf) }
