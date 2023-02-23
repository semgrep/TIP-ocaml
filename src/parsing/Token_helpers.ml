open Parser_tip
module PI = Parse_info

let is_eof = function
  | EOF _ -> true
  | _else_ -> false

(* we do not generate comment tokens right now in Lexer.mll *)
let is_comment = function
  | _else_ -> false

(*****************************************************************************)
(* Visitors *)
(*****************************************************************************)
let visitor_info_of_tok f = function
  | TIdent (x, ii) -> TIdent (x, f ii)
  | TBool (x, ii) -> TBool (x, f ii)
  | TDot ii -> TDot (f ii)
  | TEq ii -> TEq (f ii)
  | EOF ii -> EOF (f ii)
  | Twhile ii -> Twhile (f ii)
  | Treturn ii -> Treturn (f ii)
  | Tnull ii -> Tnull (f ii)
  | Tif ii -> Tif (f ii)
  | Telse ii -> Telse (f ii)
  | Talloc ii -> Talloc (f ii)
  | TPlus ii -> TPlus (f ii)
  | TOParen ii -> TOParen (f ii)
  | TCParen ii -> TCParen (f ii)
  | TOBrace ii -> TOBrace (f ii)
  | TCBrace ii -> TCBrace (f ii)
  | TMinus ii -> TMinus (f ii)
  | TLt ii -> TLt (f ii)
  | TGt ii -> TGt (f ii)
  | TInt (x, ii) -> TInt (x, f ii)
  | TStar ii -> TStar (f ii)
  | TSemiColon ii -> TSemiColon (f ii)
  | TEqEq ii -> TEqEq (f ii)
  | TColon ii -> TColon (f ii)
  | TAmpersand ii -> TAmpersand (f ii)
  | TComma ii -> TComma (f ii)
  | Tinput ii -> Tinput (f ii)
  | Toutput ii -> Toutput (f ii)
  | TDiv ii -> TDiv (f ii)
  | Tvar ii -> Tvar (f ii)

let info_of_tok tok =
  let res = ref None in
  visitor_info_of_tok
    (fun ii ->
      res := Some ii;
      ii)
    tok
  |> ignore;
  Common2.some !res

let line_of_tok tok =
  let info = info_of_tok tok in
  PI.line_of_info info
