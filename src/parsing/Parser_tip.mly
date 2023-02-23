%{

(*************************************************************************)
(* Prelude *)
(*************************************************************************)

(*************************************************************************)
(* Helpers *)
(*************************************************************************)

%}
(*************************************************************************)
(* Tokens *)
(*************************************************************************)

%token <Parse_info.t> EOF

(* tokens with "values" *)
%token <int * Parse_info.t> TInt
%token <bool * Parse_info.t> TBool
%token <string * Parse_info.t> TIdent

(* keywords tokens *)
%token <Parse_info.t>
  Tif Telse Twhile
  Treturn
  Talloc Tnull
  Tinput Toutput
  Tvar

(* syntax *)
%token <Parse_info.t>
 TOParen "(" TCParen ")" TOBrace "{" TCBrace "}"
 TSemiColon ";" TColon ":" TComma ","
 TDot "."
 TEq "="

(* operators *)
%token <Parse_info.t>
 TPlus "+" TMinus "-" TDiv "/"
 TLt "<" TGt ">" TEqEq "=="
 TStar "*" TAmpersand "&"

(*************************************************************************)
(* Priorities *)
(*************************************************************************)
(* Precedences and associativities.
 *
 * Tokens and rules have precedences.  A reduce/reduce conflict is resolved
 * in favor of the first rule (in source file order).  A shift/reduce conflict
 * is resolved by comparing the precedence and associativity of the token to
 * be shifted with those of the rule to be reduced.
 *
 * By default, a rule has the precedence of its rightmost terminal (if any).
 *
 * When there is a shift/reduce conflict between a rule and a token that
 * have the same precedence, it is resolved using the associativity:
 * if the token is left-associative, the parser will reduce; if
 * right-associative, the parser will shift; if non-associative,
 * the parser will declare a syntax error.
 *
 * We will only use associativities with operators of the kind  x * x -> x
 * for example, in the rules of the form    expr: expr BINOP expr
 * in all other cases, we define two precedences if needed to resolve
 * conflicts.
 *
 * The precedences must be listed from low to high.
 *)

(*************************************************************************)
(* Rules type declaration *)
(*************************************************************************)
%start <AST.program> program

%%
(*************************************************************************)
(* Macros *)
(*************************************************************************)

(*************************************************************************)
(* Toplevel *)
(*************************************************************************)
program: fun_+ EOF { $1 }

fun_: TDot { failwith "TODO" }

(*************************************************************************)
(* Statements *)
(*************************************************************************)

(*************************************************************************)
(* Expressions *)
(*************************************************************************)

(*************************************************************************)
(* Types *)
(*************************************************************************)

