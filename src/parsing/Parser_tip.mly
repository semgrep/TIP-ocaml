%{
open AST
(*************************************************************************)
(* Prelude *)
(*************************************************************************)
(* TIP grammar
 *)

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
%token <string * Parse_info.t> TId

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
 TSemiColon ";" TColon ":" TComma "," TDot "." TEq "="

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
%left TEqEq TLt TGt
%left TPlus TMinus
%left TStar TDiv

(*************************************************************************)
(* Rules type declaration *)
(*************************************************************************)
%start <AST.program> program

%%
(*************************************************************************)
(* Macros *)
(*************************************************************************)
list_sep(X,Sep):
 | X                      { [$1] }
 | list_sep(X,Sep) Sep X  { $1 @ [$3] }

(*************************************************************************)
(* Toplevel *)
(*************************************************************************)
program: fun_+ EOF { $1 }

fun_: 
  TId "(" list_sep(TId, ",") ")"
  "{"
   vars?
   stm
   Treturn exp ";"
  "}"
  { { fname = $1;
      fparams = $3;
      fvars = $6 |> Common.optlist_to_list;
      fbody = $7;
      freturn = ($8, $9);
    }
  }

vars: Tvar list_sep(TId, ",") ";" { $2 }

(*************************************************************************)
(* Statements *)
(*************************************************************************)
stm1:
 | TId "=" exp ";" { Assign ($1, $2, $3) }
 | Toutput exp ";" { Output ($1, $2) }
 | Tif "(" exp ")" "{" stm "}" else_opt { If ($1, $3, $6, $8) }
 | Twhile "(" exp ")" "{" stm "}" { While ($1, $3, $6) }

stm: stm1* {
  match $1 with
  | [x] -> x
  | xs -> Seq xs
  }

else_opt:
 | (* empty *) { None }
 | Telse "{" stm "}" { Some $3 }
 
(*************************************************************************)
(* Expressions *)
(*************************************************************************)
exp:
 | TInt { Int $1 }
 | TBool { Bool $1 }
 | TId { Id $1 }
 | exp "+" exp { BinaryOp ($1, (Plus, $2), $3) }
 | exp "-" exp { BinaryOp ($1, (Minus, $2), $3) }
 | exp "*" exp { BinaryOp ($1, (Mult, $2), $3) }
 | exp "/" exp { BinaryOp ($1, (Div, $2), $3) }
 | exp ">" exp { BinaryOp ($1, (Gt, $2), $3) }
 | exp "<" exp { BinaryOp ($1, (Lt, $2), $3) }
 | exp "==" exp { BinaryOp ($1, (EqEq, $2), $3) }
 | "(" exp ")" { $2 }
 | Tinput { Input $1 }
 | TId "(" list_sep(exp, ",") ")" { Call ($1, $3) }

(*************************************************************************)
(* Types *)
(*************************************************************************)

