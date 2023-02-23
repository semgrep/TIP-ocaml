(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* TIP (Tiny Imperative Programming language) AST (Abstract Syntax Tree).
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type tok = Parse_info.t
[@@deriving show]

type 'a wrap = 'a * tok
[@@deriving show]

type ident = string wrap
[@@deriving show]

type exp =
  (* basic *)
  | Int of int wrap
  | Bool of bool wrap (* not in original TIP *)
  | Id of ident
  | BinaryOp of exp * operator wrap * exp
  | Input of tok (* 'input' *)
  | Call of exp (* usually an Id *) * arg list
  (* pointers *)
  | Alloc of tok * exp
  | Ref of tok (* '&' *) * ident
  | Deref of tok (* '*' *) * exp
  | Null of tok
  (* record *)
  | Record of field list
  | DotAccess of exp * tok (* '.' *) * ident
  (* no need for ParenExpr *)

and arg = exp

(* constraint: the exp can't be a record itself (but can be a pointer to one)*)
and field = ident * tok (* ':' *) * exp

and operator =
  | Plus | Minus | Mult | Div
  | Lt | Gt
  | EqEq
[@@deriving show { with_path = false }]

type stm =
  (* alt: could define an lvalue type and factorize those Assign *)
  | Assign of ident * tok (* = *) * exp
  | AssignDeref of tok (* '*' *) * exp * tok (* = *) * exp
  | AssignField of ident * tok (* '.' *) *ident * tok (* = *) * exp
  | GenAssignField of tok (* '*' *) * exp * tok (* '.' *) * ident * tok (* = *) * exp
  | Output of tok (* 'output' *) * exp
  (* can be empty *)
  | Seq of stm list
  | If of tok * exp * stm * stm option
  | While of tok * exp * stm
[@@deriving show { with_path = false }]

type fun_ = {
  fname: ident;
  fparams: ident list;
  fvars: ident list;
  fbody: stm;
  freturn:  tok (* 'return' *) * exp;
}
[@@deriving show { with_path = false }]

type program = fun_ list
[@@deriving show]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
