open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* TIP (Tiny Imperative Programming language) AST (Abstract Syntax Tree) *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type 'a wrap = 'a * Tok.t
[@@deriving show]

type ident = string wrap
[@@deriving show]

type exp =
  (* basic *)
  | Int of int wrap
  (* ext: not in original TIP *)
  | Bool of bool wrap
  | Id of ident
  | BinaryOp of exp * operator wrap * exp
  | Input of Tok.t (* 'input' *)
  | Call of exp (* usually an Id *) * arg list
  (* pointers *)
  | Alloc of Tok.t (* 'alloc' *) * exp
  | Ref of Tok.t (* '&' *) * ident
  | Deref of Tok.t (* '*' *) * exp
  | Null of Tok.t
  (* record *)
  | Record of field list
  | DotAccess of exp * Tok.t (* '.' *) * ident
  (* no need for ParenExpr, it's an AST not CST *)

and arg = exp

(* constraint: the exp can't be a record itself (but can be a pointer to one)*)
and field = ident * Tok.t (* ':' *) * exp

and operator =
  | Plus | Minus | Mult | Div
  | Lt | Gt
  | EqEq
[@@deriving show { with_path = false }]

type stm =
  (* alt: could define an lvalue type and factorize those Assign
   * (in fact IL.ml does that)
   *)
  | Assign of ident * Tok.t (* = *) * exp
  | AssignDeref of Tok.t (* '*' *) * exp * Tok.t (* = *) * exp
  | AssignField of ident * Tok.t (* '.' *) * ident * Tok.t (* = *) * exp
  | GenAssignField of Tok.t (* '*' *) * exp * Tok.t (* '.' *) * ident * Tok.t (* = *) * exp
  | Output of Tok.t (* 'output' *) * exp
  (* can be the empty list *)
  | Seq of stm list
  | If of Tok.t * exp * stm * stm option
  | While of Tok.t * exp * stm
[@@deriving show { with_path = false }]

type fun_ = {
  fname: ident;
  fparams: ident list;
  fvars: ident list;
  fbody: stm;
  freturn:  Tok.t (* 'return' *) * exp;
}
[@@deriving show { with_path = false }]

type program = fun_ list
[@@deriving show]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let rec tok_of_expr = function
  | Int (_, tk)
  | Bool (_, tk)
  | Id (_, tk)
  | BinaryOp (_, (_, tk), _)
  | Input tk
  | Alloc (tk, _)
  | Null tk
  | Ref (tk, _)
  | Deref (tk, _)
  | DotAccess (_, tk, _)
  | Record ((_, tk, _)::_)
    -> tk
  | Call (e, _) ->
      tok_of_expr e
  | Record [] ->
      raise Impossible
