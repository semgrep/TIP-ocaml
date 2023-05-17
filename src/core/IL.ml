(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* IL (Intermediate Language).
 *
 * This is the AST in normalized form, which is a more convenient form
 * for further static analysis.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type 'a wrap = 'a AST.wrap
[@@deriving show]

type ident = AST.ident
[@@deriving show]

(* similar to a subset of AST.exp *)
type basic_expr = 
  | Int of int wrap
  (* ext: *)
  | Bool of bool wrap
  | Null of Tok.t
  | Id of ident
[@@deriving show { with_path = false }]

(* not a recursive type anymore! *)
type expr =
  | B of basic_expr
  | Input of Tok.t
  (* simpler: no more recursive exp, just basic_expr *)
  | BinaryOp of basic_expr * AST.operator wrap * basic_expr
  | Call of ident * basic_expr list
  | Alloc of Tok.t * basic_expr
  | Ref of Tok.t (* '&' *) * ident
  (* alt: R of lvalue to factorize Deref, DotAccess, and B Id *)
  | Deref of Tok.t (* '*' *) * ident
  | DotAccess of ident * Tok.t (* '.' *) * ident
  | Record of field list

and field = ident * Tok.t * basic_expr
[@@deriving show { with_path = false }]

(* alt: define AssignDeref and AssignField *)
type lvalue =
  (* Id = ...; *)
  | LId of ident
  (* *Id = ...; *)
  | LDeref of Tok.t (* '*' *) * ident
  (* Id.Id = ...; *)
  | LField of ident * Tok.t (* '.' *) * ident
[@@deriving show { with_path = false }]

type stmt =
  | Assign of lvalue * Tok.t * expr
  | Output of Tok.t (* 'output' *) * expr
  (* could also put expr for the cond *)
  | If of Tok.t * basic_expr * stmt list * stmt list
  | While of Tok.t * basic_expr * stmt list
[@@deriving show { with_path = false }]


type fun_ = {
  fname: ident;
  fparams: ident list;
  fvars: ident list;
  fbody: stmt list;
  freturn:  Tok.t (* 'return' *) * expr;
}
[@@deriving show { with_path = false }]

type program = fun_ list
[@@deriving show]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
