(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* IL for Intermediate Language.
 *
 * This is the AST in normalized form, which is a more convenient form
 * for further static analysis.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type tok = AST.tok
[@@deriving show]

type 'a wrap = 'a AST.wrap
[@@deriving show]

type ident = AST.ident
[@@deriving show]

type basic_expr = 
  | Int of int wrap
  | Bool of bool wrap
  | Null of tok
  | Id of ident
[@@deriving show { with_path = false }]

(* not a recursive anymore! *)
type expr =
  | B of basic_expr
  | Input of tok
  (* simpler: no more recursive exp, just basic_expr *)
  | BinaryOp of basic_expr * AST.operator wrap * basic_expr
  | Call of ident * basic_expr list
  | Alloc of tok * basic_expr
  | Ref of tok * ident
  | Deref of tok * ident
  | Record of field list
  | DotAccess of ident * tok * ident

and field = ident * tok * basic_expr
[@@deriving show { with_path = false }]

(* alt: define an lvalue type *)
type stmt =
  (* Id = E; which includes Id = *Id; *)
  | Assign of ident * tok * expr
  (* *Id = Id; *)
  | AssignDeref of tok * ident * tok * ident (* could be expr too *)
  (* Id.Id = E; *)
  | AssignField of ident * tok * ident * tok * expr
  | Output of tok (* 'output' *) * expr
  | If of tok * basic_expr * stmt list * stmt list
  | While of tok * basic_expr * stmt list
[@@deriving show { with_path = false }]


type fun_ = {
  fname: ident;
  fparams: ident list;
  fvars: ident list;
  fbody: stmt list;
  freturn:  tok (* 'return' *) * expr;
}
[@@deriving show { with_path = false }]

type program = fun_ list
[@@deriving show]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
