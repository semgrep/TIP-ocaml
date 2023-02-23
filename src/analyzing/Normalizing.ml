open Common

[@@@warning "-27-39-32-26"]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* types *)
(*****************************************************************************)
type env = {
  stmts: IL.stmt list ref;
}

let default_env () = 
  { stmts = ref [] }


(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let counter = ref 0

let fresh_id tok : IL.ident =
  incr counter;
  (spf "_%d" !counter, tok)

(*****************************************************************************)
(* Expr *)
(*****************************************************************************)
let expr_of_exp (exp: AST.exp) : IL.stmt list * IL.expr =
  let rec aux = function
  | AST.Int x -> failwith "XXX"
  | AST.Bool x -> failwith "XXX"
  | AST.Id id -> failwith "XXX"
  | AST.BinaryOp (exp1, op, exp2) -> failwith "XXX"
  | AST.Input tk -> failwith "XXX"
  | AST.Call (exp, args) -> failwith "XXX"
  | AST.Alloc (talloc, exp) -> failwith "XXX"
  | AST.Ref (tand, id) -> failwith "XXX"
  | AST.Deref (tstar, exp) -> failwith "XXX"
  | AST.Null tnull -> failwith "XXX"
  | AST.Record flds -> failwith "XXX"
  | AST.DotAccess (exp, tdot, id) -> failwith "XXX"
  in
  aux exp


(*****************************************************************************)
(* Stmt *)
(*****************************************************************************)
let stmts_of_stm (stm: AST.stm) : IL.stmt list =
  let rec aux = function
    | AST.Assign (id, teq, exp) ->
        failwith "XXX"
    | AST.AssignDeref (tstar, exp1, teq, exp2) ->
        failwith "XXX"
    | AST.AssignField (id, tdot, idfld, teq, exp) ->
        failwith "XXX"
    | GenAssignField (tstar, exp1, tdot, idfld, teq, exp2) ->
        failwith "XXX"
    | Output (tk, exp) ->
        failwith "XXX"
    | Seq xs ->
        failwith "XXX"
    | If (tif, exp, stm1, stm2_opt) ->
        failwith "XXX"
    | While (twhile, exp, stm) ->
        failwith "XXX"
  in
  aux stm

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let program funs =
  funs |> Common.map (function
   | { AST.fname; fparams; fvars; fbody; freturn = (tk, exp) } ->
     let st1 = stmts_of_stm fbody in
     let st2, e = expr_of_exp exp in
     { IL.fname; fparams; fvars; fbody = IL.Seq (st1 @ st2); 
       freturn = (tk, e) }
  )
