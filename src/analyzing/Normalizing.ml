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

let ident_of_exp (exp: AST.exp) : IL.stmt list * IL.ident =
  match exp with
  | AST.Id x -> [], x
  | _else_ ->
      let st, e = expr_of_exp exp in
      let tk = AST.tok_of_expr exp in
      let id = fresh_id tk in
      st @ [ IL.Assign (id, tk, e) ], id

let basic_expr_of_exp (exp: AST.exp) : IL.stmt list * IL.basic_expr =
  match exp with
  | AST.Int x -> [], IL.Int x
  | AST.Bool x -> [], IL.Bool x
  | AST.Null x -> [], IL.Null x
  | AST.Id x -> [], IL.Id x
  | AST.BinaryOp _ | AST.Input _ | AST.Call _ | AST.Alloc _
  | AST.Ref _ | AST.Deref _ | AST.Record _ | AST.DotAccess _
    -> 
      let st, id = ident_of_exp exp in
      st, IL.Id id




(*****************************************************************************)
(* Stmt *)
(*****************************************************************************)
let stmts_of_stm (stm: AST.stm) : IL.stmt list =
  let rec aux = function
    | AST.Assign (id, teq, exp) ->
        let st1, e = expr_of_exp exp in
        st1 @ [ IL.Assign (id, teq, e) ]
    | AST.AssignDeref (tstar, exp1, teq, exp2) ->
        failwith "XXX"
    | AST.AssignField (id, tdot, idfld, teq, exp) ->
        failwith "XXX"
    | GenAssignField (tstar, exp1, tdot, idfld, teq, exp2) ->
        failwith "XXX"
    | Output (tk, exp) ->
        let st, e = expr_of_exp exp in
        st @ [IL.Output (tk, e)]
    | Seq xs ->
        xs |> List.concat_map aux
    | If (tif, exp, stm1, stm2_opt) ->
        let st1, e = basic_expr_of_exp exp in
        let st2 = aux stm1 in
        let st3 =
          match stm2_opt with
          | None -> []
          | Some stm -> aux stm
        in
        st1 @ [IL.If (tif, e, st2, st3)]
    | While (twhile, exp, stm) ->
        let st1, e = basic_expr_of_exp exp in
        let st2 = aux stm in
        st1 @ [IL.While (twhile, e, st2)]
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
     { IL.fname; fparams; fvars; fbody = st1 @ st2; 
       freturn = (tk, e) }
  )
