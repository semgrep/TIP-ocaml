open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* types *)
(*****************************************************************************)

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
let rec expr_of_exp (exp: AST.exp) : IL.stmt list * IL.expr =
  match exp with
  | AST.Int x -> [], IL.B (IL.Int x)
  | AST.Bool x -> [], IL.B (IL.Bool x)
  | AST.Id id -> [], IL.B (IL.Id id)
  | AST.BinaryOp (exp1, op, exp2) ->
      let st1, e1 = basic_expr_of_exp exp1 in
      let st2, e2 = basic_expr_of_exp exp2 in
      st1 @ st2, IL.BinaryOp (e1, op, e2)
  | AST.Input tk -> [], IL.Input tk
  | AST.Call (exp, args) -> 
        let st1, id = ident_of_exp exp in
        let st_and_args = Common.map basic_expr_of_exp args in
        let st2 = st_and_args |> List.concat_map fst in
        let es = st_and_args |> Common.map snd in
        st1 @ st2, IL.Call (id, es)
  | AST.Alloc (talloc, exp) -> 
        let st, e = basic_expr_of_exp exp in
        st, IL.Alloc (talloc, e)
  | AST.Ref (tand, id) ->
        [], IL.Ref (tand, id)
  | AST.Deref (tstar, exp) -> 
        let st, id = ident_of_exp exp in
        st, IL.Deref (tstar, id)
  | AST.Null tnull ->
        [], IL.B (IL.Null tnull)
  | AST.Record flds -> 
        let st_and_flds = flds |> Common.map (fun (id, tcolon, exp) ->
             let st, e = basic_expr_of_exp exp in
             st, (id, tcolon, e)
         ) in
        let st = st_and_flds |> List.concat_map fst in
        let flds = st_and_flds |> Common.map snd in
        st, IL.Record flds
  | AST.DotAccess (exp, tdot, idfld) ->
        let st, id = ident_of_exp exp in
        st, IL.DotAccess (id, tdot, idfld)

and ident_of_exp (exp: AST.exp) : IL.stmt list * IL.ident =
  match exp with
  | AST.Id x -> [], x
  | _else_ ->
      let st, e = expr_of_exp exp in
      let tk = AST.tok_of_expr exp in
      let id = fresh_id tk in
      st @ [ IL.Assign (IL.LId id, tk, e) ], id

and basic_expr_of_exp (exp: AST.exp) : IL.stmt list * IL.basic_expr =
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
        st1 @ [ IL.Assign (IL.LId id, teq, e) ]
    | AST.AssignDeref (tstar, exp1, teq, exp2) ->
        let st1, id1 = ident_of_exp exp1 in
        let st2, e2 = expr_of_exp exp2 in
        st1 @ st2 @ [ IL.Assign (IL.LDeref (tstar, id1), teq, e2) ]
    | AST.AssignField (id, tdot, idfld, teq, exp) ->
        let st, e = expr_of_exp exp in
        st @ [ IL.Assign (IL.LField (id, tdot, idfld), teq, e) ]
    | GenAssignField (tstar, exp1, tdot, idfld, teq, exp2) ->
        let st1, id1 = ident_of_exp (AST.Deref (tstar, exp1)) in
        let st2, e2 = expr_of_exp exp2 in
        st1 @ st2 @ [ IL.Assign (IL.LField (id1, tdot, idfld), teq, e2) ]
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
