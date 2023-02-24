(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* types *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let rec cfg_stmt (g: CFG.t) (before: CFG.nodei) (st: IL.stmt) : CFG.nodei (* after *) =
  match st with
  | IL.Assign (lv, tk, e) ->
     let n = g#add_node (CFG.NAssign (lv, tk, e)) in
     g#add_arc ((before, n), CFG.Direct);
     n
  | IL.Output (tk, e) ->
     let n = g#add_node (CFG.NOutput (tk, e)) in
     g#add_arc ((before, n), CFG.Direct);
     n
  | IL.If (tk, e, then_, else_) ->
     let n = g#add_node (CFG.NCond (tk, e)) in
     g#add_arc ((before, n), CFG.Direct);
     (* alt: use labels on edges instead *)
     let nthen = g#add_node CFG.TrueNode in
     let nelse = g#add_node CFG.FalseNode in
      g#add_arc ((n, nthen), CFG.Direct);
      g#add_arc ((n, nelse), CFG.Direct);
     let endthen = cfg_stmts g nthen then_ in
     let endelse = cfg_stmts g nelse else_ in
     let join = g#add_node CFG.Join in
     g#add_arc ((endthen, join), CFG.Direct);
     g#add_arc ((endelse, join), CFG.Direct);
     join
  | IL.While (tk, e, body) ->
     let n = g#add_node (CFG.NCond (tk, e)) in
     g#add_arc ((before, n), CFG.Direct);
     let ntrue = g#add_node CFG.TrueNode in
     let nfalse =  g#add_node CFG.FalseNode in
     g#add_arc ((n, ntrue), CFG.Direct);
     g#add_arc ((n, nfalse), CFG.Direct);
     let endtrue = cfg_stmts g ntrue body in
     g#add_arc ((endtrue, n), CFG.Direct);
     nfalse

and cfg_stmts (g: CFG.t) (before: CFG.nodei) (xs: IL.stmt list) : CFG.nodei =
  xs |> List.fold_left (fun before st -> cfg_stmt g before st) before
  
(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let cfg_of_fun (f: IL.fun_) : CFG.t =
  let { IL.fvars; fbody; freturn = (tret, rete); fname = _; fparams = _ } = f
  in
  let g = new Ograph_extended.ograph_mutable in
  let entry = g#add_node CFG.Entry in
  let last = 
    fvars |> List.fold_left (fun before id ->
       let n = g#add_node (CFG.NVar id) in
       g#add_arc ((before, n), CFG.Direct);
       n
    ) entry
  in
  let last = fbody |> cfg_stmts g last in
  let nret = g#add_node (CFG.NReturn (tret, rete)) in
  g#add_arc ((last, nret), CFG.Direct);
  let exit = g#add_node CFG.Exit in
  g#add_arc ((nret, exit), CFG.Direct);
  g
