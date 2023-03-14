open AST
module Set = Set_

exception Todo of string
exception TypeError

(* Define a type variable *)

type type_variable = int
[@@deriving show]

(* Define terms and type constraints *)

type term = 
  | TypeVar of type_variable
  | Ident of string
  | Int
  | Ptr of term
  | Fun of term list * term
[@@deriving show]

type type_constraint = term * term

type type_constraints = type_constraint list

type assignments = (string * term) list
[@@deriving show]

let vars_for_function () =
  let type_variable_counter = ref 0 in
  let create_var () = 
    let var_num = !type_variable_counter in
    type_variable_counter := var_num + 1;
    TypeVar var_num
  in
  create_var

(* Union find *)

module FuncMap = Map.Make (
      struct
        type t = ident
        let compare a b = Stdlib.compare a b
      end
   )

module TermMap = Map.Make (
      struct
        type t = term
        let compare a b = Stdlib.compare a b
      end
   )

type reprs = term TermMap.t

module UnionFind = struct
  let add reprs term = 
    TermMap.add term term reprs

  let rec find_and_update reprs term = 
    let parent = TermMap.find term reprs in
    if term = parent then parent, reprs
    else 
      let reprs = TermMap.update term (fun _ -> Some parent) reprs in
      find_and_update reprs parent

  let rec find reprs term =
    let parent = TermMap.find term reprs in
    if term = parent then parent
    else 
      find reprs parent

  (* Make y the representative of x *)
  let union (reprs : reprs) x y =
    let x_repr, reprs = find_and_update reprs x in
    let y_repr, reprs = find_and_update reprs y in
    if x_repr != y_repr then
    TermMap.update x_repr (fun _ -> Some y_repr) reprs
    else reprs
end
  
let init_reprs constraints =
  let reprs = TermMap.empty in
  let type_vars = constraints |> List.fold_left (
      fun acc (v1, v2) -> Set.add v2 (Set.add v1 acc) 
  ) Set.empty
  in
  Set.fold (fun x acc -> UnionFind.add acc x) type_vars reprs

let rec unify reprs x y =
  let x_repr, reprs = UnionFind.find_and_update reprs x in
  let y_repr, reprs = UnionFind.find_and_update reprs y in
  if x_repr != y_repr then (
    match x_repr, y_repr with
    | Ident _, Ident _ -> raise TypeError
    | Ident _, _ -> UnionFind.union reprs x_repr y_repr
    | _, Ident _ -> UnionFind.union reprs y_repr x_repr
    | TypeVar _, TypeVar _ -> UnionFind.union reprs x_repr y_repr
    | TypeVar _, _ -> UnionFind.union reprs x_repr y_repr
    | _, TypeVar _ -> UnionFind.union reprs y_repr x_repr
    | Int, Int -> reprs
    | Ptr x', Ptr y' -> unify reprs x' y'
    | Fun (xargs, xret), Fun (yargs, yret) -> 
        if List.length xargs != List.length yargs then 
           raise TypeError
        else  
           let arg_pairs = List.combine xargs yargs in
           let reprs_after_args = arg_pairs |> List.fold_left (
            fun acc (x, y) -> unify acc x y
           ) reprs
           in
           unify reprs_after_args xret yret
    | _ -> raise TypeError
  ) else reprs
  
let solve_constraint reprs (t1, t2) =
   unify reprs t1 t2

let replace_vars reprs term =
  match term with
  | Int | TypeVar _ | Ident _ -> term
  | Ptr t -> Ptr (UnionFind.find reprs t)
  | Fun (arg, ret) -> Fun (arg |> List.map (fun t -> UnionFind.find reprs t), UnionFind.find reprs ret)

let solve_constraints constraints =
  let reprs = init_reprs constraints in
  let reprs = constraints |> List.fold_left (
    fun acc c -> solve_constraint acc c
  ) reprs
  in
  let idents =
  List.filter_map (
    fun (v1, v2) ->
      match v1, v2 with
      | (_t, Ident ident) | (Ident ident, _t) -> Some ident
      | _ -> None
  ) constraints
  in
  let idents = Set.of_list idents |> Set.elements in
  List.map (fun ident -> (ident, UnionFind.find reprs (Ident ident) |> replace_vars reprs)) idents

(* Gather constraints *)

type any =
  | Return of exp * term
  | Stm of stm

let gather_constraints new_var v =
  let rec constraints_of_stm stm =
    match stm with
    | Assign (ident, _t, e) -> 
        let v = new_var () in
        let e_constraints = constraints_of_exp e v in
        (Ident (fst ident), v) :: e_constraints
    | AssignDeref (_star, e1, _eq, e2) ->
        let v1 = new_var () in
        let v2 = new_var () in
        let e1_constraints = constraints_of_exp e1 v1 in
        let e2_constraints = constraints_of_exp e2 v2 in
        (v1, Ptr v2) :: (e1_constraints @ e2_constraints)
    | Output (_t, e) ->
        let v = new_var () in
        constraints_of_exp e v
    | Seq stms ->
        List.flatten (List.map constraints_of_stm stms)
    | If (_t, e, if_stm, else_stm_opt) ->
        let v = new_var () in
        let e_constraints = constraints_of_exp e v in
        let if_stm_constraints = constraints_of_stm if_stm in
        let else_stm_constraints =
          match else_stm_opt with 
          | None -> []
          | Some else_stm -> constraints_of_stm else_stm
        in
        (v, Int) :: (e_constraints @ if_stm_constraints @ else_stm_constraints)  
    | While (_t, e, stm) ->
        let v = new_var () in
        let e_constraints = constraints_of_exp e v in
        let stm_constraints = constraints_of_stm stm in
        (v, Int) :: (e_constraints @ stm_constraints)  
    | AssignField _ -> raise (Todo "AssignField")
    | GenAssignField _ -> raise (Todo "GenAssignField")
  and constraints_of_exp e v =
    match e with
    | Int _i -> [(v, Int)]
    (* TODO make bool a separate type *)
    | Bool _b -> [(v, Int)]
    | Id ident -> [(Ident (fst ident), v)]
    | BinaryOp (e1, _op, e2) -> 
        let v1 = new_var () in
        let v2 = new_var () in
        let e1_constraints = constraints_of_exp e1 v1 in
        let e2_constraints = constraints_of_exp e2 v2 in
        (v, Int) :: (v1, Int) :: (v2, Int) :: (e1_constraints @ e2_constraints)
    | Input _t -> [(v, Int)]
    | Call (e1, args) ->
        let v1 = new_var () in
        let e1_constraints = constraints_of_exp e1 v1 in
        let args_with_vars = args |> List.map (fun arg -> (arg, new_var ())) in
        let args_constraints = args_with_vars |> List.map (fun (arg, v_arg) -> constraints_of_exp arg v_arg) |> List.flatten in
        let arg_vars = args_with_vars |> List.map snd in
        let todo = raise (Todo "Call") in
        (v1, Fun (arg_vars, todo)) :: (e1_constraints @ args_constraints)
    | Alloc (_t, e1) ->
        let v1 = new_var () in
        let e1_constraints = constraints_of_exp e1 v1 in
        (v, Ptr (v1)) :: e1_constraints
    | Ref (_t, ident) ->
        [(v, Ptr (Ident (fst ident)))]
    | Deref (_t, e1) ->
        let v1 = new_var () in
        let e1_constraints = constraints_of_exp e1 v1 in
        (Ptr(v), v1) :: e1_constraints
    | Null _t -> [(v, Ptr(new_var ()))]
    | Record _ -> raise (Todo "Record")
    | DotAccess _ -> raise (Todo "DotAccess")
  in
  match v with
  | Stm s -> constraints_of_stm s
  | Return (e, v) -> constraints_of_exp e v

let constraints_of_function _ftypes func : type_constraints =
  let new_var = vars_for_function () in
  let param_constraints = func.fparams |> 
      List.map (fun ident -> (Ident (fst ident), new_var ()))
  in
  let body_constraints = Stm func.fbody |> gather_constraints new_var 
  in
  let _, return = func.freturn in
  let return_var = new_var () in
  let return_constraints = Return (return, return_var) |> gather_constraints new_var in
  let res = param_constraints @ body_constraints @ return_constraints in
  res

(* Solve constraints *)

(* Type check *)

let typecheck_function ftypes func =
  let constraints = constraints_of_function ftypes func in
  let res = solve_constraints constraints in
  Printf.printf "%s\n" (show_assignments res);
  raise (Todo "Finish typecheck function")

(* Entry point *)

let typecheck program =
   let ftypes = FuncMap.empty in
   let _res = program |> List.fold_left typecheck_function ftypes in
   raise (Todo "Finish typecheck")