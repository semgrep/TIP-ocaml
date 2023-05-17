open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* CFG (Control Flow Graph) for the IL (and so also for the AST) *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type node =
  | Entry
  | Exit
  (* for NCond. alt: have edges with True/False *)
  | TrueNode
  | FalseNode
  (* after NCond *)
  | Join 
  (* TODO? NParam? *)
  | NVar of IL.ident
  | NAssign of IL.lvalue * Tok.t * IL.expr
  | NOutput of Tok.t * IL.expr
  | NCond of Tok.t (* 'if' or 'while' *) * IL.basic_expr
  (* alt: could be merged with Exit; must always be before Exit *)
  | NReturn of Tok.t * IL.expr
[@@deriving show { with_path = false }]

type edge = Direct

(* LESS: use ocamlgraph directly *)
type t = (node, edge) Ograph_extended.ograph_mutable

(* an int representing the index of a node in the graph *)
type nodei = Ograph_extended.nodei

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* this requires to have 'gv' (ghostview) installed on your machine *)
let display_cfg (cfg: t) =
  cfg |> Ograph_extended.print_ograph_mutable_generic
   ~s_of_node:(fun (nodei, node) ->
      spf "%d:%s" nodei (String.escaped (show_node node)), None, None)
