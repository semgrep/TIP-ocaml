
(* TODO: not used for now, Typing.ml is not finished *)
type t = 
  | TInt
  (* TODO: ext: TBool *)
  | TPtr of t
  | TFun of t list * t
[@@deriving show]
