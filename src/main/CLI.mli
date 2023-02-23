
type conf = {
  targets: Fpath.t list;
  action: action_kind option;
}
and action_kind = 
  | DumpAST
  | DumpCFG
[@@deriving show]

type exit_code = int

val main: string array -> exit_code

(* internals *)
val parse_argv: string array -> conf

val run: conf -> exit_code
