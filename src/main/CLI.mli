
type conf = {
  targets: Fpath.t list;
  action: action_kind option;
  logging_level: Logs.level option;
}
and action_kind = 
  | DumpAST
  | DumpIL
  | DumpCFG
[@@deriving show]

type exit_code = int

val main: string array -> exit_code

(* internals *)
val parse_argv: string array -> conf

val run: conf -> exit_code
