open Idris_ast

let transProg (e : exp) =
  Format.printf "%a\n" Idris_ast.pp_exp e;
  Ok ()
