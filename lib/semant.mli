open Idris_ast

val transProg : exp -> (unit, (pos option * string) list) result
