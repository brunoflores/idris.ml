type pos = { pos_fname : string; pos_lnum : int; pos_bol : int; pos_cnum : int }
[@@deriving show]

type symbol = string [@@deriving show]

type exp =
  | NilExp
  | IntExp of int
  | StringExp of string * pos
  | CallExp of { id : string; args : exp list }
  | ModuleExp of { modname : string; modbody : exp }
  | Decs of functiondec list
[@@deriving show]

and functiondec = {
  funname : symbol;
  funtype : symbol option;
  fundefs : functiondef list;
}
[@@deriving show]

and functiondef = { fundefname : symbol; fundefbody : exp } [@@deriving show]
