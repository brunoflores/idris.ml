%{
  open Idris_ast

  let pos_of_lexing_pos (pos : Lexing.position) : pos =
    { pos_fname = pos.pos_fname;
      pos_lnum = pos.pos_lnum;
      pos_bol = pos.pos_bol;
      pos_cnum = pos.pos_cnum }
%}

%token MODULE          (* literal "module" *)
%token <string> ID     (* identifier *)
%token LPAREN          (* literal "(" *)
%token RPAREN          (* literal ")" *)
%token COLON           (* literal ":" *)
%token <string> STRING (* literal string *)
%token <int> INT       (* literal integer *)
%token EQ              (* literal "=" *)
%token NEWLINE         (* line break *)
%token EOF             (* end of file *)

%start <exp option> prog

%%

prog:
  | MODULE; modname = ID; NEWLINE+; decs = dec+; EOF
    { Some (ModuleExp { modname; modbody = (Decs decs) }) }
  | decs = dec+; EOF
    { Some (Decs decs) }
  | EOF
    { None }

dec:
  | funname = ID; funtype = optty?; NEWLINE+; fundef = fundef
    { { funname; funtype; fundefs = [fundef] } }

// fundefs:
//   | f = fundef; NEWLINE+
//     { fs }
//   | f = fundef
//     { f }

fundef:
  | fundefname = ID; EQ; fundefbody = exp
    { { fundefname; fundefbody } }

exp:
  | id = ID; args = constant*; NEWLINE+
    { CallExp { id; args } }

constant:
  | x = INT
    { IntExp x }
  | x = STRING
    { StringExp (x, (pos_of_lexing_pos $startpos)) }

optty:
  | COLON; ty = ty
    { ty }

ty:
  | id = ID; LPAREN; RPAREN
    { id }
