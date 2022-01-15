%{
  open Idris_ast
%}

%token MODULE          (* literal "module" *)
%token <string> ID     (* identifier *)
%token <string> STRING (* literal string *)
%token EQ              (* literal "=" *)
%token COLON           (* literal ":" *)
%token LPAREN          (* literal "(" *)
%token RPAREN          (* literal ")" *)
%token EOF             (* end of file *)

%start <exp option> prog

%%

prog:
  | EOF { Some NilExp }
