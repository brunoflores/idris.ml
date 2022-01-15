open Core
module E = MenhirLib.ErrorReports
module L = MenhirLib.LexerUtil
module I = Idris_ml.Parser.MenhirInterpreter
module Semant = Idris_ml.Semant

let print_position outx (lexbuf : Lexing.lexbuf) =
  let pos = lexbuf.lex_curr_p in
  let fname = pos.pos_fname in
  let line = pos.pos_lnum in
  let col = pos.pos_cnum - pos.pos_bol + 1 in
  fprintf outx "%s:%d:%d" fname line col

let env checkpoint =
  match checkpoint with I.HandlingError env -> env | _ -> assert false

let state checkpoint : int =
  match I.top (env checkpoint) with
  | Some (I.Element (s, _, _, _)) -> I.number s
  | None -> 0

let show text positions =
  E.extract text positions |> E.sanitize |> E.compress |> E.shorten 20

let get text checkpoint i =
  match I.get i (env checkpoint) with
  | Some (I.Element (_, _, pos1, pos2)) -> show text (pos1, pos2)
  | None -> ""

let succeed v =
  match v with
  | Some x -> (
      match Semant.transProg x with
      | Ok () -> ()
      | Error errs ->
          List.iter errs ~f:(fun (pos, s) ->
              match pos with
              | Some { pos_fname; pos_lnum; pos_bol; pos_cnum } ->
                  Format.printf "%s\tLine: %d\tColumn: %d\t%s\n" pos_fname
                    pos_lnum (pos_cnum - pos_bol) s
              | None -> print_endline s))
  | None -> ()

let fail text buffer checkpoint =
  (* Indicate where in the input file the error occurred. *)
  let location = L.range (E.last buffer) in
  (* Show the tokens just before and just after the error. *)
  let indication = sprintf "Syntax error %s.\n" (E.show (show text) buffer) in
  (* Fetch an error message from the database. *)
  let message = Idris_ml.ParserMessages.message (state checkpoint) in
  (* Expand away the $i keywords that might appear in the message. *)
  let message = E.expand (get text checkpoint) message in
  (* Show these three components. *)
  eprintf "%s%s%s%!" location indication message;
  exit 1

let parse lexbuf text =
  let supplier = I.lexer_lexbuf_to_supplier Idris_ml.Lexer.read lexbuf in
  let buffer, supplier = E.wrap_supplier supplier in
  let checkpoint = Idris_ml.Parser.Incremental.prog lexbuf.lex_curr_p in
  try I.loop_handle succeed (fail text buffer) supplier checkpoint
  with Idris_ml.Lexer.SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    ()

let get_contents s =
  let filename, content =
    match s with
    | None | Some "-" -> ("-", In_channel.input_all In_channel.stdin)
    | Some filename -> (filename, In_channel.read_all filename)
  in
  (L.init filename (content |> Lexing.from_string), content)

let loop filename =
  let lexbuf, content = get_contents filename in
  parse lexbuf content

let command =
  Command.basic ~summary:"Type-check a program"
    Command.Let_syntax.(
      let%map_open filename = anon (maybe ("filename" %: Filename.arg_type)) in
      fun () -> loop filename)

let () = Command.run command
