open Printf
open Lexing

let parse_only = ref false
let type_only = ref false
let input_file = ref ""

let set_file s = input_file := s

let options = [
  "--parse-only", Arg.Set parse_only, " Stop after parsing";
  "--type-only", Arg.Set type_only, " Stop after type checking";
]

let usage = "Usage: ./mgoc [options] file.go"

let () =
  Arg.parse options set_file usage;

  if !input_file = "" then (
    eprintf "Erreur: Aucun fichier d'entrée spécifié\n";
    Arg.usage options usage;
    exit 1
  );

  let ch = open_in !input_file in
  let lexbuf = Lexing.from_channel ch in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = !input_file };

  try
    let prog = Mgoparser.prog Mgolexer.token lexbuf in
    if !parse_only then exit 0;
    let prog_typed = Typechecker.prog prog in
    if !type_only then exit 0;
    let code = Compile.tr_prog prog_typed in
    let output_file = (Filename.chop_suffix !input_file ".go") ^ ".s" in
    let out = open_out output_file in
    Mips.print_program out code;
    close_out out;
    exit 0

  with
  | Mgolexer.Error msg ->
      let pos_start = Lexing.lexeme_start_p lexbuf in
      let pos_end = Lexing.lexeme_end_p lexbuf in
      eprintf "File \"%s\", line %d, characters %d-%d:\nLexical error: %s\n"
        !input_file
        pos_start.pos_lnum
        (pos_start.pos_cnum - pos_start.pos_bol)
        (pos_end.pos_cnum - pos_start.pos_bol)
        msg;
      exit 1
  | Mgoparser.Error ->
      let pos_start = Lexing.lexeme_start_p lexbuf in
      let pos_end = Lexing.lexeme_end_p lexbuf in
      let tok = Lexing.lexeme lexbuf in 
      eprintf "File \"%s\", line %d, characters %d-%d:\nSyntax error on token: \"%s\"\n"
        !input_file
        pos_start.pos_lnum
        (pos_start.pos_cnum - pos_start.pos_bol)
        (pos_end.pos_cnum - pos_start.pos_bol)
        tok;
      exit 1
  | Typechecker.Error (loc, msg) ->
      let (start_p, end_p) = loc in
      eprintf "File \"%s\", line %d, characters %d-%d:\nError: %s\n"
        !input_file
        start_p.pos_lnum
        (start_p.pos_cnum - start_p.pos_bol)
        (end_p.pos_cnum - start_p.pos_bol)
        msg;
      exit 1
  | e ->
      eprintf "Anomaly: %s\n" (Printexc.to_string e);
      exit 2