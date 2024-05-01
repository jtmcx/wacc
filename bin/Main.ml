open Wacc
open Sexplib

let usage = "wacc [--lex|--parse|--codegen] file"

let arg_lex = ref false
let arg_parse = ref false
let arg_codegen = ref false
let arg_input = ref ""

let argspec =
  [ ("--codegen", Arg.Set arg_codegen, "Compile an input file")
  ; ("--lex",     Arg.Set arg_lex,     "Lex an input file")
  ; ("--parse",   Arg.Set arg_parse,   "Parse an input file")
  ]

let arganon file =
  arg_input := file

let print_token tok span =
  let tok_sexps = match Wacc.Lexer.sexp_of_token tok with
    | Sexp.Atom x  -> [Sexp.Atom x]
    | Sexp.List xs -> xs
  in
  let span_sexp = Loc.sexp_of_span span in
  let sexpr = Sexp.List ([Sexp.Atom "token"; span_sexp] @ tok_sexps) in
  print_endline (Sexp.to_string sexpr)

let dump_tokens file =
  let f = open_in file in
  let lexbuf = Lexing.from_channel f in
  Wacc.Lexer.lex_iter lexbuf print_token

let dump_ast file =
  let f = open_in file in
  let lexbuf = Lexing.from_channel f in
  let prog = Wacc.Parser.program Wacc.Lexer.lex lexbuf in
  Sexp.pp_hum Format.std_formatter (Ast.sexp_of_program prog);
  Format.print_newline ()

let dump_codegen file =
  let f = open_in file in
  let lexbuf = Lexing.from_channel f in
  let prog = Wacc.Parser.program Wacc.Lexer.lex lexbuf in
  Wacc.Emit.emit stdout (Wacc.Codegen.codegen prog)

let () =
  Arg.parse argspec arganon usage;
  if !arg_input = "" then begin
    Printf.eprintf "wacc: no input files\n\n";
    Arg.usage argspec usage;
    exit 1
  end;
  if !arg_lex then dump_tokens !arg_input else
  if !arg_parse then dump_ast !arg_input else 
  dump_codegen !arg_input
