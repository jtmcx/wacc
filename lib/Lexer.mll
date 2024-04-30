{
open Sexplib.Std

type token = 
  | RPAREN
  | LPAREN
  | RBRACE
  | LBRACE
  | SEMICOLON
  | INT
  | VOID
  | RETURN
  | IDENT of string
  | LITINT of string
  | EOF
  [@@deriving eq, show, sexp]

type error =
  | UnexpectedCharacter of (char * Loc.t)
  | UnterminatedComment

exception LexerError of error
}

let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let alpha = lower | upper
let alnum = alpha | digit
let ident = (alpha | '_') (alnum | '_')*
let line_comment = "//" [^ '\n']* ('\n' | eof )

rule lex = parse
  | [' ' '\t']+             { lex lexbuf }
  | '\n'                    { Lexing.new_line lexbuf; lex lexbuf }
  | line_comment            { Lexing.new_line lexbuf; lex lexbuf }
  | "/*"                    { lex_block_comment lexbuf }
  | '('                     { LPAREN }
  | ')'                     { RPAREN }
  | '{'                     { LBRACE }
  | '}'                     { RBRACE }
  | ';'                     { SEMICOLON }
  | "int"                   { INT }
  | "void"                  { VOID }
  | "return"                { RETURN }
  | ident as x              { IDENT x }
  | ['0'-'9']+ as x         { LITINT x }
  | eof                     { EOF }
  | _ as c                  { let loc = Loc.of_lexpos lexbuf.lex_start_p in
                              raise (LexerError (UnexpectedCharacter (c, loc)))
                            }
and lex_block_comment = parse
  | "*/"                    { lex lexbuf }
  | '\n'                    { Lexing.new_line lexbuf; lex lexbuf }
  | eof                     { raise (LexerError UnterminatedComment) }
  | _                       { lex_block_comment lexbuf }

{
let lex_with_span lexbuf =
  let open Lexing in
  let tok = lex lexbuf in
  let loc1 = lexbuf.lex_start_p in
  let loc2 = lexbuf.lex_curr_p in
  (tok, Loc.span_of_lexpos (loc1, loc2))

let rec lex_iter lexbuf f =
  let (tok, span) = lex_with_span lexbuf in
  match tok with
  | EOF -> f tok span
  | _   -> f tok span; lex_iter lexbuf f
}
