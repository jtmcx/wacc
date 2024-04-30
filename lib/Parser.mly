%{
open Ast
%}

%token EOF
%token LPAREN RPAREN
%token LBRACE RBRACE
%token SEMICOLON
%token INT
%token VOID
%token RETURN
%token <string> IDENT
%token <string> LITINT

%start <Ast.program> program

%%

program:
  | fn = func EOF
      { Program fn }
  ;

func:
  | INT name = IDENT LPAREN VOID RPAREN LBRACE body = statement RBRACE
      { Function { fn_name = name; fn_body = body } }
  ;

statement:
  | RETURN e = exp SEMICOLON
      { Return e }
  ;

exp:
  | x = LITINT
      { Constant x }
  ;
