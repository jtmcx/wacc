%{
open Ast
%}

%token EOF
%token LPAREN RPAREN
%token LBRACE RBRACE
%token SEMICOLON
%token MINUS
%token MINUSMINUS
%token TILDE
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

unop:
  | MINUS
      { Negate }
  | TILDE
      { Complement }
  ;

exp:
  | x = LITINT
      { Constant x }
  | op = unop e = exp
      { Unary (op, e) }
  | LPAREN e = exp RPAREN
      { e }
  ;
