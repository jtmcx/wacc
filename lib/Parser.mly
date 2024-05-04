%{
open Ast
%}

%token EOF
%token LPAREN RPAREN
%token LBRACE RBRACE
%token SEMICOLON
%token MINUS
%token MINUSMINUS
%token PLUS
%token PLUSPLUS
%token STAR
%token SLASH
%token PERCENT
%token TILDE
%token INT
%token VOID
%token RETURN
%token <string> IDENT
%token <string> LITINT

%left PLUS MINUS                    /* lowest precedence */
%left STAR SLASH PERCENT            /* medium precedence */
%nonassoc UNARY_MINUS UNARY_NEGATE  /* highest precedence */

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
  | MINUS e = exp %prec UNARY_MINUS
      { Unary (Negate, e) }
  | TILDE e = exp %prec UNARY_NEGATE
      { Unary (Negate, e) }
  | e1 = exp PLUS e2 = exp
      { Binary (Add, e1, e2) }
  | e1 = exp MINUS e2 = exp
      { Binary (Subtract, e1, e2) }
  | e1 = exp STAR e2 = exp
      { Binary (Multiply, e1, e2) }
  | e1 = exp SLASH e2 = exp
      { Binary (Divide, e1, e2) }
  | e1 = exp PERCENT e2 = exp
      { Binary (Remainder, e1, e2) }
  | LPAREN e = exp RPAREN
      { e }
  ;
