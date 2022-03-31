%{
  open Expr
%}

%token <string> IDENT
%token LPARENS RPARENS
%token COLON COMMA LAM ARROW DEF DEFEQ ABBREV EVAL
%token EOF

%right ARROW

%start <Expr.command list> main

%%

typexp:
  | IDENT { TVar $1 }
  | typexp ARROW typexp { TFun ($1, $3) }
  | LPARENS typexp RPARENS { $2 }

param:
  | LPARENS IDENT COLON typexp RPARENS { PVar ($2, $4) }

exp:
  | IDENT { EVar $1 }
  | LAM param COMMA exp { ELam ($2, $4) }
  | exp exp { EApp ($1, $2) }
  | LPARENS exp RPARENS { $2 }

command:
  /* | ABBREV IDENT param DEFEQ typeexp { Dabb ($2, $3, $5) } */
  | DEF IDENT COLON typexp DEFEQ exp { Decl ($2, $4, $6) }
  | EVAL exp { Eval $2 }

main:
  | command main { $1 :: $2 }
  | EOF { [] }
