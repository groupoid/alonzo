%{
  open Expr
%}

%token <string> IDENT
%token COLON DOT LAM ARROW DEF DEFEQ ABBREV EVAL
%token EOF

%start <Expr.command list> main

%%

typexp:
  | IDENT { TVar $1 }
  | typexp ARROW typexp { TFun ($1, $3) }

param:
  | IDENT COLON typexp { PVar ($1, $3) }

exp:
  | IDENT { EVar $1 }
  | LAM param DOT exp { ELam ($2, $4) }
  | exp exp { EApp ($1, $2) }

command:
  /* | ABBREV IDENT param DEFEQ typeexp { Dabb ($2, $3, $5) } */
  | DEF IDENT COLON typexp DEFEQ exp { Decl ($2, $4, $6) }
  | EVAL exp { Eval $2 }

main:
  | command main { $1 :: $2 }
  | EOF { [] }
