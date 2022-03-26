%{
  open Expr
%}

%token <string> IDENT
%token COLON DOT LAM ARROW DEF
%token EOF

%start <Expr.exp> main

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
  | IDENT DEF exp { EDec ($1, $3) }

main:
  | exp EOF { $1 }
