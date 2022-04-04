%{ open Expr %}

%token <string> IDENT
%token LPARENS RPARENS
%token COLON COMMA LAM ARROW DEF DEFEQ EVAL
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

params:
  | param params { $1 :: $2 }
  | param        { [$1]     }

exp:
  | IDENT { EVar $1 }
  | LAM params COMMA exp { lam $2 $4 }
  | exp2 exp2+ { app $1 $2 }
  | LPARENS exp RPARENS { $2 }

exp2:
  | IDENT { EVar $1 }
  | LPARENS exp RPARENS { $2 }

command:
  | DEF IDENT COLON typexp DEFEQ exp { Decl ($2, $4, $6) }
  | EVAL exp COLON typexp { Eval ($2, $4) }

main:
  | command main { $1 :: $2 }
  | EOF { [] }
