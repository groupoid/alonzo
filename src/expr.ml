type name = string

type typexp =
  | TVar of name
  | TFun of typexp * typexp

type param =
  | PVar of name * typexp

type exp =
  | ELam of param * exp
  | EApp of exp * exp
  | EVar of name

type command =
  | Decl of name * typexp * exp
  | Abbr of name * typexp
  | Eval of exp * typexp

let rec lam params exp =
  match params with
  | []           -> exp
  | x :: xs -> ELam (x, lam xs exp)

let rec app f = function
  | []      -> f
  | x :: xs -> app (EApp (f, x)) xs

let rec t_to_s (t : typexp): name :> string =
  match t with
  | TVar n -> n
  | TFun (t1, t2) -> Printf.sprintf "(%s→%s)" (t_to_s t1) (t_to_s t2)

let p_to_s (p : param): name :> string =
  match p with
  | PVar (n, t) -> Printf.sprintf "(%s: %s)" n (t_to_s t)

let rec e_to_s (exp: exp): name :> string =
  match exp with
  | ELam (p, e) -> Printf.sprintf "(λ%s,%s)" (p_to_s p) (e_to_s e)
  | EApp (e1, e2) -> Printf.sprintf "(%s %s)" (e_to_s e1) (e_to_s e2)
  | EVar v -> v
