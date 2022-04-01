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

(* TODO: Checkout lean syntax for abbrev *)
type command =
  | Decl of name * typexp * exp
  | Eval of exp

let rec app f = function
  | []      -> f
  | x :: xs -> app (EApp (f, x)) xs

let rec t_to_s (t : typexp): name :> string =
  match t with
  | TVar n -> n
  | TFun (t1, t2) -> String.cat "→" (t_to_s t2) |> String.cat (t_to_s t1)

let p_to_s (p : param): name :> string =
  match p with
  | PVar (n, t) -> String.cat ":" (t_to_s t) |> String.cat n

let rec e_to_s (exp: exp): name :> string =
  match exp with
  | ELam (p, e) -> String.cat "." (e_to_s e) |> String.cat(p_to_s p) |> String.cat "λ"
  | EApp (e1, e2) -> String.cat (e_to_s e1) (e_to_s e2)
  | EVar v -> v
