(* Alonzo (c) 2025 5HT $ ocamlc -o alonzo stlc.ml *)

type term =
    | Var of string
    | Star
    | Arrow of term * term
    | Lam of string * term * term
    | App of term * term
    | Unit
    | Prod of term * term
    | Pair of term * term
    | Pr1 of term
    | Pr2 of term

let rec string_of_term = function
    | Var x -> x
    | Star -> "*"
    | Arrow (a, b) -> "(" ^ string_of_term a ^ " -> " ^ string_of_term b ^ ")"
    | Lam (x, a, t) -> "λ (" ^ x ^ " : " ^ string_of_term a ^ "), " ^ string_of_term t
    | App (t1, t2) -> "(" ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ")"
    | Unit -> "unit"
    | Prod (a, b) -> "(" ^ string_of_term a ^ " * " ^ string_of_term b ^ ")"
    | Pair (t1, t2) -> "<" ^ string_of_term t1 ^ ", " ^ string_of_term t2 ^ ">"
    | Pr1 t -> "π1(" ^ string_of_term t ^ ")"
    | Pr2 t -> "π2(" ^ string_of_term t ^ ")"

type context = (string * term) list

let rec subst x s = function
    | Var y -> if x = y then s else Var y
    | Arrow (a, b) -> Arrow (subst x s a, subst x s b)
    | Lam (y, a, b) when x <> y -> Lam (y, subst x s a, subst x s b)
    | App (f, a) -> App (subst x s f, subst x s a)
    | Prod (a, b) -> Prod (subst x s a, subst x s b)
    | Pair (t1, t2) -> Pair (subst x s t1, subst x s t2)
    | Pr1 t -> Pr1 (subst x s t)
    | Pr2 t -> Pr2 (subst x s t)
    | t -> t

let rec lookup x = function
    | [] -> raise (Failure ("Unbound variable: " ^ x))
    | (y, typ) :: rest -> if x = y then typ else lookup x rest

let rec equal ctx t1 t2 = match t1, t2 with
    | Var x, Var y -> x = y
    | Star, Star -> true
    | Arrow (a, b), Arrow (a', b') -> equal ctx a a' && equal ctx b b'
    | Lam (x, a, b), Lam (y, a', b') -> equal ctx a a' && equal ((x, a) :: ctx) b (subst y (Var x) b')
    | App (f, a), App (f', a') -> equal ctx f f' && equal ctx a a'
    | Unit, Unit -> true
    | Prod (a, b), Prod (a', b') -> equal ctx a a' && equal ctx b b'
    | Pair (t1, t2), Pair (t1', t2') -> equal ctx t1 t1' && equal ctx t2 t2'
    | Pr1 t, Pr1 t' -> equal ctx t t'
    | Pr2 t, Pr2 t' -> equal ctx t t'
    | Lam (x, _, b), t -> equal ctx b (App (t, Var x))
    | t, Lam (x, _, b) -> equal ctx (App (t, Var x)) b
    | _ -> false

and reduce ctx t = match t with
    | App (Lam (x, _, b), a) -> subst x a b
    | App (f, a) -> App (reduce ctx f, reduce ctx a)
    | Pr1 (Pair (t1, _)) -> t1
    | Pr2 (Pair (_, t2)) -> t2
    | Pr1 t -> Pr1 (reduce ctx t)
    | Pr2 t -> Pr2 (reduce ctx t)
    | _ -> t

and normalize ctx t =
    let t' = reduce ctx t in
    if equal ctx t t' then t else normalize ctx t'

and infer ctx t = let res = match t with
    | Var x -> lookup x ctx
    | Star -> Star
    | Arrow (a, b) ->
        let _ = infer ctx a in
        let _ = infer ctx b in
        if equal ctx (infer ctx a) Star && equal ctx (infer ctx b) Star then Star
        else raise (Failure "Arrow type components must be of type *")
    | Lam (x, a, b) ->
        let _ = infer ctx a in
        if not (equal ctx (infer ctx a) Star) then raise (Failure "Lambda annotation must be a type");
        Arrow (a, infer ((x, a) :: ctx) b)
    | App (f, a) ->
        let f_type = infer ctx f in
        (match f_type with
         | Arrow (a', b) ->
             let a_type = infer ctx a in
             if equal ctx a_type a' then b
             else raise (Failure "Argument type mismatch")
         | _ -> raise (Failure "Application requires an arrow type"))
    | Unit -> Star
    | Prod (a, b) ->
        let _ = infer ctx a in
        let _ = infer ctx b in
        if equal ctx (infer ctx a) Star && equal ctx (infer ctx b) Star then Star
        else raise (Failure "Product type components must be of type *")
    | Pair (t1, t2) ->
        let t1_type = infer ctx t1 in
        let t2_type = infer ctx t2 in
        Prod (t1_type, t2_type)
    | Pr1 t ->
        let t_type = infer ctx t in
        (match t_type with
         | Prod (a, _) -> a
         | _ -> raise (Failure "First projection requires a product type"))
    | Pr2 t ->
        let t_type = infer ctx t in
        (match t_type with
         | Prod (_, b) -> b
         | _ -> raise (Failure "Second projection requires a product type"))
    in normalize ctx res

(* Test Suite *)

let id_type = Arrow (Star, Star)
let id = Lam ("x", Star, Var "x")
let unit_type = Star
let pair_type = Prod (Star, Star)
let pair_test = Pair (Unit, Unit)
let pr1_test = Pr1 pair_test
let pr2_test = Pr2 pair_test

let beta = (App (Lam ("x", Star, Var "x"), Unit), Unit)
let eta = (Lam ("x", Star, App (Var "f", Var "x")), Var "f")
let pair_beta = (Pr1 (Pair (Unit, Unit)), Unit)

let rec test_equal ctx t1 t2 =
    let t1' = normalize ctx t1 in
    let t2' = normalize ctx t2 in
    equal ctx t1' t2'

let run_type_test name term expected_type =
    let ctx = [] in
    try
        let inferred = infer ctx term in
        let norm_inferred = normalize ctx inferred in
        let norm_expected = normalize ctx expected_type in
        Printf.printf "Test %s:\n- Term: %s\n- Inferred: %s\n- Expected: %s\n- Result: %s\n\n"
          name
          (string_of_term term)
          (string_of_term norm_inferred)
          (string_of_term norm_expected)
          (if test_equal [] norm_inferred norm_expected then "PASS" else "FAIL")
    with
    | Failure msg -> Printf.printf "Test %s: Failed with error: %s\n\n" name msg

let run_equality_test ctx name (t1, t2) =
    let result = test_equal ctx t1 t2 in
    Printf.printf "Equality Test %s:\n- Term1: %s\n- Term2: %s\n- Result: %s\n\n"
      name
      (string_of_term t1)
      (string_of_term t2)
      (if result then "PASS" else "FAIL")

let () =
    let ctx = [("f", Arrow (Star, Star)); ("u", Star)] in
    run_type_test "Identity" id id_type;
    run_type_test "Unit" Unit unit_type;
    run_type_test "Pair" pair_test pair_type;
    run_type_test "First Projection" pr1_test Star;
    run_type_test "Second Projection" pr2_test Star;
    run_equality_test ctx "Beta Reduction" beta;
    run_equality_test ctx "Eta Equivalence" eta;
    run_equality_test [] "Pair Beta" pair_beta;
