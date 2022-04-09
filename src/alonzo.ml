open Error

module ENV = Map.Make (String)
let decls : (Expr.exp ENV.t) ref = ref ENV.empty
let abbrs : (Expr.typexp ENV.t) ref = ref ENV.empty

let rec unabbr (texp: Expr.typexp) : Expr.typexp =
  match texp with
  | TVar n ->  Option.value (ENV.find_opt n !abbrs)  ~default:texp
  | TFun (t1, t2) -> TFun (unabbr t1, unabbr t2)

let rec teval (params: Expr.param list): Expr.exp -> Expr.typexp = function
  | EVar n -> begin
      let param = List.find_opt (function Expr.PVar(pn, _) -> pn = n) params in
      match param with
      | None -> begin match ENV.find_opt n !decls with
        | Some v -> teval [] v
        | None -> raise (UnboundVariable n)
      end
      | Some PVar (_, t) -> unabbr t
    end
  | ELam (PVar (n, t), e) -> TFun (unabbr t, teval (List.append params [PVar (n,t)]) e)
  | EApp (e1, e2) ->
    let te1 = teval params e1 in
    let te2 = teval params e2 in
    begin match te1 with
      | TFun (t1, t2) when t1 = te2 -> unabbr t2
      | _ -> raise (Application (e1, te1, e2, te2))
    end

let rec subst (x: Expr.name) (sub: Expr.exp): Expr.exp -> Expr.exp = function
  | EVar n -> if n = x then sub else EVar n
  | ELam (p, e) -> ELam(p, subst x sub e)
  | EApp (e1, e2) -> EApp (subst x sub e1, subst x sub e2)

let rec reduce (exp : Expr.exp): Expr.exp =
  match exp with
  | EVar n -> begin match ENV.find_opt n !decls with
      | Some v -> v
      | None -> EVar n
    end
  | ELam (p, e) -> ELam (p, reduce e)
  | EApp (e1, e2) ->
    let re1 = reduce e1 in
    let re2 = reduce e2 in
    begin match re1 with
      | ELam (PVar (n, _), e) -> reduce (subst n re2 e)
      | _ -> EApp (re1, re2)
    end

let run (commands : Expr.command list): unit =
  let checker (expected: Expr.typexp) (e: Expr.exp) (f: unit -> unit) =
    let actual = teval [] e in
    if actual = unabbr expected then f () else raise (Type (expected, actual)) in

  let runner = function
    | Expr.Decl (n, t, e) -> checker t e (fun () -> decls := ENV.add n (reduce e) !decls);
    | Expr.Abbr (n, t) -> abbrs := ENV.add n (unabbr t) !abbrs;
    | Expr.Eval (e, t) -> checker t e (fun () -> print_endline (reduce e |> Expr.e_to_s)); in

  List.iter runner commands

let evalFile path : unit =
  let chan = open_in path in
  let lexbuf = Lexing.from_channel chan in
  try
    let commands = Parser.main Lexer.read lexbuf in
    handle_errors run commands;
    close_in chan;
    flush_all ()
  with Parser.Error ->
    Parser (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf, Lexing.lexeme lexbuf)
    |> raise

let main () =
  Array.sub Sys.argv 1 (Array.length Sys.argv - 1) |> Array.iter evalFile;
  ()

let () = main ()
