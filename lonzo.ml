module ENV = Map.Make (String)

exception Parser of int * int * string

type tenv = Expr.exp ENV.t

let env : tenv ref = ref ENV.empty

let rec teval (params: Expr.param list): Expr.exp -> Expr.typexp = function
  | EVar n -> begin
      let param = List.find_opt (function Expr.PVar(pn, _) -> pn = n) params in
      match param with
      | None -> begin match ENV.find_opt n !env with
        | Some v -> teval [] v (* TODO: Improve by adding types to tenv *)
        | None -> failwith "Unbound variable"
      end
      | Some PVar (_, t) -> t
    end
  | ELam (PVar (n, t), e) -> TFun (t, teval (List.append params [PVar (n,t)]) e)
  | EApp (e1, e2) ->
    let te1 = teval params e1 in
    let te2 = teval params e2 in
    begin match te1 with
      | TVar _ -> failwith "Can't apply expression to a non-lambda expression"
      | TFun (t1, t2) -> if t1 = te2 then t2 else failwith "Can't apply te2 to te1"
    end

let rec subst (x: Expr.name) (sub: Expr.exp): Expr.exp -> Expr.exp = function
  | EVar n -> if n = x then sub else EVar n
  | ELam (p, e) -> ELam(p, subst x sub e)
  | EApp (e1, e2) -> EApp (subst x sub e1, subst x sub e2)

let rec reduce (exp : Expr.exp): Expr.exp =
  match exp with
  | EVar n -> begin match ENV.find_opt n !env with
      | Some v -> v
      | None -> EVar n
    end
  | ELam (p, e) -> ELam (p, reduce e)
  | EApp (e1, e2) ->
    let re1 = reduce e1 in
    let re2 = reduce e2 in
    begin match re1 with
      (* TODO: Is this typesafe? *)
      | ELam (PVar (n, _), e) -> reduce (subst n re2 e)
      | _ -> EApp (re1, re2)
    end

let run (commands : Expr.command list): unit =
  let runner = function
    | Expr.Decl (n, t, e) -> begin
        if teval [] e = t then
          env := ENV.add n (reduce e) !env
        else
          failwith "Incorrect resulting type"
      end
    | Expr.Eval (e, t) -> begin
        if teval [] e = t then
          print_endline (reduce e |> Expr.e_to_s)
        else
          failwith "Incorrect resulting type"
      end
  in List.iter runner commands

let evalFile path : unit =
  let chan = open_in path in
  let lexbuf = Lexing.from_channel chan in
  try
    let commands = Parser.main Lexer.read lexbuf in
    run commands;
    close_in chan;
    flush_all ()
  with Parser.Error ->
    Parser (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf, Lexing.lexeme lexbuf)
    |> raise

let main () =
  Array.sub Sys.argv 1 (Array.length Sys.argv - 1) |> Array.iter evalFile;
  ()

let () = main ()
