module ENV = Map.Make (String)

exception Parser of int * int * string

type tenv = Expr.exp ENV.t

let env : tenv ref = ref ENV.empty

let rec subst (x: Expr.name) (sub: Expr.exp): Expr.exp -> Expr.exp = function
  | EVar n -> if n = x then sub else EVar n
  | ELam (p, e) -> ELam(p, subst x sub e)
  | EApp (e1, e2) -> EApp (subst x sub e1, subst x sub e2)

let rec reduce (exp : Expr.exp): Expr.exp =
  match exp with
  | EVar n ->
    begin
      (* Printf.printf "EVar %s\n" n; *)
      match ENV.find_opt n !env with
      | Some v -> v
      | None -> EVar n
    end
  | ELam (p, e) ->
    begin
      (* Printf.printf "ELam %s\n" (Expr.e_to_s e); *)
      ELam (p, e)
    end
  | EApp (e1, e2) ->
    (* Printf.printf "EApp %s * %s\n" (Expr.e_to_s e1) (Expr.e_to_s e2); *)
    let re1 = reduce e1 in
    let re2 = reduce e2 in
    begin match re1 with
      | ELam (PVar (n, _), e) ->
        begin
          (* Printf.printf "SUB ELam %s\n" (subst n re2 e |> Expr.e_to_s); *)
          subst n re2 e
        end
      | _ -> EApp (re1, re2)
    end

let run (commands : Expr.command list): unit =
  let runner = function
    | Expr.Decl (n, _, e) -> env := ENV.add n (reduce e) !env;
    | Expr.Eval e -> print_endline (reduce e |> Expr.e_to_s)
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
