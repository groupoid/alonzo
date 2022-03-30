module ENV = Map.Make (String)
type tenv = Expr.exp ENV.t
let env : tenv ref = ref ENV.empty

exception Parser of int * int * string

let rec reduce (exp : Expr.exp) (wexp : Expr.exp): Expr.exp =
  match exp, wexp with
  | Unit, _ -> Unit
  | EVar _, _ -> wexp
  | ELam (_, e), _ -> reduce e wexp
  | EApp (e1, e2), _ -> reduce e1 e2

let run (commands : Expr.command list): unit =
  let runner = function
    | Expr.Decl (n, _, e) -> env := ENV.add n e !env;
    | Expr.Eval e -> print_endline (reduce e e |> Expr.e_to_s)
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
