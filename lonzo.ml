let rec reduce (exp : Expr.exp) (wexp : Expr.exp): Expr.exp =
  match exp, wexp with
  | Unit, _ -> Unit
  | EVar _, _ -> wexp
  | ELam (_, e), _ -> reduce e wexp
  | EApp (e1, e2), _ -> reduce e1 e2
  | EDec (_, e), _ -> reduce e e

let evalFile path : unit =
  let chan = open_in path in
  let exp = Parser.main Lexer.read (Lexing.from_channel chan) in
  print_endline (Expr.e_to_s exp);
  print_endline (reduce exp exp |> Expr.e_to_s);
  close_in chan;
  flush_all ()

let main () =
  Array.sub Sys.argv 1 (Array.length Sys.argv - 1) |> Array.iter evalFile;
  ()

let () = main ()
