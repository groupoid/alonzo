open Expr

exception Parser of int * int * string
exception UnboundVariable of name
exception Type of typexp * typexp
exception Application of exp * typexp * exp * typexp

let print_error : exn -> unit = function
  | UnboundVariable var -> Printf.printf "Unbound variable: %s\n" var
  | Type (expected, actual) -> Printf.printf "Types don't match.\n Expected: %s\nActual: %s\n" (t_to_s expected) (t_to_s actual)
  | Application (e1, te1, e2, te2) -> Printf.printf "Can't apply %s:%s\nto %s:%s.\n" (e_to_s e1) (t_to_s te1) (e_to_s e2) (t_to_s te2)
  | ex -> Printf.printf "Uncaught exception: %s\n" (Printexc.to_string ex)

let handle_errors (f : 'a -> 'b) (arg : 'a) : unit =
  try f arg with ex -> print_error ex; ()
