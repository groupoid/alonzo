{
  open Lexing
  open Parser

  exception SyntaxError of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
                pos_lnum = pos.pos_lnum + 1
      }
}

let ch     = ['a'-'z' 'A'-'Z' '0'-'9']
let ws     = ['\t' ' ' '\r' '\n']
let arrow  = "\xE2\x86\x92"
let def    = "="
let lam    = "\xCE\xBB"

rule read = parse
| ws+      { read lexbuf }
| lam      { LAM }
| arrow    { ARROW }
| ':'      { COLON }
| '.'      { DOT }
| def      { DEF }
| ch+ as s { IDENT s }
| eof      { EOF }
