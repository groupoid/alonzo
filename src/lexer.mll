{
  open Parser
  open Lexing

  let nextLine lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with pos_bol = pos.pos_cnum; pos_lnum = pos.pos_lnum + 1 }

}

let nl = "\r\n"|"\r"|"\n"
let inlineComment = "--" [^ '\n' '\r']* (nl|eof)

let ws     = ['\t' ' ']
let latin1 = [^ '\t' ' ' '\r' '\n' '(' ')' ':' ','] # '-'
let bytes2 = ['\192'-'\223']['\128'-'\191']
let bytes3 = ['\224'-'\239']['\128'-'\191']['\128'-'\191']
let bytes4 = ['\240'-'\247']['\128'-'\191']['\128'-'\191']['\128'-'\191']
let utf8   = latin1|bytes2|bytes3|bytes4
let ident  = utf8*

let defeq  = ":=" | "\xE2\x89\x94"
let arrow  = "->" | "\xE2\x86\x92"
let lam    = "\\" | "\xCE\xBB"

rule read = parse
| nl { nextLine lexbuf; read lexbuf }
| inlineComment { nextLine lexbuf; read lexbuf }
| ws+ { read lexbuf }
| '(' { LPARENS }
| ')' { RPARENS }
| lam { LAM }
| arrow { ARROW }
| ':' { COLON }
| ',' { COMMA }
| "def" { DEF }
| defeq { DEFEQ }
| "#eval" { EVAL }
| ident as s { IDENT s }
| eof { EOF }
