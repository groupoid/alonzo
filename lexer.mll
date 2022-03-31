{
  open Parser
}

let ch     = ['a'-'z' 'A'-'Z' '0'-'9']
let ws     = ['\t' ' ' '\r' '\n']
let arrow  = "\xE2\x86\x92"
let lam    = "\xCE\xBB"

rule read = parse
| ws+      { read lexbuf }
| '('      { LPARENS }
| ')'      { RPARENS }
| lam      { LAM }
| arrow    { ARROW }
| ':'      { COLON }
| ','      { COMMA }
| "def"    { DEF }
| ":="     { DEFEQ }
| "abbrev" { ABBREV }
| "#eval"  { EVAL }
| ch+ as s { IDENT s }
| eof      { EOF }
