(* Ocamllex scanner for S-Python*)

{ open Parser }

rule tokenize = parse
  [' ' '\t' '\r' '\n'] { tokenize lexbuf }
| '=' { ASSIGN }
| ';' { SEMI }
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIVIDE }
| ['a' - 'z']+ as lit { VAR(lit) }
| ['0'-'9']+ as lit { LITERAL(int_of_string lit) }
| eof { EOF }
