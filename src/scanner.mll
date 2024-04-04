(* Ocamllex scanner for S-Python*)

{ open Sparser }

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']

rule token = parse
  | '\r' { token lexbuf }
  | ' ' { SPACE}
  | '\t' { TAB }
  | '\n' { EOL }
  | "#" { comment lexbuf }
  | "\'\'\'" { m_comment lexbuf }
  | "int" { INT }
  | '=' { ASSIGN }
  | digit+ as lem { INT_LITERAL(int_of_string lem) }
  | letter (digit | letter | '_')* as lem { VARIABLE(lem) }
  | eof { EOF }
  | _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

 and m_comment = parse
  | "\'\'\'" { token lexbuf }
  | _ { m_comment lexbuf }

 and comment = parse
  | '\n' { token lexbuf }
  | _ { comment lexbuf }
