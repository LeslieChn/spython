(* Ocamllex scanner for S-Python*)

{ open Parser }

let digit = ['0'-'9']
let letter = 
let exp = ('e'|'E')('+'|'-')?['0'-'9']+
let cfloat = ('.'['0'-'9']+exp?|['0'-'9']+('.'['0'-'9']*exp?|exp))

rule tokenize = parse
  | ['\r'] { token lexbuf }
  | ':' { COLON }
  | '\t' { TAB }
  | ' ' { SPACE }
  | '\n' { EOL }
  | "not" { NOT }
  | "if" { IF }
  | "else" { ELSE }
  | "elif" { ELIF }
  | "assert" { ASSERT }
  | "pass" { PASS }
  | "continue" { CONTINUE }
  | "break" { BREAK }
  | "class" { CLASS }
  | "for" { FOR }
  | "while" { WHILE }
  | "def" { DEF }
  | "int" { INT }
  | "float" { FLOAT }
  | "str" { STRING }
  | "bool" { BOOL }
  | ',' { COMMA }
  | '.' { DOT }
  | "!=" { NEQ }
  | '<' { LT }
  | '>' { GT }
  | "<=" { LEQ }
  | ">=" { GEQ }
  | "and" { AND }
  | "or" { OR }
  | "in" { IN }
  | "return" { RETURN }
  | "range" { RANGE }
  | "None" { NONE }
  | '#' { comment lexbuf }
  | '+' { PLUS }
  | '-' { MINUS } 
  | '*' { TIMES }
  | '/' { DIVIDE }
  | "**" { EXP }
  | "+=" { PLUSEQ }
  | "-=" { MINUSEQ }
  | "*=" { TIMESEQ }
  | "/=" { DIVIDEEQ}
  | "**=" { EXPEQ }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LBRACK }
  | ']' { RBRACK }
  | "==" { EQ }
  | '=' { ASN }  
  | "->" { ARROW }
  | "print" { PRINT }
  | digit+ as id { INT_LITERAL(int_of_string id) }
  | cfloat as id { FLOAT_LITERAL(float_of_string lit) }
  | letter+(letter|digit)* as id { VARIABLE(id) }
  | eof { EOF }
  | _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

 and comment = parse
  | '\n' { EOL }
  | _ { comment lexbuf }
