(* Ocamllex scanner for S-Python *)

{ open Sparser }

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let exp = ('e'|'E')('+'|'-')?['0'-'9']+
let cfloat = ('.'['0'-'9']+exp?|['0'-'9']+('.'['0'-'9']*exp?|exp))

rule token = parse
  | '\r'     { token lexbuf }
  | "#"      { comment lexbuf }
  | "\'\'\'" { m_comment lexbuf }
  | ' '      { SPACE }
  | '\t'     { TAB }
  | '\n'     { EOL }
  | '='      { ASSIGN }
  | '+'      { PLUS }
  | '-'      { MINUS }
  | '*'      { TIMES }
  | '/'      { DIVIDE }
  | '%'      { MOD }
  | "**"     { EXP }
  | "+="     { PLUSEQ }
  | "-="     { MINUSEQ }
  | "*="     { TIMESEQ }
  | "/="     { DIVIDEEQ }
  | "%="     { MODEQ }
  | "**="    { EXPEQ }
  | '('      { LPAREN }
  | ')'      { RPAREN }
  | '['      { LBRACK }
  | ']'      { RBRACK }
  | '{'      { LBRACE }
  | '}'      { RBRACE }
  | ';'      { SEMI }
  | ','      { COMMA }
  | "=="     { EQ }
  | "!="     { NEQ }
  | '<'      { LT }
  | "<="     { LEQ }
  | '>'      { GT }
  | ">="     { GEQ }
  | "&&"     { AND }
  | "||"     { OR }
  | "not"    { NOT }
  | "if"     { IF }
  | "else"   { ELSE }
  | "elif"   { ELIF }
  | "while"  { WHILE }
  | "for"    { FOR }
  | "return" { RETURN }
  | "break"  { BREAK }
  | "continue" { CONTINUE }
  | "def"    { DEF }
  | "->"     { ARROW }
  | ":"      { COLON }
  | "print"  { PRINT }
  | "range"  { RANGE }
  | "pass"   { PASS }
  | "assert" { ASSERT }
  | "bool"   { BOOL }
  | "int"    { INT }
  | "true" | "false" as lem { BLIT(bool_of_string lem)  }
  | digit+ as lem { INT_LITERAL(int_of_string lem) }
  | cfloat as lem { FLOAT_LITERAL(float_of_string lem) }
  | letter (digit | letter | '_')* as lem { VARIABLE(lem) }
  | eof { EOF }
  | _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

 and m_comment = parse
  | "\'\'\'" { token lexbuf }
  | _ { m_comment lexbuf }

 and comment = parse
  | '\n' { token lexbuf }
  | _ { comment lexbuf }
