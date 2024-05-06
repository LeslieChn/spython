(* Ocamllex scanner for S-Python *)

{ 
  open Parser
  exception Error of char
  let rm_quotes str =
    let len = String.length str in
      if len < 2 then
        ""
      else
        String.sub str 1 (len - 2)

}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let exp = ('e'|'E')('+'|'-')?['0'-'9']+
let cfloat = ('.'['0'-'9']+exp?|['0'-'9']+('.'['0'-'9']*exp?|exp))
let cstring = ('"'[^'"''\\']*('\\'_[^'"''\\']*)*'"')

rule token = parse
 | '\r'     { token lexbuf }
 | ""       { token lexbuf }
 | "$"      { token lexbuf }
 | "#"      { comment lexbuf }
 | "\'\'\'" { m_comment lexbuf }
 | ' '      { SPACE } 
 | '\t'     { TAB }
 | '\n'     { Lexing.new_line lexbuf; EOL }
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
 | "char"   { STRING }
 | "if"     { IF }
 | "else"   { ELSE }
 | "$noelse" { NOELSE }
 | "elif"   { ELIF }
 | "while"  { WHILE }
 | "for"    { FOR }
 | "in"     { IN }
 | "return" { RETURN }
 | "break"  { BREAK }
 | "continue" { CONTINUE }
 | "import" { IMPORT }
 | "def"    { DEF }
 | "->"     { ARROW }
 | ":"      { COLON }
 | "."      { DOT }
 | "print"  { PRINT }
 | "type"   { TYPE }
 | "range"  { RANGE }
 | "pass"   { PASS }
 | "assert" { ASSERT }
 | "bool"   { BOOL }
 | "void"   { VOID }
 | "int"    { INT }
 | "float"  { FLOAT }
 | "str"    { STRING }
 | "list"   { ARR }
 | "class"  { CLASS }
 | "true" | "false" as lem { BOOL_LITERAL(bool_of_string lem)  }
 | digit+ as lem { INT_LITERAL(int_of_string lem) }
 | cfloat as lem { FLOAT_LITERAL(float_of_string lem) }
 | cstring as lem { STRING_LITERAL(rm_quotes lem) }
 | letter (digit | letter | '_')* as lem { VARIABLE(lem) }
 | eof { EOF }
 | _ as c { raise (Error c) }

and m_comment = parse
 | "\'\'\'" { token lexbuf }
 | "\'\'\'\n" { token lexbuf }
 | _ { m_comment lexbuf }

and comment = parse
 | '\n' { token lexbuf }
 | _ { comment lexbuf }
