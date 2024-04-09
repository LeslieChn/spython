open Printf

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = 
    try
      Sparser.program_rule Scanner.token lexbuf
    with
    | Scanner.Error(c) -> fprintf stderr "Scanner error at line %d: Unknown character '%c'\n" lexbuf.lex_curr_p.pos_lnum c;
    exit 1
    | Sparser.Error -> fprintf stderr "Parser error at line %d" lexbuf.lex_curr_p.pos_lnum;
    exit 1
  in
  print_endline (Ast.string_of_program program)
