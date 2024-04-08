let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Sparser.program_rule Scanner.token lexbuf in
  print_endline (Ast.string_of_program program)
