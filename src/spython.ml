open Ast

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let tokenseq = Sparser.token_stream Scanner.token lexbuf in
  print_endline (stream_of_token tokenseq)
