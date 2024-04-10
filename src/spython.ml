open Printf
open Sparser

let _ =
  let lexbuf = Lexing.from_channel stdin in
   let token_list = Util.get_token_list lexbuf in
   let stat_list = Util.split_by_line token_list in
   List.iter (fun x -> Util.print_token_list x) stat_list
 (*  List.map Util.string_of_token token_list |> List.iter (printf "%s\n")
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
*)
