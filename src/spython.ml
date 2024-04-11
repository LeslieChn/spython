open Printf
open Sparser

(*let _ =
  let lexbuf = Lexing.from_channel stdin in
   let token_list = Util.get_token_list lexbuf in
   let stat_list = Util.split_by_line token_list in
   List.iter (fun x -> Util.print_token_list x) stat_list
   *)

let _ =
    let lexbuf = Lexing.from_channel stdin in
    let token_list = Util.get_token_list lexbuf in
    let stat_list = Util.split_by_line token_list in
    let counted_list = Util.get_indent_width stat_list in
    let indented_list = Util.indent_to_scope counted_list in
    let iii = List.flatten indented_list in
    printf "stat_list %d, Counted_list %d\n" (List.length stat_list) (List.length counted_list);
    List.iter (fun x -> printf "counted_list: %d\n" (fst x)) counted_list;

    Util.print_token_list iii
    (*let bb = Util.create_lexbuf ll in
    (*printf "token_list %d, ll %d" (List.length token_list) (List.length ll)*)
    let program = 
        try 
          Sparser.program_rule Scanner.token bb
        with
        | Scanner.Error(c) -> 
                fprintf stderr "Scanner error at line %d: Unknow char '%c'.\n"
                lexbuf.lex_curr_p.pos_lnum c; exit 1
        | Sparser.Error -> 
                fprintf stderr "Sparser error at line %d.\n"
                lexbuf.lex_curr_p.pos_lnum; exit 1
    in 
    print_endline (Ast.string_of_program program)
    *)
