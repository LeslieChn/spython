open Printf
open Token
open Parser

(* turn Lexing tokens into Parser.token list*)
let get_token_list lexbuf = 
  let rec work acc = 
    match Scanner.token lexbuf with
    | EOF -> acc
    | t -> work (t :: acc)
  in List.rev  (work [])
(* split token list into a list of token lists line by line
   and skip blank lines *)
let split_by_line lst =
  let blank_line = ref true in
  let rec split_acc lst acc current =
    match lst with
    | [] -> List.rev (acc)
    | hd :: tl when hd = EOL && not !blank_line ->
            blank_line := true ; split_acc tl (List.rev (SEMI :: current) :: acc) []
    | hd :: tl when hd = EOL && !blank_line ->
            split_acc tl acc [] 
    | hd :: tl when hd = TAB || hd = SPACE -> (* parsing heading space*)
            split_acc tl acc (hd :: current)
    | hd :: tl ->
            blank_line := false; split_acc tl acc (hd :: current)
  in
  split_acc lst [] []

(* convert list of tokens back into lexbuf*)
let create_lexbuf token_lst =
  let input = String.concat "$" (List.map (string_of_token) token_lst) in
  (* printf "token string: %s token_lst : %d\n" input (List.length token_lst);*)
  Lexing.from_string input

(* count indented space for each line *)
let get_indent_width (lst: token list list)  =
    let tab_width = 8 in 
    let rec chk_ln ln acc =
        match ln with
        hd :: tl when hd = TAB -> chk_ln tl (acc + tab_width)
        | hd :: tl when hd = SPACE -> chk_ln tl (acc + 1)
        | hd :: tl -> (acc, hd :: tl)
        | _ -> (acc, ln)
    in
    let rec iter_ln lln acc =
        match lln with
        [] -> List.rev acc
        | hd :: tl -> iter_ln tl ((chk_ln hd 0) :: acc)
    in
    iter_ln lst []

(* put indent and dedent into each line *)
let indent_to_scope (counted_lst: (int * token list) list) =
    let st = Stack.create () in 
    let rec indent_or_dedent counted_lst dedenting = 
        let indent_width = fst counted_lst in
        let lst_data = snd counted_lst in
        if Stack.is_empty st then
            (fprintf stderr "indentation error\n"; exit 1)
        else ( 
            let st_top = Stack.top st in
            if dedenting then
            (
                if indent_width <> st_top then (
                    let _ = Stack.pop st in
                    indent_or_dedent (indent_width, DEDENT :: lst_data) true
                )
                else (
                    lst_data
                )
            )
            else (
                if indent_width > st_top then (
                    Stack.push indent_width st;
                    INDENT :: lst_data
                )
                else if indent_width < st_top then (
                    let _ = Stack.pop st in
                    indent_or_dedent (indent_width, DEDENT :: lst_data) true
                ) 
                else
                    lst_data 
            )
        )
    in
    let rec flush_indent acc = 
        if Stack.length st > 1 then (let _ = Stack.pop st in flush_indent (DEDENT :: acc))
        else
            acc
    in
    let rec go lst acc = 
        match lst with
        [] -> if Stack.length st > 1 then List.rev ((flush_indent []) :: acc) else List.rev acc
        | hd :: tl -> go tl ((indent_or_dedent hd false) :: acc)
    in Stack.push 0 st; go counted_lst []

(* integrate functions above: lexbuf -> indent_to_scope lexbuf*)
let prepare_token lexbuf = get_token_list lexbuf |> split_by_line |> get_indent_width |> indent_to_scope |> List.flatten |> List.filter (fun x -> x != SPACE)

let print_token_list lst =
  List.iter (fun x -> Printf.printf "%s " (string_of_token x)) lst;
  print_endline "end of print_token_list"

