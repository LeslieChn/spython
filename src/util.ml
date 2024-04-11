open Printf
open Token
open Sparser

let get_token_list lexbuf = 
  let rec work acc = 
    match Scanner.token lexbuf with
    | EOF -> acc
    | t -> work (t :: acc)
  in List.rev  (work [])

let split_by_line lst =
  let blank_line = ref true in
  let rec split_acc lst acc current =
    match lst with
    | [] -> List.rev (acc)
    | hd :: tl when hd = EOL && not !blank_line ->
            blank_line := true ; split_acc tl (List.rev (hd :: current) :: acc) [] 
    | hd :: tl when hd = EOL && !blank_line ->
            split_acc tl acc [] 
    | hd :: tl when hd = TAB || hd = SPACE ->
            split_acc tl acc (hd :: current)
    | hd :: tl ->
            blank_line := false; split_acc tl acc (hd :: current)
  in
  split_acc lst [] []

let create_lexbuf token_lst =
  let input = String.concat "" (List.map (string_of_token) token_lst ) in
  (*printf "token string: %s token_lst : %d\n" input (List.length token_lst);*)
  Lexing.from_string input

let get_indent_width (lst: token list list)  =
    let tab_width = 4 in 
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

let indent_to_scope (counted_lst: (int * token list) list) =
    let st = Stack.create () in 
    let rec indent_or_dedent counted_lst = 
        let indent_width = fst counted_lst in
        let lst_data = snd counted_lst in
        let st_top = Stack.top st in
        if indent_width > st_top then begin
            Stack.push indent_width st;
            LBRACE :: lst_data
        end
        else if indent_width < (Stack.top st) then begin
            Stack.pop st;
            indent_or_dedent (indent_width, RBRACE :: lst_data)
        end 
        else
            lst_data
    in
    let rec flush_indent acc = 
        if Stack.length st > 1 then (Stack.pop st; flush_indent (RBRACE :: acc))
        else
            acc
    in
    let rec go lst acc = 
        match lst with
        [] -> List.rev ((flush_indent []) :: acc)
        | hd :: tl -> go tl ((indent_or_dedent hd) :: acc)
    in Stack.push 0 st; go counted_lst []
(*
let convert_to_c (lst : token list list) = 
    let max_indent = ref 0 in
    let cur_indent = ref 0 in
    let indent_start = ref false in
    let tab_width = 4 in
    let rec append lst acc =
        match lst with
        | [] when !cur_indent > !max_indent -> acc 
        | [] when !cur_indent < !max_indent -> acc 
        | [] -> acc 
        (* during indent *)
        | hd :: tl when !indent_start = true && hd = TAB ->
                cur_indent := !cur_indent + tab_width; append tl ([] :: acc)
        | hd :: tl when !indent_start = true && hd = SPACE ->
                cur_indent := !cur_indent + 1; append tl ([] :: acc)
        | hd :: tl when !indent_start = true -> (* hit 1st token after indent *)
                if !current_indent < append tl ([] :: acc)
        (* not indent *)
        | hd :: tl when !indent_start = false && hd = TAB ->
                indent_start := true; append tl (hd :: acc)
        | hd :: tl -> append tl (hd :: acc) in
    let rec flatten lst =
        match lst with
        | [] -> [] 
        | hd :: tl -> append hd (flatten tl) in
    flatten lst
*)


let print_token_list lst =
  List.iter (fun x -> Printf.printf "%s " (string_of_token x)) lst;
  print_endline "" 
