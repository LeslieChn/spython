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
    let rec indent_or_dedent counted_lst dedenting = 
        let indent_width = fst counted_lst in
        let lst_data = snd counted_lst in
        if Stack.is_empty st then
            (fprintf stderr "indentation error\n"; exit 1)
        else ( 
            let st_top = Stack.top st in
            if dedenting then
            (
                if indent_width > st_top then
                    (fprintf stderr "indentation error\n"; exit 1)
                else if indent_width < st_top then (
                    Stack.pop st;
                    indent_or_dedent (indent_width, RBRACE :: lst_data) true
                )
                else (
                    lst_data
                )
            )
            else (
                if indent_width > st_top then (
                    Stack.push indent_width st;
                    LBRACE :: lst_data
                )
                else if indent_width < st_top then (
                    Stack.pop st;
                    indent_or_dedent (indent_width, RBRACE :: lst_data) true
                ) 
                else
                    (* indent_width *)
                    lst_data 
            )
        )
    in
    let rec flush_indent acc = 
        if Stack.length st > 1 then (Stack.pop st; flush_indent (RBRACE :: acc))
        else
            acc
    in
    let rec go lst acc = 
        match lst with
        [] -> List.rev ((flush_indent []) :: acc)
        | hd :: tl -> go tl ((indent_or_dedent hd false) :: acc)
    in Stack.push 0 st; go counted_lst []


let print_token_list lst =
  List.iter (fun x -> Printf.printf "%s " (string_of_token x)) lst;
  print_endline "" 
