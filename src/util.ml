open Printf
open Token
open Sparser

let get_token_list lexbuf = 
  let rec work acc = 
    match Scanner.token lexbuf with
    | EOF -> acc
    | t -> work (t :: acc)
  in List.rev (work [])

let split_by_line lst =
  let blank_line = ref true in
  let rec split_acc lst acc current =
    match lst with
    | [] -> List.rev (current :: acc)
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

let print_token_list lst =
  List.iter (fun x -> Printf.printf "%s " (string_of_token x)) lst;
  print_endline "" 
