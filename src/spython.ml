open Printf
open Sparser
open Utilities

type action = Token | Ast | Sast | LLVM_IR | Compile

let () =
  let action = ref Compile in
  let output = ref "a.out" in
  let set_action a () = action := a in
  let speclist = [
    ("-o", Arg.Set_string output, "Set output file");
    ("-t", Arg.Unit (set_action Token), "Print the Tokens");
    ("-a", Arg.Unit (set_action Ast), "Print the AST");
    ("-s", Arg.Unit (set_action Sast), "Print the SAST");
    ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
    ("-c", Arg.Unit (set_action Compile),
      "Check and print the generated LLVM IR (default)");
  ] in
  let usage_msg = "usage: ./spython [-a|-s|-l|-c] <file.spy> [-o] <output>" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;

  let lexbuf = Lexing.from_channel !channel in
  let prep_token = ref (Prep.prepare_token lexbuf) in
    match !action with
        Token -> Prep.print_token_list !prep_token
        | _ -> let prep_lexbuf = Prep.create_lexbuf !prep_token in
  let token prep_lexbuf = 
      match !prep_token with
      | [] -> Sparser.EOF
      | ht :: tl -> prep_token := tl; ht in
  let ast = Sparser.program token (Lexing.from_string "") in
  let program = Util.strip_after [] ast in
  let (sast, map') = (Semant.check [] [] { forloop = false; inclass = false; cond = false; noeval =  false; stack = TypeMap.empty; func = false; locals = StringMap.empty; globals = StringMap.empty; } program)
  in Printf.printf "hi\n"
