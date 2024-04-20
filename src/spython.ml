open Printf
open Sparser
open Utilities

type action = Ast | Sast | LLVM_IR | Compile

let () =
  let action = ref Compile in
  let output = ref "a.out" in
  let set_action a () = action := a in
  let speclist = [
    ("-o", Arg.Set_string output, "Set output file");
    ("-a", Arg.Unit (set_action Ast), "Print the AST");
    ("-s", Arg.Unit (set_action Sast), "Print the SAST");
    ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
    ("-c", Arg.Unit (set_action Compile),
      "Check and print the generated LLVM IR (default)");
  ] in
  let usage_msg = "usage: ./spython [-a|-s|-l|-c] <file.mc> [-o] <output>" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;

  let lexbuf = Lexing.from_channel !channel in
  let ast = Sparser.program Scanner.token (Prep.prepare_lexbuf lexbuf) in
  match !action with
    Ast -> print_string (Ast.string_of_program ast)
    | Sast    -> Printf.printf "sast\n"
    | LLVM_IR -> Printf.printf "LLVM\n"
    | Compile -> Printf.printf "Compile\n"
