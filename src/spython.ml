open Ast
open Sast
open Util

let fpath = ref ""
let fpath_set = ref false
let exe_name = ref ""
let llvm_name = ref ""
let usage_msg = "usage: ./spython [file.mc] [-o] <output>" 

let speclist =
[
  ( "[file]", Arg.String (fun foo -> ()), "input file name");
  ( "-o", Arg.Set_string exe_name, "output file name");
]

let search_env_opt env name = 
  if not (Filename.is_relative name) 
    then if Sys.file_exists name 
      then Some name 
      else None
  else let env_string = Sys.getenv_opt env in
    match env_string with
      | None -> if Sys.file_exists name then Some name else None
      | Some x -> let paths = String.split_on_char ':' x in
          let curr = List.find_opt (fun path -> Sys.file_exists (Filename.concat path name)) paths in
          match curr with
            | None -> if Sys.file_exists name then Some name else None
            | Some path -> Some (Filename.concat path name)

let rec strip_after_stmt = function 
  | If(a, b, c) -> If(a, strip_after_stmt b, strip_after_stmt c)
  | While(a, b) -> While(a, strip_after_stmt b)
  | For(a, b, c) -> For(a, b, strip_after_stmt c)
  | Range(a, b, c) -> Range(a, b, strip_after_stmt c)
  | Block(x) -> Block(strip_after [] x)
  | Func(a, b, c) -> Func(a, b, strip_after_stmt c)
  | _ as x -> x
and strip_after out = function
  | [] -> List.rev out
  | Return e :: t -> List.rev ((strip_after_stmt (Return e)) :: out)
  | Continue :: t -> List.rev ((strip_after_stmt Continue) :: out)
  | Break :: t -> List.rev ((strip_after_stmt Break) :: out)
  | a :: t -> strip_after ((strip_after_stmt a) :: out) t

let pre_process fname = 
  let path = search_env_opt "PATH" fname in
  let name = match path with
    | None -> raise (Failure ("FileNotFoundError: unable to open file " ^ fname)) 
    | Some x -> x in
  let get_ast path = 
    let input = open_in path in
    let lexbuf = Lexing.from_channel input in
     let prep_token = ref (Prep.prepare_token lexbuf) in
     let prep_lexbuf = Prep.create_lexbuf !prep_token in
     let token prep_lexbuf =
         match !prep_token with
         | [] -> Parser.EOF
         | ht :: tl -> prep_token := tl; ht in
    let program = Parser.program token (Lexing.from_string "") in program
  in strip_after [] (get_ast name)

let process_output_to_list = fun command -> 
  let chan = Unix.open_process_in command in
  let res = ref ([] : string list) in
  let rec process_otl_aux () =  
    let e = input_line chan in
    res := e :: !res;
    process_otl_aux() in
  try process_otl_aux ()
  with End_of_file ->
    let stat = Unix.close_process_in chan in (List.rev !res, stat)

let cmd_to_list command =
  let (l, _) = process_output_to_list command in l

let rec sstmt_iterator = function 
  | SIf(a, b, c) -> SIf(strip_return_expr a, sstmt_iterator b, sstmt_iterator c)
  | SWhile(a, b) -> SWhile(strip_return_expr a, sstmt_iterator b)
  | SFor(a, b, c) -> SFor(a, strip_return_expr b, sstmt_iterator c)
  | SRange(a, b, c) -> SRange(a, strip_return_expr b, sstmt_iterator c)
  | SBlock(x) -> SBlock(strip_return [] x)
  | SFunc({ styp; sfname; sformals; slocals; sbody }) -> SFunc({ styp; sfname; sformals; slocals; sbody = sstmt_iterator sbody; })
  | SExpr(e) -> SExpr(strip_return_expr e)
  | SReturn(e) -> SReturn(strip_return_expr e)
  | SAsn(a, e) -> SAsn(a, strip_return_expr e)
  | SPrint(e) -> SPrint(strip_return_expr e)
  | SClass(a, b) -> SClass(a, sstmt_iterator b)
  | SStage(a, b, c) -> SStage(sstmt_iterator a, sstmt_iterator b, sstmt_iterator c)
  | SType(e) -> let (e', typ) = e in print_endline (string_of_typ typ); let _ = strip_return_expr e in SNop
  | _ as x -> x
and strip_return_expr sexpr = let (e, t) = sexpr in 
  let e' = (match e with
  | SCall(e, el, s) -> SCall(e, el, sstmt_iterator s)
  | _ as x -> x) in
  (e', t)
and strip_return out = function
  | [] -> List.rev out
  | SReturn e :: t -> List.rev ((sstmt_iterator (SReturn e)) :: out)
  | a :: t -> strip_return ((sstmt_iterator a) :: out) t
let safe_remove name = try Sys.remove name with _ -> () 
let codegen sast fname = 
  try 
    let m = Codegen.translate sast true in
    Llvm_analysis.assert_valid_module m;
    llvm_name := (match String.length !exe_name with 
      | 0 -> fname ^ ".ll"
      | _ -> !exe_name ^ ".ll");
    let oc = open_out !llvm_name in
    Printf.fprintf oc "%s\n" (Llvm.string_of_llmodule m); close_out oc;
    exe_name := (match String.length !exe_name with 
      | 0 -> "a.out"
      | _ -> !exe_name);
    let output = cmd_to_list ("clang -lm -w " ^ !llvm_name ^ " -o " ^ !exe_name ^ " && ./" ^ !exe_name)
    in safe_remove !llvm_name; output
  with
    | Not_found -> raise (Failure ("CodegenError: compiling error!"))

let run map fname =
  try
    let original_path = Sys.getcwd () in
    let program = Sys.chdir (Filename.dirname fname); pre_process (Filename.basename fname) in
    let (sast, map') = (Semant.check [] [] { forloop = false; inclass = false; cond = false; noeval = false; stack = TypeMap.empty; func = false; globals = map; locals = map; } program) in
    let (sast, globals) = sast in
    let sast = (strip_return [] sast, globals) in 
    let _ = Sys.chdir original_path in
    let output = codegen sast (Filename.remove_extension (Filename.basename fname)) in
    List.iter print_endline output; flush stdout;
  with
    | Parsing.Parse_error -> Printf.printf "ParseError: invalid syntax!\n"; flush stdout
    | _ -> Printf.printf "Unknown error!\n"; flush stdout

let _ =
  Arg.parse speclist (fun path -> fpath := path) usage_msg;
  if (String.length !fpath) <> 0 then run StringMap.empty !fpath 
  else
      Printf.eprintf "%s\n" usage_msg; flush stderr
