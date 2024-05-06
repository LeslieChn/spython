open Ast

type sprogram = sstmt list * bind list

and sfunc_decl = {
  styp : typ;
  sfname : string;
  sformals : bind list;
  slocals : bind list;
  sbody : sstmt
}

and sexp =
  | SBinop of sexpr * operator * sexpr 
  | SLit of literal 
  | SVar of string 
  | SUnop of uop * sexpr 
  | SCall of sexpr * sexpr list * sstmt 
  | SMethod of sexpr * string * sexpr list 
  | SField of sexpr * string 
  | SList of sexpr list * typ 
  | SNoexpr 
  | SListAccess of sexpr * sexpr 
  | SListSlice of sexpr * sexpr * sexpr 
  | SCast of typ * typ * sexpr 

and sexpr = sexp * typ

and sstmt = 
  | SFunc of sfunc_decl 
  | SBlock of sstmt list 
  | SExpr of sexpr 
  | SIf of sexpr * sstmt * sstmt 
  | SFor of bind * sexpr * sstmt 
  | SWhile of sexpr * sstmt 
  | SRange of bind * sexpr * sstmt
  | SReturn of sexpr 
  | SClass of string * sstmt 
  | SAsn of lvalue list * sexpr 
  | STransform of string * typ * typ 
  | SStage of sstmt * sstmt * sstmt 
  | SPrint of sexpr
  | SType of sexpr
  | SContinue
  | SBreak
  | SNop

and lvalue = 
  | SLVar of bind
  | SLListAccess of sexpr * sexpr
  | SLListSlice of sexpr * sexpr * sexpr

let concat_end delim = List.fold_left (fun a c -> a ^ delim ^ c) ""
let append_list v = List.map (fun c -> c ^ v)

let rec string_of_sexpr (e, t) = "(" ^ string_of_sexp e ^ ": " ^ string_of_typ t ^ ")"

and string_of_sbind = function
  | Bind(s, t) -> s ^ ": " ^ string_of_typ t

and string_of_sexp = function
  | SBinop(e1, o, e2) -> string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
  | SLit(l) -> string_of_lit l 
  | SVar(str) -> str 
  | SUnop(o, e) -> string_of_uop o ^ string_of_sexpr e
  | SCall(e, el, s) -> string_of_sexpr e ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ "):\n" ^ string_of_sstmt 1 s
  | SMethod(obj, m, el) -> string_of_sexpr obj ^ "." ^ m ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  | SField(obj, s) -> string_of_sexpr obj ^ "." ^ s
  | SList(el, t) -> string_of_typ t ^ " list : " ^ String.concat ", " (List.map string_of_sexpr el)
  | SListAccess(e1, e2) -> string_of_sexpr e1 ^ "[" ^ string_of_sexpr e2 ^ "]"
  | SListSlice(e1, e2, e3) -> string_of_sexpr e1 ^ "[" ^ string_of_sexpr e2 ^ ":" ^ string_of_sexpr e3 ^ "]"
  | SCast(t1, t2, e) -> string_of_typ t2 ^ "(" ^ string_of_sexpr e ^ ") -> " ^ string_of_typ t1
  | SNoexpr -> ""

and string_of_sstmt depth = function
  | SFunc({ styp; sfname; sformals; slocals; sbody }) -> "def " ^ sfname ^ "(" ^ String.concat ", " (List.map string_of_sbind sformals) ^ ") -> " ^ (string_of_typ styp) ^ ": [" ^ String.concat ", " (List.map string_of_sbind slocals) ^ "]\n" ^ string_of_sstmt depth sbody
  | SBlock(sl) -> concat_end (String.make (2 * depth) ' ') (append_list "\n" (List.map (string_of_sstmt (depth + 1)) sl))
  | SExpr(e) -> string_of_sexpr e
  | SIf(e, s1, s2) ->  "if " ^ string_of_sexpr e ^ ":\n" ^ string_of_sstmt depth s1 ^ (String.make (2 * (depth - 1)) ' ') ^ "else:\n" ^ string_of_sstmt depth s2
  | SFor(b, e, s) -> "for " ^ string_of_sbind b ^ " in " ^ string_of_sexpr e ^ ":\n" ^ string_of_sstmt depth s
  | SRange(b, e, s) -> "range " ^ string_of_sbind b ^ " in range (" ^ string_of_sexpr e ^ ") :\n" ^ string_of_sstmt depth s
  | SWhile(e, s) -> "while " ^ string_of_sexpr e ^ ":\n" ^ string_of_sstmt depth s
  | SReturn(e) -> "return " ^ string_of_sexpr e
  | SClass(b, s) -> "class " ^ b ^ ":\n" ^ string_of_sstmt depth s
  | SAsn(lvalues, e) -> String.concat ", " (List.map string_of_lvalue lvalues) ^ " = "  ^ string_of_sexpr e
  | STransform(s, t1, t2) -> "transform " ^ s ^ ": " ^ string_of_typ t1 ^ " -> " ^ string_of_typ t2
  | SStage(s1, s2, s3) -> "entry: " ^ string_of_sstmt depth s1 ^ " body: " ^ string_of_sstmt depth s2 ^ " exit: " ^ string_of_sstmt depth s3
  | SPrint(e) -> "print(" ^ string_of_sexpr e ^ ")"
  | SBreak -> "break"
  | SContinue -> "continue"
  | SNop -> ""

and string_of_lvalue = function
  | SLVar(sbind) -> string_of_sbind sbind
  | SLListAccess(e1, e2) ->  string_of_sexpr e1 ^ "[" ^ string_of_sexpr e2 ^ "]"
  | SLListSlice(e1, e2, e3) -> string_of_sexpr e1 ^ "[" ^ string_of_sexpr e2 ^ ":" ^ string_of_sexpr e3 ^ "]"

and string_of_sprogram (sl, bl) = String.concat "\n" (List.map (string_of_sstmt 1) sl) ^ "\n\nGlobals: [" ^ String.concat ", " (List.map string_of_sbind bl) ^ "]"
