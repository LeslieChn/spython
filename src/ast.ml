(* Abstract Syntax Tree and functions for printing it *)

type bop = Add | Sub | Mult | Div | Exp | Mod | Eq | Neq | Less | Leq | Greater | Geq |
           And | Or

type uop = Neg | Not

type typ = Bool | Int | Float | String | Void
(*
type literal = 
  | BoolLit of bool
  | IntLit of int
  | FloatLit of float
  | StringLit of string
*)
type expr = 
  | StringLit of string

type stmt = 
  | Expr of expr
  | Print of expr

(* print program *)
(* let rec string_of_lit = function
  | IntLit(i) -> string_of_int i
  | BoolLit(b) -> string_of_bool b
  | FloatLit(f) -> string_of_float f
  | StringLit(s) -> s
*)
let rec string_of_expr = function
  | StringLit(s) -> s

let rec string_of_stmt = function
  | Expr(e) -> string_of_expr e ^ "\n"
  | Print(e) -> string_of_expr e

let string_of_program l = 
  String.concat "" (List.map string_of_stmt l)

(* print token seq *) 
type tokenseq = string list

let string_of_token l =
  "\n\nScanned program: \n" ^ (List.fold_left (fun s e -> s ^ "\n" ^ e) "" l)
