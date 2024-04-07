(* Abstract Syntax Tree and functions for printing it *)

type bop = Add | Sub | Mult | Div | Exp | Mod | Eq | Neq | Less | Leq | Greater | Geq |
           And | Or

type uop = Neg | Not

type typ = Bool | Int | Float | String | Void

type literal = 
  | BoolLit of bool
  | IntLit of int
  | FloatLit of float
  | StringLit of string

type expr = 
  | Lit of literal

type stmt = 
  | Expr of expr
  | Print of expr

(* Pretty-printing functions *)
let rec string_of_lit = function
  | IntLit(i) -> string_of_int i
  | BoolLit(b) -> string_of_bool b
  | FloatLit(f) -> string_of_float f
  | StringLit(s) -> s

let string_of_expr = function
  | Lit(l) -> string_of_lit l

let string_of_stmt = function
  | Expr(e) -> string_of_expr e ^ "\n"
  | Print(e) -> string_of_expr e

let string_of_program l = 
  "\n\nParsed program: \n\n" ^
  String.concat "" (List.map string_of_stmt l) ^ "\n"
