open Ast 

module StringMap = Map.Make(String)
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
