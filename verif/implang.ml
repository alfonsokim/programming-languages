open Printf;;


type binop = Plus | Minus | Times | And | Or | Lt | Eq ;;

type unop = Not ;;

type expr = Binary of binop * expr * expr 
	      | Unary of unop * expr 
	      | Var of string 
	      | Num of int;;

type stmt = Skip 
          | Post of expr
	      | Pre of expr 
          | Assign of string * expr 
	      | Seq of stmt * stmt
	      | Ifthen of expr * stmt * stmt
	      | Whileloop of expr * expr* stmt ;;

let binopToStr op = match op with 
                  | Plus -> "+" 
                  | Minus -> "-" 
                  | Times -> "*" 
                  | And -> "&" 
                  | Or -> "|" 
                  | Lt -> "<"  
                  | Eq -> "==" ;;

let unopToStr op = match op with 
                 | Not -> "!" ;;

let rec exprToStr e = match e with 
		            | Num(a) -> string_of_int (a)
		            | Var v -> v
		            | Unary( op, e ) -> unopToStr(op) ^ exprToStr(e)  
		            | Binary(op, e1, e2) -> exprToStr(e1) ^ binopToStr(op) ^ exprToStr(e2);;

 
let rec stmtToStr c = match c with 
                    | Skip -> "\n"
                    | Pre(e) -> "Pre( " ^ exprToStr(e) ^ ");\n"
		            | Post(e) -> "Post(" ^ exprToStr(e) ^ ");\n"
		            | Assign(lhs, rhs) -> lhs ^ "= " ^ exprToStr(rhs) ^ "; \n"
		            | Seq(c1, c2) -> stmtToStr(c1) ^ stmtToStr(c2)
		            | Ifthen(e, c1, c2) -> "if( " ^ exprToStr(e) ^ "){ \n" ^ 
                                            stmtToStr(c1) ^ "} else { \n" ^ 
                                            stmtToStr(c2) ^ "} \n" 
		            | Whileloop(e, inv, c1) -> 
		                      "while(" ^ exprToStr(e) ^ "){\n" ^ 
                              "Inv(" ^ exprToStr(inv) ^ "); \n" ^ 
                              stmtToStr(c1) ^ "}\n" ;;





