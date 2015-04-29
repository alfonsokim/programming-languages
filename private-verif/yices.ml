let yices_logical_binop p q op = match q with
								| "" -> "(" ^ op ^ " " ^ p ^ ")"
								| _ -> "(" ^ op ^ " " ^ p ^ " " ^ q ^ ")"
let yices_logical_unop p op = yices_logical_binop p "" op

(*
	Boolean operators
*)
let yices_and p q = yices_logical_binop p q "and"
let yices_or p q = yices_logical_binop p q "or"
let yices_not p = yices_logical_unop p "not"
let yices_implies p q = yices_or (yices_not p) q
let yices_bool_implies p q = yices_logical_binop p q "=>"

(* 
	Arithmetic expresions
 *)
let yices_less_than p q = yices_logical_binop p q "<"
let yices_greater_than p q = yices_logical_binop p q ">"
let yices_less_equal p q = yices_logical_binop p q "<="
let yices_greater_equal p q = yices_logical_binop p q ">="
let yices_add p q = yices_logical_binop p q "+"
let yices_mult p q = yices_logical_binop p q "*"
let yices_neg p = yices_logical_unop p "-"
let yices_diff p q = yices_logical_binop p q "-"
let yices_exp p q = yices_logical_binop p q "^"
let yices_div p q = yices_logical_binop p q "/"


(*
	Equality and disequality
*)
let yices_equal p q = yices_logical_binop p q "="
let yices_notequal p q = yices_logical_binop p q "/="

(* 
	Assertions
 *)
let yices_assert p = yices_logical_unop p "assert"

(* 
	Definitions
*)
let yices_define p t = "(define " ^ p ^ "::" ^ t ^ ")" 

(* 
	Check & Model
*)
let yices_check = "(check)"
let yices_model = "(show-model)"