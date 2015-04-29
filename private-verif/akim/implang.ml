
open Printf;;
open Yices;;
open Varmap;;

module StringSet = Set.Make(String)

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
	  		| Whileloop of expr * expr * stmt ;;

let unopToStr op = match op with | Not -> "!" ;;

let binopToStr op = match op with 
			| Plus -> "+" 
			| Minus -> "-" 
			| Times -> "*" 
			| And -> "&" 
			| Or -> "|" 
			| Lt -> "<"  
			| Eq -> "==" ;;


let rec exprToStr e = match e with 
		| Num(a) -> string_of_int (a)
		| Var v -> v
		| Unary( op, e ) -> unopToStr(op) ^ exprToStr(e)  
		| Binary(op, e1, e2) -> exprToStr(e1) ^ binopToStr(op) ^ exprToStr(e2);;

(* ============================================================ *)
(* ============================================================ *)

(** Converts a binary operation into yices idiom *)
let binop_to_yices op p q = match op with
			| Plus -> yices_add p q
			| Minus -> yices_diff p q
			| Times -> yices_mult p q
			| And -> yices_and p q
			| Or -> yices_or p q
			| Lt -> yices_less_than p q 
			| Eq -> yices_equal p q;;


(** Converts a binary operation into yices idiom *)
let unop_to_yices op = match op with | Not -> yices_not ;;

(** Converts a C expr to yices *)
let rec expr_to_yices expr = match expr with
		| Unary (op, expr) -> let eStr = expr_to_yices(expr) in
						   let yicesOp = unop_to_yices(op) in  
						   yicesOp(eStr)
		| Binary (op, expr1, expr2) -> let p = expr_to_yices(expr1) in
								 let q = expr_to_yices(expr2) in 
							  	 binop_to_yices(op) p q 
		| _ -> exprToStr(expr) ;;


(** Extracts all definitions of expr over set *)
let rec extract_var expr set = match expr with
						| Var v -> StringSet.add (yices_define v "int") set 
						| Binary(op, expr1, expr2) -> extract_var expr1 (extract_var expr2 set)
						| Unary(op, expr1) -> extract_var expr1 set 
						| Num(a) -> set;;


(** Converts an statement to a string *)
let rec stmt_to_str c = match c with 
                | Skip -> "\n" 
                | Pre(e) -> "Pre( " ^ exprToStr(e) ^ ");\n"
				| Post(e) -> "Post(" ^ exprToStr(e) ^ ");\n"
				| Assign(lhs, rhs) -> lhs ^ "= " ^ exprToStr(rhs) ^ "; \n"
				| Seq(c1, c2) -> stmt_to_str(c1) ^ stmt_to_str(c2)
				| Ifthen(e, c1, c2) -> "if( " ^ exprToStr(e) ^ "){ \n" 
				                    ^ stmt_to_str(c1) ^ "} else { \n" 
				                    ^ stmt_to_str(c2) ^ "} \n" 
				| Whileloop(e, inv, c1) -> 
		   			 "while(" ^ exprToStr(e) ^ "){\n" 
                              ^ "Inv(" ^ exprToStr(inv) ^ "); \n" 
                              ^ stmt_to_str(c1) ^ "}\n" ;;


(** Converts a list of asserts into a string representation *)
let rec context_to_str r = match r with
					| head :: tail -> head ^ "\n" ^ (context_to_str tail)
					| [] -> "";;

(** Converts Pre and Post conditions into yices *)
let pre_post_to_yices e r varset = let expr = (yices_assert (expr_to_yices(e))) in ([expr] @ r, varset)


(** ====== Weakest Pre Condition ========
	Returns the yices code for the current statement

	@param is_assert:	indicates if the current generation should be an assert
	@param stmt: 		current statement to translate
	@param context:		current transformation context
	@param varsSet:		variables defined so far
	@param varsMap: 	variables generated so far

	@return context: 	the initial context appended with the transformation
	@return varsSet:	the initial variables set union the variables defined in this transformation
  *)
let rec wpc ?(is_assert = true) stmt context varsSet varsMap = match stmt with
						| Skip -> (context, varsSet)

		                | Pre(expr) -> let evars = extract_var expr StringSet.empty in 
		                			let newvarsSet = (StringSet.union varsSet evars) in
		                			pre_post_to_yices expr context newvarsSet

						| Post(expr) -> let evars = extract_var expr StringSet.empty in 
									let expr = (yices_assert (yices_not (expr_to_yices(expr)))) in 
									let newvarsSet = StringSet.add expr (StringSet.union varsSet evars) in 
									(context, newvarsSet)

						| Assign(var, expr) -> let equals = (yices_equal var (expr_to_yices(expr))) in 
									let assign_expr = (if is_assert then (yices_assert equals) else equals) in 
									([assign_expr] @ context, (StringSet.add (yices_define var "int") varsSet))

						| Seq(stmt1, stmt2) -> let (expr, vars) = (wpc ~is_assert stmt2 context varsSet varsMap) in 
									wpc ~is_assert stmt1 expr (StringSet.union vars varsSet) varsMap

						| Ifthen(condition, then_body, else_body) -> 
						            let defining_expr = extract_var condition StringSet.empty in 
									let (ifVarName, varsMap) = get_varname_for "e" varsMap in 
									let assert_expr = (yices_assert (yices_equal ifVarName (expr_to_yices condition))) in 
									let varsSet = (StringSet.union defining_expr (StringSet.add assert_expr 
									  			  (StringSet.add (yices_define ifVarName "bool") varsSet))) in 
									let (trueExprs, trueVars) = match then_body with
									  					      | Skip -> (["true"], StringSet.empty)
															  | _ -> wpc ~is_assert:false then_body context varsSet varsMap in 
									let (falseExprs, falseVars) = match else_body with
																| Skip -> (["true"], StringSet.empty)
																| _ -> wpc ~is_assert:false else_body context varsSet varsMap in 
									let truePart = (yices_bool_implies ifVarName (context_to_str trueExprs)) in 
									let falsePart = (yices_bool_implies (yices_not ifVarName) (context_to_str falseExprs)) in 
									let newvarsSet = (StringSet.union falseVars (StringSet.union trueVars varsSet)) in
									let andPart = yices_and truePart falsePart in
									    ([if is_assert then (yices_assert andPart) else andPart], newvarsSet)

						| Whileloop(condition, invariants, while_body) -> (context, varsSet);;


(** Initial call for the recursive function
	builds empty context, varsSet and varsMap for the recursive parsing
  *)
let weakest_precondition stmt = wpc stmt [] StringSet.empty StringMap.empty 


