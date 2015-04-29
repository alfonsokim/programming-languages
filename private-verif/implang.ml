open Printf;;
open Yices;;
open Varmap;;

module SS = Set.Make(String)

type binop = Plus | Minus | Times | And | Or | Lt | Eq ;;

type unop = Not ;;

type expr = Binary of binop * expr * expr 
	  | Unary of unop * expr 
	  | Var of string 
	  | Num of int;;

type stmt = 
              Skip 
          	| Post of expr
	  		| Pre of expr 
          	| Assign of string * expr 
	  		| Seq of stmt * stmt
	  		| Ifthen of expr * stmt * stmt
	  		| Whileloop of expr * expr * stmt ;;

let binopToStr op = match op with 
			| Plus -> "+" 
			| Minus -> "-" 
			| Times -> "*" 
			| And -> "&" 
			| Or -> "|" 
			| Lt -> "<"  
			| Eq -> "==" ;;

let binopToYices op p q = match op with
			| Plus -> yices_add p q
			| Minus -> yices_diff p q
			| Times -> yices_mult p q
			| And -> yices_and p q
			| Or -> yices_or p q
			| Lt -> yices_less_than p q 
			| Eq -> yices_equal p q;;

let unopToStr op = match op with | Not -> "!" ;;

let unopToYices op = match op with | Not -> yices_not ;;

let rec exprToStr e = match e with 
		| Num(a) -> string_of_int (a)
		| Var v -> v
		| Unary( op, e ) -> unopToStr(op) ^ exprToStr(e)  
		| Binary(op, e1, e2) -> exprToStr(e1) ^ binopToStr(op) ^ exprToStr(e2);;

let rec exprToYices e = match e with
		| Unary (op, e) -> let eStr = exprToYices(e) in
						   let yicesOp = unopToYices(op) in  
						   yicesOp(eStr)
		| Binary (op, e1, e2) -> let p = exprToYices(e1) in
								 let q = exprToYices(e2) in 
							  	 binopToYices(op) p q 
		| _ -> exprToStr(e);;


(* 
	Extrae todas las definiciones de la expresion e
	sobre el conjunto s
*)
let rec extractVar e s = match e with
						| Var v -> SS.add (yices_define v "int") s 
						| Binary(op, e1, e2) -> extractVar e1 (extractVar e2 s)
						| Unary(op, e1) -> extractVar e1 s 
						| Num(a) -> s;;

let rec stmtToStr c = match c with 
                | Skip -> "\n" 
                | Pre(e) -> "Pre( " ^ exprToStr(e) ^ ");\n"
				| Post(e) -> "Post(" ^ exprToStr(e) ^ ");\n"
				| Assign(lhs, rhs) -> lhs ^ "= " ^ exprToStr(rhs) ^ "; \n"
				| Seq(c1, c2) -> stmtToStr(c1) ^ stmtToStr(c2)
				| Ifthen(e, c1, c2) -> "if( " ^ exprToStr(e) ^ "){ \n" ^ stmtToStr(c1) ^ "} else { \n" ^ stmtToStr(c2) ^ "} \n" 
				(* e = condicion; c1 = if; c2 = else *)
				| Whileloop(e, inv, c1) -> 
		   			 "while(" ^ exprToStr(e) ^ "){\n" 
                              ^ "Inv(" ^ exprToStr(inv) ^ "); \n" 
                              ^ stmtToStr(c1) ^ "}\n" ;;


(* *)
let rec contextToStr ?(sep = "\n") r = match r with
						| h :: tail -> h ^ sep ^ (contextToStr tail)
						| [] -> "";;

(* 
	Manejo de las condiciones PRE y POST
*)
let managePrePost e r varset = let expr = (yices_assert (exprToYices(e))) in ([expr] @ r, varset)


(* 
	Funcion principal
	Devuelve una tupla (conds, varset)
	donde
		conds - string en formato YICES con las WPC del codigo parseado
		varset - conjunto de variables definidas en el codigo

	input:
		with_assert : boolean - indica si los assign generan asserts (false en el contexto de while e if)
		op : stmt - stmt a procesar 
		r : string - contexto previo
		varset - conjunto de variables definidas hasta el momento
		varsMap - mapa con los renombres de variables
*)
let rec wpc ?(with_assert = true) op r varset varsMap = match op with
							| Skip -> (r, varset)

			                | Pre(e) -> let evars = extractVar e SS.empty in 
			                			let newvarset = (SS.union varset evars) in
			                			managePrePost e r newvarset

							| Post(e) -> let evars = extractVar e SS.empty in 
										 (* managePrePost e r newvarset *)
										 let expr = (yices_assert (yices_not (exprToYices(e)))) in 
										 let newvarset = SS.add expr (SS.union varset evars) in 
										 (r, newvarset)

							| Assign(v, e) -> let eq = (yices_equal v (exprToYices(e))) in 
											  let expr = (if with_assert then (yices_assert eq) else eq) in 
											  ([expr] @ r, (SS.add (yices_define v "int") varset))

							| Seq(c1, c2) -> let (expr, vars) = (wpc ~with_assert c2 r varset varsMap) in 
											 wpc ~with_assert c1 expr (SS.union vars varset) varsMap
							

							| Ifthen(e, c1, c2) -> let defE = extractVar e SS.empty in 
												   let (ifVarName, varsMap) = get_varname_for "e" varsMap in 
												   let assertE = (yices_assert (yices_equal ifVarName (exprToYices e))) in 
												   let varset = (SS.union defE (SS.add assertE (SS.add (yices_define ifVarName "bool") varset))) in  (* JA! *)
												   let (trueExprs, trueVars) = match c1 with
																			   | Skip -> (["true"], SS.empty)
																			   | _ -> wpc ~with_assert:false c1 r varset varsMap in 
												   let (falseExprs, falseVars) = match c2 with
																			   | Skip -> (["true"], SS.empty)
																			   | _ -> wpc ~with_assert:false c2 r varset varsMap in 
												   let truePart = (yices_bool_implies ifVarName (contextToStr trueExprs)) in 
												   let falsePart = (yices_bool_implies (yices_not ifVarName) (contextToStr falseExprs)) in 
												   let newvarset = (SS.union falseVars (SS.union trueVars varset)) in
												   let andPart = yices_and truePart falsePart in
												   ([if with_assert then (yices_assert andPart) else andPart], newvarset)

							| Whileloop(e, inv, c1) -> (r, varset);;

let wpc_varlist op = wpc op [] SS.empty StringMap.empty 


