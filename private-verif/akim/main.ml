
open Implang;;
open Printf;;
open Yices;;

let rec print_vars l = match l with
					| head :: tail -> (print_vars tail) ^ "\n" ^ head
					| [] -> ""

let lexbuf = Lexing.from_channel stdin ;;

let result = Parser.main Lexer.token lexbuf in
	let yices, varset = (weakest_precondition result) in 
  		print_string ((context_to_str yices)),
  		print_string (print_vars (StringSet.elements varset) ^ "\n\n");;

print_string (yices_check ^ "\n")