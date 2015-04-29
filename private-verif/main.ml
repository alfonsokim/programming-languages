
open Implang;;
open Printf;;
open Yices;;

let rec printVars l = match l with
					| h :: tail -> (printVars tail) ^ "\n" ^ h
					| [] -> ""

let lexbuf = Lexing.from_channel stdin ;;

let result = Parser.main Lexer.token lexbuf in
	let yices, varset = (wpc_varlist result) in 
  		print_string ((contextToStr yices)),
  		print_string (printVars (SS.elements varset) ^ "\n\n");;

print_string (yices_check ^ "\n")