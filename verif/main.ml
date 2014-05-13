
open Implang;;

let lexbuf = Lexing.from_channel stdin ;;

let result = Parser.main Lexer.token lexbuf in
  print_string (stmtToStr result)

