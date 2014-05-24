{
  open Parser        (* The type token is defined in parser.mli *)
  exception Eof
}

rule token = parse
          [' ' '\t' '\n' ]          { token lexbuf }     (* skip blanks *)
          |  "//"[' '-'~']*'\n'     { token lexbuf }     (* skip comments *)
          | '+'            { PLUS }
          | '-'            { MINUS }
          | '*'            { TIMES }
          | '&'            { AND }
	     | '|'            { OR }
          | '<'            { LT }
          | '='            { EQ }
          | "=="           { EQQ}
          | '>'            { GT }
          | "<="           { LE }
          | ">="           { GE }
          | '!'            { NOT }
          | '('            { LPAREN }
          | ')'            { RPAREN }
	     | '{'            { LCURL }
	     | '}'            { RCURL }
	     | ';'            { SEMI }
          | "if"           { IF }
          | "else"         { ELSE }
          | "while"        { WHILE }
          | "Pre"          { PRE }
          | "Post"         { POST }
          | "Inv"          { INV }
          | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
	     | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9']* as lxm { VAR(lxm) }
          | eof            { EOF }
