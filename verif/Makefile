a.out : lexer.mll parser.mly main.ml
	ocamllex lexer.mll
	ocamlyacc parser.mly
	ocamlc -c yices.ml
	ocamlc -c varmap.ml
	ocamlc -c implang.ml
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c main.ml
	ocamlc -o a.out varmap.cmo yices.cmo implang.cmo lexer.cmo parser.cmo main.cmo

clean:
	rm -f a.out varmap.cm* yices.cm* implang.cm* lexer.ml lexer.cm* parser.cm* parser.ml parser.mli main.cm*

