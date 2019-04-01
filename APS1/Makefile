all :
	ocamlc -c ast.ml
	ocamllex lexer.mll
	ocamlyacc -v parser.mly
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	#ocamlc parser.cmo lexer.cmo print.ml
	ocamlc -o prolog parser.cmo lexer.cmo prolog.ml

clean:
	rm -rf a.out *.cmi *.cmo lexer.ml parser.mli parser.output prolog parser.ml eval
