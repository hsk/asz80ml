all:
	ocamlc -c ast.ml
	menhir --infer parser.mly
	rm -rf parser.mli
	ocamllex lexer.mll
	ocamlc ast.cmo parser.ml lexer.ml main.ml -o test
	rm *.cm*
	./test test.txt



