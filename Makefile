all:
	ocamlc -c ast.ml
	menhir --infer parser.mly
	rm -rf parser.mli
	ocamllex lexer.mll
	ocamlc ast.cmo parser.ml lexer.ml macro.ml main.ml -o test
	rm *.cm* parser.ml lexer.ml
	./test test.txt
