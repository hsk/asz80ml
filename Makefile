all:
	ocamlc -c ast.ml
	menhir --infer parser.mly
	rm -rf parser.mli
	ocamllex lexer.mll
	ocamlc ast.cmo parser.ml lexer.ml macro.ml assembler.ml main.ml -o test
	rm *.cm* parser.ml lexer.ml
	./test test.txt test.bin
	z80asm test.txt -o test2.bin
	z80dasm test.bin > testd.txt
	z80dasm test2.bin > test2d.txt
	diff testd.txt test2d.txt
