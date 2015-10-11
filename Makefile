main : main.ml parser.ml lex.ml maths.ml utils.ml selection.ml parser.cmi
	ocamlc -o main utils.ml maths.ml selection.ml parser.ml lex.ml main.ml

parser.ml : parser.mly
	ocamlyacc parser.mly

lex.ml : lex.mll
	ocamllex lex.mll

parser.cmi : parser.mli maths.cmi
	ocamlc -c parser.mli

maths.cmi : maths.ml
	ocamlc -c maths.ml 

.phony: clean

clean : 
	rm -f *.cmi *.cmo *.cmx parser.ml parser.mli lex.ml main *~
