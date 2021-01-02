.PHONY: all

all: mlverilog

mlverilog: mlverilog.cmo lex_and_parse.cmo lex_and_parse.cmi
	ocamlc -o mlverilog lex_and_parse.cmo mlverilog.cmo

lex_and_parse.cmo: lex_and_parse.ml
	ocamlc -c lex_and_parse.ml

lex_and_parse.cmi: lex_and_parse.mli
	ocamlc -c lex_and_parse.mli

mlverilog.cmo: mlverilog.ml lex_and_parse.cmi
	ocamlc -c mlverilog.ml

clean:
	-rm -f mlverilog mlverilog.cmo mlverilog.cmi lex_and_parse.cmo lex_and_parse.cmi
