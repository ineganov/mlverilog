.PHONY: all clean

all: mlverilog

mlverilog: mlverilog.cmo lex_and_parse.cmo four_state.cmo
	ocamlc -o mlverilog lex_and_parse.cmo four_state.cmo mlverilog.cmo

four_state.cmi: four_state.cmo
four_state.cmo: lex_and_parse.mli
	ocamlc -c four_state.ml

lex_and_parse.cmo: lex_and_parse.ml
	ocamlc -c lex_and_parse.ml

lex_and_parse.cmi: lex_and_parse.mli
	ocamlc -c lex_and_parse.mli

mlverilog.cmo: mlverilog.ml lex_and_parse.cmi four_state.cmi
	ocamlc -c mlverilog.ml

clean:
	-rm -f mlverilog mlverilog.cmo mlverilog.cmi lex_and_parse.cmo lex_and_parse.cmi four_state.cmo four_state.cmi
