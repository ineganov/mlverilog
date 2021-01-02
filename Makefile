.PHONY: all

all: mlverilog

mlverilog: read_netlist.cmo lex_and_parse.cmo lex_and_parse.cmi
	ocamlc -o mlverilog lex_and_parse.cmo read_netlist.cmo

lex_and_parse.cmo: lex_and_parse.ml
	ocamlc -c lex_and_parse.ml

lex_and_parse.cmi: lex_and_parse.mli
	ocamlc -c lex_and_parse.mli

read_netlist.cmo: read_netlist.ml lex_and_parse.cmi
	ocamlc -c read_netlist.ml

clean:
	-rm -f mlverilog read_netlist.cmo lex_and_parse.cmo lex_and_parse.cmi
