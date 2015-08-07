.PHONY: all clean

all:
	ocamlbuild -use-ocamlfind src/cli.native

clean:
	ocamlbuild -clean
