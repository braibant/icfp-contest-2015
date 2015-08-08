.PHONY: all clean

all:
	ocamlbuild -use-ocamlfind src/cli.native
	ln -s -f cli.native play_icfp2015

clean:
	ocamlbuild -clean

VERSION=$(shell git log --pretty=format:'%h' -n 1)
archive:
	git archive --format zip master -o lightning-$(VERSION).zip
