all:
	cd src; \
	ocamlbuild -use-menhir -use-ocamlfind -pkg core -tag thread Main.native
