# GNU Makefile to to assist in building and runnin the 331 Language
.PHONY: all clean

#: Build the 311-to-assembler compiler
OCAML_OPTS ?= -g
CC ?= gcc
PACKAGES ?= -package ounit2 -package sexplib

#: Build everything. This includes the compiler
all: compile

#: Compile a OCaml program
%: %.ml
	 ocamlfind ocamlc $(OCAML_OPTS) -o $@ -thread $(PACKAGES) -linkpkg $<

#: Create an OCaml runnable executable, suffix .run
%.run: %.o
	$(CC) $(OCAML_OPTS) -o $@ main.c $<

%.o: %.s
	$(CC) -c $<

%.s: %.331 compile
	./compile $< > $@

clean:
	@rm compile *.run *.cmi *.cmo || true
