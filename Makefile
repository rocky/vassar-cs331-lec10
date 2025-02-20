# GNU Makefile to to assist in building and runnin the 331 Language
.PHONY: all clean

MAKE ?= make
SHELL ?= bash

#: Build the 311-to-assembler compiler
OCAML_OPTS ?= -g
CC_311 ?= gcc
CFLAGS_311 ?= -g
PACKAGES ?= -package ounit2 -package sexplib

#: Build the 331 compile.
all: compile

#: Compile an OCaml program
%: %.ml
	 ocamlfind ocamlc $(OCAML_OPTS) -o $@ -thread $(PACKAGES) -linkpkg $<

#: Create an OCaml runnable executable, suffix .run
%.run: %.o
	$(CC_311) $(OCAML_OPTS) -o $@ main.c $<

%.o: %.s
	$(CC_311) $(CFLAGS_311) -c $< -o $@

%.s: %.331 ./compile
	./compile $< > $@

# Remove all derived files
clean:
	@rm compile *.cmi *.cmo; $(MAKE) -C example clean
