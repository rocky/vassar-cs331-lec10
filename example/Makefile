OCAML_OPTS ?= -g
CC_331 ?= gcc
CFLAGS_331 ?= -g
PACKAGES ?= -package ounit2 -package sexplib
COMPILER=./compile331

#: Build and run a 331 program
%: %.run
	./$<

#: Build a 331 executable
%.run: %.o
	$(CC_331) $(OCAML_OPTS) -o $@ main.c $<

%.o: %.s
	$(CC_331) $(CFLAGS_331) -c $< -o $@

#: Compile to assembly a 331 program
%.s: %.331 ../compile331
	$(COMPILER) $< > $@

$(COMPILER): $(COMPILER).ml
	ocamlfind ocamlc -o $@ -thread -package ounit2 -package sexplib -linkpkg -g $<

clean:
	@rm -v *.run || true
