compile: compile.ml
	 ocamlfind ocamlc -o compile -thread -package ounit2 -package sexplib -linkpkg -g compile.ml

%.run: %.o
	gcc -o $@ main.c $<

%.o: %.s
	gcc -c $<

%.s: %.331 compile
	./compile $< > $@
	 

