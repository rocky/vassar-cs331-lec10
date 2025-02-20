CC_331 ?= gcc
CFLAGS_331 ?= -g
MAKE ?= make
PACKAGES ?= -package ounit2 -package sexplib
COMPILER=../compile331

#: Build and run a 331 program
%: %.run
	./$<

#: Build a 331 executable
%.run: %.o
	$(MAKE) $(*).331.h
	$(CC_331) $(CFLAGS_331) -z noexecstack -o $@ main.c $<

%.o: %.s
	$(CC_331) $(CFLAGS_331) -c $< -o $@

#: Compile to a 331 source program to assembly
%.s: %.331 ../compile331
	$(COMPILER) $< > $@

#: Build a dummy include file as a substitute for the 331 source
%.331.h: %.331
	./make-debug-include.sh $<


$(COMPILER): $(COMPILER).ml
	ocamlfind ocamlc -o $@ -thread -package ounit2 -package sexplib -linkpkg -g $<

clean:
	@rm -v *.run || true
