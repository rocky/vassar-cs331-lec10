# Common Makefile definitions for 311 compiling system

CC_331 ?= gcc
CFLAGS_331 ?= -g

#: Build and run a 331 program
%: %.run
	./$<

#: Build a 331 executable
%.run: %.o
	$(CC) $(OCAML_OPTS) -o $@ main.c $<

%.o: %.s
	$(CC) $(CFLAGS) -c $< -o $@

#: Compile to assembly a 331 program
%.s: %.331 $(COMPILER)
	$(COMPILER) $< > $@
