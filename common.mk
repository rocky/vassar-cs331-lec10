# Common Makefile definitions for 331 compiling system

AS_331 ?= as
CC_331 ?= gcc
CFLAGS_331 ?= -g
MAKE ?= make
PACKAGES ?= -package ounit2 -package sexplib
SHELL ?= bash
VERBOSE ?= "-v"  # or set to ""

# # $(MAIN_STUB_FILE) should have already been set.
# #: Build a 331 executable
# %.run: %.o
# 	$(MAKE) $(*).331.h
# 	$(CC_331) $(CFLAGS_311) -z noexecstack -o $@ $(MAIN_STUB_FILE) $<

%.o: %.s
	$(AS_331) $(CFLAGS_331) -c $< -o $@

%.o: %.c
	$(CC_331) $(CFLAGS_331) -c $< -o $@


#: Build and run a 331 program
%: %.run
	./$<

# $(COMPILER) should be have been set.
#: Build the 331 Language to assembly compiler
$(COMPILER): $(COMPILER).ml
	ocamlfind ocamlc -o $@ -thread -package ounit2 -package sexplib -linkpkg -g $<
