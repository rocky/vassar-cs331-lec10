# GNU Makefile to assist in building and running the 331 Language.

# These variables are used by common.mk and need to be set first
COMPILER=./compile331
MAIN_STUB_FILE=example/main.c

# Makefile code that is common this Makefile and others in the project
include common.mk

.PHONY: all clean

#: Build the 331-to-assembler compiler
all: $(COMPILER)

#: Compile to assembly a 331 program
%.s: %.331 compile331
	$(COMPILER) $< > $@

#: Remove all derived files in this directory and in example
clean:
	@rm -v $(COMPILER) *.cmi *.cmo; $(MAKE) -C example clean
