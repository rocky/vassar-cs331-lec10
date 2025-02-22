# GNU Makefile to assist in building and running the 331 Language.

# These variables are used by common.mk and need to be set first
COMPILER=../compile331
MAIN_STUB_FILE=main.c

# Makefile code that is common this Makefile and others in the project
include ../common.mk


#: Build and run a 331 program
%: %.run
	./$<

#: Compile to a 331 source program to assembly
%.s: %.331 ../compile331
	$(COMPILER) $< > $@

#: Build a dummy include file as a substitute for the 331 source
%.331.h: %.331
	./make-debug-include.sh $<


clean:
	@rm -v *.run || true
