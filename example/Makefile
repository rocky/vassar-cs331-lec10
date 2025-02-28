# GNU Makefile to assist in building and running the 331 Language.

# These variables are used by common.mk and need to be set first
COMPILER ?= ../compile331

# Makefile code that is common this Makefile and others in the project
include ../common.mk


#: Build and run a 331 program
%: %.run %
	./$<

#: Build a 331 executable
%.run: %.o main-for-%.c
	$(MAKE) $(*).331.h
	$(CC_331) $(CFLAGS_311) -z noexecstack -o $@ main-for-$(*).c $<

main-for-%.c: main.c.in
	./make-main-for-331.sh $(VERBOSE) $(*).331

#: Compile to a 331 source program to assembly
%.s: %.331 ../compile331
	$(COMPILER) $< > $@

#: Build a dummy include file as a substitute for the 331 source
%.331.h: %.331
	./make-debug-include.sh $(VERBOSE) $<


clean:
	@rm -v *.run *.h main-for-*.c || true
