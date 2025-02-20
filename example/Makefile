CC ?= gcc
CFLAGS ?= -g

../compile:
	(cd .. && $(MAKE) compile)

%.run: %.o
	$(CC) $(OCAML_OPTS) -o $@ main.c $<

%.o: %.s
	$(CC) $(CFLAGS) -c $< -o $@

%.s: %.331 ../compile
	../compile $< > $@

clean:
	@rm *.run || true
