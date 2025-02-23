#!/bin/bash
# Compile and run all 331 programs

# First clear out any junk from prior runs.
make clean

for program in *.331; do
    executable=$(basename $program .331).run
    if ! make $executable; then
	echo "Problems compiling $program"
	continue
    fi
    ./$executable
    if (($? == 0)); then
	statu="ok"
    else
	status="not ok"
    fi
    echo "Running $executable $status"
done
