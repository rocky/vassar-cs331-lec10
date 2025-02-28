#!/bin/bash
# Compile all (bad) 331 programs

bs=${BASH_SOURCE[0]}
test_all_owd=$(pwd)
mydir=$(dirname $bs)

for program in *.331; do
    printf "\nCompiling $program\n"
    ../compile331 $program
done

cd $test_all_owd
