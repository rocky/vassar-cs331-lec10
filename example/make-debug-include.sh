#!/bin/bash

# Equivalent to basename $0; the short program name
typeset pname=${0##*/}

usage() {
  printf "Usage:

${pname} [OPTIONS] *file-path*.331

Creates a dummy 331 language #include file to refer to when debugging a 331 program.

options:
  -h  this help
  -v  verbose - show assemby file
"""
    exit 100
}


typeset -i verbose=0
TEMP=$(getopt -o 'hv' --long 'help,verbose' -- "$@")
if (( $? != 0 )); then
    echo 'Problems parsing options...' >&2
    exit 1
fi

eval set -- "$TEMP"
unset TEMP
while true; do
    case "$1" in
        -h | --help )
            usage
	    exit
	    ;;
        -v | --verbose )
            verbose=1
	    shift
	    ;;
        '--' )
	    shift
	    break
	    ;;
        * )
            echo "Unknown option $opt. Use -h or --help to see options." >&2
            exit 2
	    ;;
    esac
done

if (( $# != 1 )) ; then
    usage
fi

file_331=$1

if [[ ! -r "$file_331" ]] ; then
    echo >&2 "331 program file ${file_331} is not readable."
    usage
fi

if ((verbose)); then
    printf "Input 311-language file ${file_331}:\n"
    cat $file_331
    echo
fi

# Checking all done. Now get to work!

file_331_h="${file_331}.h"
echo " /* ${file_331} code starts in the line below: " > $file_331_h
cat "$file_331" >> "$file_331_h"
echo "*/" >> "$file_331_h"

if ((verbose)); then
    printf "Created file ${file_331_h}:\n"
    cat --number ${file_331_h}
fi
