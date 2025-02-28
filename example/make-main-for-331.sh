#!/bin/bash
# Equivalent to basename $0; the short program name
typeset pname=${0##*/}

usage() {
  printf "Usage:

${pname} [OPTIONS] *file-path*.331

Creates a dummy C main program which includes a copy of a include 331 language file to facilitate debugging.

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

short_name=$(basename $file_331 .331)
main_file=main-for-${short_name}.c
sed -e "s/dummy.331/$file_331/ 1" main.c.in > ${main_file}
if (($?)); then
    echo >&2 "Something went wrong with replacing name in ${main_file}"
    exit 2
fi
if ((verbose)); then
    printf "Substituted part of ${main_file}\n"
    head --lines=3 ${main_file}
    echo "..."
fi
