#!/bin/bash

usage() {
    cat <<EOF
usage:
$0 331-file

Creates a dummy 331 to refer to when debugging a 331 program.
EOF
    exit 1
}



if (( $# != 1 )) ; then
    usage
fi

file_331=$1

if [[ ! -r "$file_331" ]] ; then
    echo >&2 "331 program file ${file_331} is not readable."
    usage
fi

# Checking all done. Now get to work!

file_331_h="${file_331}.h"
echo " /* ${file_331} code starts in the line below: " > $file_331_h
cat "$file_331" >> "$file_331_h"
echo "*/" >> "$file_331_h"

sed -e "s/dummy.331.h.in/$file_331_h/ 1" main.c.in > main.c
if (($?)); then
    echo >&2 "Something went wrong with replacing name in main.c"
    exit 2
fi
echo "File $file_331_h and main.c updated to support debugging."
