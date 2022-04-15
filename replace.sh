#!/bin/sh
#
# replace.sh - replace placeholders in template config files
# usage:
#   replace.sh template [xresources]
#   - template:   a .tmp file containing placeholders
#   - xresources: the location of the xresources to use (default ~/.Xresources)

# output file stripped of .tmp
OUTFILE=$(echo $1 | sed -e "s/.tmp//")
cat $1 > $OUTFILE

# grab xrdb colors
xrdb ${2:-~/.Xresources}

# replace numbered colors
for i in {0..15}
do
    sed -i "s/%cl$i%/'$(xrdb -get "color$i")'/" $OUTFILE
done

# replace named colors
for color in "foreground" "background"
do
    sed -i "s/%$color%/'$(xrdb -get $color)'/" $OUTFILE
done
