#!/bin/sh
#
# replace.sh - replace placeholders in template config files
# usage:
#   replace.sh template [xresources]
#   - template:   a .in file containing placeholders
#   - xresources: the location of the xresources to use (default ~/.Xresources)

# check file extension
[[ $1 == *.in ]] || {
    echo "replace.sh: template must be a .in file"
    exit 1
}

# output file stripped of .in
OUTFILE=$(echo $1 | sed -e "s/.in//")
cat $1 > $OUTFILE

# grab xrdb colors
XRES=${2:-~/.Xresources}
echo "using Xresources from $XRES"
xrdb $XRES

# replace numbered colors
for i in {0..15}
do
    sed -i "s/%cl$i%/$(xrdb -get "color$i")/" $OUTFILE
done

# replace named colors
for color in "foreground" "background"
do
    sed -i "s/%$color%/$(xrdb -get $color)/" $OUTFILE
done

echo "output written to $OUTFILE"
