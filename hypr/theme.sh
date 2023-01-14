#!/usr/bin/env zsh

HYPRLAND_DIR=$(dirname $0)
CONFIG_DIR=$HOME/.config

THEME=$HYPRLAND_DIR/colors.json
WALLPAPER=$HYPRLAND_DIR

# clear wal cache
wal -c 

# check for a specified theme, otherwise use wallpaper
if [[ -f $THEME ]] then 
    wal --cols16 -e -f $THEME 
else 
    wal --cols16 -e -n -i $WALLPAPER
fi

# set wallpaper
swaybg --image $HYPRLAND_DIR/wallpaper.png &

# update sass
cd $CONFIG_DIR/swaync 
sass style.scss style.css

cd $CONFIG_DIR/wofi
sass style.scss style.css
