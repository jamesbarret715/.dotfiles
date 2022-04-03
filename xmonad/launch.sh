#!/bin/sh
#
# launch.sh - xmonad launch script
# 

feh --bg-fill --no-fehbg $HOME/.config/xmonad/wallpaper.* # set wallpaper
picom -b --backend glx                                    # start compositor
pulseaudio &                                              # start pulseaudio
