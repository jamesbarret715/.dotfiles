#!/usr/bin/zsh

# start hyprland
if [[ ! $DISPLAY && $XDG_VTNR -le 1 ]]; then
    export MOZ_ENABLE_WAYLAND=1
    exec Hyprland
fi

