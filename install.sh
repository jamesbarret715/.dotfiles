#!/bin/sh
#
# install.sh - link dotfiles to their relevant locations
# usage:
#   install.sh [config]
#   - config: parent directory of config files (defaults to ~/.config)
#

shopt -s nocasematch

# ensure that config directory exists
CONFIG=${XDG_CONFIG_HOME:-$HOME/.config}
[[ -d $CONFIG ]] || {
    echo "install.sh: $CONFIG is not a directory"
    exit 1
}

# alacritty
read -p "add alacritty config? [Y/n] " choice
[[ $choice =~ (n|no) ]] || {
    mkdir -p $CONFIG/alacritty
ln -sf $PWD/alacritty/alacritty.yml $CONFIG/alacritty/alacritty.yml

    echo "linked alacritty.yml to $CONFIG/alacritty/"
}

# git
read -p "add gitconfig? [Y/n] " choice 
[[ $choice =~ (n|no) ]] || {
    ln -sf $PWD/git/gitconfig $HOME/.gitconfig

    echo "linked gitconfig to $HOME/.gitconfig"
}

# nvim 
read -p "add nvim config? [Y/n] " choice
[[ $choice =~ (n|no) ]] || {
    mkdir -p $CONFIG/nvim/autoload
    ln -sf $PWD/nvim/init.vim $CONFIG/nvim/init.vim
    ln -sf $PWD/nvim/autoload/plug.vim $CONFIG/nvim/autoload/plug.vim
    ln -sf $PWD/nvim/coc-settings.json $CONFIG/nvim/coc-settings.json

    echo "linked init.vim and plug.vim to $CONFIG/nvim/"
}

# picom
read -p "add picom config? [Y/n] " choice
[[ $choice =~ (n|no) ]] || {
    mkdir -p $CONFIG/picom
    ln -sf $PWD/picom/picom.conf $CONFIG/picom/picom.conf

    echo "linked picom.conf to $CONFIG/picom/"
}

# rofi
read -p "add rofi config? [Y/n] " choice 
[[ $choice =~ (n|no) ]] || {
    mkdir -p $CONFIG/rofi/themes
    ln -sf $PWD/rofi/config.rasi $CONFIG/rofi/config.rasi
    ln -sf $PWD/rofi/themes/myTheme.rasi $CONFIG/rofi/themes/myTheme.rasi

    echo "linked config.rasi and myTheme.rasi to $CONFIG/rofi/"
}

# xmobar
read -p "add xmobar config [Y/n] " choice
[[ $choice =~ (n|no) ]] || {
    mkdir -p $CONFIG/xmobar
    ln -sf $PWD/xmobar/xmobarrc $CONFIG/xmobar/xmobarrc

    echo "linked xmobarrc to $CONFIG/xmobar/"
}

# xmonad
read -p "add xmonad config? [Y/n] " choice
[[ $choice =~ (n|no) ]] || {
    mkdir -p $CONFIG/xmonad
    ln -sf $PWD/xmonad/xmonad.hs $CONFIG/xmonad/xmonad.hs
    ln -sf $PWD/xmonad/wallpaper.png $CONFIG/xmonad/wallpaper.png
    ln -sf $PWD/xmonad/launch.sh $CONFIG/xmonad/launch.sh
    chmod +x $CONFIG/xmonad/launch.sh

    echo "linked xmonad.hs, wallpaper.png, and launch.sh to $CONFIG/xmonad/"
}

# xorg
read -p "add xorg config? [Y/n] " choice
[[ $choice =~ (n|no) ]] || {
    ln -sf $PWD/xorg/Xresources $HOME/.Xresources
    
    echo "linked Xresources to $HOME/.Xresources"
}

# zsh
read -p "add zsh config? [Y/n] " choice
[[ $choice =~ (n|no) ]] || {
    ln -sf $PWD/zsh/zshrc $HOME/.zshrc
    
    echo "linked zshrc to $HOME/.zshrc"
}

