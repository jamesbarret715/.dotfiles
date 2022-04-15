#!/bin/sh
#
# install.sh - link dotfiles to their relevant locations
#

CONFIG=$HOME/.config

# alacritty
mkdir -p $CONFIG/alacritty
ln -sf ./alacritty/alacritty.yml $CONFIG/alacritty/alacritty.yml

# git
ln -sf ./git/gitconfig $HOME/.gitconfig

# nvim 
mkdir -p $CONFIG/nvim/autoload
ln -sf ./nvim/init.vim $CONFIG/nvim/init.vim
ln -sf ./nvim/autoload/plug.vim $CONFIG/nvim/autoload/plug.vim

# picom
mkdir -p $CONFIG/picom
ln -sf ./picom/picom.conf $CONFIG/picom/picom.conf

# rofi
mkdir -p $CONFIG/rofi/themes
ln -sf ./rofi/config.rasi $CONFIG/rofi/config.rasi
ln -sf ./rofi/themes/myTheme.rasi $CONFIG/rofi/themes/myTheme.rasi

# xmobar
mkdir -p $CONFIG/xmobar
ln -sf ./xmobar/xmobarrc $CONFIG/xmobar/xmobarrc

# xmonad
mkdir -p $CONFIG/xmonad
ln -sf ./xmonad/xmonad.hs $CONFIG/xmonad/xmonad.hs
ln -sf ./xmonad/wallpaper.png $CONFIG/xmonad/wallpaper.png
ln -sf ./xmonad/launch.sh $CONFIG/xmonad/launch.sh
chmod +x $CONFIG/xmonad/launch.sh

# xorg
ln -sf ./xorg/Xresources $HOME/.Xresources

# zsh
ln -sf ./zsh/zshrc $HOME/.zshrc
