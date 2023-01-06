#
# zsh - POSIX shell
#

# if not running interactively, exit
[[ $- == *i* ]] || return

# environment
export EDITOR="nvim"
export MANPAGER="nvim +Man\!"
export TERMINAL="/usr/bin/wezterm"

# history
HISTFILE=$HOME/.zsh_history
HISTSIZE=1000
SAVEHIST=10000

# keybinds
bindkey  "^[[H"    beginning-of-line
bindkey  "^[[F"    end-of-line
bindkey  "^[[3~"   delete-char
bindkey  "^[[1;5C" forward-word
bindkey  "^[[1;5D" backward-word

# alias
if hash exa 2> /dev/null; then
    alias ls="exa --group-directories-first --color=always"
    alias la="ls -laFh"
    alias ll="ls -1"
    alias tree="ls --tree"
fi

if hash nvim 2> /dev/null; then
    alias vi="nvim"
fi

# starship
if hash starship 2> /dev/null; then
    eval "$(starship init zsh)"
fi

# zoxide
if hash zoxide 2> /dev/null; then
    eval "$(zoxide init zsh --cmd cd)"
fi

# pywal 
if hash wal 2> /dev/null; then 
    (cat ~/.cache/wal/sequences &)
fi

# zsh-autocomplete
if [[ -d ~/.zsh/zsh-autocomplete ]]; then
    source ~/.zsh/zsh-autocomplete/zsh-autocomplete.plugin.zsh

    zstyle ':autocomplete:*' default-context ''
    zstyle ':autocomplete:*' min-input 0
    zstyle ':autocomplete:*' fzf-completion yes
    zstyle ':autocomplete:*' widget-style menu-select
fi

clear
