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

# alias
which exa > /dev/null && {
    alias ls="exa --group-directories-first --color=always"
    alias la="ls -laFh"
    alias ll="ls -1"
    alias tree="ls --tree"
}

which nvim > /dev/null && \
    alias vi="nvim"

# keybinds
bindkey  "^[[H"    beginning-of-line
bindkey  "^[[F"    end-of-line
bindkey  "^[[3~"   delete-char
bindkey  "^[[1;5C" forward-word
bindkey  "^[[1;5D" backward-word

# starship
which starship > /dev/null && \
    eval "$(starship init zsh)"

# zoxide
which zoxide > /dev/null && \
    eval "$(zoxide init zsh --cmd cd)"

# pywal 
which wal > /dev/null && 
    (cat ~/.cache/wal/sequences &)

# zsh-autocomplete
[[ -d ~/.zsh/zsh-autocomplete ]] && {
    source ~/.zsh/zsh-autocomplete/zsh-autocomplete.plugin.zsh

    zstyle ':autocomplete:*' default-context ''
    zstyle ':autocomplete:*' min-input 0
    zstyle ':autocomplete:*' fzf-completion yes
    zstyle ':autocomplete:*' widget-style menu-select
}

clear
