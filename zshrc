export PATH=$PATH:~/.bin/

HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory
setopt share_history

bindkey -e
bindkey '^R' history-incremental-search-backward

zstyle :compinstall filename '/Users/derek/.zshrc'
zstyle ':completion:*' menu select

autoload -Uz compinit
compinit

autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:git*' formats " (%b)"

precmd() {
    vcs_info
}

setopt prompt_subst
PROMPT='[%n@%m] %2d${vcs_info_msg_0_}: '

chpwd () {
    print -Pn "\e]2; %~/ \a"
}

alias vim='/Applications/MacVim.app/Contents/MacOS/Vim'
