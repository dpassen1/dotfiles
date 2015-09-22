export PATH=/usr/local/bin:/usr/local/sbin:~/.bin:$PATH

alias tmux='tmux -2'
alias tmux-select='tmux attach -t $(tmux ls | selecta | cut -f1 -d:)'

alias emacs='emacs -nw'
alias emacs-select='emacsclient -nw -c -s $(ps -ewwo command | grep "^emacs.*daemon=." | cut -d= -f2 | selecta)'

HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory
setopt share_history

setopt interactivecomments

bindkey -e
bindkey '^R' history-incremental-search-backward
bindkey "^[[3~" delete-char

zstyle :compinstall filename '/Users/derek/.zshrc'
zstyle ':completion:*' menu select

autoload -Uz compinit
compinit

autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git hg
zstyle ':vcs_info:git*' formats " (%b)"
zstyle ':vcs_info:hg*' formats " (%b)"

precmd() {
    vcs_info
}

setopt prompt_subst
PROMPT='[%n@%m] %2d${vcs_info_msg_0_}: '

chpwd () {
    print -Pn "\e]2; %~/ \a"
}

source $(brew --prefix nvm)/nvm.sh
