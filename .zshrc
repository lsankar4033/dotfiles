# Stuff from bash
eval "$(rbenv init -)"

# Path to your oh-my-zsh installation.
export ZSH=/Users/lakshmansankar/.oh-my-zsh

ZSH_THEME="robbyrussell"

plugins=(git lein)

PATH="$PATH:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"

# JVM opts
export JVM_OPTS="-Xmx4g -XX:MaxPermSize=256M"
export JAVA_HOME=$(/usr/libexec/java_home)
export PATH="$JAVA_HOME/bin:$PATH"

# rbenv
eval "$(rbenv init -)"
PATH="$PATH:/Users/lakshmansankar/.rbenv/shims"

# go
PATH="$PATH:/usr/local/go/bin"

source $ZSH/oh-my-zsh.sh

alias sz="source ~/.zshrc"
alias bef="bundle exec fez"

alias gclone="git clone"
alias gcommit="git commit"
alias grebase="git rebase"
alias gpull="git pull"
alias gpr="git pull --rebase"
alias gpush="git push"
alias gdiff="git diff"
alias glog="git log"
alias gstatus="git status"

alias tmuxls="tmux list-sessions"

# add ssh identities to the ssh agent
ssh-add

# increase key repeat rate
defaults write NSGlobalDomain InitialKeyRepeat -int 15
defaults write NSGlobalDomain KeyRepeat -int 0

source "$HOME/.local_profile"
