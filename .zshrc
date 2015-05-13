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

alias g="git"
alias com="git commit"
alias reb="git rebase"
alias pu="git pull"
alias ph="git push"

# add ssh identities to the ssh agent
ssh-add

# increase key repeat rate
defaults write NSGlobalDomain InitialKeyRepeat -int 15
defaults write NSGlobalDomain KeyRepeat -int 0

source "$HOME/.liftoff_profile"
