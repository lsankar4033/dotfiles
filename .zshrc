# Stuff from bash
eval "$(rbenv init -)"

# Path to your oh-my-zsh installation.
export ZSH=/Users/lakshmansankar/.oh-my-zsh

ZSH_THEME="robbyrussell"

plugins=(git lein colored-man colorize cp)

PATH="$PATH:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"

# JVM opts
export JVM_OPTS="-Xmx4g -XX:MaxPermSize=256M"
export JAVA_HOME=$(/usr/libexec/java_home)
export PATH="$JAVA_HOME/bin:$PATH"

# rbenv
eval "$(rbenv init -)"
export PATH="$PATH:/Users/lakshmansankar/.rbenv/shims"

# go
export PATH="$PATH:/usr/local/go/bin"

# android platform-tools
export PATH="$PATH:/Users/lakshmansankar/Library/Android/sdk/platform-tools"

# general repos
export REPOS="$HOME/git"
alias repos='cd $REPOS'

# thought docs
alias tdoc='cd $REPOS/thought-docs'

alias sz="source ~/.zshrc"
alias bef="bundle exec fez"

# git shortcuts
alias gc="git commit"
alias gpr="git pull --rebase"
alias gpush="git push"
alias gd="git diff"
alias gl="git log"
alias gcm="git checkout master"
alias grm="git rebase master"
alias gst='git status'
alias gsh="git show"
alias gb="git branch"
alias gbd="git branch -D"
alias gcb="git checkout -b"
alias ga="git add"
alias gaa="git add -A"

# tig shortcuts
alias tst="tig status"

# tmux shortcuts
alias tls="tmux list-sessions"
alias tattach="tmux attach -t"
alias tnew="tmux new-session -s"
alias tkill="tmux kill-session -t"

# Quick history search
alias hgrep="history |grep"

# Clear out vim swap
alias cs="rm ~/.vim/tmp/swap/*.swp"

# Drip
alias dps="drip ps"
alias dkill="drip kill"

# add ssh identities to the ssh agent
ssh-add

# increase key repeat rate
defaults write NSGlobalDomain InitialKeyRepeat -int 15
defaults write NSGlobalDomain KeyRepeat -int 0

# to not screw with tmux window names
DISABLE_AUTO_TITLE=true

# Use vim as the editor for git commits, etc.
git config --global core.editor "vim"

alias als="aws s3 ls"
alias acp="aws s3 cp"
alias amv="aws s3 mv"


source $ZSH/oh-my-zsh.sh
source "$HOME/.local_profile"
source "$HOME/.liftoff_profile"

# Set file descriptor limit to 500000
sudo launchctl limit maxfiles 500000 500000
ulimit -n 500000

alias rstudio='open -a RStudio'
