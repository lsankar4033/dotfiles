# Profiling code.  Will write profiling stats by command to a file at $HOME/tmp/startlog.${pid} if
# PROFILE_STARTUP=true
PROFILE_STARTUP=false
if [[ "$PROFILE_STARTUP" == true ]]; then
    # http://zsh.sourceforge.net/Doc/Release/Prompt-Expansion.html
    PS4=$'%D{%M%S%.} %N:%i> '
    exec 3>&2 2>$HOME/tmp/startlog.$$
    setopt xtrace prompt_subst
fi

# Stuff from bash
eval "$(rbenv init -)"

# Path to your oh-my-zsh installation.
export ZSH=/Users/lakshmansankar/.oh-my-zsh

# zsh Theme/plugins
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
export PATH="$PATH:/Users/lakshmansankar/Library/Android/sdk/tools"

# general repos
export REPOS="$HOME/src"
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

# Clear out vim swap
alias cs="rm ~/.vim/tmp/swap/*.swp"

# Drip
alias dps="drip ps"
alias dkill="drip kill"

# increase key repeat rate
defaults write NSGlobalDomain InitialKeyRepeat -int 12
defaults write NSGlobalDomain KeyRepeat -int 1

# to not screw with tmux window names
DISABLE_AUTO_TITLE=true

# Use vim as the editor for git commits, etc.
git config --global core.editor "vim"

alias als="aws s3 ls"
alias acp="aws s3 cp"
alias amv="aws s3 mv"

source $ZSH/oh-my-zsh.sh

# Set file descriptor limit to 500000
sudo launchctl limit maxfiles 500000 500000
ulimit -n 500000

# Profiling code
if [[ "$PROFILE_STARTUP" == true ]]; then
    unsetopt xtrace
    exec 2>&3 3>&-
fi

# Eclipse (for eclim)
export ECLIPSE_HOME="/Applications/eclipse/Eclipse.app/Contents/Eclipse"
alias eclimd="$ECLIPSE_HOME/eclimd"
