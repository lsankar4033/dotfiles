# Profiling code.  Will write profiling stats by command to a file at $HOME/tmp/startlog.${pid} if
# PROFILE_STARTUP=true
PROFILE_STARTUP=false
if [[ "$PROFILE_STARTUP" == true ]]; then
    # http://zsh.sourceforge.net/Doc/Release/Prompt-Expansion.html
    PS4=$'%D{%M%S%.} %N:%i> '
    exec 3>&2 2>$HOME/tmp/startlog.$$
    setopt xtrace prompt_subst
fi

# Path to your oh-my-zsh installation.
export ZSH=/Users/lakshmansankar/.oh-my-zsh

# zsh Theme/plugins
ZSH_THEME="robbyrussell"
plugins=(git lein colored-man colorize cp)

export PATH="$PATH:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"

# JVM opts
export JVM_OPTS="-Xmx4g -XX:MaxPermSize=256M"
export JAVA_HOME=$(/usr/libexec/java_home)
export PATH="$JAVA_HOME/bin:$PATH"

# rbenv
eval "$(rbenv init -)"
export PATH="$PATH:/Users/lakshmansankar/.rbenv/shims"

# go
export PATH="$PATH:/usr/local/go/bin"

# general repos
export REPOS="$HOME/src"

# special cds
alias repos='cd $REPOS'
alias tdoc='cd $REPOS/thought_docs'
alias dotfiles='cd $REPOS/dotfiles'
alias gorepos='cd $REPOS/go/src/github.com/lsankar4033'

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

# Profiling code
if [[ "$PROFILE_STARTUP" == true ]]; then
    unsetopt xtrace
    exec 2>&3 3>&-
fi

# pyenv virtual-env
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

# depot_tools is used by chromium
export PATH="$REPOS/depot_tools:$PATH"

# used by haskell stack
export PATH="$HOME/.local/bin:$PATH"

# make openSSL visible to compilers
export LDFLAGS="-L/usr/local/opt/openssl/lib"
export CPPFLAGS="-I/usr/local/opt/openssl/include"

# golang
export GOPATH="$HOME/src/go"
export PATH="$PATH:$GOPATH/bin"
