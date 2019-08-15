# Path to your oh-my-zsh installation.
export ZSH=/Users/lakshmansankar/.oh-my-zsh

# zsh Theme/plugins
ZSH_THEME="robbyrussell"
plugins=(git lein colorize cp)

export PATH="$PATH:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"

# general repos
export REPOS="$HOME/src"

# special cds
alias repos='cd $REPOS'
alias tdoc='cd $REPOS/thought_docs'
alias dotfiles='cd $REPOS/dotfiles'
alias gorepos='cd $REPOS/go/src/github.com/lsankar4033'

alias sz="source ~/.zshrc"

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
alias grc="git rebase --continue"
alias grh="git reset --hard"

# tig shortcuts
alias tst="tig status"

# tmux shortcuts
alias tls="tmux list-sessions"
alias tattach="tmux attach -t"
alias tnew="tmux new-session -s"
alias tkill="tmux kill-session -t"

# truffle shortcuts
alias tc="truffle compile"
alias tm="truffle migrate"
alias tt="truffle test"

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

# depot_tools is used by chromium
export PATH="$REPOS/depot_tools:$PATH"

# used by haskell stack
export PATH="$HOME/.local/bin:$PATH"

# go
export PATH="$PATH:/usr/local/go/bin"
export GOPATH="$HOME/src/go"
export PATH="$PATH:$GOPATH/bin"
alias gop='cd $GOPATH'

# python virtualenv-wrapper. default to python3 for everything
export VIRTUALENVWRAPPER_PYTHON=/usr/local/bin/python3
export WORKON_HOME=$HOME/.virtualenvs
source /usr/local/bin/virtualenvwrapper.sh

 #make openSSL visible to compilers
export LDFLAGS="-L/usr/local/opt/openssl/lib"
export CPPFLAGS="-I/usr/local/opt/openssl/include"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
