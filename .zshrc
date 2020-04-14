# Path to your oh-my-zsh installation.
export ZSH=/Users/lakshmansankar/.oh-my-zsh

# zsh Theme/plugins
ZSH_THEME="robbyrussell"
plugins=(git lein colorize cp)

# basic PATH additions
export PATH="$PATH:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"

# relaod this file
alias sz="source ~/.zshrc"

# general repos
export REPOS="$HOME/src"

# special cds
alias repos='cd $REPOS'
alias tdoc='cd $REPOS/thought_docs'
alias dotfiles='cd $REPOS/dotfiles'

# git shortcuts
alias gc="git commit"
alias gpr="git pull --rebase"
alias gpush="git push"
alias ghpush="git push heroku master"
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
alias tremigrate="sudo rm build/contracts/* && truffle migrate --reset --compile-all"

# docker shortcuts
alias dps="docker ps -a"
alias drm="docker rm"
alias dcp="docker cp"
alias dcreate="docker create"
alias dstart="docker start"
alias dlogs="docker logs"
alias dpull="docker pull"
alias dpush="docker push"
alias dbuild="docker build"
alias drun="docker run"
alias dinspect="docker inspect"
alias dhistory="docker history"
alias dls="docker image ls"

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
alias gorepos='cd $REPOS/go/src'
alias gogit='cd $REPOS/go/src/github.com'

# python virtualenv-wrapper. default to python3 for everything
export VIRTUALENVWRAPPER_PYTHON=/usr/local/bin/python3
export WORKON_HOME=$HOME/.virtualenvs
source /usr/local/bin/virtualenvwrapper.sh

# make openSSL visible to compilers
export LDFLAGS="-L/usr/local/opt/openssl/lib"
export CPPFLAGS="-I/usr/local/opt/openssl/include"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# node always in async mode
alias node="node --experimental-repl-await"

source "$HOME/.local_profile"

# for bazel
export PATH="$PATH:$HOME/bin"
alias bazel="bazelisk"

# copy last cmd to system clipboard
alias lc='fc -ln -1 | awk "{\$1=\$1}1" ORS="" | pbcopy'

# use venv python for IPython (requires IPython installed in venv)
alias ipy="python -c 'import IPython; IPython.terminal.ipapp.launch_new_instance()'"

# check available ports alias
pidportfunction() {
  lsof -n -i4TCP:$1 | grep LISTEN
}
alias lsp=pidportfunction
