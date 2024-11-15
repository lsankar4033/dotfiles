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
alias drmi="docker rmi"
alias dcp="docker cp"
alias dcreate="docker create"
alias dstart="docker start"
alias dstop="docker stop"
alias dlogs="docker logs"
alias dpull="docker pull"
alias dpush="docker push"
alias dbuild="docker build"
alias drun="docker run"
alias dinspect="docker inspect"
alias dhistory="docker history"
alias dls="docker image ls"
alias drme="drm $(dps -q)"
alias drmin="docker rmi $(docker images --filter "dangling=true" -q --no-trunc)"

docker_bash() {
  docker exec -it $1 /bin/bash
}
alias dbash=docker_bash

# build, run, and log in one go
build_run_log_image() {
  docker build -t $1 -f $2 $3 && docker run -it $1
}
alias dbrl=build_run_log_image

# stop and rm container
stop_and_remove_container() {
  docker stop $1 && docker rm $1
}
alias dsrm=stop_and_remove_container

# run bash on a running container
docker_run_bash() {
  docker exec -it $1 bash
}
alias dbash=docker_run_bash

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
#export VIRTUALENVWRAPPER_PYTHON=/usr/local/bin/python3
#export WORKON_HOME=$HOME/.virtualenvs
#source /usr/local/bin/virtualenvwrapper.sh

# make openSSL visible to compilers
export LDFLAGS="-L/usr/local/opt/openssl/lib"
export CPPFLAGS="-I/usr/local/opt/openssl/include"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

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

# create python experimental space
pythonexperiment() {
  mkdir $1 && \
    cd $1 && \
    mkvirtualenv --python=python3 $1 && \
    touch main.py && \
    touch test_main.py && \
    pip3 install pytest
}
alias pyexp=pythonexperiment
[ -f "/Users/lakshmansankar/.ghcup/env" ] && source "/Users/lakshmansankar/.ghcup/env" # ghcup-env

export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"
if [ -e /Users/lakshmansankar/.nix-profile/etc/profile.d/nix.sh ]; then . /Users/lakshmansankar/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

# Solana
export PATH="/Users/lakshmansankar/src/solana/solana-1.8.5"/bin:"$PATH"

alias circom2=circom

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

export PATH="$PATH:/Users/lakshmansankar/.foundry/bin"

export PATH="/usr/local/opt/libpq/bin:$PATH"


# pnpm
export PNPM_HOME="/Users/lakshmansankar/Library/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac
# pnpm end

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/Users/lakshmansankar/mambaforge/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/Users/lakshmansankar/mambaforge/etc/profile.d/conda.sh" ]; then
        . "/Users/lakshmansankar/mambaforge/etc/profile.d/conda.sh"
    else
        export PATH="/Users/lakshmansankar/mambaforge/bin:$PATH"
    fi
fi
unset __conda_setup

if [ -f "/Users/lakshmansankar/mambaforge/etc/profile.d/mamba.sh" ]; then
    . "/Users/lakshmansankar/mambaforge/etc/profile.d/mamba.sh"
fi
# <<< conda initialize <<<


# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/lakshmansankar/Downloads/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/lakshmansankar/Downloads/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/lakshmansankar/Downloads/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/lakshmansankar/Downloads/google-cloud-sdk/completion.zsh.inc'; fi


# local to whalewatcher job for now
export GOOGLE_APPLICATION_CREDENTIALS="./.secret/google-storage-creds.json"

source "$HOME/.secrets.zshrc.sh"
