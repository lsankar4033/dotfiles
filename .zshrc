# Path to your oh-my-zsh installation.
export ZSH=/Users/lakshmansankar/.oh-my-zsh

# zsh Theme/plugins
ZSH_THEME="robbyrussell"
plugins=(git lein colorize cp)

# basic PATH additions
export PATH="$HOME/.cargo/bin:$PATH:/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin"

# solana
export PATH="$HOME/.local/share/solana/install/active_release/bin:$PATH"

# relaod this file
alias sz="source ~/.zshrc"

# general repos
export REPOS="$HOME/src"

# special cds
alias repos='cd $REPOS'
alias cabal='cd $REPOS/cabal-research'
alias tdoc='cd $REPOS/thought_docs'
alias dotfiles='cd $REPOS/dotfiles'

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
alias gba="git branch -a"
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

# to not screw with tmux window names
DISABLE_AUTO_TITLE=true

source $ZSH/oh-my-zsh.sh

export PATH="$HOME/.local/bin:$PATH"

# identity
source "$HOME/.identity.zshrc.sh"

# opencode
export PATH=/Users/lakshmansankar/.opencode/bin:$PATH

# ARM Homebrew (last so it takes highest PATH priority)
eval "$(/opt/homebrew/bin/brew shellenv)"

export PATH="$PATH:/Users/lakshmansankar/.foundry/bin"

# jujutsu init
alias jjinit="jj git init --colocate"
alias jjl="jj log"
alias jjd="jj describe"
alias jjn="jj new"
alias jjst="jj status"
alias jju="jj undo"
alias jje="jj edit"
alias jjsq="jj squash"
alias jjb="jj bookmark"
alias jjbc="jj bookmark create"

jjbpush() {
  local name="$1"
  if [[ -z "$name" ]]; then
    echo "Usage: jjbpush <bookmark-name>"
    return 1
  fi
  jj bookmark create "$name" && \
  jj git push --bookmark "$name" && \
  jj bookmark track "${name}@origin"
}

