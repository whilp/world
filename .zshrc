export STRIPE_DO_NOT_MANAGE=1

export PS1='%# '
set -o vi

# Set terminal title
HOST_IDENTIFIER=$(whereami)
precmd() {
  print -Pn "\e]0;${HOST_IDENTIFIER}\a"
}

alias co='git checkout'
alias ci='git commit'
alias st='git status'
alias br='git branch'
alias di='git diff'
alias dc='git diff --cached'
alias lo='git log --oneline'
alias up='git pull --rebase'
alias rb='git rebase'
alias rc='git rebase --continue'
alias ra='git rebase --abort'
