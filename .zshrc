export STRIPE_DO_NOT_MANAGE=1

export COLORTERM=truecolor
export PS1='%# '
set -o vi

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

typeset -aU path

path=(
  "$HOME/.local/bin"
  "/Applications/Alacritty.app/Contents/MacOS"
  "$HOME/stripe/space-commander/bin"
  $path
)