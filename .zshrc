export STRIPE_DO_NOT_MANAGE=1

export RIPGREP_CONFIG_PATH=~/.config/ripgrep/rg.conf
export COLORTERM=truecolor
export TERM=xterm-ghostty
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
  "$HOME/stripe/space-commander/bin"
  "/pay/deploy/claude-wrapper-hosts/current/bin"
  "/opt/homebrew/bin"
  $path
)
path+=(/usr/*/bin(N))

egress=/usr/stripe/etc/stripe-egress-env.sh
[ -r "$egress" ] && source "$egress"
