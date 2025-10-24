export STRIPE_DO_NOT_MANAGE=1

export RIPGREP_CONFIG_PATH=~/.config/ripgrep/rg.conf
export COLORTERM=truecolor
export TERM=xterm-ghostty
export PS1='%# '
set -o vi

# Set up PATH first
typeset -aU path

path=(
  "$HOME/.local/bin"
  "$HOME/.local/share/shimlink/bin"
  "$HOME/stripe/space-commander/bin"
  "/pay/deploy/claude-wrapper-hosts/current/bin"
  "/deploy/dev-tools/current/bin"
  "/opt/homebrew/bin"
  $path
)
path+=(/usr/*/bin(N))

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

egress=(/usr/*/etc/*egress-env.sh(N))
for f in $egress; do
  [ -r "$f" ] && source "$f"
done
