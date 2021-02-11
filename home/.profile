set -o vi

paths=(
  "$HOME/bin"
  "/Applications/Visual Studio Code - Insiders.app/Contents/Resources/app/bin"
  "/Applications/Visual Studio Code.app/Contents/Resources/app/bin"
  "${BROWSER%/bin/*}/bin"
  /usr/local/bin
  /usr/bin
  /bin
  /usr/sbin
  /sbin
)
export PATH=$(
  IFS=:
  echo "${paths[*]}"
)
export PROMPT_DIRTRIM=2
export PS1='${PWD##*/} ▶ '

export GIT_EDITOR="edit -w"
export GIT_AUTHOR_NAME="Will Maier"
export GIT_AUTHOR_EMAIL="189851+whilp@users.noreply.github.com"

export LC_ALL=C.UTF-8
export LANG=C.UTF-8
export LANGUAGE=C.UTF-8

(
  mkdir -p ~/bin
  mkdir -p ~/.ssh
  chmod 700 ~/.ssh
  chmod 400 ~/.ssh/config
) > /dev/null 2>&1
