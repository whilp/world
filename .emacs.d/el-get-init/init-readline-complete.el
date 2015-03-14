(require 'shell)
(push 'company-readline company-backends)
(add-hook 'rlc-no-readline-hook (lambda () (company-mode -1)))
(setq explicit-shell-file-name "bash"
      explicit-bash-args '("-c" "export EMACS=; stty echo; bash")
      comint-process-echoes t)
