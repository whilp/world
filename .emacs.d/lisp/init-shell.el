;;; init-shell --- Initialize shell stuff.

;;; Commentary:

;;; My shells.

;;; Code:

(require 'init-whilp)
(require 'use-package)

(defvar whilp-gopath
  (file-name-as-directory
   (expand-file-name "~/go"))
  "GOPATH.")

(use-package shell
  :demand t
  :config (setq explicit-shell-file-name "/bin/bash"))

(setq exec-path
      (append
       (mapcar
        'expand-file-name
        (list
         "~/bin"
         (concat whilp-gopath "bin")
         "/usr/local/go/bin/go"
         "/usr/pkg/sbin"
         "/usr/pkg/bin"
         "/usr/local/sbin"
         "/usr/local/bin"
         "/usr/local/MacGPG2/bin"))
       exec-path))
(setenv "PATH"
        (mapconcat 'identity exec-path path-separator))
(setenv "MANPATH"
        (mapconcat
         'identity
         (append
          (mapcar
           'expand-file-name
           (list
            "/usr/pkg/man")))
         ":"))
(setenv "TMPDIR" "/tmp")
(setenv "PAGER" "cat")
(setenv "EDITOR" "emacsclient")
(setenv "SUDO_EDITOR" "emacsclient")
(setenv "ALTERNATE_EDITOR" "emacs")
(setenv "PROMPT_COMMAND" "")
(setenv "GPG_AGENT_INFO" nil)
(setenv "SSH_AUTH_SOCK" (expand-file-name "~/.ssh/agent.sock"))
(setenv "PS1" "${debian_chroot:+($debian_chroot)}\\u@\\h:\\w \\$ ")
(setenv "_JAVA_OPTIONS" "-Djava.awt.headless=true")
(setenv "MAN_WIDTH" "72")

(setenv "GIT_EDITOR" "emacsclient")
(setenv "GIT_COMMITTER_NAME" whilp-full-name)
(setenv "GIT_COMMITTER_EMAIL" whilp-email)
(setenv "GIT_AUTHOR_NAME" whilp-full-name)
(setenv "GIT_AUTHOR_EMAIL" whilp-email)

(setenv "GOPATH" whilp-gopath)

(use-package comint
  :demand t
  :config
  (progn
    (setq comint-scroll-show-maximum-output nil)
    (remove-hook 'comint-output-filter-functions
                 'comint-postoutput-scroll-to-bottom)))

(use-package tramp
  :demand t
  :config
  (progn
    (add-to-list 'tramp-default-proxies-alist
                 '(nil "\\`root\\'" "/ssh:%h:"))
    (add-to-list 'tramp-default-proxies-alist
                 '((regexp-quote (system-name)) nil nil))
    (setq tramp-password-prompt-regexp
          (concat "^.*"
                  (regexp-opt '("[pP]assword" "[pP]assphrase" "Verification code") t)
                  ".*:? *"))))

(provide 'init-shell)
;;; init-shell ends here
