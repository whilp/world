;;; init --- Emacs initialization

;;; Commentary:

;;; My personal configuration.
;;; TODO:
;;;  - convert explicit (bind-keys) calls to :bind (after updating use-package)

;;; Code:

(setq load-prefer-newer t)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
;;             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'auth-source)
  (require 'cl)
  (require 'color-theme)
  (require 'url)
  (require 'use-package))

(require 'bind-key)
(require 'diminish)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; TODO
;; http://blog.danielgempesaw.com/post/79353633199/installing-mu4e-with-homebrew-with-emacs-from
;; (use-package mu4e
;;   :ensure t
;;   :defer t)

(defconst whilp-full-name "Will Maier"
  "My name.")

(defconst whilp-email "wcmaier@gmail.com"
  "My preferred email.")

(use-package shell
  :demand t
  :config (setq explicit-shell-file-name "/bin/bash"))

(setq exec-path
      (append
       (mapcar
        'expand-file-name
        (list
         "~/bin"
         "~/go/bin"
         "~/homebrew/Cellar/go/1.3/libexec/bin"
         "/usr/local/opt/go/libexec/bin/godoc"
         "/usr/local/sbin"
         "/usr/local/bin"
         "/usr/local/MacGPG2/bin"
         ))
        exec-path))
(setenv "TMPDIR" "/tmp")
(setenv "PATH"
        (mapconcat 'identity exec-path path-separator))
(setenv "PAGER" "cat")
(setenv "EDITOR" "emacsclient")
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

(setenv "GOPATH" (expand-file-name "~/go"))

(use-package init-rcirc :demand t)
(use-package init-ui :demand t)
(use-package init-el-get :demand t)
(use-package init-dev :demand t)
(use-package init-org :demand t)
(use-package init-helm :demand t)
(use-package init-git :demand t)
(use-package init-ace :demand t)
(use-package init-text :demand t)
(use-package init-emacs :demand t)

;; TODO
;; (use-package go-oracle)

;; TODO: migrate all these bits to separate libraries.
(require 'init-old)

(provide 'init)
;;; init ends here
