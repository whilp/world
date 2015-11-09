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

(defvar nix-link (file-name-as-directory (expand-file-name "~/.nix-profile"))
  "NIX_LINK.")

(defvar nix-path (file-name-as-directory (expand-file-name "~/.nix-defexpr/nixpkgs")))

(setenv "NIX_PATH" (format "%s:nixpkgs=%s" nix-path nix-path))

(setenv "NIX_CONF_DIR" (file-name-as-directory (expand-file-name "~/.nix")))

(setenv "SSL_CERT_FILE" (concat nix-link "etc/ssl/certs/ca-bundle.crt"))

(setq exec-path
      (mapcar
       'expand-file-name
       (list
        (concat nix-link "bin")
        (concat nix-link "sbin")
        "~/bin"
        (concat whilp-gopath "bin")
        "/usr/pkg/sbin"
        "/usr/pkg/bin"
        "/usr/local/sbin"
        "/usr/local/bin"
        "/usr/local/texlive/2015basic/bin/x86_64-darwin/"
        "/usr/local/MacGPG2/bin"
        "/usr/bin/"
        "/bin/"
        "/usr/sbin/"
        "/sbin/"
        "/Applications/Emacs.app/Contents/MacOS/bin-x86_64-10_9/"
        "/Applications/Emacs.app/Contents/MacOS/libexec-x86_64-10_9/")))

(defvar shell-path (mapconcat 'identity exec-path path-separator)
  "Shell PATH string.")
(setenv "PATH" shell-path)

(defvar man-path (mapcar
                  'expand-file-name
                  (list
                   (concat nix-link "share/man")
                   "/usr/pkg/man"
                   "/usr/share/man"
                   "/usr/local/share/man"))
  "MANPATH.")

(setenv "MANPATH" (mapconcat 'identity man-path path-separator))
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

(use-package eshell
  :demand t)

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
