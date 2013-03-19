;; environment.
(setq exec-path (append '(
                          "~/bin"
                          "/usr/local/sbin"
                          "/usr/local/bin"
                          )
                        exec-path))
(setq explicit-shell-file-name "/bin/bash")
(setenv "PAGER" "cat")
(setenv "EDITOR" "emacsclient")
(setenv "PROMPT_COMMAND" "")
(setenv "PS1" "${debian_chroot:+($debian_chroot)}\u@\h:\w\$")

;; no bars.
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; modeline.
(setq display-time-format "%a %Y-%m-%d %H:%M")
(display-time-mode 1)

;; no splash.
(setq inhibit-startup-message t
  inhibit-startup-echo-area-message t)

;; no prompts.
(setq confirm-nonexistent-file-or-buffer nil)
(fset 'yes-or-no-p 'y-or-n-p)

;; syntax highlighting.
(global-font-lock-mode t)
(transient-mark-mode t)

;; ido matching.
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode t)

;; GC buffers, uniquify buffer names, ibuffer.
(require 'midnight)
(add-to-list 'clean-buffer-list-kill-never-regexps "^#.*")
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(require 'ibuffer)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-expert t)
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-saved-filter-groups
      '(("default"
         ("erc" (mode . erc-mode)))))
(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-auto-mode t)
             (ibuffer-switch-to-saved-filter-groups "default")))

;; comint.
(setq comint-scroll-show-maximum-output nil)
(remove-hook 'comint-output-filter-functions
             'comint-postoutput-scroll-to-bottom)

;; tramp.
(add-to-list 'tramp-default-proxies-alist
             '(nil "\\`root\\'" "/ssh:%h:"))
(add-to-list 'tramp-default-proxies-alist
             '((regexp-quote (system-name)) nil nil))

(defun remote-shell (&optional host)
  "Open a remote shell to a host."
  (interactive)
  (with-temp-buffer
    (let ((host (if host host (read-string "Host: "))))
      (cd (concat "/" host ":"))
      (shell (concat "*" host "*")))))

;; GPG.
(require 'epa-file)
(setq epa-file-name-regexp "\\.\\(gpg\\|asc\\)$"
      epa-armor t)
(epa-file-name-regexp-update)
(epa-file-enable)

;; backups.
(setq make-backup-files nil)
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; pretty-print expression evals.
(global-set-key [remap eval-expression] 'pp-eval-expression)
(global-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)

;; windows.
(global-set-key (kbd "M-RET") 'ns-toggle-fullscreen)
(global-set-key (kbd "M-i") 'windmove-left)
(global-set-key (kbd "M-j") 'windmove-down)
(global-set-key (kbd "M-k") 'windmove-up)
(global-set-key (kbd "M-l") 'windmove-right)

;; s/meta/c/
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; command-as-meta.
(setq mac-option-key-is-meta nil)       
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
(setq kill-read-only-ok)

;; whitespace.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(global-set-key (kbd "<backtab>") 'indent-relative)

;; wrap.
(global-visual-line-mode 1)

;; easier backward-kill-word.
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

(global-set-key (kbd "C-_") 'dabbrev-expand)

;; packages.
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/")
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
             t)
             
(package-initialize)

(defun install-package-unless (pkg)
  "Install or refresh a package."
  (unless (package-installed-p pkg)
    (package-refresh-contents) (package-install pkg)))

;; erc.
(require 'tls)
(require 'erc)

;; vc
(eval-after-load "vc-hooks"
         '(define-key vc-prefix-map "=" 'ediff-revision))

;; magit.
(autoload 'magit-status' "magit" nil t)
(setq magit-git-executable "hub")
(global-set-key (kbd "C-x C-g") 'magit-status)

;; org-mode.
(require 'org)
(setq auto-indent-start-org-indent t
      org-startup-indented t
      org-agenda-files (list "~/notes/todo.org")
      org-default-notes-file "~/notes/todo.org")
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; http://jblevins.org/projects/deft/
(add-to-list 'load-path (expand-file-name "~/.emacs.d/deft"))
(require 'deft)
(setq deft-extension "org")
(setq deft-directory "~/notes")
(setq deft-text-mode 'org-mode)
(setq deft-use-filename-as-title t)

;; tags.
(install-package-unless 'etags-select)
(install-package-unless 'etags-table)
(require 'etags-select)
(require 'etags-table)
(setq etags-table-search-up-depth 8)
(global-set-key "\M-?" 'etags-select-find-tag-at-point)
(global-set-key "\M-." 'etags-select-find-tag)

;; languages.
(require 'inf-ruby)
(install-package-unless scala-mode2)
(install-package-unless 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(set-default-font "Source Code Pro-15")
