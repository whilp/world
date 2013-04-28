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
(setenv "ALTERNATE_EDITOR" "emacs")
(setenv "PROMPT_COMMAND" "")
(setenv "GPG_AGENT_INFO" nil)
(setenv "SSH_AUTH_SOCK" (expand-file-name "~/.ssh/agent.sock"))
(setenv "PS1" "${debian_chroot:+($debian_chroot)}\\u@\\h:\\w \\$ ")

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
(require 'tramp)
(add-to-list 'tramp-default-proxies-alist
             '(nil "\\`root\\'" "/ssh:%h:"))
(add-to-list 'tramp-default-proxies-alist
             '((regexp-quote (system-name)) nil nil))

(global-set-key (kbd "C-x m") 'shell)
(defun remote-shell (&optional host)
  "Open a remote shell to a host."
  (interactive)
  (with-temp-buffer
    (let ((host (if host host (read-string "Host: "))))
      (cd (concat "/" host ":"))
      (shell (concat "*" host "*")))))
(global-set-key (kbd "C-x s") 'remote-shell)

;; GPG.
(require 'epa-file)
(setq epa-file-name-regexp "\\.\\(gpg\\|asc\\)$"
      epa-armor t)
(epa-file-name-regexp-update)
(epa-file-enable)

;; backups.
(setq make-backup-files nil
      backup-directory-alist `(("." . "~/.saves"))
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

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

;; erc.
(require 'tls)
(require 'erc)

;; ido
(setq
 ido-enable-flex-matching t
 ido-everywhere t)
(ido-mode t)
(icomplete-mode t)
(ido-init-completion-maps)

;; vc
(eval-after-load "vc-hooks"
         '(define-key vc-prefix-map "=" 'ediff-revision))

;; org-mode.
(require 'org)
(setq auto-indent-start-org-indent t
      org-startup-indented t
      org-agenda-restore-windows-after-quit t
      org-agenda-file-regexp "\\`[^.].*\\.\\(txt\\|org\\)\\'"
      org-agenda-files (list "~/notes")
      org-default-notes-file "~/notes/todo.txt")
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(add-to-list 'auto-mode-alist '("\\.\\(txt\\|org\\)$" . org-mode))

(set-default-font "Source Code Pro-15")

;; (setq el-get-sources
;;       '(
;;         ipython
;;         python-mode

;;         ))

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(setq
 el-get-byte-compile t
 el-get-user-package-directory "~/.emacs.d"
 el-get-sources '(
                  (:name deft
                         :after (lambda ()
                                  (setq
                                   deft-extension "txt"
                                   deft-directory "~/notes"
                                   deft-text-mode 'org-mode
                                   deft-use-filename-as-title t)))

                  (:name golden-ratio
                         :after (lambda ()
                                  (golden-ratio-enable)))

                  (:name magit
                         :after (lambda ()
                                  (autoload 'magit-status' "magit" nil t)
                                  (setq magit-git-executable "hub")
                                  (global-set-key (kbd "C-x C-g") 'magit-status)))

                  (:name markdown-mode
                         :after (lambda ()
                                  (add-to-list 'auto-mode-alist '("\\.md\\'" .markdown-mode))))

                  (:name smex
                         :after (lambda ()
                                  (global-set-key (kbd "C-x C-m") 'smex)
                                  (global-set-key (kbd "M-x") 'smex)
                                  (global-set-key (kbd "M-X") 'smex-major-mode-commands)))

                  )
 )

(setq my-packages
      (append
       '(
         clojure-mode
         deft
         find-file-in-project
         golden-ratio
         ipython
         magit
         markdown-mode
         scala-mode2
         smex
         )
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync my-packages)
