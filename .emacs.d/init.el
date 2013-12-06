;; environment.
(setq exec-path (append '(
                          "~/bin"
                          "/usr/local/sbin"
                          "/usr/local/bin"
                          )
                        exec-path))
(setq explicit-shell-file-name "/bin/bash")
(setenv "PATH"
        (mapconcat 'identity exec-path path-separator))
(setenv "PAGER" "cat")
(setenv "EDITOR" "emacsclient")
(setenv "ALTERNATE_EDITOR" "emacs")
(setenv "PROMPT_COMMAND" "")
(setenv "GPG_AGENT_INFO" nil)
(setenv "SSH_AUTH_SOCK" (expand-file-name "~/.ssh/agent.sock"))
(setenv "PS1" "${debian_chroot:+($debian_chroot)}\\u@\\h:\\w \\$ ")

(setenv "GIT_EDITOR" "emacsclient")
(setenv "GIT_COMMITTER_NAME" "Will Maier")
(setenv "GIT_COMMITTER_EMAIL" "wcmaier@m.aier.us")
(setenv "GIT_AUTHOR_NAME" "Will Maier")
(setenv "GIT_AUTHOR_EMAIL" "wcmaier@m.aier.us")

(define-minor-mode whilp/bindings "My global bindings" t nil (make-sparse-keymap))

;; no bars, bells.
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode 0)
(setq ring-bell-function 'ignore
      visible-bell t)
(winner-mode 1)
(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

(define-key whilp/bindings-map (kbd "M-RET") 'toggle-fullscreen)
(define-key whilp/bindings-map (kbd "M-`") 'other-frame)
(define-key whilp/bindings-map (kbd "M-w") 'delete-frame)

;; remember.
(winner-mode 1)
(desktop-save-mode 1)
(setq savehist-file "~/.emacs.d/savehist"
      savehist-additional-variables '(
                                      search-ring
                                      regexp-search-ring
                                      ))
(savehist-mode 1)

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

;; GC buffers, uniquify buffer names, ibuffer.
(require 'midnight)
(add-to-list 'clean-buffer-list-kill-never-regexps "^#.*")
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(require 'ibuffer)

(define-key whilp/bindings-map (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-expert t)
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-saved-filter-groups
      '(("default"
         ("erc" (mode . erc-mode)))))
(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-auto-mode t)
             (ibuffer-switch-to-saved-filter-groups "default")))

(define-key whilp/bindings-map (kbd "C-c C-o") 'occur)

;; I'm an adult.
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

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

(defun remote-shell (&optional host)
  "Open a remote shell to a host."
  (interactive)
  (with-temp-buffer
    (let ((host (if host host (read-string "Host: "))))
      (cd (concat "/" host ":"))
      (shell (concat "*" host "*")))))
(define-key whilp/bindings-map (kbd "C-x s") 'remote-shell)

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
(define-key whilp/bindings-map (kbd "M-i") 'windmove-left)
(define-key whilp/bindings-map (kbd "M-j") 'windmove-down)
(define-key whilp/bindings-map (kbd "M-k") 'windmove-up)
(define-key whilp/bindings-map (kbd "M-l") 'windmove-right)

;; command-as-meta.
(setq mac-option-key-is-meta nil)       
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
(setq kill-read-only-ok)

;; whitespace.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(define-key whilp/bindings-map (kbd "<backtab>") 'indent-relative)

;; wrap.
(global-visual-line-mode 1)

;; easier backward-kill-word.
(define-key whilp/bindings-map (kbd "C-w") 'backward-kill-word)
(define-key whilp/bindings-map (kbd "C-x C-k") 'kill-region)
(define-key whilp/bindings-map (kbd "C-c C-k") 'kill-region)

(define-key whilp/bindings-map (kbd "C-_") 'dabbrev-expand)

;; erc.
(require 'tls)
(require 'erc)

;; ido
;; https://gist.github.com/timcharper/493269/raw/72d9063b8aef61a851026f3acb1d27a4b7c17eca/ido-other-window.el
(setq confirm-nonexistent-file-or-buffer nil)
(load-file "~/.emacs.d/ido-other-window.el")
(setq
 ido-create-new-buffer 'always
 ido-enable-flex-matching t
 ido-everywhere t)
(ido-mode t)
(icomplete-mode t)
(ido-init-completion-maps)

;; shell-here
(load-file "~/.emacs.d/shell-here.el")
(define-key whilp/bindings-map (kbd "C-x m") 'shell-here)

;; vc
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(eval-after-load "vc-hooks"
         '(define-key vc-prefix-map "=" 'ediff-revision))

;; org-mode.
(require 'org)
(setq auto-indent-start-org-indent t
      org-startup-indented t
      org-use-tag-inheritance t
      org-todo-keywords '((sequence "TODO" "DONE"))
      org-agenda-restore-windows-after-quit t
      org-agenda-search-headline-for-time nil
      org-extend-today-until 6
      org-agenda-start-with-follow-mode t
      org-agenda-file-regexp "\\`[^.].*\\.\\(txt\\|org\\)\\'"
      org-agenda-files (list "~/notes" "~/notes/projects" "~/notes/ops")
      org-default-notes-file "~/notes/todo.txt")
(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c c") 'org-capture)
(add-to-list 'auto-mode-alist '("\\.\\(txt\\|org\\)$" . org-mode))

(cond
 ((string-equal system-type "darwin")
  (set-face-attribute 'default nil :font "Monaco-18")))

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
 el-get-git-shallow-clone t
 el-get-user-package-directory "~/.emacs.d"
 el-get-sources '(
                  (:name apache-mode)
                  (:name clojure-mode)
                  (:name find-file-in-project)
                  (:name flycheck)
                  (:name gist)
                  (:name go-mode)
                  (:name inf-ruby)
                  (:name markdown-mode)
                  (:name nginx-mode)
                  (:name request)
                  (:name scala-mode2)
                  (:name color-theme-solarized
                         :after (progn
                                  (load-theme 'solarized-light t)))

                  (:name deft
                         :after (progn
                                  (setq
                                   deft-extension "txt"
                                   deft-directory "~/notes"
                                   deft-text-mode 'org-mode
                                   deft-use-filename-as-title t)))

                  (:name golden-ratio
                         :after (progn
                                  (require 'golden-ratio)
                                  (golden-ratio-mode)))

                  (:name magit
                         :after (progn
                                  (autoload 'magit-status' "magit" nil t)
                                  (setq
                                   magit-git-executable "git"
                                   magit-save-some-buffers 'dontask
                                   magit-status-buffer-switch-function 'switch-to-buffer
                                   )
                                  (define-key whilp/bindings-map (kbd "C-x C-g") 'magit-status)))


                  (:name markdown-mode
                         :after (progn
                                  (add-to-list 'auto-mode-alist '("\\.md\\'" .markdown-mode))))

                  (:name smex
                         :after (progn
                                  (define-key whilp/bindings-map (kbd "C-x C-m") 'smex)
                                  (define-key whilp/bindings-map (kbd "M-x") 'smex)
                                  (define-key whilp/bindings-map (kbd "M-X") 'smex-major-mode-commands)))

                  )
)

(el-get 'sync (mapcar 'el-get-source-name el-get-sources))
