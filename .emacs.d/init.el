;; customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; environment.
(setq explicit-shell-file-name "/bin/bash")
(setq exec-path
      (append
       (mapcar
        'expand-file-name
        '(
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
(setenv "GIT_COMMITTER_NAME" "Will Maier")
(setenv "GIT_COMMITTER_EMAIL" "wcmaier@m.aier.us")
(setenv "GIT_AUTHOR_NAME" "Will Maier")
(setenv "GIT_AUTHOR_EMAIL" "wcmaier@m.aier.us")

(setenv "GOPATH" (expand-file-name "~/go"))

(define-minor-mode whilp/bindings "My global bindings" t nil (make-sparse-keymap))

;; no bars, bells.
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode '(nil . -1))
(setq ring-bell-function 'ignore
      visible-bell t)
(winner-mode 1)
(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

(defun yank-pop-forwards (arg)
  (interactive "p")
  (yank-pop (- arg)))
(define-key whilp/bindings-map (kbd "M-S-Y") 'yank-pop-forwards)

(setq redisplay-dont-pause t)

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
(setq display-time-format "%a %Y-%m-%d %H:%M"
      display-time-default-load-average nil
      )
(display-time-mode 1)
(line-number-mode 0)
(column-number-mode 0)
(size-indication-mode 0)
(display-battery-mode 0)
(which-function-mode 0)

;; no splash.
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; no prompts.
(setq confirm-nonexistent-file-or-buffer nil)
(fset 'yes-or-no-p 'y-or-n-p)

;; syntax highlighting.
(global-font-lock-mode t)
(transient-mark-mode t)

;; javascript
(setq js-indent-level 2)

;; python
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i")
(define-key whilp/bindings-map (kbd "C-c y r") 'run-python)


;; GC buffers, uniquify buffer names, ibuffer.
(require 'midnight)
(add-to-list 'clean-buffer-list-kill-never-regexps "^#.*")
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(require 'ibuffer)

(setq completion-cycle-threshold 10)

;; Unbind C-x C-b.
(define-key whilp/bindings-map (kbd "C-x C-b") 'switch-to-buffer)
(setq ibuffer-expert t)
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-saved-filter-groups
      (quote
      '("default"
        ("rcirc" (mode . rcirc-mode)))))
(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-auto-mode t)
             (ibuffer-switch-to-saved-filter-groups "default")))

(eval-when-compile
  (require 'cl))
 
(defun get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE"
  (let ((buffer-mode-matches '()))
   (dolist (buf (buffer-list))
     (with-current-buffer buf
       (if (eq mode major-mode)
           (add-to-list 'buffer-mode-matches buf))))
   buffer-mode-matches))
 
(defun multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur
   (get-buffers-matching-mode major-mode)
   (car (occur-read-primary-args))))

;; I'm an adult.
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; term.
(defun term-send-function-key ()
  (interactive)
  (let* ((char last-input-event)
         (output (cdr (assoc char term-function-key-alist))))
    (term-send-raw-string output)))

(defconst term-function-key-alist '((f1 . "\e[11~")
                                    (f2 . "\e[12~")
                                    (f3 . "\e[13~")
                                    (f4 . "\e[14~")
                                    (f5 . "\e[15~")
                                    (f6 . "\e[17~")
                                    (f7 . "\e[18~")
                                    (f8 . "\e[19~")
                                    (f9 . "\e[20~")
                                    (f10 . "\e[21~")
                                    (f11 . "\e[23~")
                                    (f12 . "\e[24~")
                                    ))

;; parens trap.
(setq
 show-paren-delay 0
 show-paren-style 'expression
 )
(show-paren-mode t)
;; disable hl-line-mode because it conflicts with show-paren when
;; using solarized colors.
(global-hl-line-mode -1)

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
(setq tramp-password-prompt-regexp
      (concat "^.*"
              (regexp-opt '("[pP]assword" "[pP]assphrase" "Verification code") t)
              ".*:? *"))

;; from @tom, to fix CM shiz
;; '(tramp-default-proxies-alist (quote ((nil "\\`root\\'" "/ssh:%h:"))))
;; '(tramp-ssh-controlmaster-options
;;  "-o ControlPath=%t.%%r@%%h:%%p -o ControlMaster=auto -o ControlPersist=no" t)
;; '(tramp-use-ssh-controlmaster-options nil)
;; 2015-02-12 18:29:50 <ieure> (eval-after-load "tramp" '(progn (setq tramp-use-ssh-controlmaster-options nil)))

(defun remote-shell (&optional host)
  "Open a remote shell to a host."
  (interactive)
  (with-temp-buffer
    (let ((host (if host host (read-string "Host: "))))
      (cd (concat "/" host ":"))
      (shell (concat "*" host "*")))))
(define-key whilp/bindings-map (kbd "C-x s") 'remote-shell)
(define-key whilp/bindings-map (kbd "s-s") 'remote-shell)

;; GPG.
(require 'epa-file)
(setq epa-file-name-regexp "\\.\\(gpg\\|asc\\)$"
      epa-armor t)
(epa-file-name-regexp-update)
(epa-file-enable)

;; backups.
(setq make-backup-files nil
      auto-save-default nil
      backup-directory-alist `(("." . "~/.saves"))
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" "~/.saves")))

;; pretty-print expression evals.
(global-set-key [remap eval-expression] 'pp-eval-expression)
(global-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)

;; windows.
(define-key whilp/bindings-map (kbd "M-i") 'windmove-left)
(define-key whilp/bindings-map (kbd "s-i") 'windmove-left)
(define-key whilp/bindings-map (kbd "M-j") 'windmove-down)
(define-key whilp/bindings-map (kbd "s-j") 'windmove-down)
(define-key whilp/bindings-map (kbd "M-k") 'windmove-up)
(define-key whilp/bindings-map (kbd "s-k") 'windmove-up)
(define-key whilp/bindings-map (kbd "M-l") 'windmove-right)
(define-key whilp/bindings-map (kbd "s-l") 'windmove-right)
(define-key whilp/bindings-map (kbd "s-1") 'delete-other-windows)
(define-key whilp/bindings-map (kbd "s-2") 'split-window-below)
(define-key whilp/bindings-map (kbd "s-3") 'split-window-right)

;; command-as-meta.
(setq mac-option-key-is-meta nil)       
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
(setq ns-function-modifier 'hyper)
(setq kill-read-only-ok)

;; speling
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; whitespace.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(define-key whilp/bindings-map (kbd "<backtab>") 'indent-relative)

;; wrap.
(global-visual-line-mode 1)

;; fix myself.
(setq default-abbrev-mode t)
(define-key whilp/bindings-map (kbd "C-_") 'dabbrev-expand)

;; nssh
(load-file "~/.emacs.d/nssh.el")

(load-file "~/.emacs.d/rcirc.el")

;; vc
(setq
 compilation-scroll-output t
 compilation-ask-about-save nil
 compilation-save-buffers-predicate '(lambda () nil))
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(eval-after-load "vc-hooks"
         '(define-key vc-prefix-map "=" 'ediff-revision))

(cond
 ((string-equal system-type "darwin")
  (progn
    (setenv "GPG_AGENT_INFO" (expand-file-name "~/.gnupg/S.gpg-agent::1"))
    (setq epg-gpg-program "gpg2"))
  (set-face-attribute 'default nil :font "Monaco-16")))

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))
(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-recipes")

(setq
 el-get-byte-compile t
 el-get-git-shallow-clone t
 el-get-user-package-directory "~/.emacs.d/el-get-init"
 el-get-sources '()
 my-el-get-packages (append
                     '(
                       ace-jump-mode
                       anaconda-mode
                       apache-mode
                       cider
                       clojure-mode
                       color-theme-solarized
                       company-mode
                       company-anaconda
                       company-inf-ruby
                       csv-mode
                       deferred
                       deft
                       delight
                       emacs-async
                       emms
                       erc-highlight-nicknames
                       expand-region
                       find-file-in-project
                       flx
                       flycheck
                       gist
                       git-modes
                       git-gutter-fringe+
                       go-company
                       go-eldoc
                       go-oracle
                       go-mode
                       golden-ratio
                       ibuffer-projectile
                       inf-ruby
                       magit
                       markdown-mode
                       minitest-mode
                       mu4e
                       nginx-mode
                       ;; TODO: origami-mode
                       org-mode
                       projectile
                       rcirc-color
                       request
                       restclient
                       robe-mode
                       rubocop-mode
                       scala-mode2
                       smart-mode-line
                       smex
                       twittering-mode
                       web-mode
                       yaml-mode
                       znc
                       )
                     (mapcar 'el-get-source-name el-get-sources))
 )

(el-get 'sync my-el-get-packages)

;; browse in the background.
(defun browse-url-default-macosx-browser (url &optional _new-window)
  (interactive (browse-url-interactive-arg "URL: "))
  (start-process (concat "open -g" url) nil "open" "-g" url))
