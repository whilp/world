;;; init-whilp --- Emacs initialization

;;; Commentary:
;;; User configuration

;;; Code:

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

;; check out https://github.com/jwiegley/use-package
;; and do this: http://www.lunaryorn.com/2015/01/06/my-emacs-configuration-with-use-package.html
;; eval-in-repl https://github.com/kaz-yos/eval-in-repl
;; https://github.com/svend/emacs-essh
;; http://ess.r-project.org/
;; https://github.com/uk-ar/key-combo
;; https://github.com/capitaomorte/yasnippet
;; https://github.com/abo-abo/lispy
;; https://github.com/purcell/unfill

;; environment.

(define-minor-mode whilp/bindings "My global bindings" t nil (make-sparse-keymap))

(defun yank-pop-forwards (arg)
  (interactive "p")
  (yank-pop (- arg)))
(define-key whilp/bindings-map (kbd "M-S-Y") 'yank-pop-forwards)

;; remember.
(winner-mode 1)
(desktop-save-mode 1)
(setq savehist-file "~/.emacs.d/savehist"
      savehist-additional-variables '(
                                      search-ring
                                      regexp-search-ring
                                      ))
(savehist-mode 1)


;; javascript
(setq js-indent-level 2)

;; python
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i")
(define-key whilp/bindings-map (kbd "C-c y r") 'run-python)

(setq completion-cycle-threshold 10)

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
(setq show-paren-delay 0
      show-paren-style 'parenthesis)
(show-paren-mode t)
(global-hl-line-mode t)

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
(bind-keys ("s-SPC" . other-window)
           ("s-1" . delete-other-windows)
           ("s-2" . split-window-below)
           ("s-3" . split-window-right))

;; command-as-meta.
(setq mac-option-key-is-meta nil       
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'super
      ns-function-modifier 'hyper
      kill-read-only-ok)

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
 el-get-sources '())

(el-get 'sync '(mu4e))

;; browse in the background.
(defun browse-url-default-macosx-browser (url &optional _new-window)
  (interactive (browse-url-interactive-arg "URL: "))
  (start-process (concat "open -g" url) nil "open" "-g" url))

(provide 'init-whilp)
;;; init ends here
