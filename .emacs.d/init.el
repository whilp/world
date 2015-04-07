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
  (require 'cl)
  (require 'color-theme)
  (require 'use-package))

(require 'bind-key)
(require 'diminish)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(use-package whilp-environment)
(use-package whilp-rcirc)
(use-package whilp-ui)

(use-package whilp-buffers
  :demand t
  :bind (("C-x C-b" . switch-to-buffer)
         ("s-b" . ibuffer)))

(use-package projectile
  :ensure t
  :init (setq projectile-keymap-prefix (kbd "s-p"))
  :config
  (progn
    (bind-keys :map projectile-command-map
               ("f" . projectile-find-file)
               ("g" . projectile-vc-grep)
               ("!" . projectile-run-shell))

    (projectile-global-mode)

    (setq projectile-switch-project-action 'projectile-run-shell
          projectile-globally-ignored-directories
          (quote
           (
            ".idea"
            ".eunit"
            ".git"
            ".hg"
            ".fslckout"
            ".bzr"
            "_darcs"
            ".tox"
            ".svn"
            "build"
            "_workspace"))
          projectile-mode-line
          (quote
           (:eval (format " [%s]" (projectile-project-name)))))

    (defun projectile-run-shell (&optional buffer)
      "Start a shell in the project's root."
      (interactive "P")
      (projectile-with-default-dir (projectile-project-root)
        (shell (format "*shell %s*" (projectile-project-name)))))

    (defun projectile-vc-grep ()
      "Start a shell in the project's root."
      (interactive)
      (let ((regexp (if (and transient-mark-mode mark-active)
                        (buffer-substring (region-beginning) (region-end))
                      (read-string (projectile-prepend-project-name "Grep for: ")
                                   (projectile-symbol-at-point)))))
        (projectile-with-default-dir (projectile-project-root)
          (vc-git-grep regexp "\\*" (projectile-project-root)))))))

;; TODO
;; http://blog.danielgempesaw.com/post/79353633199/installing-mu4e-with-homebrew-with-emacs-from
;; (use-package mu4e
;;   :ensure t
;;   :defer t)

(use-package color-theme-solarized
  :ensure t
  :bind (("s-`" . whilp-toggle-solarized))
  :config
  (progn
    (require 'frame)
    (load-theme 'solarized t)
    (enable-theme 'solarized)

    (defun whilp-toggle-solarized ()
      "Toggles between solarized light and dark."
      (interactive)
      (let ((mode (if (equal (frame-parameter nil 'background-mode) 'dark) 'light 'dark)))
        (set-frame-parameter nil 'background-mode mode)
        (enable-theme 'solarized)))))

(use-package lispy
  :ensure t
  :defer t
  :config
  (progn
    (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))))

(use-package ace-jump-mode
  :ensure t
  :defer t
  :bind (("C-c SPC" . ace-jump-mode)))

(use-package deft
  :ensure t
  :defer t
  :bind (("s-d" . deft))
  :config
  (progn
    (setq deft-extension "txt"
          deft-directory "~/src/github.banksimple.com/whilp/notes"
          deft-text-mode 'org-mode
          deft-use-filename-as-title t)))

(use-package anaconda-mode
  :ensure t
  :defer t
  :config
  (progn
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'eldoc-mode)))

(use-package cider
  :ensure t
  :defer t)

(use-package clojure-mode
  :ensure t
  :defer t)

(use-package company
  :ensure t
  :defer t
  :diminish company-mode
  :config
  (progn
    (global-company-mode)
    (bind-keys :map company-active-map
               ("\C-d" . company-show-doc-buffer)
               ("<tab>" . company-complete))
    (setq company-echo-delay 0
          company-tooltip-minimum-width 30
          company-idle-delay .7)))

(use-package company-go
  :ensure t
  :defer t
  :config
  (progn
    (add-hook 'go-mode-hook
              (lambda ()
                (set (make-local-variable 'company-backends) '(company-go))
                (company-mode)))))

(use-package company-restclient
  :ensure t
  :defer t)

(use-package company-inf-ruby
  :ensure t
  :defer t)

(use-package deferred
  :ensure t
  :defer t)

(use-package async
  :ensure t
  :config
  (progn
    (require 'smtpmail-async)
    (setq ;;send-mail-function 'smtpmail-send-it
          ;;message-send-mail-function 'message-send-mail-with-sendmail
          send-mail-function 'async-smtpmail-send-it
          message-send-mail-function 'async-smtpmail-send-it
          )))

(use-package expand-region
  :ensure t
  :defer t
  :bind (("C-=" . er/expand-region)))

(use-package flx
  :ensure t
  :config
  (progn
    (ido-mode 1)
    (ido-everywhere 1)
    (flx-ido-mode 1)
    (icomplete-mode t)
    (ido-init-completion-maps)
    (ffap-bindings)
    (setq ffap-require-prefix t
          confirm-nonexistent-file-or-buffer nil
          ido-create-new-buffer 'always
          ido-enable-flex-matching t
          ido-everywhere t
          ido-enable-flex-matching t
          ido-use-faces nil)))

(use-package magit
  :ensure t
  :diminish magit-auto-revert-mode
  :config
  (progn
    (bind-keys :prefix-map whilp-magit-map
               :prefix "s-g"
               ("g" . magit-status)
               ("u" . magit-push)
               ("p" . magit-grep)
               ("c" . magit-commit))
    (autoload 'magit-status' "magit" nil t)
    (setq magit-git-executable "gh"
          magit-save-some-buffers nil
          magit-status-buffer-switch-function 'switch-to-buffer
          magit-set-upstream-on-push 'dontask
          magit-completing-read-function 'magit-ido-completing-read
          magit-last-seen-setup-instructions "1.4.0"
          magit-use-overlays nil)))
  
(use-package yasnippet
  :ensure t
  :defer t
  :diminish yas-minor-mode
  :config (yas-global-mode))

(use-package flycheck
  :ensure t
  :defer t
  :config
  (progn
    (setq-default flycheck-emacs-lisp-load-path load-path)
    (global-flycheck-mode)))

(use-package epa-file
  :config
  (progn
    (setq epa-file-name-regexp "\\.\\(gpg\\|asc\\)$"
          epa-armor t)
    (epa-file-name-regexp-update)
    (epa-file-enable)))

(use-package golden-ratio
  :ensure t
  :diminish golden-ratio-mode
  :config (golden-ratio-mode))

(use-package flyspell
  :diminish flyspell-mode)

(use-package simple
  :diminish visual-line-mode)

(use-package abbrev
  :diminish abbrev-mode)

;; TODO: migrate all these bits to separate libraries.
(require 'init-whilp)

(provide 'init)
;;; init ends here
