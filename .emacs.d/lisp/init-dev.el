;;; init-dev --- Initialize development environment

;;; Commentary:

;;; My dev env

;;; Code:

(require 'use-package)

(use-package clojure-mode
  :ensure t
  :demand t)

(use-package paredit
  :ensure t
  :demand t
  :diminish paredit-mode
  :init
  (progn
    (add-hook 'clojure-mode-hook 'enable-paredit-mode)
    (add-hook 'cider-repl-mode-hook 'enable-paredit-mode)
    (add-hook 'lisp-mode-hook 'enable-paredit-mode)
    (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
    (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
    (add-hook 'ielm-mode-hook 'enable-paredit-mode)
    (add-hook 'json-mode-hook 'enable-paredit-mode))
  :config
  (bind-keys :map clojure-mode-map
             ("M-[" . paredit-wrap-square)
             ("M-{" . paredit-wrap-curly)))

(use-package json-rpc
  :ensure t)

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

(use-package eldoc
  :demand t
  :diminish eldoc-mode
  :config
  (progn
    (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
    (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
    (add-hook 'ielm-mode-hook 'eldoc-mode)))

(use-package company
  :ensure t
  :demand t
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

(use-package inf-ruby
  :ensure t
  :defer t
  :config
  (progn
    (setq ruby-deep-indent-paren nil
          ruby-deep-arglist nil)))

(use-package js2-mode
  :ensure t
  :defer t
  :config (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(use-package go-mode
  :ensure t
  :defer t
  :config
  (progn
    (setq gofmt-command "goimports")
    (add-hook 'go-mode-hook (lambda ()
                              (local-set-key (kbd "M-.") #'godef-jump)))))

(use-package restclient
  :ensure t
  :defer t)

(use-package minitest
  :ensure t
  :defer t)

(use-package scala-mode2
  :ensure t
  :defer t)

(use-package robe
  :ensure t
  :defer t
  :config
  (progn
    (add-hook 'ruby-mode-hook 'robe-mode)
    (eval-after-load 'company
      '(push 'company-robe company-backends))))

(use-package flycheck
  :ensure t
  :config
  (progn
    (setq-default flycheck-emacs-lisp-load-path load-path)
    (global-flycheck-mode)))

(use-package rubocop
  :ensure t
  :defer t
  :config
  (progn
    (add-hook 'ruby-mode-hook 'rubocop-mode)
    (eval-after-load 'flycheck
      (flycheck-add-next-checker 'chef-foodcritic 'ruby-rubocop))))

(use-package go-eldoc
  :ensure t
  :defer t
  :config (add-hook 'go-mode-hook 'go-eldoc-setup))

(provide 'init-dev)
;;; init-dev ends here
