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

(use-package json-mode
  :ensure t
  :mode "\\.json\\'"
  :config
  (progn
    (use-package js :demand t)
    (defun whilp-json-mode-hook ()
      (interactive)
      (setq js-indent-level 2))

    (add-hook 'json-mode-hook 'whilp-json-mode-hook)))

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
  :diminish company-mode
  :init (global-company-mode 1)
  :config
  (progn
    (dolist (package '(company-go
                       company-inf-ruby
                       company-tern
                       company-math
                       company-restclient))
      (use-package package
        :ensure t
        :demand t
        :init (add-to-list 'company-backends package)))

    ;; Use Helm to complete suggestions
    (bind-keys :map company-active-map
               ("\C-d" . company-show-doc-buffer)
               ("C-:" . helm-company))
    (bind-keys :map company-mode-map
               ("C-:" . helm-company))
    (setq company-echo-delay 0
          company-tooltip-minimum-width 30
          company-idle-delay .7)))

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
    (add-hook 'before-save-hook #'gofmt-before-save)
    (add-hook 'go-mode-hook (lambda ()
                              (local-set-key (kbd "M-.") #'godef-jump)))))

;; TODO
;; (use-package go-oracle)

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
    (require 'flycheck)
    (add-hook 'ruby-mode-hook 'rubocop-mode)
    (eval-after-load 'flycheck
      (flycheck-add-next-checker 'chef-foodcritic 'ruby-rubocop))))

(use-package go-eldoc
  :ensure t
  :defer t
  :config (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package mmm-mode
  :ensure t
  :commands mmm-mode
  :config
  (progn
    (setq mmm-global-mode 'buffers-with-submode-classes
          mmm-submode-decoration-level 0)
    (use-package mmm-auto)))

(use-package csv-mode
  :ensure t
  :mode "\\.[Cc][Ss][Vv]\\'"
  :init (setq csv-separators '("," ";" "|" " "))
  :config (use-package csv-nav :ensure t))

(use-package python
  :ensure t
  :mode (("\\.py\\'" . python-mode))
  :config
  (progn
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "-i"))

    (use-package elpy
      :ensure t
      :init
      (elpy-enable)
      (elpy-use-ipython)))

(use-package ruby-mode
  :ensure t
  :commands ruby-mode
  :mode (("Gemfile\\'" . ruby-mode)
         ("\\.builder\\'" . ruby-mode)
         ("\\.gemspec\\'" . ruby-mode)
         ("\\.irbrc\\'" . ruby-mode)
         ("\\.pryrc\\'" . ruby-mode)
         ("\\.rake\\'" . ruby-mode)
         ("\\.ru\\'" . ruby-mode)
         ("\\.rxml\\'" . ruby-mode))
  :init
  (setq ruby-use-encoding-map nil)
  :config
  (progn
    (use-package inf-ruby :ensure t)
    (use-package ruby-hash-syntax :ensure t)

    (eval-after-load 'ruby-mode
      (bind-keys :map ruby-mode-map
                 ("RET" . reindent-then-newline-and-indent)
                 ("TAB" . indent-for-tab-command)))

    (add-hook 'ruby-mode-hook 'subword-mode)

      (use-package robe
        :ensure t
        :config (add-hook 'ruby-mode-hook 'robe-mode))

      (use-package ruby-compilation
        :ensure t)

      (use-package yari
        :ensure t
        :init (defalias 'ri 'yari))

      (use-package rinari
        :ensure t
        :init
        (global-rinari-mode))

      (use-package rspec-mode
        :ensure t
        :config (rspec-mode 1))

      (use-package bundler
        :ensure t)

      ;; Stupidly the non-bundled ruby-mode isn't a derived mode of
      ;; prog-mode: we run the latter's hooks anyway in that case.
      (add-hook 'ruby-mode-hook
                (lambda ()
                  (unless (derived-mode-p 'prog-mode)
                    (run-hooks 'prog-mode-hook))))))
(use-package pp
  :demand t
  :config
  (progn
    (global-set-key [remap eval-expression] 'pp-eval-expression)
    (global-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)))

(use-package paren
  :demand t
  :config
  (progn
    (setq show-paren-delay 0
          show-paren-style 'parenthesis)
    (show-paren-mode t)))

(use-package compile
  :demand t
  :config
  (setq compilation-scroll-output t
        compilation-ask-about-save nil
        compilation-save-buffers-predicate '(lambda () nil)))

(use-package vc-hooks
  :demand t
  :config
  (bind-keys :map vc-prefix-map ("=" . ediff-revision)))

(use-package ediff
  :demand t
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(provide 'init-dev)
;;; init-dev ends here
