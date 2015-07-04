;;; init-dev --- Initialize development environment

;;; Commentary:

;;; My dev env

;;; Code:

(require 'use-package)
(require 'init-shell)
(require 'hydra)

;; go get code.google.com/p/go.tools/cmd/oracle
(add-to-list 'load-path
             (concat whilp-gopath "src/code.google.com/p/go.tools/cmd/oracle/"))

(defvar eir-key "C-<return>"
  "Eval-in-REPL key.")

(use-package eshell
  :init
  (add-hook 'eshell-mode-hook
            #'(lambda ()
                (define-key eshell-mode-map [remap pcomplete] 'helm-esh-pcomplete))))

(use-package ess
  :ensure t)

(use-package htmlize
  :ensure t)

(use-package eval-in-repl
  :ensure t)

(use-package graphviz-dot-mode
  :ensure t)

(use-package clojure-mode
  :ensure t
  :demand t)

;; TODO: could be neat to make a hydra for sp motions.
(use-package smartparens
  :ensure t
  :demand t
  :diminish (smartparens-mode)
  :init
  (setq sp-base-key-bindings 'paredit
        sp-autoskip-closing-pair 'always
        sp-ignore-modes-list '(minibuffer-inactive-mode
                               shell-mode))
  :config
  (progn
    (bind-keys :map smartparens-mode-map
               ("C-)" . sp-forward-slurp-sexp)
               ("C-(" . sp-backward-barf-sexp)
               ("C-M-)" . sp-backward-slurp-sexp)
               ("C-M-(" . sp-backward-barf-sexp))
    (require 'smartparens-config)
    (sp-use-paredit-bindings)
    (show-smartparens-global-mode)
    (smartparens-global-strict-mode t)))

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
  (add-hook 'python-mode-hook 'anaconda-mode))

(use-package cider
  :ensure t
  :defer t)

(use-package ielm
  :init
  (progn
    (require 'info)
    (require 'lisp-mode)
    (dolist (map (list emacs-lisp-mode-map
                       lisp-interaction-mode-map
                       Info-mode-map))
      (bind-keys :map map
                 (eir-key . eir-eval-in-ielm)))))

(use-package eldoc
  :demand t
  :diminish eldoc-mode
  :config
  (dolist (hook '(emacs-lisp-mode-hook
                  lisp-interaction-mode-hook
                  ielm-mode-hook
                  python-mode-hook))
    (add-hook (quote hook) 'eldoc-mode)))

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
          company-idle-delay nil)))

(use-package inf-ruby
  :ensure t
  :defer t
  :config
  (progn
    (setq ruby-deep-indent-paren nil
          ruby-deep-arglist nil)))

(use-package js2-mode
  :ensure t
  :defer 1
  :mode ("\\.(json|js)$" . js-mode)
  :config
  (add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2))))
            
(use-package go-mode
  :ensure t
  :defer t
  :config
  (progn
    (use-package go-eldoc
      :ensure t
      :demand t
      :init (add-hook 'go-mode-hook 'go-eldoc-setup))
    (use-package go-oracle
      :init (add-hook 'go-mode-hook 'go-oracle-mode))
    (setq gofmt-command "goimports")
    (add-hook 'before-save-hook #'gofmt-before-save)
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

(use-package flycheck
  :ensure t
  :config
  (progn
    (bind-key
     "C-c f"
     (defhydra hydra-flycheck () "flycheck"
       ("a" first-error "first")
       ("c" flycheck-compile "compile")
       ("n" next-error "next")
       ("p" previous-error "previous")))
    ;; Move flycheck to a junk binding to avoid shadowing org's C-c !.
    (define-key flycheck-mode-map flycheck-keymap-prefix nil)
    (setq flycheck-keymap-prefix (kbd "s-0"))
    (define-key flycheck-mode-map flycheck-keymap-prefix flycheck-command-map)
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
          python-shell-interpreter-args "-i")
    (bind-keys :map python-mode-map
               (eir-key . eir-eval-in-python))
    
    (use-package elpy
      :ensure t
      :init
      (elpy-enable)
      (elpy-use-ipython))))

(use-package ein
  :ensure t)

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
    (dolist (package '(inf-ruby
                       ruby-hash-syntax
                       ruby-compilation
                       bundler))
      (use-package package :ensure t))
    (bind-keys :map ruby-mode-map
               ("RET" . reindent-then-newline-and-indent)
               ("TAB" . indent-for-tab-command)
               (eir-key . eir-eval-in-ruby))
    
    (add-hook 'ruby-mode-hook 'subword-mode)
    
    (use-package robe
      :ensure t
      :init
      (progn
        (with-eval-after-load 'company
          (push 'company-robe company-backends))
        (add-hook 'ruby-mode-hook 'robe-mode)))

    (use-package yari
      :ensure t
      :init (defalias 'ri 'yari))

    (use-package rinari
      :ensure t
      :init (global-rinari-mode))

    (use-package rspec-mode
      :ensure t
      :config
      (add-hook 'ruby-mode-hook (lambda () (rspec-mode 1))))

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

;; TODO: essh https://github.com/svend/emacs-essh

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

(use-package multiple-cursors
  :ensure t
  :demand t
  :init
  (bind-key
   "C-c c"
   (defhydra hydra-multiple-cursors () "multiple-curors"
     ("e" mc/edit-lines "edit")
     ("n" mc/mark-next-like-this "next")
     ("p" mc/mark-previous-like-this "previous")
     ("a" mc/mark-all-like-this "all"))))

(use-package know-your-http-well
  :ensure t)

(use-package aggressive-indent
  :ensure t
  :diminish aggressive-indent-mode
  :init
  (progn
    ;; (add-to-list 'aggressive-indent-excluded-modes ...) as needed.
    (global-aggressive-indent-mode 1)))

(provide 'init-dev)
;;; init-dev ends here
