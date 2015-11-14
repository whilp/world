;;; init-dev --- Initialize development environment

;;; Commentary:

;;; My dev env
;;; TODO:
;;; - Add M-, / M-. for each runtime

;;; Code:

(require 'use-package)
(require 'init-shell)

(defvar eir-key "C-<return>"
  "Eval-in-REPL key.")

(use-package nix-mode
  :ensure t)

(use-package company-nixos-options
  :ensure t
  :config (add-to-list 'company-backends 'company-nixos-options))

(use-package edit-server-htmlize
  :ensure t
  :config
  (progn
    (setq edit-server-new-frame nil)
    (edit-server-start)))

(use-package yaml-mode
  :ensure t)

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

;; (use-package cask
;;   :ensure t)

(use-package prog-mode
  :demand t
  :diminish auto-fill-function
  :config
  (add-hook 'prog-mode-hook 'turn-on-auto-fill))

(use-package newcomment
  :config
  (setq comment-auto-fill-only-comments t))

(use-package comment-dwim-2
  :ensure t
  :bind ("M-;" . comment-dwim-2))

(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'")

(use-package eval-in-repl
  :ensure t)

(use-package graphviz-dot-mode
  :ensure t)

(use-package clojure-mode
  :ensure t
  :demand t)

(use-package json-mode
  :ensure t
  :mode "\\.json\\'"
  :config
  (progn
    (use-package js :demand t)
    (defun whilp-json-mode-hook ()
      (interactive)
      (setq js-indent-level 2))

    (add-hook 'json-mode-hook 'whilp-json-mode-hook)
    (add-hook 'js-mode-hook 'whilp-json-mode-hook)))

(use-package lisp-mode
  :config
  (bind-keys :map emacs-lisp-mode-map
             ("M-." . find-function-at-point)))

(use-package eldoc
  :demand t
  :diminish eldoc-mode
  :config
  (dolist (hook '(emacs-lisp-mode-hook
                  lisp-interaction-mode-hook
                  ielm-mode-hook
                  python-mode-hook
                  ))
    (add-hook hook 'eldoc-mode)))

(use-package company
  :ensure t
  :diminish company-mode
  :init (global-company-mode 1)
  :config
  (progn
    (use-package company-go
      :ensure t
      :init (add-to-list 'company-backends 'company-go))
    (setq company-auto-complete t
          company-echo-delay 5
          company-tooltip-minimum-width 30
          company-idle-delay nil)))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package paredit
  :diminish paredit-mode
  :config
    (dolist (hook '(emacs-lisp-mode-hook
                  lisp-interaction-mode-hook
                  ielm-mode-hook
                  python-mode-hook
                  ))
    (add-hook hook 'paredit-mode)))

(use-package paredit-everywhere
  :ensure t
  :diminish paredit-everywhere-mode
  :config
  (progn
    (add-hook 'prog-mode-hook 'paredit-everywhere-mode)))

(use-package elec-pair
  :init
  (progn
    (require 'lisp-mode)
    (add-hook 'emacs-lisp-mode-hook 'electric-pair-mode)))

(use-package js2-mode
  :ensure t
  :defer 1
  :mode ("\\.(json|js)$" . js-mode)
  :config
  (add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2))))

;; go get github.com/rogpeppe/godef
(use-package go-mode
  :mode "\\.go"
  :ensure t
  :config
  (progn
    (bind-keys :map go-mode-map
               ("C-c C-d" . godoc-at-point))
    (setq compilation-error-regexp-alist
          (cons 'go-test compilation-error-regexp-alist))

    (setq gofmt-command "goimports")
    (add-hook 'before-save-hook #'gofmt-before-save)
    (bind-keys :map go-mode-map
               ("M-." . godef-jump))))

(use-package go-eldoc
  :ensure t
  :config (add-hook 'go-mode-hook 'go-eldoc-setup))

;; go get code.google.com/p/go.tools/cmd/oracle
(defvar go-oracle-command)
(use-package go-oracle
  :load-path (lambda () (concat whilp-gopath "src/code.google.com/p/go.tools/cmd/oracle/"))
  :config
  (progn
    (setq go-oracle-command (executable-find "oracle"))
    (add-hook 'go-mode-hook 'go-oracle-mode)))

(use-package restclient
  :ensure t
  :defer t)

;; (use-package minitest
;;   :ensure t
;;   :defer t)

(use-package scala-mode2
  :ensure t
  :defer t)

(use-package flycheck
  :ensure t
  :config
  (progn
    (define-key flycheck-mode-map flycheck-keymap-prefix nil)
    (setq flycheck-keymap-prefix (kbd "C-c f"))
    (define-key flycheck-mode-map flycheck-keymap-prefix flycheck-command-map)
    (setq-default flycheck-emacs-lisp-load-path load-path)
    (global-flycheck-mode)))

;; (use-package rubocop
;;   :ensure t
;;   :defer t
;;   :config
;;   (progn
;;     (require 'flycheck)
;;     (add-hook 'ruby-mode-hook 'rubocop-mode)
;;     (eval-after-load 'flycheck
;;       (flycheck-add-next-checker 'chef-foodcritic 'ruby-rubocop))))

;; (use-package mmm-mode
;;   :ensure t
;;   :commands mmm-mode
;;   :config
;;   (progn
;;     (setq mmm-global-mode 'buffers-with-submode-classes
;;           mmm-submode-decoration-level 0)
;;     (use-package mmm-auto)))

(use-package csv-mode
  :ensure t
  :mode "\\.[Cc][Ss][Vv]\\'"
  :init (setq csv-separators '("," ";" "|" " "))
  :config (use-package csv-nav :ensure t))

(use-package python
  :ensure t
  :mode (("\\.py\\'" . python-mode)
         ("BUILD.*" . python-mode))
  :config
  (progn
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "-i")
    (bind-keys :map python-mode-map
               (eir-key . eir-eval-in-python))
    
    ;; (use-package elpy
    ;;   :ensure t
    ;;   :config
    ;;   (progn
    ;;     ;; Remove elpy company configuration; I like my own.
    ;;     (setq elpy-modules
    ;;           (remove 'elpy-module-company elpy-modules))
    ;;     (elpy-enable)
    ;;     (elpy-use-ipython)))
    ;; (use-package pyenv-mode
    ;;   :ensure t))
    ))

;; (use-package ein
;;   :ensure t)

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
    (add-hook 'ruby-mode-hook 'subword-mode)
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
        compilation-read-command nil
        compilation-ask-about-save nil
        compilation-auto-jump-to-first-error nil
        compilation-save-buffers-predicate nil))

(use-package vc-hooks
  :demand t
  :config
  (bind-keys :map vc-prefix-map ("=" . ediff-revision)))

(use-package ediff
  :demand t
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package projectile
  :ensure t
  :demand t
  :config
  (progn
    (eval-when-compile
      (require 'counsel))
    (defun project-compilation-buffer (prefix)
      (format "*%s: %s*" prefix (projectile-project-root)))
    (defun with-compilation-buffer-name-function-for (prefix orig args)
      (let* ((compilation-buffer-name-function
              (lambda (name-of-mode)
                (format "*%s: %s*" prefix (projectile-project-root)))))
        (apply orig args)))

    (defun with-compile-project (orig &rest args)
      (with-compilation-buffer-name-function-for "compile-project" orig args))
    (defun with-test-project (orig &rest args)
      (with-compilation-buffer-name-function-for "test-project" orig args))

    (advice-add 'projectile-compile-project
                :around #'with-compile-project)
    (advice-add 'projectile-test-project
                :around #'with-test-project)

    (defun projectile-ignore-project (name)
      "Return nil if project NAME should be ignored."
      (string-match
       (rx
        (or "/homebrew/"
            "/.emacs.d/"))
       name))
    (defun projectile-run-shell (&optional buffer)
      "Start a shell in the project's root."
      (interactive "P")
      (projectile-with-default-dir (projectile-project-root)
        (let ((eshell-buffer-name (format "*shell %s*" (projectile-project-name))))
          (eshell))))

    (defun ivy-switch-project ()
      (interactive)
      (ivy-read
       "Switch to project: "
       (if (projectile-project-p)
           (cons (abbreviate-file-name (projectile-project-root))
                 (projectile-relevant-known-projects))
         projectile-known-projects)
       :action #'projectile-switch-project-by-name))

    (ivy-set-actions
     'ivy-switch-project
     '(("v" projectile-vc "Open project root in vc-dir or magit")
       ("e"
        (lambda (dir)
          (projectile-with-default-dir dir
            (let ((eshell-buffer-name (format "*shell %s*" (projectile-project-name))))
              (eshell))))
        "Shell in project")
       ("g"
        (lambda (dir)
          (projectile-with-default-dir dir
            (counsel-git-grep)))
        "Grep in project")))
        
    (bind-keys :map projectile-command-map
               ("p" . ivy-switch-project)
               ("!" . projectile-run-shell)
               ("i" . projectile-compile-project)
               ("o" . projectile-test-project)
               ("g" . counsel-git-grep))

    (setq projectile-keymap-prefix (kbd "C-c p")
          projectile-completion-system 'ivy
          projectile-ignored-project-function 'projectile-ignore-project
          projectile-globally-ignored-directories
          (quote (".idea"
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
    (define-key projectile-mode-map projectile-keymap-prefix 'projectile-command-map)
    (projectile-global-mode)))

(provide 'init-dev)
;;; init-dev ends here
