;;; init-helm --- Initialize helm.

;;; Commentary:

;;; My helm.

;;; Code:

(require 'use-package)

(use-package helm
  :ensure t
  :demand t
  :diminish helm-mode
  :bind (("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files))
  :config
  (progn
    (global-set-key (kbd "C-c h") 'helm-command-prefix)
    (global-unset-key (kbd "C-x c"))
    (setq helm-split-window-in-side-p t
          helm-move-to-line-cycle-in-source t
          helm-scroll-amount 8)
    (helm-mode 1)))

(use-package helm-semantic
  :demand t
  :config
  (setq helm-semantic-fuzzy-match t))

(use-package helm-command
  :demand t
  :config
  (setq helm-M-x-fuzzy-match t))

(use-package helm-buffers
  :demand t
  :config
  (setq helm-buffers-fuzzy-matching t))

(use-package helm-imenu
  :demand t
  :config
  (setq helm-imenu-fuzzy-match t))

(use-package helm-elisp
  :demand t
  :config
  (setq helm-apropos-fuzzy-match t
        helm-lisp-fuzzy-completion t))

(use-package helm-locate
  :demand t
  :config
  (setq helm-locate-fuzzy-match t))

(use-package helm-files
  :demand t
  :config
  (setq helm-ff-search-library-in-sexp t
        helm-recentf-fuzzy-match t
        helm-ff-file-name-history-use-recentf t))

(use-package helm-git-grep
  :ensure t
  :demand t
  :bind (("C-c g" . helm-git-grep))
  :config
  (progn
    (bind-keys :map isearch-mode-map ("C-c g" . helm-git-grep-from-isearch))
    (eval-after-load 'helm
      (bind-keys :map helm-map ("C-c g" . helm-git-grep-from-helm)))
    ))

(use-package projectile
  :ensure t
  :demand t
  :init (setq projectile-keymap-prefix (kbd "s-p"))
  :config
  (progn
    (bind-keys :map projectile-command-map
               ("!" . projectile-run-shell))

    (projectile-global-mode)

    (setq projectile-switch-project-action 'helm-projectile
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
        (shell (format "*shell %s*" (projectile-project-name)))))))

(use-package helm-projectile
  :ensure t
  :demand t
  :config
  (progn
    (setq projectile-completion-system 'helm
          helm-projectile-fuzzy-match t)
    (helm-projectile-on)
    (bind-keys :map projectile-command-map
               ("f" . helm-projectile-find-file-dwim)
               ("g" . helm-git-grep))))

(use-package helm-swoop
  :ensure t
  :bind (("M-i" . helm-swoop))
  :config
  (progn
    (defun whilp-helm-pre-input ()
      "")
    (setq helm-swoop-pre-input-function 'whilp-helm-pre-input)))

(use-package helm-descbinds
  :ensure t)

;; TODO
(use-package helm-dash
  :ensure t
  :demand t
  :config
  (setq helm-dash-browser-func 'eww))

(provide 'init-helm)
;;; init-helm ends here
