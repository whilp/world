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
    (bind-keys :prefix-map helm-command-map
               :prefix "s-h")

    (use-package helm-unicode
      :init
      (bind-keys :map helm-command-map
                 ("u" . helm-unicode)))

    (use-package helm-c-yasnippet
      :ensure t
      :init
      (progn
        (setq helm-yas-display-key-on-candidate t)
        (bind-keys :map helm-command-map
                   ("y" . helm-yas-complete))))

    (use-package helm-git-grep
      :ensure t
      :demand t
      :bind (("C-c g" . helm-git-grep))
      :config
      (progn
        (bind-keys :map isearch-mode-map ("C-c g" . helm-git-grep-from-isearch))
        (bind-keys :map helm-map ("C-c g" . helm-git-grep-from-helm))))

    (global-unset-key (kbd "C-x c"))
    (setq helm-split-window-in-side-p t
          helm-move-to-line-cycle-in-source t
          helm-scroll-amount 8)
    (helm-mode 1)))

(use-package helm-semantic
  :demand t)

(use-package helm-command
  :demand t)

(use-package helm-buffers
  :demand t)

(use-package helm-imenu
  :demand t)

(use-package helm-elisp
  :demand t)

(use-package helm-locate
  :demand t)

(use-package helm-files
  :demand t
  :config
  (setq helm-ff-search-library-in-sexp t
        helm-ff-file-name-history-use-recentf t))

(use-package helm-company
  :ensure t)

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
    (setq projectile-completion-system 'helm)
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
