;;; init-helm --- Initialize helm.

;;; Commentary:

;;; My helm.

;;; Code:

(require 'use-package)
(require 'hydra)

;; For the compiler's benefit.
(require 'projectile nil t)
(require 'helm nil t)

(use-package helm
  :ensure t
  :demand t
  :diminish helm-mode
  :bind (("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini)
         ("C-x C-b" . helm-mini))
  :config
  (progn
    (bind-key
     "C-c h"
     (defhydra hydra-helm () "helm"
       ("e" helm-flycheck "errors")
       ("g" helm-git-grep "git-grep")
       ("r" helm-resume "resume")
       ("x" whilp-run-external-command "run" :exit 1)
       ("u" helm-unicode "unicode")))

    (use-package helm-external
      :init
      (progn
        (defun whilp-run-external-command (arg)
          (interactive "P")
          (when arg
            (setq helm-external-commands-list '()))
          (let ((program (helm-comp-read
                          "Run: "
                          (helm-external-commands-list-1 'sort)
                          :must-match nil
                          :del-input nil
                          :name "External Commands"
                          :history helm-external-command-history)))
            (helm-run-or-raise program)
            (setq helm-external-command-history
                  (cons program
                        (delete program
                                (cl-loop for i in helm-external-command-history
                                         when (executable-find i) collect i))))))))

    (use-package helm-unicode
      :ensure t)

    (use-package helm-c-yasnippet
      :ensure t
      :init
      (setq helm-yas-display-key-on-candidate t))

    (use-package helm-git-grep
      :ensure t
      :demand t
      :config
      (progn
        (bind-keys :map isearch-mode-map ("C-c g" . helm-git-grep-from-isearch))
        (bind-keys :map helm-map ("C-c g" . helm-git-grep-from-helm))))

    (use-package helm-flycheck
      :ensure t
      :commands helm-flycheck)

    (global-unset-key (kbd "C-x c"))
    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*helm" (* not-newline) "*" eos)
                   (display-buffer-in-side-window)
                   (inhibit-same-window . t)
                   (window-height . 0.4)))
    
    (setq helm-split-window-in-side-p t
          helm-echo-input-in-header-line t
          helm-autoresize-max-height 30
          helm-autoresize-min-height 10
          helm-display-header-line nil
          helm-move-to-line-cycle-in-source t
          helm-scroll-amount 8)
    (helm-autoresize-mode 1)
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
  :ensure t
  :bind ("C-:" . helm-company))

(use-package projectile
  :ensure t
  :demand t
  :config
  (progn
    (setq projectile-keymap-prefix (kbd "C-c p")
          projectile-switch-project-action 'helm-projectile
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
    (define-key projectile-mode-map projectile-keymap-prefix 'projectile-command-map)
    (projectile-global-mode)

    (defun projectile-run-shell (&optional buffer)
      "Start a shell in the project's root."
      (interactive "P")
      (projectile-with-default-dir (projectile-project-root)
        (shell (format "*shell %s*" (projectile-project-name)))))
    (bind-keys :map projectile-command-map
             ("!" . projectile-run-shell))))

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
