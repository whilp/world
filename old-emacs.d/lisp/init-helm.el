;;; init-helm --- Initialize helm.

;;; Commentary:

;;; My helm.

;;; Code:

(require 'use-package)

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
    
    (setq helm-split-window-in-side-p t
          helm-echo-input-in-header-line t
          helm-autoresize-max-height nil
          helm-autoresize-min-height nil
          helm-display-header-line nil
          helm-move-to-line-cycle-in-source t
          helm-scroll-amount 8)
    (helm-autoresize-mode -1)
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



(use-package helm-projectile
  :ensure t
  :demand t
  :config
  (progn
    (setq projectile-completion-system 'helm)
    (helm-projectile-on)
    (helm-projectile-define-key helm-projectile-find-file-map
      (kbd "M-v") #'helm-projectile-vc
      (kbd "M-b") #'helm-projectile-switch-to-buffer
      (kbd "M-g") #'helm-git-grep-2)

    (defun helm-git-grep-2 (&optional arg)
      "Throw away arg."
      (helm-git-grep))
    
    ;; (helm-projectile-define-key helm-projectile)
    (bind-keys :map projectile-command-map
               ("f" . helm-projectile-find-file-dwim)
               ("g" . helm-git-grep))
    ))

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

(provide 'init-helm)
;;; init-helm ends here
