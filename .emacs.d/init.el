;;; init --- Emacs initialization

;;; Commentary:
;;; User configuration

;;; Code:

(setq load-prefer-newer t)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'cl)
  (require 'use-package))

(require 'bind-key)
(require 'diminish)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(use-package whilp-environment)
(use-package whilp-rcirc)
(use-package whilp-ui)

(use-package whilp-buffers
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

;; TODO: migrate all these bits to separate libraries.
(require 'init-whilp)

(provide 'init)
;;; init ends here
