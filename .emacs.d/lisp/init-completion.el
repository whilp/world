;;; init-completion --- Initialize swiper.

;;; Commentary:

;;; My swiper.

;;; Code:

(require 'use-package)

(use-package projectile
  :ensure t
  :demand t
  :config
  (progn
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
        (shell (format "*shell %s*" (projectile-project-name)))))
    (bind-keys :map projectile-command-map
               ("!" . projectile-run-shell)
               ("i" . projectile-compile-project)
               ("o" . projectile-test-project))
    (setq projectile-keymap-prefix (kbd "C-c p")
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

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x))
  :init
  (progn
    (bind-keys :map projectile-command-map
               ("g" . counsel-git-grep))))

(use-package swiper
  :ensure t
  :bind (("M-i" . swiper))
  :diminish ivy-mode
  :config
  (progn
    (eval-when-compile
      (require 'magit)
      (require 'projectile))

    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t
          magit-completing-read-function 'ivy-completing-read
          projectile-completion-system 'ivy)))

(provide 'init-completion)
;;; init-completion ends here
