(spacemacs|use-package-add-hook projectile
  :post-config
  (progn
    (advice-add 'projectile-compile-project
                :around #'with-compile-project)
    (advice-add 'projectile-test-project
                :around #'with-test-project)
    (bind-keys :map projectile-command-map
               ("!" . projectile-run-shell)
               ("i" . projectile-compile-project)
               ("o" . projectile-test-project)
               ("g" . helm-projectile-grep))
    (setq projectile-completion-system 'helm
          projectile-mode-line
          (quote
           (:eval (format " [%s]" (projectile-project-name)))))))

(spacemacs|use-package-add-hook helm-projectile
  :post-config
  (progn
    (helm-projectile-define-key helm-projectile-projects-map
                                (kbd  "C-s") #'spacemacs/helm-project-smart-do-search)
    (helm-projectile-define-key helm-projectile-find-file-map
                                (kbd "C-s") #'spacemacs/helm-project-smart-do-search)))

(defun projectile-run-shell (&optional buffer)
  "Start a shell in the project's root (ignoring BUFFER)."
  (interactive "P")
  (projectile-with-default-dir (projectile-project-root)
    (let ((eshell-buffer-name (format "*shell %s*" (projectile-project-name))))
      (eshell))))

(defun with-compilation-buffer-name-function-for (prefix orig args)
  "With a compilation buffer name beginning with PREFIX, apply ORIG and ARGS."
  (let* ((compilation-buffer-name-function
          (lambda (name-of-mode)
            (format "*%s: %s*" prefix (projectile-project-root)))))
    (apply orig args)))

(defun with-compile-project (orig &rest args)
  "Wrap ORIG with ARGS to compile a project in a dedicated buffer."
  (with-compilation-buffer-name-function-for "compile-project" orig args))

(defun with-test-project (orig &rest args)
  "Wrap ORIG with ARGS to test a project in a dedicated buffer."
  (with-compilation-buffer-name-function-for "test-project" orig args))
