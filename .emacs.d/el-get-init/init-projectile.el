(require 'projectile)
(projectile-global-mode)

(setq
 projectile-switch-project-action 'projectile-run-shell
 projectile-globally-ignored-directories '(
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
                                           "_workspace")
 projectile-ignored-file-extensions '("class" "o" "so" "elc" "test")
 projectile-mode-line '(
                        :eval (format " [%s]"
                                      (projectile-project-name)))
 )


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
      (vc-git-grep regexp "\\*" (projectile-project-root)))))

(define-key whilp/bindings-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map [?\s-f] 'projectile-find-file)

(define-key whilp/bindings-map (kbd "s-p !") 'projectile-run-shell)
