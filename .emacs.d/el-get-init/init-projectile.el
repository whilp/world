(require 'projectile)
(projectile-global-mode)

(setq
 projectile-mode-line '(
                        :eval (format " [%s]"
                                      (projectile-project-name)))
 projectile-switch-project-action 'projectile-vc
 )
(define-key whilp/bindings-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map [?\s-d] 'projectile-find-dir)
(define-key projectile-mode-map [?\s-p] 'projectile-switch-project)
(define-key projectile-mode-map [?\s-f] 'projectile-find-file)
(define-key projectile-mode-map [?\s-g] 'projectile-grep)
(define-key projectile-mode-map [?\s-v] 'projectile-vc)
