(require 'projectile)
(projectile-global-mode)

(setq
 projectile-switch-project-action 'projectile-find-file
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
(define-key whilp/bindings-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map [?\s-d] 'projectile-find-dir)
(define-key projectile-mode-map [?\s-p] 'projectile-switch-project)
(define-key projectile-mode-map [?\s-f] 'projectile-find-file)
(define-key projectile-mode-map [?\s-g] 'projectile-grep)
(define-key projectile-mode-map [?\s-v] 'projectile-vc)
