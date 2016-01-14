;;; packages.el --- whilp-projectile Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq whilp-projectile-packages
      '(
        projectile
        ivy
      ))

;; List of packages to exclude.
(setq whilp-projectile-excluded-packages '())

;; For each package, define a function whilp-projectile/init-<package-name>
;;
;; (defun whilp-projectile/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun whilp-projectile/pre-init-ivy ()
  (spacemacs|use-package-add-hook projectile
    :post-config
    (progn
      (defun ivy-switch-project ()
        (interactive)
        (ivy-read
         "Switch to project: "
         (if (projectile-project-p)
             (cons (abbreviate-file-name (projectile-project-root))
                   (projectile-relevant-known-projects))
           projectile-known-projects)
         :action #'projectile-switch-project-by-name))

      (ivy-set-actions
       'ivy-switch-project
       '(("v" projectile-vc "vc")
         ("s" projectile-run-shell "shell")
         ("g" projectile-git-grep "grep")
         ("c" projectile-compile-a-project "compile")
         ("t" projectile-test-a-project "test")))

      (setq projectile-completion-system 'ivy)
      (bind-keys :map projectile-command-map ("p" . ivy-switch-project))
      (bind-keys ("C-c p p" . ivy-switch-project)))))

(defun whilp-projectile/post-init-projectile ()
  (spacemacs|use-package-add-hook projectile
    :post-config
    (progn
      (bind-keys :map projectile-command-map
                 ("!" . projectile-run-shell)
                 ("i" . projectile-compile-a-project)
                 ("o" . projectile-test-a-project)
                 ("f" . projectile-find-file-dwim)
                 ("g" . counsel-git-grep))

      (setq projectile-mode-line
            (quote
             (:eval (format " [%s]" (projectile-project-name)))))

      (defun projectile-run-shell (&optional root)
        "Start a shell in a project's ROOT."
        (interactive "P")
        (projectile-with-default-dir (or root (projectile-project-root))
          (let ((eshell-buffer-name (format "*shell %s*" (projectile-project-name))))
            (eshell))))

      (defun projectile-git-grep (&optional root)
        "Git grep in a project."
        (interactive "P")
        (projectile-with-default-dir (or root (proejctile-project-root))
                                     (counsel-git-grep)))

      (defun projectile-test-a-project (&optional root)
        (interactive "P")
        (let* ((cmd (if (consp root) root nil))
               (root (if (consp root) (projectile-project-root) root)))
          (projectile-with-default-dir (or root (projectile-project-root))
                                       (let* ((compilation-buffer-name-function
                                               (lambda (mode) (projectile-prepend-project-name "test"))))
                                         (projectile-test-project cmd)))))

      (defun projectile-compile-a-project (&optional root)
        (interactive "P")
        (let* ((cmd (if (consp root) root nil))
               (root (if (consp root) (projectile-project-root) root)))
          (projectile-with-default-dir (or root (projectile-project-root))
                                       (let* ((compilation-buffer-name-function
                                               (lambda (mode) (projectile-prepend-project-name "compile"))))
                                         (projectile-compile-project cmd))))))))
