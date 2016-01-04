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
        helm-projectile
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

(defun whilp-projectile/pre-init-helm-projectile ()
  (spacemacs|use-package-add-hook helm-projectile
    :post-config
    (helm-projectile-define-key helm-projectile-find-file-map
      (kbd "C-s") #'spacemacs/helm-project-smart-do-search)))
    ;; (progn

      ;; (helm-projectile-define-key helm-projectile-projects-map
      ;;   (kbd "C-s") #')


(defun whilp-projectile/post-init-projectile ()
  (spacemacs|use-package-add-hook projectile
    :post-config
    (bind-keys :map projectile-command-map
               ("!" . projectile-run-shell)
               ("i" . projectile-compile-project)
               ("o" . projectile-test-project)
               ("g" . spacemacs/helm-project-smart-do-search)
               ("p" . helm-projectile-switch-project)))

  (advice-add 'projectile-compile-project
              :around #'with-compile-project)
  (advice-add 'projectile-test-project
              :around #'with-test-project)
  (setq projectile-completion-system 'helm
        projectile-mode-line
        (quote
         (:eval (format " [%s]" (projectile-project-name)))))

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
    (with-compilation-buffer-name-function-for "test-project" orig args)))
