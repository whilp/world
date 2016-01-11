;;; packages.el --- whilp Layer packages File for Spacemacs
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

;;; TODO:
;;; - https://github.com/lewang/flx/blob/master/README.md
;;; - http://oremacs.com/2016/01/06/ivy-flx/
;;; - http://irreal.org/blog/?p=4867
;;; - https://github.com/jorgenschaefer/emacs-buttercup/blob/master/README.md

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq whilp-packages
    '(
      ;; package names go here
      comment-dwim-2
      flyspell
      window-numbering
      counsel
      ivy
      swiper
      ace-link
      go-mode
      flycheck-gometalinter
      exec-path-from-shell
      rich-minority
      (files :location local)
      (whitespace :location local)
      (hl-line :location local)
      (simple :location local)
      (epa :location local)
      (prog-mode :location local)
      (text-mode :location local)
      (goto-addr :location local)
      ))

;; List of packages to exclude.
(setq whilp-excluded-packages '(
                                ido
                                org-bullets
                                powerline
                                spaceline
                                vi-tilde-fringe
                                persp-mode
                                ))

;; For each package, define a function whilp/init-<package-name>
;;
;; (defun whilp/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun whilp/pre-init-flyspell ()
  (spacemacs|use-package-add-hook flyspell
    :pre-init
    (setenv "ASPELL_CONF" (format "dict-dir %slib/aspell" nix-link))))

(defun whilp/init-comment-dwim-2 ()
  (bind-keys ("M-;" . comment-dwim-2)))

(defun whilp/pre-init-window-numbering ()
  (spacemacs|use-package-add-hook window-numbering
    :post-config
    (bind-keys ("s-1" . select-window-1)
               ("s-2" . select-window-2)
               ("s-3" . select-window-3)
               ("s-4" . select-window-4))))

(defun whilp/pre-init-counsel ()
  (spacemacs|use-package-add-hook counsel
    :post-config
    (bind-keys ("C-x C-f" . counsel-find-file))
    (setq counsel-find-file-at-point t)))

(defun whilp/pre-init-ivy ()
  (spacemacs|use-package-add-hook ivy
    :post-config
    (bind-keys ("C-x b" . ivy-switch-buffer)
               ("C-x C-b" . ivy-switch-buffer))))

(defun whilp/pre-init-swiper ()
  (spacemacs|use-package-add-hook swiper
    :post-config
    (bind-keys ("C-r" . swiper))))

(defun whilp/pre-init-ace-link ()
  (spacemacs|use-package-add-hook ace-link
    :post-config
    (when (fboundp #'ace-link-addr)
      (bind-keys ("M-o" . ace-link-addr)))))

;; go get -u github.com/golang/lint/golint
;; go get -u golang.org/x/tools/cmd/cover
;; go get -u golang.org/x/tools/cmd/oracle

(defun whilp/pre-init-go-mode ()
  (spacemacs|use-package-add-hook go-mode
    :post-config
    (setq gofmt-command "goimports")))

(defun whilp/init-flycheck-gometalinter ()
  (add-hook 'flycheck-mode-hook #'flycheck-gometalinter-setup))

(defun whilp/pre-init-exec-path-from-shell ()
  (spacemacs|use-package-add-hook exec-path-from-shell
    :pre-init
    (setq exec-path-from-shell-variables '())))

(defun whilp/init-epa ()
  (setq epa-armor t
        epa-file-name-regexp "\\.\\(gpg\\|asc\\)$")
  (epa-file-name-regexp-update)
  (unless (memq epa-file-handler file-name-handler-alist)
    (epa-file-enable)))

(defun whilp/init-simple ()
  (global-visual-line-mode 1))

(defun whilp/init-hl-line ()
  (global-hl-line-mode -1))

(defun whilp/init-prog-mode ())

(defun whilp/init-text-mode ())

(defun whilp/init-goto-addr ()
  (spacemacs|use-package-add-hook text-mode
    :post-config
    (add-hook 'text-mode-hook #'goto-address-mode))
  (spacemacs|use-package-add-hook prog-mode
    :post-config
    (add-hook 'prog-mode-hook #'goto-address-prog-mode)))

(defun whilp/init-rich-minority ()
  (setq rm-whitelist "nothin")
  (when (not rich-minority-mode)
    (rich-minority-mode 1)))

(defun whilp/pre-init-whitespace ()
  (spacemacs|use-package-add-hook whitespace
    :post-config
    (add-hook 'before-save-hook 'whitespace-cleanup)))

(defun whilp/init-files ()
  (setq enable-local-variables nil
        enable-local-eval nil))
