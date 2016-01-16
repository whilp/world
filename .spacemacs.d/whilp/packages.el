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
;;; - http://irreal.org/blog/?p=4867
;;; - https://github.com/jorgenschaefer/emacs-buttercup/blob/master/README.md

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq whilp-packages
    '(
      ;; package names go here
      (uniquify :location local)
      (time :location local)
      (ns-win :location local)
      easy-kill
      makefile-mode
      (tls :location local)
      (gnutls :location local)
      async
      ace-jump-zap
      comment-dwim-2
      flyspell
      window-numbering
      counsel
      ivy
      swiper
      ace-link
      go-mode
      golint
      flycheck-gometalinter
      exec-path-from-shell
      rich-minority
      markdown-mode
      (compile :location local)
      (files :location local)
      (whitespace :location local)
      (hl-line :location local)
      (simple :location local)
      (epa :location local)
      (prog-mode :location local)
      (text-mode :location local)
      (goto-addr :location local)
      (browse-url :location local)
      browse-at-remote
      flx
      ))

;; List of packages to exclude.
(setq whilp-excluded-packages '(
                                ido
                                org-bullets
                                persp-mode
                                powerline
                                spaceline
                                vi-tilde-fringe
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

(defun whilp/init-golint ())

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

(defun whilp/pre-init-markdown-mode ()
  (spacemacs|use-package-add-hook markdown-mode
    :post-config
    (progn
      (add-to-list 'auto-mode-alist '("\\.m[k]d" . gfm-mode))
      (setq markdown-asymmetric-header t
            markdown-list-indent-width 2))))

(defun whilp/init-browse-at-remote ()
  (bind-keys ("C-c b" . browse-at-remote)))

(defun whilp/pre-init-flx ()
  (spacemacs|use-package-add-hook ivy
    :post-config
    (setq ivy-initial-inputs-alist nil
          ivy-re-builders-alist '((t . ivy--regex-fuzzy)))))

(defun whilp/init-compile ()
  (setq compilation-scroll-output t
        compilation-read-command nil
        compilation-ask-about-save nil
        compilation-auto-jump-to-first-error nil
        compilation-save-buffers-predicate nil))

(defun whilp/init-ace-jump-zap ()
  (bind-keys ("M-z" . ace-jump-zap-up-to-char-dwim)
             ("C-M-z" . ace-jump-zap-up-to-char-dwim)))

(defun whilp/init-browse-url ()
  (defun browse-url-default-macosx-browser (url &optional new-window)
    "Browse URL in the background. (NEW-WINDOW is ignored)."
    (interactive (browse-url-interactive-arg "URL: "))
    (start-process (concat "open -g" url) nil "open" "-g" url)))

(defun whilp/init-async ()
  (setq send-mail-function 'async-smtpmail-send-it
        message-send-mail-function 'async-smtpmail-send-it))

(defun whilp/init-tls ()
  (setq tls-checktrust t
        tls-program
        (list
         (format "gnutls-cli --x509cafile %s -p %%p %%h" ssl-cert-file))))

(defun whilp/init-gnutls ()
  (setq gnutls-verify-error t
        gnutls-trustfiles (list ssl-cert-file)))

(defun whilp/init-unfill ())

(defun whilp/init-makefile-mode ()
  (defun makefile-mode-config ()
    (setq tab-width 8))
  (add-to-list 'makefile-mode-hook #'makefile-mode-config))

(defun whilp/init-easy-kill ()
  (global-set-key [remap kill-ring-save] 'easy-kill)
  (global-set-key [remap mark-sexp] 'easy-mark))

(defun whilp/init-ns-win ()
  (bind-keys ("s-p" . nil)))

(defun whilp/init-time ()
  (setq display-time-default-load-average nil
        display-time-format "%a %Y-%m-%d %H:%M")
  (defun message-time ()
    "Print the current time as a message."
    (interactive)
    (message
     (mapconcat
      #'identity
      (list
       (format-time-string display-time-format)
       (battery-format "%L %B %p%% %t" (battery-pmset)))
      " | ")))
  (bind-keys ("s-SPC" . message-time)))

(defun whilp/pre-init-uniquify ()
  (spacemacs|use-package-add-hook uniquify
    :post-config
    (setq uniquify-buffer-name-style 'forward)))
