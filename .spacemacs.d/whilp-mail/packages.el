;;; packages.el --- whilp-mail layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Will Maier <whilp@radioactivity.local>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `whilp-mail-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `whilp-mail/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `whilp-mail/pre-init-PACKAGE' and/or
;;   `whilp-mail/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst whilp-mail-packages
  '(
    (smtpmail :location built-in)
    (starttls :location built-in)
    (message :location built-in)
    async
    notmuch
    bbdb
    gnus-alias
    nm
    (mml-sec :location built-in)
    (mml2015 :location built-in)
    )
  "The list of Lisp packages required by the whilp-mail layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun whilp-mail/init-smtpmail ()
  (use-package smtpmail
    :config
    (setq smtpmail-queue-mail nil
          smtpmail-queue-dir "~/mail/queue/cur"
          smtpmail-debug-info t
          smtpmail-stream-type 'ssl
          smtpmail-starttls-credentials
          '(("smtp.gmail.com" 465 "will@simple.com" nil)
            ("smtp.gmail.com" 465 "wcmaier@m.aier.us" nil))
          smtpmail-default-smtp-server "smtp.gmail.com"
          smtpmail-smtp-server "smtp.gmail.com"
          smtpmail-smtp-service 465)))

(defun whilp-mail/init-message ()
  (use-package message
    :config
    (setq message-kill-buffer-on-exit t
          message-signature nil
          mail-host-address "m.aier.us")))

(defun whilp-mail/init-starttls ()
  (use-package starttls
    :init
    (setq starttls-use-gnutls t)))

(defun whilp-mail/pre-init-async ()
  (use-package smtpmail-async
    :config
    (setq send-mail-function 'async-smtpmail-send-it
          message-send-mail-function 'async-smtpmail-send-it)))

(defun whilp-mail/init-notmuch ()
  (use-package notmuch
    :bind ("s-m" . notmuch)
    :defer t
    :config
    (progn
      (defun compose-message ()
        ;; (auto-fill-mode t)
        (setq fill-column 72)
        (auto-fill-mode -1)
        (visual-line-mode 1)
        ;; (visual-fill-column-mode 1)
        )

      (define-key notmuch-search-mode-map "e"
        (lambda ()
          "delete message"
          (interactive)
          (notmuch-search-tag (list "+deleted" "-inbox"))
          (notmuch-search-next-thread)))

      (define-key notmuch-show-mode-map "e"
        (lambda ()
          "delete message"
          (interactive)
          (notmuch-show-tag (list "+deleted" "-inbox"))))

      (add-to-list 'notmuch-message-mode-hook #'compose-message)
      (setq notmuch-search-oldest-first nil
            notmuch-crypto-process-mime t
            notmuch-show-indent-messages-width 4
            notmuch-saved-searches
            '((:name "inbox" :query "tag:inbox" :key "i" :sort-order newest-first)
              (:name "maier" :query "tag:maier" :key "m" :sort-order newest-first)
              (:name "simple" :query "tag:simple" :key "s" :sort-order newest-first)))
      (with-eval-after-load 'swiper
        (add-to-list 'swiper-font-lock-exclude 'notmuch-show-mode)
        (add-to-list 'swiper-font-lock-exclude 'notmuch-search-mode)))))

(defun whilp-mail/init-bbdb ()
  (use-package bbdb
    :defer t
    :config
    (bbdb-initialize 'message)))

(defun whilp-mail/init-gnus-alias ()
  (use-package gnus-alias
    :defer t
    :config
    (progn
      (defun gnus-alias-configure-identity ()
        ;; gnus-alias-current-identity
        ;; configure smtpmail
        (let ((from (cdr
                     (gnus-extract-address-components
                      (message-fetch-field "From"))))))
        (setq mml2015-signers (list from)))

      (setq gnus-alias-default-identity "simple"
            gnus-alias-identity-rules
            '(("simple" ("any" "@simple\\.com" both) "simple")
              ("maier" ("any" ".*" both) "maier"))
            gnus-alias-identity-alist
            '(("maier" "" "Will Maier <wcmaier@gmail.com>")
              ("simple" "" "Will Maier <whilp@simple.com>")))

      (add-hook 'message-send-hook #'gnus-alias-configure-identity)
      (add-hook 'message-setup-hook #'gnus-alias-determine-identity))))

(defun whilp-mail/init-mml-sec ()
  (use-package mml-sec
    :defer t
    :config
    (with-eval-after-load 'message
      (add-hook 'message-setup-hook 'mml-secure-message-sign-pgpmime))))

(defun whilp-mail/init-nm ()
  (use-package nm
    :defer t
    :commands (nm)
    :config
    (with-eval-after-load 'swiper
      (add-to-list 'swiper-font-lock-exclude 'nm-mode)))
  (use-package nm-company
    :defer t))

;;; packages.el ends here
