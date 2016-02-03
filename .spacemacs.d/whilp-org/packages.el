;;; packages.el --- whilp-org layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Will Maier <whilp@m.aier.us>
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
;; added to `whilp-org-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `whilp-org/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `whilp-org/pre-init-PACKAGE' and/or
;;   `whilp-org/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst whilp-org-packages
  '(
    notmuch
    org
    )
  "The list of Lisp packages required by the whilp-org layer.

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

(defun whilp-org/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-init
    (progn
      (spacemacs|disable-company org-mode)
      (bind-keys ("C-c C-l" . org-store-link)
                 ("s-a" . org-agenda-list))
      (setq org-startup-indented t
            org-use-speed-commands t
            org-speed-commands-user
            '(("I" org-insert-heading-respect-content)
              ("s" call-interactively 'org-schedule)
              ("k" org-cut-subtree)
              ("w" org-copy-subtree))
            org-extend-today-until 0
            org-directory nil
            org-agenda-files '("~/src/github.banksimple.com/whilp/notes/log.org")
            org-refile-targets '((nil . (:maxlevel . 6)))
            org-agenda-search-headline-for-time nil
            org-agenda-sorting-strategy
            '((agenda todo-state-down priority-down scheduled-up)
              (todo todo-state-up priority-down category-keep)
              (tags priority-down category-keep)
              (search category-keep))
            ;; org-enforce-todo-dependencies t
            ;; org-return-follows-link t
            ;; org-src-fontify-natively t
            ;; org-return-follows-link nil
            ;; org-use-tag-inheritance t
            ;; org-highest-priority ?A
            ;; org-lowest-priority ?E
            ;; org-default-priority ?A
            ;; org-bookmark-names-plist '()
            ;; org-use-fast-todo-selection nil
            ;; org-default-notes-file "~/src/github.banksimple.com/whilp/notes/log.org"
            ;; org-extend-today-until 6
            ;; org-todo-keywords '((sequence "TODO" "|" "DONE" "PUNTED"))
            ;; org-log-done 'note
            ;; org-log-reschedule 'time
            ;; org-log-redeadline 'time
            ;; org-log-into-drawer "LOGBOOK"
            ))))

(defun whilp-org/pre-init-notmuch ()
  (spacemacs|use-package-add-hook notmuch
    :post-config
    (require 'org-notmuch nil 'noerror)))

;;; packages.el ends here
