;;; init-org --- Initialize org-mode

;;; Commentary:

;;; My org.

;;; Code:

(require 'use-package)
(eval-when-compile
  (require 'org))

(bind-keys :prefix-map whilp-org-map
           :prefix "s-o")

(use-package org-plus-contrib
  :demand t
  :ensure t
  :mode ("\\.\\(txt\\|org\\)$" . org-mode)
  :config
  (progn
    (setq org-startup-indented t
          org-enforce-todo-dependencies t
          org-return-follows-link t
          org-src-fontify-natively t
          org-completion-use-ido t
          org-return-follows-link t
          org-use-tag-inheritance t
          org-bookmark-names-plist '()
          org-default-notes-file "~/notes/todo.txt"
          org-extend-today-until 6
          org-todo-keywords '((sequence "TODO" "DONE"))
          org-log-into-drawer "LOGBOOK")))

(use-package org-capture
  :demand t
  :config
  (progn
    (bind-keys :map whilp-org-map
               ("c" . org-capture))
    (setq org-capture-templates
          '(("j" "Journal" entry (file "~/src/github.banksimple.com/whilp/notes/journal.org")
             "* %U %?")
            ("t" "Todo" entry (file "~/src/github.banksimple.com/whilp/notes/plan.org")
             "* TODO %?\nDEADLINE: %^t")))))

(use-package org-context
  :ensure t
  :demand t
  :init (org-context-activate))

(use-package org-agenda
  :demand t
  :config
  (progn
    (bind-keys :map whilp-org-map
               ("a" . org-agenda))
    (setq org-agenda-dim-blocked-tasks t
          org-agenda-file-regexp "\\`[^.].*\\.\\(txt\\|org\\)\\'"
          org-agenda-files '("~/src/github.banksimple.com/whilp/notes/plan.txt")
          org-agenda-follow-mode nil
          org-agenda-repeating-timestamp-show-all nil
          org-agenda-restore-windows-after-quit nil
          org-agenda-search-headline-for-time nil
          org-agenda-skip-deadline-if-done t
          org-agenda-skip-scheduled-if-deadline-is-shown t
          org-agenda-skip-scheduled-if-done t
          org-agenda-start-on-weekday 1
          org-agenda-start-with-follow-mode nil
          org-agenda-tags-todo-honor-ignore-options t
          org-agenda-todo-ignore-deadlines 'past
          org-agenda-todo-ignore-scheduled 'all
          org-agenda-todo-ignore-timestamp 'all
          org-agenda-todo-ignore-with-date t
          org-agenda-window-setup 'current-window)))

(use-package org-clock
  :init
  (bind-keys :map whilp-org-map
             ("o" . org-clock-out)
             ("x" . org-clock-in-last)
             ("i" . org-clock-in))
  :config
  (setq org-clock-history-length 50
        org-clock-mode-line-total 'today))

(provide 'init-org)
;;; init-org ends here
