;;; init-org --- Initialize org-mode

;;; Commentary:

;;; My org.

;;; Code:

(require 'use-package)

(bind-keys :prefix-map whilp-org-map
           :prefix "s-o")

(use-package-ensure-elpa 'org-plus-contrib)

(use-package org
  :demand t
  :ensure t
  :mode ("\\.\\(txt\\|org\\)$" . org-mode)
  :config
  (progn
    (bind-keys :map org-mode-map
               ("C-M-e" . org-forward-heading-same-level)
               ("C-M-a" . org-backward-heading-same-level)
               ("C-M-f" . outline-next-visible-heading)
               ("C-M-b" . outline-previous-visible-heading)
               ("C-)" . org-do-demote)
               ("C-(" . org-do-promote)
               ("C-M-)" . org-demote-subtree)
               ("C-M-(" . org-promote-subtree)
               ("C-M-u" . org-move-subtree-up)
               ("C-M-d" . org-move-subtree-down))
    ;; TODO: make a hydra that does these as well? (Both above and below)
    (setq org-use-speed-commands t
          org-speed-commands-user '(("I" org-insert-heading-respect-content)
                                    ("s" call-interactively 'org-schedule)
                                    ("k" org-cut-subtree)
                                    ("w" org-copy-subtree))
          org-startup-indented t
          org-enforce-todo-dependencies t
          org-return-follows-link t
          org-src-fontify-natively t
          org-completion-use-ido t
          org-return-follows-link t
          org-use-tag-inheritance t
          org-highest-priority ?A
          org-lowest-priority ?E
          org-default-priority ?A
          org-bookmark-names-plist '()
          org-default-notes-file "~/src/github.banksimple.com/whilp/notes/log.org"
          org-extend-today-until 6
          org-todo-keywords '((sequence "TODO" "DONE"))
          org-log-done 'time
          org-log-reschedule 'time
          org-log-redeadline 'time
          org-log-into-drawer "LOGBOOK")))

(use-package org-capture
  :demand t
  :functions (whilp-capture)
  :config
  (progn
    (defun whilp-capture (&optional GOTO KEYS)
      "Call org-capture with extend-today-until as 0."
      (interactive)
      (let ((org-extend-today-until 0))
        (org-capture GOTO KEYS)))
    (defun org-capture-todo ()
      "Capture a todo."
      (interactive)
      (whilp-capture nil "t"))
    (defun org-capture-journal ()
      "Capture a journal entry."
      (interactive)
      (whilp-capture nil "j"))
    (bind-keys :map whilp-org-map
               ("c" . org-capture)
               ("t" . org-capture-todo)
               ("j" . org-capture-journal))
    (setq org-capture-templates
          '(("j" "Journal" entry (file+datetree "~/src/github.banksimple.com/whilp/notes/log.org")
             "* %^{Title} %^g\n- filed %U\n%?")))))

(use-package org-context
  :ensure t
  :demand t
  :init (org-context-activate))

(use-package org-babel
  :demand t
  :init
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)
     (emacs-lisp . t))))

(use-package org-src
  :demand t
  :init
  (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot)))

(use-package ob-ipython
  :ensure t)

(use-package org-agenda
  :demand t
  :config
  (progn
    (bind-keys :map whilp-org-map
               ("a" . org-agenda))
    (setq org-agenda-dim-blocked-tasks nil
          org-agenda-file-regexp "\\`[^.].*\\.\\(txt\\|org\\)\\'"
          org-agenda-files '( "~/src/github.banksimple.com/whilp/notes/log.org")
          org-agenda-follow-mode nil
          org-agenda-repeating-timestamp-show-all nil
          org-agenda-show-inherited-tags 'always
          org-agenda-inhibit-startup t
          org-agenda-sorting-strategy '(priority-down deadline-down scheduled-down)
          org-agenda-ignore-drawer-properties '(effort appt category)
          org-agenda-restore-windows-after-quit nil
          org-agenda-search-headline-for-time nil
          org-agenda-skip-deadline-if-done t
          org-agenda-skip-scheduled-if-deadline-is-shown t
          org-agenda-skip-scheduled-if-done t
          org-agenda-start-on-weekday 1
          org-agenda-start-with-follow-mode nil
          org-scheduled-delay-days 1
          org-agenda-tags-todo-honor-ignore-options t
          org-agenda-todo-ignore-deadlines 'past
          org-agenda-todo-ignore-scheduled 'future
          org-agenda-todo-ignore-timestamp 'all
          org-agenda-sticky t
          org-agenda-todo-ignore-with-date t
          org-agenda-tags-todo-honor-ignore-options t
          org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled
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
