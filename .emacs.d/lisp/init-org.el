;;; init-org --- Initialize org-mode

;;; Commentary:

;;; My org.

;;; Code:

(require 'use-package)
(require 'hydra)

(use-package-ensure-elpa 'org-plus-contrib)

(use-package org
  :demand t
  :ensure t
  :mode ("\\.\\(txt\\|org\\)$" . org-mode)
  :config
  (progn
    (bind-key
     "C-c o"
     (defhydra hyrda-org-mode () "org-mode"
       ("l" org-store-link "store link" :exit t)
       ("L" org-insert-link "insert link" :exit t)
       ("a" (lambda () (interactive) (org-agenda nil "n")) "agenda" :exit t)
       ("c" org-capture "capture" :exit t)
       ("j" (lambda () (interactive) (org-capture nil "j")) "journal" :exit t)))
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
    (defun whilp-insert-link ()
      (interactive)
      (let* ((types (mapcar (lambda (e) `(,e nil)) org-link-types))
             (link-stored
              (helm-build-sync-source "org-link-stored" :candidates 'org-stored-links))
             ;; :candidates (cl-mapcar 'org-link-prettify org-stored-links)))
             (link-types
              (helm-build-sync-source "org-link-types" :candidates 'types))
             (fallback
              (helm-build-dummy-source "org-link-new"))
             (link
              (helm :sources '(link-stored link-types fallback)
                    :buffer "*helm-org-links*")))
        (org-insert-link nil (car link) (cdr link))))

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
          org-use-fast-todo-selection nil
          org-default-notes-file "~/src/github.banksimple.com/whilp/notes/log.org"
          org-extend-today-until 6
          org-todo-keywords '((sequence "TODO" "|" "DONE" "PUNTED"))
          org-log-done 'note
          org-log-reschedule 'time
          org-log-redeadline 'time
          org-log-into-drawer "LOGBOOK")))

(use-package org-capture
  :demand t
  :functions (org-capture)
  :config
  (progn
    (defun org-deal-with-it (orig &rest args)
      "Run ORIG with ARGS and some hacks that make org Deal With It."
      (interactive)
      (cl-letf ((org-extend-today-until 0)
                (completing-read-function #'completing-read-default)
                ((symbol-function 'delete-other-windows) #'ignore)
                ((symbol-function 'org-switch-to-buffer-other-window) #'switch-to-buffer-other-window)
                ((symbol-function 'org-pop-to-buffer-same-window) #'pop-to-buffer-same-window))
        (apply orig args)))
    (advice-add 'org-capture :around #'org-deal-with-it)
    (advice-add 'org-add-log-note :around #'org-deal-with-it)
    (advice-add 'org-agenda :around #'org-deal-with-it)
    ;; (advice-remove 'org-insert-link #'org-deal-with-it)
    (setq org-capture-templates
          '(("j" "Journal" entry (file+datetree "~/src/github.banksimple.com/whilp/notes/log.org")
             "* %^{Title} %^g\n:PROPERTIES:\n:FILED: %U\n:LINK: %a\n:END:\n%?")))))

(use-package org-context
  :ensure t
  :demand t
  :init (org-context-activate))

(use-package org-babel
  :demand t
  :init
  (progn
    (setq org-confirm-babel-evaluate nil)
    (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((dot . t)
       (gnuplot . t)
       (emacs-lisp . t)))))

(use-package ox
  :config
  (progn
    (setq org-export-with-toc nil)
    (use-package ox-html
      :config
      (setq org-html-doctype "html5"))))

(use-package org-src
  :demand t
  :init
  (progn
    (setq org-src-fontify-natively t)
    (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))))

(use-package org-mime
  :demand t)

(use-package ob-ipython
  :ensure t)

(use-package org-agenda
  :demand t
  :config
  (progn
    (setq org-agenda-dim-blocked-tasks nil
          org-agenda-file-regexp "\\`[^.].*\\.\\(txt\\|org\\)\\'"
          org-agenda-files '( "~/src/github.banksimple.com/whilp/notes/log.org")
          org-agenda-follow-mode nil
          org-agenda-repeating-timestamp-show-all nil
          org-agenda-show-inherited-tags 'always
          org-agenda-inhibit-startup t
          org-agenda-sorting-strategy '(priority-down deadline-down scheduled-down)
          org-agenda-ignore-properties '(effort appt category)
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
          org-agenda-window-setup 'current-window
          org-agenda-remove-tags t
          org-agenda-prefix-format '((agenda . "%?-12t% s")
                                     (timeline . "  % s")
                                     (todo . " %i %-12:c")
                                     (tags . " %i %-12:c")
                                     (search . " %i %-12:c")))))

(use-package org-clock
  :config
  (setq org-clock-history-length 50
        org-clock-mode-line-total 'today))

(provide 'init-org)
;;; init-org ends here
