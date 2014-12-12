(require 'org)
(setq auto-indent-start-org-indent t
      org-startup-indented t
      org-enforce-todo-dependencies t
      org-return-follows-link t
      org-src-fontify-natively t
      org-completion-use-ido t
      org-return-follows-link t
      org-use-tag-inheritance t
      org-agenda-start-on-weekday 1
      org-agenda-dim-blocked-tasks t
      org-todo-keywords '((sequence "TODO" "DONE"))
      org-agenda-skip-scheduled-if-done t
      org-agenda-restore-windows-after-quit nil
      org-agenda-window-setup 'current-window
      org-agenda-search-headline-for-time nil
      org-extend-today-until 6
      org-agenda-start-with-follow-mode nil
      org-agenda-todo-ignore-deadlines 'past
      org-agenda-todo-ignore-with-date t
      org-agenda-skip-scheduled-if-deadline-is-shown t
      org-agenda-todo-ignore-timestamp 'all
      org-agenda-todo-ignore-scheduled 'all
      org-agenda-skip-deadline-if-done t
      org-agenda-tags-todo-honor-ignore-options t
      org-agenda-follow-mode nil
      org-agenda-file-regexp "\\`[^.].*\\.\\(txt\\|org\\)\\'"
      org-agenda-files '("~/src/github.banksimple.com/whilp/plan/plan.txt")
      org-bookmark-names-plist '()
      org-default-notes-file "~/notes/todo.txt"
      org-clock-mode-line-total 'today
      org-clock-history-length 50
      org-agenda-repeating-timestamp-show-all nil)

(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c c") 'org-capture)
(define-key global-map (kbd "C-c C-x C-o") 'org-clock-out)
(define-key global-map (kbd "C-c C-x C-x") 'org-clock-in-last)
(define-key global-map (kbd "C-c C-x C-i") 'org-clock-in)
(add-to-list 'auto-mode-alist '("\\.\\(txt\\|org\\)$" . org-mode))
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/notes/todo.txt" "Tasks")
         "* TODO %? [/]")))
