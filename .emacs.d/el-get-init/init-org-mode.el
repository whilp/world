(require 'org)
(setq auto-indent-start-org-indent t
      org-startup-indented t
      org-use-tag-inheritance t
      org-todo-keywords '((sequence "TODO" "DONE"))
      org-agenda-restore-windows-after-quit t
      org-agenda-search-headline-for-time nil
      org-extend-today-until 6
      org-agenda-start-with-follow-mode t
      org-agenda-file-regexp "\\`[^.].*\\.\\(txt\\|org\\)\\'"
      org-agenda-files '("~/notes/todo.txt")
      org-bookmark-names-plist '()
      org-default-notes-file "~/notes/todo.txt")
(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c c") 'org-capture)
(add-to-list 'auto-mode-alist '("\\.\\(txt\\|org\\)$" . org-mode))
