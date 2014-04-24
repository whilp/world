(require 'org)
(setq auto-indent-start-org-indent t
      org-startup-indented t
      org-return-follows-link t
      org-src-fontify-natively t
      org-use-tag-inheritance t
      org-todo-keywords '((sequence "TODO" "DONE"))
      org-agenda-skip-scheduled-if-done t
      org-agenda-restore-windows-after-quit t
      org-agenda-search-headline-for-time nil
      org-extend-today-until 6
      org-agenda-start-with-follow-mode t
      org-agenda-file-regexp "\\`[^.].*\\.\\(txt\\|org\\)\\'"
      org-agenda-files '("~/notes/todo.txt")
      org-bookmark-names-plist '()
      org-default-notes-file "~/notes/todo.txt"
      org-clock-mode-line-total 'today
      org-clock-history-length 50)
(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c c") 'org-capture)
(define-key global-map (kbd "C-c C-x C-o") 'org-clock-out)
(define-key global-map (kbd "C-c C-x C-x") 'org-clock-in-last)
(define-key global-map (kbd "C-c C-x C-i") 'org-clock-in)
(add-to-list 'auto-mode-alist '("\\.\\(txt\\|org\\)$" . org-mode))
