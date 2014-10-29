(autoload 'magit-status' "magit" nil t)
(setq
 magit-git-executable "gh"
 magit-save-some-buffers nil
 magit-status-buffer-switch-function 'switch-to-buffer
 magit-set-upstream-on-push 'dontask
 )
(define-key whilp/bindings-map (kbd "C-c g p") 'magit-push)
(define-key whilp/bindings-map (kbd "C-c g g") 'magit-grep)
(define-key whilp/bindings-map (kbd "C-c g c") 'magit-commit)
