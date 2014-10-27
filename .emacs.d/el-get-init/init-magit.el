(autoload 'magit-status' "magit" nil t)
(setq
 magit-git-executable "gh"
 magit-save-some-buffers nil
 magit-status-buffer-switch-function 'switch-to-buffer
 magit-set-upstream-on-push 'dontask
 )
(define-key whilp/bindings-map (kbd "C-x C-g") 'magit-status)
;; (define-key whilp/bindings-map (kbd "C-c C-p") 'magit-push)
(define-key whilp/bindings-map (kbd "C-x C-c") 'magit-commit)
