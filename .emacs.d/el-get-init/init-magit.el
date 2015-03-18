(autoload 'magit-status' "magit" nil t)
(setq
 magit-git-executable "gh"
 magit-save-some-buffers nil
 magit-status-buffer-switch-function 'switch-to-buffer
 magit-set-upstream-on-push 'dontask
 magit-completing-read-function 'magit-ido-completing-read
 magit-use-overlays nil
 )

(define-key whilp/bindings-map (kbd "s-g g") 'magit-status)
(define-key whilp/bindings-map (kbd "s-g u") 'magit-push)
(define-key whilp/bindings-map (kbd "s-g p") 'magit-grep)
(define-key whilp/bindings-map (kbd "s-g c") 'magit-commit)
