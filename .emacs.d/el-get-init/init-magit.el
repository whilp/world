(autoload 'magit-status' "magit" nil t)
(setq
 magit-git-executable "git"
 magit-save-some-buffers nil
 magit-status-buffer-switch-function 'switch-to-buffer
 )
(define-key whilp/bindings-map (kbd "C-x C-g") 'magit-status)
