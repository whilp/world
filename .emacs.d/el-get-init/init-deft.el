(setq
 deft-extension "txt"
 deft-directory "~/src/github.banksimple.com/whilp/notes"
 deft-text-mode 'org-mode
 deft-use-filename-as-title t)
(define-key whilp/bindings-map (kbd "s-d") 'deft)
