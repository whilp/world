(require 'go-autocomplete)
(require 'auto-complete-config)
(add-hook 'go-mode-hook (lambda ()
                          (go-autocomplete-mode)))
