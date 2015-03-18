(add-hook 'after-init-hook 'global-company-mode)
(global-company-mode)
(define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
(define-key company-active-map (kbd "<tab>") 'company-complete)
(setq company-echo-delay 0
      company-tooltip-minimum-width 30
      company-idle-delay .7)
