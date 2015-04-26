;;; init-text --- Initialize text helpers.

;;; Commentary:

;;; My text stuff.

;;; Code:

(require 'use-package)

(use-package markdown-mode
  :ensure t
  :defer t
  :config (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(use-package auto-indent-mode
  :ensure t
  :demand t
  :config
  (setq auto-indent-start-org-indent t))

(use-package expand-region
  :ensure t
  :defer t
  :bind (("C-=" . er/expand-region)))

(use-package yasnippet
  :ensure t
  :defer 60
  :diminish yas-minor-mode
  :config (yas-global-mode))

(use-package epa-file
  :config
  (progn
    (setq epa-file-name-regexp "\\.\\(gpg\\|asc\\)$"
          epa-armor t)
    (epa-file-name-regexp-update)
    (epa-file-enable)))

(use-package flyspell
  :diminish flyspell-mode)
  :demand t
  :config
  (progn
    (add-hook 'text-mode-hook 'flyspell-mode)
    (add-hook 'prog-mode-hook 'flyspell-prog-mode))

(bind-keys ("<backtab>" . indent-relative))
(setq-default indent-tabs-mode nil
              tab-width 2)

(use-package dabbrev
  :demand t
  :bind ("C-_" . dabbrev-expand)
  :config (setq abbrev-mode t))

(provide 'init-text)
;;; init-text ends here
