;;; init-ace --- Initialize ace.

;;; Commentary:

;;; My ace.

;;; Code:

(require 'use-package)

(eval-when-compile
  (require 'avy-jump)
  (require 'org))

(use-package avy
  :ensure t
  :bind ("M-`" . avy-goto-word-1)
  :config
  (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        avy-style 'at))

(use-package ace-window
  :ensure t
  :bind (("M-SPC" . ace-window)
         ("s-SPC" . ace-window))
  :config
  (setq aw-keys avy-keys))

(use-package ace-link
  :ensure t
  :init
  (progn
    (bind-keys :map org-mode-map
               ("M-o" . ace-link-org))
    (ace-link-setup-default)))

(use-package ace-jump-helm-line
  :ensure t
  :init
  (with-eval-after-load 'helm
    (bind-keys :map helm-map
               ("C-'" . ace-jump-helm-line))))

(use-package ace-jump-zap
  :ensure t
  :bind (("M-z" . ace-jump-zap-up-to-char-dwim)
         ("C-M-z" . ace-jump-zap-to-char-dwim)))

(provide 'init-ace)
;;; init-ace ends here
