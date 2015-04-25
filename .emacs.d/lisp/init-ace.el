;;; init-ace --- Initialize ace.

;;; Commentary:

;;; My ace.

;;; Code:

(require 'use-package)

(use-package ace-jump-mode
  :ensure t
  :defer t
  :bind (("C-c SPC" . ace-jump-mode)))

(use-package ace-jump-buffer
  :ensure t
  :defer t)

(use-package ace-jump-mode
  :ensure t
  :defer t)

(use-package ace-jump-zap
  :ensure t
  :bind (("M-z" . ace-jump-zap-up-to-char-dwim)
         ("C-M-z" . ace-jump-zap-to-char-dwim)))

(provide 'init-ace)
;;; init-ace ends here
