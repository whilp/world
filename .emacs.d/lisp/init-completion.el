;;; init-completion --- Initialize swiper.

;;; Commentary:

;;; My swiper.

;;; Code:

(require 'use-package)

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x))
  :init
  (progn
    (eval-when-compile
      (require 'projectile))
    (bind-keys :map projectile-command-map
               ("g" . counsel-git-grep))))

(use-package ivy
  :diminish ivy-mode
  :init
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)))

(use-package swiper
  :ensure t
  :bind (("M-i" . swiper)))

(provide 'init-completion)
;;; init-completion ends here
