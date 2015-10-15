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

(use-package swiper
  :ensure t
  :bind (("M-i" . swiper))
  :diminish ivy-mode
  :config
  (progn
    (eval-when-compile
      (require 'magit)
      (require 'projectile))

    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t
          magit-completing-read-function 'ivy-completing-read
          projectile-completion-system 'ivy)))

(provide 'init-completion)
;;; init-completion ends here
