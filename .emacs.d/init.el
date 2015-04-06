;;; init --- Emacs initialization

;;; Commentary:
;;; User configuration

;;; Code:

(setq load-prefer-newer t)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'cl)
  (require 'use-package))

(require 'bind-key)
(require 'diminish)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'whilp-const)
(require 'init-environment)
(require 'whilp-ui)
(require 'init-whilp)

(use-package whilp-buffers
  :bind (("C-x C-b" . switch-to-buffer)
         ("s-b" . ibuffer)))

(use-package yasnippet
  :ensure t
  :defer t
  :diminish yas-minor-mode
  :config (yas-global-mode))

(use-package flycheck
  :ensure t
  :defer t
  :config
  (progn
    (setq-default flycheck-emacs-lisp-load-path load-path)
    (global-flycheck-mode)))

(use-package epa-file
  :config
  (progn
    (setq epa-file-name-regexp "\\.\\(gpg\\|asc\\)$"
          epa-armor t)
    (epa-file-name-regexp-update)
    (epa-file-enable)))

(provide 'init)
;;; init ends here
