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
  (require 'use-package))

(require 'bind-key)
(require 'diminish)

(add-to-list 'load-path (expand-file-name "init" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(defconst whilp-full-name "Will Maier"
  "My name.")

(require 'init-environment)
(require 'init-whilp)

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config (yas-global-mode))

(provide 'init)
;;; init ends here
