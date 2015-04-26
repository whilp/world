;;; init --- Emacs initialization

;;; Commentary:

;;; My personal configuration.
;;; TODO:
;;;  - convert explicit (bind-keys) calls to :bind (after updating use-package)

;;; Code:

(setq load-prefer-newer t)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
;;             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'auth-source)
  (require 'cl)
  (require 'color-theme)
  (require 'url)
  (require 'use-package))

(require 'bind-key)
(require 'diminish)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; TODO
;; http://blog.danielgempesaw.com/post/79353633199/installing-mu4e-with-homebrew-with-emacs-from
;; (use-package mu4e
;;   :ensure t
;;   :defer t)

(use-package init-shell  :demand t)
(use-package init-rcirc  :demand t)
(use-package init-ui     :demand t)
(use-package init-el-get :demand t)
(use-package init-dev    :demand t)
(use-package init-org    :demand t)
(use-package init-helm   :demand t)
(use-package init-git    :demand t)
(use-package init-ace    :demand t)
(use-package init-text   :demand t)
(use-package init-emacs  :demand t)

(provide 'init)
;;; init ends here
