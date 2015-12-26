;;; init --- Emacs initialization

;;; Commentary:

;;; My personal configuration.
;;; TODO:
;;;  - convert explicit (bind-keys) calls to :bind (after updating use-package)

;;; Code:

(setq load-prefer-newer t)

(require 'package)
(setq package-enable-at-startup nil
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ;; ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize nil)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'bind-key)
(require 'diminish)

(defconst whilp-lisp
  (expand-file-name "lisp" user-emacs-directory)
  "My configurations.")
(add-to-list 'load-path whilp-lisp)

;; TODO
;; http://blog.danielgempesaw.com/post/79353633199/installing-mu4e-with-homebrew-with-emacs-from
;; or https://github.com/edvorg/req-package#el-get
;; (use-package mu4e
;;   :ensure t
;;   :defer t)

(use-package init-shell  :demand t)
(use-package init-rcirc  :demand t)
(use-package init-ui     :demand t)
(use-package init-org    :demand t)
(use-package init-dev    :demand t)
(use-package init-completion :demand t)
(use-package init-git    :demand t)
(use-package init-ace    :demand t)
(use-package init-text   :demand t)
(use-package init-emacs  :demand t)

(provide 'init)
;;; init ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(package-selected-packages
   (quote
    (company-go pkg paredit-everywhere counsel ivy pandoc-mode browse-at-remote package-build shut-up epl git commander f dash s))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
