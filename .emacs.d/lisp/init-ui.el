;;; init-ui --- My preferred UI settings.

;;; Commentary:

;;; Opinions.

;;; Code:

(require 'use-package)

;; TODO
;; (bind-key "C-x C-c" nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package files
  :demand t
  :config
  (setq make-backup-files nil
        auto-save-default nil
        backup-directory-alist `(("." . "~/.saves"))
        backup-directory-alist `((".*" . ,temporary-file-directory))
        auto-save-file-name-transforms `((".*" "~/.saves"))
        confirm-nonexistent-file-or-buffer nil))

;; startup
(setq inhibit-startup-echo-area-message t
      inhibit-startup-message t)

(defun whilp-last-buffer ()
  "Flip to last buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; window
(bind-keys ("s-SPC" . other-window)
           ("s-0" . delete-window)
           ("s-1" . delete-other-windows)
           ("s-2" . split-window-below)
           ("s-3" . split-window-right)
           ("s--" . whilp-last-buffer))
(setq recenter-positions '(top middle bottom))

(setq ring-bell-function 'ignore
      visible-bell t)

(use-package fringe
  :demand t
  :config (fringe-mode '(nil . -1)))

(use-package menu-bar
  :demand t
  :config (menu-bar-mode -1))

(use-package scroll-bar
  :demand t
  :config (scroll-bar-mode -1))

(use-package tool-bar
  :demand t
  :config (tool-bar-mode -1))

(use-package winner
  :demand t
  :config (winner-mode 1))

(use-package battery
  :demand t
  :config (display-battery-mode 0))

(use-package which-func
  :demand t
  :config (which-function-mode 0))

;; no prompts.
(fset 'yes-or-no-p 'y-or-n-p)

;; syntax highlighting.
(use-package font-core
  :demand t
  :config (global-font-lock-mode t))

(use-package color-theme-solarized
  :ensure t
  :demand t
  :bind (("s-`" . whilp-toggle-solarized))
  :config
  (progn
    (require 'frame)
    (load-theme 'solarized t)
    (enable-theme 'solarized)

    (defun whilp-toggle-solarized ()
      "Toggles between solarized light and dark."
      (interactive)
      (let ((mode (if (equal (frame-parameter nil 'background-mode) 'dark) 'light 'dark)))
        (set-frame-parameter nil 'background-mode mode)
        (enable-theme 'solarized)))))

(use-package smart-mode-line
  :ensure t
  :demand t
  :config
  (progn
    (setq sml/mode-width 'right
          sml/theme 'respectful
          sml/use-projectile-p nil
          sml/shorten-directory t
          sml/full-mode-string ""
          sml/shorten-mode-string ""
          sml/name-width '(12 . 18))

    (sml/setup)
    
    (setq-default global-mode-string '("")
                  mode-line-format
                  '(
                    "%e"
                    display-time-string
                    mode-line-front-space
                    mode-line-mule-info
                    mode-line-client
                    mode-line-remote
                    mode-line-frame-identification
                    mode-line-buffer-identification
                    (vc-mode vc-mode)
                    "  " mode-line-modes
                    mode-line-misc-info
                    mode-line-end-spaces))))

(use-package golden-ratio
  :ensure t
  :diminish golden-ratio-mode
  :config (golden-ratio-mode))


(use-package simple
  :diminish visual-line-mode
  :demand t
  :bind (("M-S-Y" . yank-pop-forwards))
  :config
  (progn
    (transient-mark-mode t)
    (column-number-mode 0)
    (line-number-mode 0)
    (size-indication-mode 0)

    (setq kill-read-only-ok nil)

    (defun yank-pop-forwards (arg)
      (interactive "p")
      (yank-pop (- arg)))))

(use-package time
  :demand t
  :config
  (progn
    (setq
     display-time-default-load-average nil
     display-time-format "%a %Y-%m-%d %H:%M")
    ;; display-time-mode appends the time string to global-mode-string
    ;; by default, so we set global-mode-string back to zero after
    ;; calling it.
    (display-time-mode 1)
    (setq global-mode-string '(""))))

(use-package abbrev
  :diminish abbrev-mode)

(use-package midnight
  :demand t
  :config (add-to-list 'clean-buffer-list-kill-never-regexps "^#.*"))

(use-package uniquify
  :demand t
  :config
  (setq uniquify-buffer-name-style 'forward))

(provide 'init-ui)
;;; init-ui ends here
