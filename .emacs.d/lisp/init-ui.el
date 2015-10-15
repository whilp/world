;;; init-ui --- My preferred UI settings.

;;; Commentary:

;;; Opinions.

;;; Code:

(eval-when-compile
  (require 'color-theme)
  (require 'use-package)
  (require 'browse-url)
  (require 'frame)
  (require 'midnight)
  (require 'ediff))

(bind-keys ("C-x C-c" . nil)
           ("s-p" . nil))

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

(use-package easy-kill
  :ensure t
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill))

(use-package files
  :demand t
  :config
  (setq make-backup-files nil
        auto-save-default nil
        backup-directory-alist `(("." . "~/.saves"))
        backup-directory-alist `((".*" . ,temporary-file-directory))
        auto-save-file-name-transforms `((".*" "~/.saves"))
        ;; brew install coreutils
        insert-directory-program (executable-find "gls")
        confirm-nonexistent-file-or-buffer nil))

(use-package fringe
  :demand t
  :config (fringe-mode '(4 . 0)))

(use-package frame
  :bind ("M-`" . other-frame)
  :config
  (setq default-frame-alist '((fullscreen . fullscreen)
                              (vertical-scroll-bars)
                              (right-fringe . -1)
                              (left-fringe . 4))))

(setq inhibit-startup-echo-area-message t
      inhibit-startup-message t)

(defun whilp-last-buffer ()
  "Flip to last buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; window
(bind-keys ("s-n" . scroll-other-window)
           ("s-p" . scroll-other-window-down))
(setq recenter-positions '(top middle bottom)
      scroll-preserve-screen-position 'always)

(setq ring-bell-function 'ignore
      visible-bell t)

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
  :config
  (progn
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
    (require 'smart-mode-line)
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

    (setq kill-read-only-ok nil
          blink-matching-paren nil
          set-mark-command-repeat-pop t)

    (defun yank-pop-forwards (arg)
      (interactive "p")
      (yank-pop (- arg)))))

(use-package time
  :demand t
  :bind ("s-SPC" . message-time)
  :config
  (progn
    (defun message-time ()
      "Print the current time as a message."
      (interactive)
      (message "%s | %s"
               (format-time-string display-time-format)
               (battery-format "%L %B %p%% %t" (battery-pmset))))
    (setq
     display-time-default-load-average nil
     display-time-format "%a %Y-%m-%d %H:%M")
    ;; display-time-mode appends the time string to global-mode-string
    ;; by default, so we set global-mode-string back to zero after
    ;; calling it.
    (display-time-mode -1)
    (setq global-mode-string '(""))))

(use-package abbrev
  :diminish abbrev-mode)

(use-package midnight
  :demand t
  :config
  (progn
    (setq clean-buffer-list-delay-general 1)
    (add-to-list 'clean-buffer-list-kill-never-regexps "^#.*")))

(use-package uniquify
  :demand t
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package desktop
  :demand t
  :config (desktop-save-mode 1))

(use-package savehist
  :demand t
  :config
  (progn
    (setq savehist-file "~/.emacs.d/savehist"
          savehist-additional-variables
          (mapcar 'make-symbol
                  (append search-ring
                          regexp-search-ring)))
    (savehist-mode 1)))

(use-package minibuffer
  :demand t
  :config
  (setq completion-cycle-threshold 10))

;; I'm an adult.
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(use-package hl-line
  :demand t
  :config (global-hl-line-mode t))

(global-visual-line-mode 1)

(use-package browse-url
  :demand t
  :config
  (defun browse-url-default-macosx-browser (url &optional new-window)
    "Browse URL in the background. (NEW-WINDOW is ignored)."
    (interactive (browse-url-interactive-arg "URL: "))
    (start-process (concat "open -g" url) nil "open" "-g" url)))

(provide 'init-ui)
;;; init-ui ends here
