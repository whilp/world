;;; whilp-ui --- My preferred UI settings.

;;; Commentary:

;;; Opinions.

;;; Code:

(require 'time)

(setq redisplay-dont-pause t)

;; no bars, bells.
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode '(nil . -1))
(setq ring-bell-function 'ignore
      visible-bell t)
(winner-mode 1)
(defun toggle-fullscreen ()
  "Toggle full screen."
  (interactive)
  (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

;; modeline.
(setq display-time-format "%a %Y-%m-%d %H:%M"
      display-time-default-load-average nil
      )
(display-time-mode 1)
(line-number-mode 0)
(column-number-mode 0)
(size-indication-mode 0)
(display-battery-mode 0)
(which-function-mode 0)

;; no splash.
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; no prompts.
(setq confirm-nonexistent-file-or-buffer nil)
(fset 'yes-or-no-p 'y-or-n-p)

;; syntax highlighting.
(global-font-lock-mode t)
(transient-mark-mode t)

(provide 'whilp-ui)
;;; whilp-ui ends here
