;;; whilp-buffers --- Buffer management.

;;; Commentary:

;;; Buffers.

;;; Code:

(require 'ibuffer)
(require 'ibuf-ext)
(require 'midnight)
(require 'uniquify)

(add-to-list 'clean-buffer-list-kill-never-regexps "^#.*")

(setq uniquify-buffer-name-style 'forward
      ibuffer-expert t
      ibuffer-show-empty-filter-groups nil
      ibuffer-saved-filter-groups
      (quote
       '("default"
         ("rcirc" (mode . rcirc-mode)))))

(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-auto-mode t)
             (ibuffer-switch-to-saved-filter-groups "default")))

(defun get-buffers-matching-mode (mode)
  "Return a list of buffers where their `major-mode` is equal to MODE."
  (let ((buffer-mode-matches '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (if (eq mode major-mode)
            (add-to-list 'buffer-mode-matches buf))))
    buffer-mode-matches))

(defun multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur
   (get-buffers-matching-mode major-mode)
   (car (occur-read-primary-args))))

(provide 'whilp-buffers)
;;; whilp-buffers ends here
