(require 'frame)
(load-theme 'solarized t)

(defun my-toggle-solarized ()
  "Toggles between solarized light and dark"
  (interactive)
  (let ((mode (if (equal (frame-parameter nil 'background-mode) 'dark) 'light 'dark)))
    (set-frame-parameter nil 'background-mode mode)
    (enable-theme 'solarized)
    (enable-theme 'solarized)))

(define-key whilp/bindings-map (kbd "s-`") 'my-toggle-solarized)
