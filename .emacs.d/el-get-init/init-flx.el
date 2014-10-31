(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.

(setq confirm-nonexistent-file-or-buffer nil)

;; https://gist.github.com/timcharper/493269/raw/72d9063b8aef61a851026f3acb1d27a4b7c17eca/ido-other-window.el
(load-file "~/.emacs.d/ido-other-window.el")
(setq
 ido-create-new-buffer 'always
 ido-enable-flex-matching t
 ido-everywhere t
 ido-enable-flex-matching t
 ido-use-faces nil)
(icomplete-mode t)
(ido-init-completion-maps)
(ffap-bindings)
(setq ffap-require-prefix t)
