(global-set-key [remap eval-expression] 'pp-eval-expression)
(global-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(autoload 'magit-status' "magit" nil t)
