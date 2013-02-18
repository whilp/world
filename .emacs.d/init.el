(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn t)

(global-set-key [remap eval-expression] 'pp-eval-expression)
(global-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(autoload 'magit-status' "magit" nil t)
