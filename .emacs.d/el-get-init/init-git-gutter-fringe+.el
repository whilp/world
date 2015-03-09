(require 'git-gutter-fringe+)
(global-git-gutter+-mode t)
(git-gutter-fr+-minimal)

(define-key git-gutter+-mode-map (kbd "C-x n") 'git-gutter+-next-hunk)
(define-key git-gutter+-mode-map (kbd "C-x p") 'git-gutter+-previous-hunk)

;;; Act on hunks
(define-key git-gutter+-mode-map (kbd "C-x v =") 'git-gutter+-show-hunk)
(define-key git-gutter+-mode-map (kbd "C-x r") 'git-gutter+-revert-hunks)

;; Stage hunk at point.
;; If region is active, stage all hunk lines within the region.
(define-key git-gutter+-mode-map (kbd "C-x t") 'git-gutter+-stage-hunks)
(define-key git-gutter+-mode-map (kbd "C-x c") 'git-gutter+-commit)
(define-key git-gutter+-mode-map (kbd "C-x C") 'git-gutter+-stage-and-commit)
(define-key git-gutter+-mode-map (kbd "C-x C-y") 'git-gutter+-stage-and-commit-whole-buffer)
(define-key git-gutter+-mode-map (kbd "C-x U") 'git-gutter+-unstage-whole-buffer)
