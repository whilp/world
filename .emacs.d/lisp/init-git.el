;;; init-git --- Initialize git stuffs.

;;; Commentary:

;;; My git.

;;; Code:

(require 'use-package)
(require 'auth-source)
(require 'url-parse)
(require 'hydra)

(eval-when-compile
  (require 'git-gutter-fringe+)
  (require 'magit))

(bind-key
 "s-g"
 (defhydra hydra-git () "git"
   ("g" magit-status :exit t)
   ("c" magit-commit :exit t)
   ("u" magit-push)
   ("n" git-gutter+-next-hunk nil)
   ("p" git-gutter+-previous-hunk nil)
   ("=" git-gutter+-show-hunk nil)
   ("r" git-gutter+-revert-hunks "revert")
   ("s" git-gutter+-stage-hunks "stage")
   ("C" git-gutter+-stage-and-commit "commit" :exit t)
   ("G" git-gutter+-stage-and-commit-whole-buffer "commit buffer" :exit t)
   ("U" git-gutter+-unstage-whole-buffer "unstage buffer")))

(use-package git-gutter-fringe+
  :ensure t
  :config
  (progn
    (global-git-gutter+-mode t)
    (git-gutter-fr+-minimal)
    (setq git-gutter+-lighter "")))

(use-package gh
  :ensure t
  :demand t
  :config
  (progn
    
    (defun* whilp-gh-profile (url user)
      (let* (
             (urlobj (url-generic-parse-url url))
             (host (url-host urlobj))
             (auth-info
              (car
               (auth-source-search
                :max 1
                :host host
                :user user
                :port 443
                :create nil)))
             (token (funcall (plist-get auth-info :secret))))
        (list
         :url url
         :username user
         :token token
         :remote-regexp (gh-profile-remote-regexp host))))

    (setq
     gh-profile-default-profile "bh"
     gh-profile-current-profile nil
     gh-profile-alist
     (list
      (cons "bh" (whilp-gh-profile "https://github.banksimple.com/api/v3" "whilp"))
      (cons "gh" (whilp-gh-profile "https://api.github.com" "whilp"))))))

(use-package gist
  :ensure t
  :defer t)

(use-package git-timemachine
  :ensure t
  :defer t)

(use-package git-link
  :ensure t
  :defer t)

(use-package magit
  :ensure t
  :diminish magit-auto-revert-mode
  :config
  (progn
    (autoload 'magit-status' "magit" nil t)
    (setq magit-git-executable "gh"
          magit-save-some-buffers nil
          magit-status-buffer-switch-function 'switch-to-buffer
          magit-set-upstream-on-push 'dontask
          magit-completing-read-function 'magit-builtin-completing-read
          magit-last-seen-setup-instructions "1.4.0"
          magit-use-overlays nil)))

(provide 'init-git)
;;; init-git ends here
