;;; init-git --- Initialize git stuffs.

;;; Commentary:

;;; My git.

;;; Code:

(require 'use-package)
(require 'auth-source)
(require 'url-parse)

(eval-when-compile
  (require 'cl)
  (require 'magit))

(use-package gh
  :ensure t
  :demand t
  :config
)

(use-package gist
  :ensure t
  :defer t)

(use-package magit
  :ensure t
  :bind ("C-c g" . magit-dispatch-popup)
  :config
  (progn
    (autoload 'magit-status' "magit" nil t)
    (add-hook 'git-commit-mode-hook 'turn-off-auto-fill)

    (magit-define-popup-action
      'magit-dispatch-popup ?. "Status" 'magit-status)
    (magit-define-popup-action
      'magit-dispatch-popup ?H "Github" 'whilp-github-popup)
    (magit-define-popup whilp-github-popup
      "Popup console for Github interaction."
      :actions  '((?p "Pull request" whilp-pull-request)
                  (?f "Fork" whilp-fork))
      :default-action 'whilp-pull-request)

    (defun whilp-pull-request ()
      "Open a Github pull request.

Run `hub pull-request' asynchronously; see
`whilp-pull-request-sentinel' for the interesting bits."
      (interactive)
      (set-process-sentinel
       (magit-run-git-with-editor "pull-request")
       'whilp-pull-request-sentinel))
    (defun whilp-pull-request-sentinel (process event)
      "Handle EVENT in PROCESS.

After `hub pull-request' exits, check the output logged in the magit
process log for a pattern that looks like a pull request URL and add
it to the kill ring."
      (magit-process-sentinel process event)
      (when (eq (process-status process) 'exit)
        (with-current-buffer (process-buffer process)
          (let* ((section (process-get process 'section))
                 (beg (marker-position (magit-section-content section)))
                 (end (marker-position (magit-section-end section)))
                 (content (buffer-substring beg end))
                 (match (string-match "http.*/pull/.*" content))
                 (url (match-string 0 content)))
            (kill-new url)))))
    (defun whilp-fork ()
      "Fork a Github repository."
      (interactive)
      (magit-run-git-async "fork"))

    (setq magit-git-executable "hub"
          magit-save-repository-buffers 'dontask
          magit-completing-read-function 'magit-builtin-completing-read
          magit-push-always-verify nil
          magit-completing-read-function 'ivy-completing-read
          magit-revert-buffers nil
          magit-after-revert-hook '(magit-refresh-vc-mode-line)
          magit-not-reverted-hook '(magit-refresh-vc-mode-line)
          magit-refresh-buffer-hook nil
          magit-refs-sections-hook '(
                                     ;; magit-insert-branch-description
                                     ;; magit-insert-local-branches
                                     ;; magit-insert-remote-branches
                                     ;; magit-insert-tags
                                     )
          magit-status-refresh-hook nil
          magit-wip-after-apply-mode nil
          magit-wip-after-save-mode nil
          magit-wip-before-change-mode nil
          magit-delete-by-moving-to-trash nil
          magit-diff-highlight-hunk-body t
          magit-diff-highlight-indentation nil
          magit-diff-highlight-trailing t
          magit-diff-paint-whitespace t
          magit-diff-refine-hunk nil
          magit-after-revert-hook '(magit-refresh-vc-mode-line)
          magit-log-arguments '("-n256" "--graph" "--decorate")
          magit-log-select-arguments '("-n256" "--decorate")
          magit-log-section-arguments '("--decorate"))))

(provide 'init-git)
;;; init-git ends here
