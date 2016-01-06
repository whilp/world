(spacemacs|use-package-add-hook magit
  :post-init
  (progn
    (require 'magit-popup)
    (magit-define-popup-action
     'magit-dispatch-popup ?H "Github" 'magit-github-popup)

    (magit-define-popup magit-github-popup
                        "Popup console for Github interaction."
                        :actions  '((?p "Pull request" magit-pull-request)
                                    (?f "Fork" magit-fork))
                        :default-action 'magit-pull-request)))

(defun magit-pull-request ()
  "Open a Github pull request.

Run `hub pull-request' asynchronously; see
`magit-pull-request-sentinel' for the interesting bits."
  (interactive)
  (let ((magit-git-executable "hub"))
    (set-process-sentinel
     (magit-run-git-with-editor "pull-request")
     'magit-pull-request-sentinel)))

(defun magit-pull-request-sentinel (process event)
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

(defun magit-fork ()
  "Fork a Github repository."
  (interactive)
  (let ((magit-git-executable "hub"))
    (magit-run-git-async "fork")))
