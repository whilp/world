;;; packages.el --- whilp-git Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq whilp-git-packages
    '(
      magit
      ))

;; List of packages to exclude.
(setq whilp-git-excluded-packages '())

;; For each package, define a function whilp-git/init-<package-name>
;;
;; (defun whilp-git/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun whilp-git/pre-init-magit ()
  (require 'magit-popup)
  (magit-define-popup-action
    'magit-dispatch-popup ?H "Github" 'magit-github-popup)

  (magit-define-popup magit-github-popup
    "Popup console for Github interaction."
    :actions  '((?p "Pull request" magit-pull-request)
                (?f "Fork" magit-fork))
    :default-action 'magit-pull-request))

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
