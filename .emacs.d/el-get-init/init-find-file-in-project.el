(require 'find-file-in-project)

(defun git-grep-in-root ()
  (interactive)
  (let ((default-directory (ffip-project-root)))
    (call-interactively 'vc-git-grep)))

(defun compile-in-root ()
  (interactive)
  (let ((default-directory (ffip-project-root)))
    (call-interactively 'compile)))

