(require 'find-file-in-project)
(defun compile-in-root ()
  (interactive)
  (let ((default-directory (ffip-project-root)))
    (call-interactively 'compile)))

