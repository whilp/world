;;; init-el-get --- Legacy el-get setup

;;; Commentary:

;;; el-get is still needed for mu4e.

;;; Code:

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(require 'comint)

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(require 'el-get)

(setq
 el-get-byte-compile t
 el-get-git-shallow-clone t
 el-get-user-package-directory "~/.emacs.d/lisp"
 el-get-sources '())

(el-get 'sync '(mu4e))

(defun whilp-mbsync ()
  "Run mbsync (via sync-mail)."
  (let ((buf "*mbsync*"))
    (shell buf)
    (comint-send-string (get-buffer-process buf) "sync-mail\n")))

(provide 'init-el-get)
;;; init-el-get ends here
