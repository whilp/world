;;; nssh-n.el --- SSH mode for Emacs

;; Copyright (C) 2014  Ian Eure

;; Author: Ian Eure <ian.eure@gmail.com>
;; Keywords: tools, unix, processes

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(defvar nssh-history nil
  "nssh-mode history")

(defvar nssh-sudo t
  "Use sudo

  When T, `nssh' will use the sudo protocol in the case where the
  username of the `nssh' destination is different from
  USER-LOGIN-NAME.

  When NIL, `nssl' will use the ssh protocol")

(defvar nssh-known-hosts-files
  '("/etc/ssh/ssh_known_hosts"
    "~/.ssh/known_hosts"))

(defun nssh-replace (from to)
  (goto-char (point-min))
  (while (re-search-forward from nil t)
    (replace-match to nil nil)))

(defun nssh-process-hosts ()
  (save-match-data
  (nssh-replace " .*$" "")
  (nssh-replace "," "\n")
  ;; IPv4 addresses
  (flush-lines "[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+" (point-min) (point-max))
  ;; IPv4 addresses
  (flush-lines "[0-9a-f]\\{4\\}:+" (point-min) (point-max))
  (flush-lines "^$")
  (nssh-replace "^\\(.*\\)$" "\"\\1\"")
  (goto-char (point-min))
  (insert "(\n")
  (goto-char (point-max))
  (insert ")")
  (goto-char (point-min))
  (delete-dups (read (current-buffer)))))

(defun nssh-known-hosts ()
  (with-temp-buffer
    (mapc (lambda (file)
            (ignore-errors
              (insert-file file)
              (insert "\n")))
          nssh-known-hosts-files)
    (nssh-process-hosts)))

(defun nssh-resolve (host)
  "Resolve HOSTNAME. Returns a list of IPs."
  (if url-gateway-nslookup-program
      (let ((proc (start-process " *nslookup*" " *nslookup*"
                                 url-gateway-nslookup-program host))
            (res))
        (set-process-query-on-exit-flag proc nil)
        (with-current-buffer (process-buffer proc)
          (while (memq (process-status proc) '(run open))
            (accept-process-output proc))
          (goto-char (point-min))
            (while (re-search-forward "Name:.*\nAddress: *\\(.*\\)$" nil t)
              (add-to-list 'res (buffer-substring (match-beginning 1)
                                          (match-end 1))))
          (kill-buffer (current-buffer)))
        res)
    host))

(defun nssh-user-host (dest)
  "Extract user and host from DEST.

   DEST is a SSH-style 'user@host'
   Returns a list of USER HOST."
  (let* ((host-parts (split-string dest "@"))
         (host (car (last host-parts)))
         (user (or (if (= 2 (length host-parts)) (car host-parts))
                   (user-login-name))))
    (list user host)))

(defun nssh-buffer-name (user host)
  "Generate a nssh buffer name."
  (if (string= user (user-login-name))
      (format "*ssh %s*" host)
    (format "*ssh %s@%s*" user host)))

(defun nssh-buffer (user host buffer)
  "Return the target buffer for this nssh command."
  (cond ((stringp buffer) buffer)
        ((bufferp buffer) (buffer-name buffer))
        ((or (and (not (eq nil buffer)) (listp buffer)) (numberp buffer))
         (generate-new-buffer-name (nssh-buffer-name user host)))
        (t (nssh-buffer-name user host))))

(defun nssh-protocol (user)
  (if (and (not (string= user user-login-name)) nssh-sudo)
      "sudo"
    "ssh"))

(defun nssh (dest &optional buffer)
  "Log into a remote machine with SSH."
  (interactive (list (completing-read "Host: "
                                      (append nssh-history (nssh-known-hosts))
                                      nil nil nil 'nssh-history)
                     current-prefix-arg))
  (add-to-list 'nssh-history dest)
  (let* ((user-host (nssh-user-host dest))
         (user (car user-host))
         (host (cadr user-host))
         (buf (nssh-buffer user host buffer)))
    (pop-to-buffer buf)
    (unless (comint-check-proc (current-buffer))
      (setq comint-prompt-read-only t)
      (cd (format "/%s:%s@%s:" (nssh-protocol user) user host))
      (shell (current-buffer)))))

(defun nssh-all (dest)
  "Log into all hosts DEST resolves to."
  (interactive (list (completing-read "Host: "
                                      (append nssh-history (nssh-known-hosts))
                                      nil nil nil 'nssh-history)))
  (let* ((user-host (nssh-user-host dest))
         (user (car user-host))
         (host (cadr user-host)))
    (mapc
     (lambda (ip)
       (nssh ip (get-buffer-create (nssh-buffer user (format "%s(%s)" host ip) nil))))

     (nssh-resolve host))))


(provide 'nssh)
;;; nssh-n.el ends here
