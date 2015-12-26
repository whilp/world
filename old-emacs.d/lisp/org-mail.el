;;; org-mail --- Send email from org-mode.

;;; Commentary:

;;; Interact with email from org-mode.
;;;
;;; References:
;;; https://github.com/jkitchin/jmax/blob/master/email.el#L31

;;; Code:

(require 'org)

(defvar org-mail-compose-function compose-mail
  "The function to use to send the message.")

;;;###autoload
(defun org-mail-subtree ()
  "Send subtree as an email message."
  (interactive)
  (save-excursion
    (org-mark-subtree)
    (let ((content (org-get-entry))
          (TO nil)
          (SUBJECT (nth 4 (org-heading-components)))
          (OTHER-HEADERS (eval (org-entry-get (point) "OTHER-HEADERS")))
          (CONTINUE nil)
          (SWITCH-FUNCTION nil)
          (YANK-ACTION nil)
          (SEND-ACTIONS nil)
          (RETURN-ACTION nil))
      (compose-mail TO
                    SUBJECT
                    OTHER-HEADERS
                    CONTINUE
                    SWITCH-FUNCTION
                    YANK-ACTION
                    SEND-ACTIONS
                    RETURN-ACTION)
      (goto-char (point-max))
      (mml-insert-empty-tag "multipart" 'type "alternative")
      (forward-line -1)
      (insert content)
      (insert "\n")
      (mml-insert-tag 'part 'type "text/html")
      (insert
       (org-export-string-as content 'html t
                             '(:html-preamble nil
                               :html-postamble nil
                               :with-toc nil
                               :section-numbers nil))))))

(provide 'org-mail)
;;; org-mail ends here
