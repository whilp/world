(require 'mu4e)
(require 'org-mu4e)

(setq
 mu4e-maildir "~/Maildir"
 mu4e-sent-messages-behavior 'delete
 mu4e-compose-signature-auto-include nil
 mu4e-show-images t)

(require 'smtpmail)
(setq
 send-mail-function 'smtpmail-send-it
 message-send-mail-function 'smtpmail-send-it
 message-kill-buffer-on-exit t
 message-signature nil
 starttls-use-gnutls t
 smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
 smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg")
 smtpmail-debug-info t
 smtpmail-stream-type 'starttls
 smtpmail-default-smtp-server "smtp.gmail.com"
 smtpmail-smtp-server "smtp.gmail.com"
 smtpmail-smtp-service 587)

(defalias 'org-mail 'org-mu4e-compose-org-mode)

(setq my-mu4e-account-alist
  '(
    ("will@simple.com"
     (mu4e-drafts-folder "/will@simple.com/[Gmail].Drafts")
     (mu4e-sent-folder   "/will@simple.com/[Gmail].Sent Mail")
     (mu4e-trash-folder  "/will@simple.com/[Gmail].Trash")
     (user-mail-address  "will@simple.com"))
    ("wcmaier@m.aier.us"
     (mu4e-drafts-folder "/wcmaier@m.aier.us/[Gmail].Drafts")
     (mu4e-sent-folder   "/wcmaier@m.aier.us/[Gmail].Sent Mail")
     (mu4e-trash-folder  "/wcmaier@m.aier.us/[Gmail].Trash")
     (user-mail-address  "wcmaier@m.aier.us"))))

(setq my-mu4e-default-account "wcmaier@m.aier.us")
(defun my-mu4e-set-account ()
  "Set the account for composing a message."
  (let* ((account
          (if mu4e-compose-parent-message
              (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                (string-match "/\\(.*?\\)/" maildir)
                (match-string 1 maildir))
            (completing-read (format "Compose with account: (%s) "
                                     (mapconcat #'(lambda (var) (car var)) my-mu4e-account-alist "/"))
                             (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                             nil t nil nil (caar my-mu4e-account-alist))))
         (account-vars (cdr (assoc account my-mu4e-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (error "No email account found"))))

(mapc #'(lambda (var) (set (car var) (cadr var)))
      (cdr (assoc my-mu4e-default-account my-mu4e-account-alist)))

(add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)
