(require 'mu4e)
(require 'org-mu4e)

(setq
 mu4e-maildir "~/Maildir"
 mu4e-sent-messages-behavior 'delete
 mu4e-compose-signature-auto-include nil
 mu4e-compose-complete-only-personal nil
 mu4e-compose-dont-reply-to-self t
 mu4e-show-images t
 mu4e-headers-skip-duplicates t
 mu4e-headers-include-related t
 mu4e-headers-show-target nil
 mu4e-hide-index-messages t
 mu4e-confirm-quit nil
 mu4e-headers-leave-behavior 'apply
 mu4e-use-fancy-chars nil
 mu4e-get-mail-command "offlineimap -q"
 mu4e-update-interval 180)

(require 'smtpmail)
(setq
 mail-user-agent 'mu4e-user-agent
 send-mail-function 'smtpmail-send-it
 message-send-mail-function 'smtpmail-send-it
 message-kill-buffer-on-exit t
 message-signature nil
 starttls-use-gnutls t
 smtpmail-queue-mail nil
 smtpmail-queue-dir "~/Maildir/queue/cur"
 smtpmail-debug-info t
 smtpmail-stream-type 'starttls
 smtpmail-starttls-credentials '(
   ("smtp.gmail.com" 587 "will@simple.com" nil)
   ("smtp.gmail.com" 587 "wcmaier@m.aier.us" nil))
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
     (smtpmail-smtp-user "will@simple.com")
     (user-full-name     "Will Maier")
     (user-mail-address  "will@simple.com"))
    ("wcmaier@m.aier.us"
     (mu4e-drafts-folder "/wcmaier@m.aier.us/[Gmail].Drafts")
     (mu4e-sent-folder   "/wcmaier@m.aier.us/[Gmail].Sent Mail")
     (mu4e-trash-folder  "/wcmaier@m.aier.us/[Gmail].Trash")

     (smtpmail-smtp-user "wcmaier@m.aier.us")
     (user-full-name     "Will Maier")
     (user-mail-address  "wcmaier@m.aier.us"))))

(setq user-mail-address "wcmaier@m.aier.us")
(setq
 mu4e-user-mail-address-list '(
                              "wcmaier@m.aier.us"
                              "will@simple.com"
                              "wcmaier@openbsd.org"
                              "wcmaier@hep.wisc.edu"
                              "willmaier@ml1.net"
                              "wcmaier@wisc.edu"))
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

(setq mu4e-bookmarks
      '(("flag:unread AND NOT flag:trashed" "unread" ?u)
        ("date:7d..now flag:unread AND NOT flag:trashed" "new" ?n)
        ("date:today..now" "today" ?t)
        ("date:7d..now" "week" ?w)))

(add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)
(add-hook 'mu4e-compose-mode-hook 'mml-secure-message-sign-pgpmime)
