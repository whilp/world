;;; init-emacs --- Initialize emacs stuff.

;;; Commentary:

;;; My emacs stuff.

;;; Code:

(require 'use-package)

(eval-when-compile
    (require 'message))

(use-package deferred
  :ensure t
  :defer t)

(use-package async
  :ensure t
  :config
  (progn
    (require 'smtpmail-async)
    (setq ;;send-mail-function 'smtpmail-send-it
          ;;message-send-mail-function 'message-send-mail-with-sendmail
          send-mail-function 'async-smtpmail-send-it
          message-send-mail-function 'async-smtpmail-send-it
          )))

(provide 'init-emacs)
;;; init-emacs ends here
