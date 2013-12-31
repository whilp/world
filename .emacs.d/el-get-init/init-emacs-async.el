(require 'smtpmail-async)
(setq
 send-mail-function 'async-smtpmail-send-it
 message-send-mail-function 'async-smtpmail-send-it)
