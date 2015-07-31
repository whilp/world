;;; init-emacs --- Initialize emacs stuff.

;;; Commentary:

;;; My emacs stuff.

;;; Code:

(require 'use-package)

(eval-when-compile
  (require 'message))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config (which-key-mode))

(use-package guru-mode
  :ensure t
  :diminish guru-mode
  :init (guru-global-mode +1))

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

(use-package ns-win
  :demand t
  :bind ("M-RET" . toggle-frame-fullscreen)
  :config
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super
        ns-function-modifier 'hyper
        ns-auto-hide-menu-bar t
        ns-use-native-fullscreen nil
        ;; TODO: these aren't defined in ns-win -- are they actually used anywhere?
        ;; mac-option-key-is-meta nil
        ;; mac-command-key-is-meta t
        ))

(cond
 ((string-equal system-type "darwin")
  (progn
    (setenv "GPG_AGENT_INFO" (expand-file-name "~/.gnupg/S.gpg-agent::1"))
    (setq epg-gpg-program "gpg2"))
  (set-face-attribute 'default nil :font "Monaco-16")))

(provide 'init-emacs)
;;; init-emacs ends here
