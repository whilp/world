(require 'rcirc)
(require 'auth-source)

(rcirc-track-minor-mode 1)

(setq rcirc-fill-flag nil
      rcirc-fill-column nil
      rcirc-time-format "%Y-%m-%dT%H:%M:%S "
      rcirc-log-flag t
      rcirc-log-directory (expand-file-name "~/.irclogs"))
;; rcirc-timeout-seconds
(setq rcirc-keywords '())
(setq rcirc-omit-responses
      '(
        "JOIN"
        "PART"
        "NICK"
        "QUIT"))

(defun my-rcirc-mode-setup ()
  "Sets things up for channel and query buffers spawned by rcirc."
  ;; rcirc-omit-mode always *toggles*, so we first 'disable' it
  ;; and then let the function toggle it *and* set things up.
  (setq rcirc-omit-mode nil)
  (rcirc-omit-mode))

(add-hook 'rcirc-mode-hook 'my-rcirc-mode-setup)

(defun* my-rcirc-profile (host user port)
  (let* (
         (auth-info
          (car
           (auth-source-search
            :max 1
            :host host
            :user user
            :port port
            :create nil)))
         (password (funcall (plist-get auth-info :secret))))
    (list
     host
     :nick user
     :password (format "%s:%s" user password)
     :full-name user
     :port port
     :encryption 'tls)))

(setq rcirc-server-alist
      (list
;;       (my-rcirc-profile "chat.banksimple.com" "whilp" 9999)
       (my-rcirc-profile "furnace.firrre.com" "whilp" 9090)))
