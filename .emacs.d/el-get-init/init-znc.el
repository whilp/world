(require 'auth-source)

(defun* my-znc-profile (name host user port)
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
    (list host port t (list (list name user password)))))

(setq
 znc-erc-connector 'erc
 znc-servers
 (list
  (my-znc-profile 'simple "chat.banksimple.com" "whilp" 9999)
  (my-znc-profile 'furnance "furnace.firrre.com" "whilp" 9090)
  ))

;; buffers from the day i killed everything https://github.banksimple.com/gist/1c622769aee278dc2aad
