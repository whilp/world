(require 'auth-source)
(require 'url)

(defun* my-gh-profile (url user)
  (let* (
         (urlobj (url-generic-parse-url url))
         (host (url-host urlobj))
         (auth-info
          (car
           (auth-source-search
            :max 1
            :host host
            :user user
            :port 443
            :create nil)))
         (token (funcall (plist-get auth-info :secret))))
  (list
   :url url
   :username user
   :token token
   :remote-regexp (gh-profile-remote-regexp host))))

(setq
 gh-profile-default-profile "bh"
 gh-profile-current-profile nil
 gh-profile-alist (list
                   (cons "bh" (my-gh-profile "https://github.banksimple.com/api/v3" "whilp"))
                   (cons "gh" (my-gh-profile "https://api.github.com" "whilp"))
                   )
 )

