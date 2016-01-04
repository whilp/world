;;; packages.el --- whilp-rcirc Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq whilp-rcirc-packages
      '(
        ;; package names go here
        rcirc
        ))

;; List of packages to exclude.
(setq whilp-rcirc-excluded-packages '())

;; For each package, define a function whilp-rcirc/init-<package-name>
;;
;; (defun whilp-rcirc/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun whilp-rcirc/pre-init-rcirc ()
  (spacemacs|use-package-add-hook rcirc
    :post-config
    (progn
      (setq rcirc-fill-flag nil
            rcirc-fill-column nil
            rcirc-time-format "%Y-%m-%dT%H:%M:%S "
            rcirc-color-is-deterministic t
            rcirc-omit-threshold 0
            rcirc-omit-responses
            '(
              "324"
              "329"
              "332"
              "333"
              "353"
              "353"
              "477"
              "MODE"
              "JOIN"
              "NICK"
              "PART"
              "QUIT"
              )
            rcirc-keywords
            '(
              "whilp"
              "@\\(here\\|channel\\|group\\|everyone\\)"
              "\\bSimpleFinance\\b"
              "\\bbackend[\\?!]"
              "\\bbackend\\b"
              "\\bchemex\\b"
              "\\bcoffee\\b"
              "\\bcowboy\\b"
              "\\beng\\b"
              "\\bmadison\\b"
              "\\bmaier\\b"
              "\\bmilwaukee\\b"
              "\\bmke\\b"
              "\\bmsn\\b"
              "\\bopa\\b"
              "\\bops\\b"
              "\\belt!?\\b"
              "\\bpolish merge\\b"
              "\\brum club\\b"
              "whilip"
              "\\bwisconsin\\b"
              "github.banksimple.com/it"
              "github.banksimple.com/ops"
              )
            rcirc-server-alist
            (list
             (rcirc-profile "banksimple.irc.slack.com" "whilp" 6697)
             ;; (my-rcirc-profile "furnace.firrre.com" "whilp" 9090)
             ;; (my-rcirc-profile "rands-leadership.irc.slack.com" "whilp" 6697)
             ;; (my-rcirc-profile "dist-sys.irc.slack.com" "whilp" 6697)
             ;; (my-rcirc-profile "monitorama.irc.slack.com" "whilp" 6697)
             ;; (my-rcirc-profile "remotes.irc.slack.com" "whilp" 6697)
             )
            )

      (defun rcirc-keepalive ()
        "Disable rcirc keepalives"))))

(defun rcirc-profile (host user port)
  "Search auth-info for an entry matching HOST, USER, and PORT."
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
     :password password
     :full-name user
     :port port
     :encryption 'tls)))
