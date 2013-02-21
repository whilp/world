;; (load "~/.emacs.d/.erc-auth")

(erc-match-mode)
(erc-track-mode t)
(erc-ring-mode t)
(erc-button-mode nil)
(erc-fill-disable)

(setq erc-keywords '(
                     "\\b#ops\\b"
                     "\\bops\\b"
                     "\\bchef\\b"
                     "\\bmsn\\b"
                     "\\bwisconsin\\b"
                     "\\bmke\\b"
                     "\\bmadison\\b"
                     "\\bmilwaukee\\b"
                     "\\bwcmaier\\b"
                     "\\bmaier\\b"
                     ))

(add-to-list 'erc-modules 'autoaway)
(setq erc-autoaway-use-emacs-idle t)

(erc-timestamp-mode t)
(setq erc-timestamp-format "%Y-%m-%d %H:%M:%S ")

(add-to-list 'erc-modules 'log)
;; '(erc-enable-logging 'erc-log-all-but-server-buffers)
(setq erc-log-channels t
      erc-hide-timestamps nil
      erc-log-insert-log-on-open nil
      erc-log-write-after-send t
      erc-log-write-after-insert t
      erc-generate-log-file-name-function 'erc-generate-log-file-name-with-date
      erc-log-channels-directory "~/logs/")

(setq erc-auto-query 'bury
      erc-save-buffer-on-part nil
      erc-save-queries-on-quit nil
      erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT")
      erc-track-exclude-server-buffer t
      erc-format-query-as-channel-p t
      erc-prompt ">"
      erc-timestamp-only-if-changed-flag nil
      erc-insert-timestamp-function 'erc-insert-timestamp-left
      erc-track-priority-faces-only 'all
      erc-log-matches-flag nil
      erc-current-nick-highlight-type 'nick
      erc-track-use-faces t
      erc-track-faces-priority-list
          '(erc-current-nick-face erc-keyword-face)
      )

(erc-autojoin-mode t)
(setq erc-autojoin-channels-alist
      '(
        ("irc.oftc.net"
         )
        ("irc.freenode.net"
         "##infra-talk"
         "##welp"
         "#OpsSchool"
         "#chef"
         "#hangops"
         "#monitoringlove"
         "#monitoringsucks"
         "#pdxchef"
         )
        ("chat.banksimple.com"
         "#Buttram"
         "#achewood"
         "#analytics"
         "#backend"
         "#bananastand"
         "#biz"
         "#booze"
         "#brooklyn"
         "#cats"
         "#commits"
         "#deployments"
         "#emacs"
         "#engineering"
         "#finance"
         "#food"
         "#frontend"
         "#github"
         "#happyplace"
         "#ideas"
         "#infosec"
         "#marketing"
         "#mobile"
         "#music"
         "#nagios"
         "#notifications"
         "#onboarding"
         "#ops"
         "#overflow"
         "#politics"
         "#portland"
         "#pr"
         "#puns"
         "#remotecontrol"
         "#review"
         "#ruby"
         "#security"
         "#sensu"
         "#simple"
         "#statements"
         "#support"
         "#swiper"
         "#tea"
         "#tourettes"
         "#twitter"
         "#txvia"
         "#ux"
         "#vim"
         "#warroom"
         "#website"
         "#weekenders"
         )
        ("localhost"
         )
        ))

(defun erc-oftc ()
  "Connect to OFTC."
  (interactive)
  (erc-tls :server "irc.oftc.net" :port 6697 :nick "whilp" :full-name "Will Maier"))

(defun erc-freenode ()
  "Connect to Freenode."
  (interactive)
  (erc-tls :server "irc.freenode.net" :port 6697 :nick "whilp" :full-name "Will Maier"))

(defun erc-simple ()
  "Connect to Simple."
  (interactive)
  (erc-tls :server "chat.banksimple.com" :port 6697 :nick "whilp" :full-name "Will Maier"))

(defun erc-bitlbee ()
  "Connect to bitlbee."
  (interactive)
  (erc :server "127.0.0.1" :port 16667 :nick "will" :full-name "Will Maier"))
