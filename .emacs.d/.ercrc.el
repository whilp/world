;; (load "~/.emacs.d/.erc-auth")

(setq erc-log-channels-directory "~/logs/")

(setq erc-save-buffer-on-part nil
      erc-save-queries-on-quit nil
      erc-log-write-after-send t
      erc-log-write-after-insert t
      erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT")
      erc-track-exclude-server-buffer t
      erc-format-query-as-channel-p t
      erc-track-priority-faces-only 'all
      erc-current-nick-highlight-type 'nick
      erc-track-use-faces t
      erc-track-faces-priority-list
          '(erc-current-nick-face erc-keyword-face)
      erc-keywords '(
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
                     )
      )

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

