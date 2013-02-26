;; (load "~/.emacs.d/.erc-auth")

(erc-match-mode)
(erc-track-mode t)
(erc-ring-mode t)
(erc-button-mode nil)
(erc-fill-disable)
(setq erc-modules (delq 'fill erc-modules))
(erc-track-enable)
(erc-scrolltobottom-enable)

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

(erc-timestamp-mode t)
(setq erc-timestamp-format "%Y-%m-%d %H:%M:%S ")

(set-face-foreground 'erc-keyword-face "slateblue")

(add-to-list 'erc-modules 'log)
(defun erc-generate-log-file-name-perfect (buffer target nick server port)
  (convert-standard-filename
   (concat server "!" (buffer-name buffer))))

(defun erc-generate-log-directory-name ()
  (format-time-string "~/logs/%Y/%m/%d"))

(defun erc-generate-log-channels-directory (buffer target nick server port)
  (erc-generate-log-directory-name))

(defun erc-mkdir-log-channels-directory (&optional buffer)
  (make-directory (erc-generate-log-directory-name) t))

(add-hook 'erc-insert-post-hook 'erc-mkdir-log-channels-directory)

(setq erc-log-channels t
      erc-hide-timestamps nil
      erc-enable-logging `erc-log-all-but-server-buffers
      erc-log-insert-log-on-open nil
      erc-log-write-after-send t
      erc-log-write-after-insert t
      erc-log-file-coding-system 'utf-8
      erc-generate-log-file-name-function 'erc-generate-log-file-name-perfect
      erc-log-channels-directory 'erc-generate-log-channels-directory)

(setq erc-auto-query 'bury
      erc-save-buffer-on-part nil
      erc-save-queries-on-quit nil
      erc-hide-list '("JOIN" "PART" "QUIT")
      erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT")
      erc-track-exclude-server-buffer t
      erc-format-query-as-channel-p t
      erc-prompt ">"
      erc-timestamp-only-if-changed-flag nil
      erc-insert-timestamp-function 'erc-insert-timestamp-left
      erc-track-priority-faces-only '("&bitlbee" "#chef" "##infra-talk" "#onboarding_support")
      erc-mode-line-format "%t"
      erc-join-buffer 'bury
      erc-flood-protect nil
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
        ("freenode.net"
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
         "#onboarding_support"
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
