;; (load "~/.emacs.d/.erc-auth")

(erc-match-mode t)
(erc-readonly-mode t)
(erc-track-mode t)
(erc-ring-mode t)
(erc-button-mode nil)
(erc-fill-disable)
(erc-track-enable)
(erc-scrolltobottom-enable)

(setq erc-modules '(
                    autojoin
                    button
                    completion
                    irccontrols
                    keep-place
                    list
                    log
                    match
                    menu
                    move-to-prompt
                    netsplit
                    networks
                    noncommands
                    readonly
                    ring
                    scrolltobottom
                    stamp
                    track
                    truncate
                    ))
(erc-update-modules)

(define-key erc-mode-map (kbd "C-c C-c") 'end-of-buffer)

(setq erc-keywords '(
                     "\\bcowboy\\b"
                     "\\bmaier\\b"
                     "\\bSimpleFinance\\b"
                     "\\bmke\\b"
                     "\\bmadison\\b"
                     "\\bmilwaukee\\b"
                     "\\bwisconsin\\b"
                     "\\bbackend[\\?!]"
                     "\\beng\\b"
                     "\\bops\\b"
                     "\\bmsn\\b"
                     ))

(erc-timestamp-mode t)
(setq erc-timestamp-format "%Y-%m-%d %H:%M:%S ")

(set-face-foreground 'erc-keyword-face "slateblue")

(defadvice erc-track-find-face (around erc-track-find-face-promote-query activate)
  (if (erc-query-buffer-p) 
      (setq ad-return-value (intern "erc-current-nick-face"))
    ad-do-it))

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

(setq erc-log-matches-types-alist
      '((keyword . "&activity")
        (current-nick . "&activity")))

(setq erc-log-channels t
      erc-hide-timestamps nil
      erc-enable-logging `erc-log-all-but-server-buffers
      erc-log-insert-log-on-open nil
      erc-log-write-after-send t
      erc-log-write-after-insert t
      erc-log-file-coding-system 'utf-8
      erc-log-matches-flag t
      erc-generate-log-file-name-function 'erc-generate-log-file-name-perfect
      erc-log-channels-directory 'erc-generate-log-channels-directory)

(setq erc-auto-query 'bury
      erc-save-buffer-on-part nil
      erc-save-queries-on-quit nil
      erc-hide-list '("JOIN" "NICK" "PART" "QUIT")
      erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE")
      erc-track-exclude-server-buffer t
      erc-format-query-as-channel-p t
      erc-prompt ">"
      erc-timestamp-only-if-changed-flag nil
      erc-insert-timestamp-function 'erc-insert-timestamp-left
      erc-mode-line-format "%t"
      erc-header-line-format "%n at %S"
      erc-join-buffer 'bury
      erc-flood-protect nil
      erc-current-nick-highlight-type 'all
      erc-interpret-controls-p 'remove
      erc-interpret-mirc-color t
      erc-track-use-faces t
      erc-track-faces-priority-list
      '(
        erc-current-nick-face
        erc-keyword-face
        erc-prompt-face
        erc-nick-msg-face
        erc-direct-msg-face
        erc-notice-face
        )
      )

(setq erc-track-priority-faces-only '(
                                      "&bitlbee"
                                      "#chef"
                                      "##infra-talk"
                                      "#foodfightshow"
                                      "#onboarding_support"
                                      "#nagios"
                                      "#sensu"
                                      "#OpsSchool"))

(erc-autojoin-mode t)
(setq erc-autojoin-channels-alist
      '(
        ("irc.oftc.net"
         )
        ("freenode.net"
         "##infra-talk"
         "#foodfightshow"
         "#heavywater"
         "##welp"
         "##buried"
         "#OpsSchool"
         "#chef"
         "#hangops"
         "#monitorama"
         "#monitoringlove"
         "#monitoringsucks"
         "#pdxchef"
         "#pdxdevopsdays"
         "#netflixoss"
         "##remotes"
         "##simple"
         )
        ("chat.banksimple.com"
         "##@"
         "#/b/anksimple"
         "##/b/anksimple"
         "#Buttram"
         "#ASL"
         "##ASL"
         "##adorbs"
         "#achewood"
         "##achewood"
         "#analytics"
         "#appreciate"
         "#backend"
         "#bananastand"
         "#belvedere"
         "#biz"
         "##bikes"
         "##books"
         "#booze"
         "##booze"
         "#brooklyn"
         "##brooklyn"
         "#c2c"
         "#cats"
         "##cats"
         "#checks"
         "#commits"
         "#deployments"
         "#emacs"
         "##emacs"
         "#engineering"
         "#finance"
         "#food"
         "#fowler"
         "#frontend"
         "#github"
         "#happyplace"
         "#ideas"
         "#infosec"
         "#internal"
         "##literature"
         "#marketing"
         "##math"
         "#mobile"
         "#music"
         "##music"
         "#nagios"
         "##nighters"
         "#notifications"
         "#onboarding"
         "#onboarding_support"
         "#ops"
         "##optional-review"
         "#overflow"
         "#physics"
         "##physics"
         "#politics"
         "##politics"
         "#portland"
         "##portland"
         "#pr"
         "#product"
         "#puns"
         "##reading"
         "#remotecontrol"
         "#review"
         "#risk"
         "#ruby"
         "#security"
         "#sensu"
         "#sign_ups"
         "#simple"
         "#statements"
         "#support"
         "#swiper"
         "##swiper"
         "#tea"
         "##tea"
         "##television"
         "#tourettes"
         "##tourettes"
         "#twitter"
         "#txvia"
         "#ux"
         "#vim"
         "##vim"
         "#warroom"
         "#website"
         "#weekenders"
         "##weekenders"
         )
        ("localhost"
         )
        ))

(defun erc-track-mode-reset ()
  (interactive)
  (setq erc-modified-channels-alist nil)
  (erc-modified-channels-update))

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
