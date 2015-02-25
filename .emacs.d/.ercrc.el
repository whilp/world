;; (load "~/.emacs.d/.erc-auth")

(erc-match-mode t)
(erc-readonly-mode t)
(erc-track-mode t)
(erc-ring-mode t)
(erc-button-mode nil)
(erc-fill-disable)
(erc-track-enable)
(erc-highlight-nicknames-enable)
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
(define-key erc-mode-map (kbd "C-c C-x") nil)

(setq erc-keywords '(
                     "\\bwhilip\\b"
                     "\\bopa\\b"
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
                     "\\bbackend\\b"
                     "github.banksimple.com/it"
                     "github.banksimple.com/ops"
                     "\\bmsn\\b"
                     "\\brum club\\b"
                     "\\bchemex\\b"
                     "\\bpolish merge\\b"
                     "\\bcoffee\\b"
                     ))

(setq erc-autojoin-channels-alist
      '(("chat.banksimple.com"
         "###vertigo"
         "##/b/anksimple"
         "##achewood"
         "##adorbs"
         "##bikes"
         "##booze"
         "##brooklyn"
         "##cats"
         "##clojure"
         "##coffee"
         "##ctf"
         "##cycling"
         "##datareview"
         "##davis"
         "##emacs"
         "##futsal"
         "##go"
         "##got"
         "##literature"
         "##make"
         "##math"
         "##music"
         "##nighters"
         "##optional-review"
         "##physics"
         "##politics"
         "##portland"
         "##pride"
         "##python"
         "##ruby"
         "##running"
         "##schadenfreude"
         "##screamchamber"
         "##sql"
         "##swiper"
         "##tea"
         "##vim"
         "##weekenders"
         "##worldcup"
         "#_b_anksimple"
         "#account-linking"
         "#achewood"
         "#addresschanges"
         "#adorbs"
         "#alder"
         "#allhands"
         "#analytics"
         "#apiversion"
         "#appreciate"
         "#backend"
         "#bananastand"
         "#belvedere"
         "#bikes"
         "#bilbo"
         "#biz"
         "#booze"
         "#brooklyn"
         "#c2c"
         "#cards"
         "#cats"
         "#checks"
         "#choochoo"
         "#ci4"
         "#cloudbank"
         "#coffee"
         "#commits"
         "#compass"
         "#compliance"
         "#conversion"
         "#cr_bugs"
         "#creative"
         "#cycling"
         "#davis"
         "#deployments"
         "#dev"
         "#education"
         "#emacs"
         "#engineering"
         "#finance"
         "#fiserv"
         "#fowler"
         "#frontend"
         "#futsal"
         "#general"
         "#got"
         "#happyplace"
         "#houston"
         "#ic"
         "#ideas"
         "#integration"
         "#internal"
         "#it"
         "#launchpad"
         "#literature"
         "#make"
         "#marketing"
         "#math"
         "#megaphone"
         "#mobile"
         "#monitoring"
         "#notifications"
         "#ocs"
         "#onboarding"
         "#onboarding_support"
         "#opc"
         "#ops"
         "#optional-review"
         "#overflow"
         "#overspentgoals"
         "#partproxydeploy"
         "#payments"
         "#physics"
         "#pin"
         "#politics"
         "#portland"
         "#postmortem"
         "#pr"
         "#processor"
         "#product"
         "#projectonboarding"
         "#properfucked"
         "#puns"
         "#random"
         "#remotecontrol"
         "#reversals"
         "#review"
         "#risk"
         "#safespace"
         "#scala-nwo"
         "#scholars"
         "#security"
         "#sensu"
         "#sign_ups"
         "#silentdisco"
         "#simple"
         "#social"
         "#sre"
         "#statements"
         "#support"
         "#swipertest"
         "#tbbk"
         "#thing"
         "#ux"
         "#warroom"
         "#website"
         "#weekenders"
         "#worldcup"
         )))


;; https://news.ycombinator.com/item?id=1654487
(setq Buffer-menu-use-frame-buffer-list nil)

(put 'erc-quit-server 'disabled t)

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
      erc-match-exclude-server-buffer t
      erc-generate-log-file-name-function 'erc-generate-log-file-name-perfect
      erc-log-channels-directory 'erc-generate-log-channels-directory)

(setq erc-auto-query 'bury
      erc-save-buffer-on-part nil
      erc-save-queries-on-quit nil
      erc-hide-list '("JOIN" "NICK" "PART" "QUIT")
      erc-track-exclude-server-buffer t
      erc-track-switch-direction 'newest
      erc-track-position-in-mode-line 'after-modes
      erc-format-query-as-channel-p t
      erc-prompt ">"
      erc-timestamp-only-if-changed-flag nil
      erc-insert-timestamp-function 'erc-insert-timestamp-left
      erc-mode-line-format "%t"
      erc-header-line-format "%n at %S"
      erc-join-buffer 'bury
      erc-query-display 'bury
      erc-flood-protect nil
      erc-current-nick-highlight-type 'all
      erc-interpret-controls-p 'remove
      erc-interpret-mirc-color t
      erc-track-when-inactive nil
      erc-track-visibility nil
      erc-track-use-faces t
      erc-track-switch-direction 'newest
      erc-track-showcount nil
      erc-track-exclude-types '(
                                "JOIN"
                                "PART"
                                "TOPIC"
                                "NICK"
                                "MODE"
                                "324"
                                "329"
                                "332"
                                "333"
                                "353"
                                "477"
                                )
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
                                      "#graphite"
                                      "##infra-talk"
                                      "#foodfightshow"
                                      "#onboarding_support"
                                      "#nagios"
                                      "#dev"
                                      "#sensu"
                                      "#OpsSchool"))

(defun erc-channel-cleanup ()
  "Kill any buffer whose server is not alive"
  (interactive)
  (erc-buffer-list
   (lambda ()
     (when (not (erc-server-process-alive))
       (kill-buffer)))))

(defun erc-track-mode-reset ()
  (interactive)
  (setq erc-modified-channels-alist nil)
  (erc-modified-channels-update))
