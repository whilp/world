;;; init-rcirc --- rcirc settings

;;; Commentary:

;;; rcirc

;;; Code:

(require 'rcirc)
(require 'auth-source)
(require 'use-package)
(eval-when-compile
  (require 'aggressive-indent))

(rcirc-track-minor-mode 1)

(setq rcirc-fill-flag nil
      rcirc-fill-column nil
      rcirc-time-format "%Y-%m-%dT%H:%M:%S "
      rcirc-log-flag t
      rcirc-log-directory (expand-file-name "~/.irclogs"))
;; rcirc-timeout-seconds

(defun rcirc-keepalive ()
  "Disable rcirc keepalives")

(setq rcirc-keywords
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
        "github.banksimple.com/ops"))

(setq rcirc-omit-threshold 0
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
        ))

(add-to-list 'aggressive-indent-excluded-modes 'rcirc-mode)

(defun my-rcirc-mode-setup ()
  "Set things up for channel and query buffers spawned by rcirc."
  ;; rcirc-omit-mode always *toggles*, so we first 'disable' it
  ;; and then let the function toggle it *and* set things up.
  (setq rcirc-omit-mode nil)
  (rcirc-omit-mode))

(add-hook 'rcirc-mode-hook 'my-rcirc-mode-setup)

(defun my-rcirc-profile (host user port)
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

(setq rcirc-server-alist
      (list
       (my-rcirc-profile "furnace.firrre.com" "whilp" 9090)
       (my-rcirc-profile "banksimple.irc.slack.com" "whilp" 6697)
       ;; (my-rcirc-profile "rands-leadership.irc.slack.com" "whilp" 6697)
       ;; (my-rcirc-profile "dist-sys.irc.slack.com" "whilp" 6697)
       ;; (my-rcirc-profile "monitorama.irc.slack.com" "whilp" 6697)
       ;; (my-rcirc-profile "remotes.irc.slack.com" "whilp" 6697)
       ))

(use-package rcirc-color
  :ensure t
  :demand t
  :config
  (setq rcirc-color-is-deterministic t))

(use-package rcirc-controls
  :ensure t)

(provide 'init-rcirc)
;;; init-rcirc ends here
