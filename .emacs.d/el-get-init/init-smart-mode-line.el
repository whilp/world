(require 'smart-mode-line)
(sml/setup)
(setq sml/mode-width 'right
      sml/shorten-directory t
      sml/name-width '(12 . 18))

(setq-default mode-line-format
              '(
                "%e"
                display-time-string
                mode-line-front-space
                mode-line-mule-info
                mode-line-client
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification
                (vc-mode vc-mode)
                "  " mode-line-modes
                mode-line-misc-info
                mode-line-end-spaces))
