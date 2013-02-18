(setq gnus-select-method '(nntp "127.0.0.1"))

(setq gnus-secondary-select-methods
      '((nnimap "m.aier.us"
                (nnimap-address "imap.gmail.com")
                (nnimap-server-port 993)
                (nnimap-stream ssl)
                (nnimap-authinfo-file "~/.authinfo"))
        (nnimap "simple.com"
                (nnimap-address "imap.gmail.com")
                (nnimap-server-port 993)
                (nnimap-stream ssl)
                (nnimap-authinfo-file "~/.authinfo"))))
