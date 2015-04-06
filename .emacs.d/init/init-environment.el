;;; init-environment --- environment configuration

;;; Commentary:

;;; Configuration for system environment.

;;; Code:

(setq explicit-shell-file-name "/bin/bash")
(setq exec-path
      (append
       (mapcar
        'expand-file-name
        '(
          "~/bin"
          "~/go/bin"
          "~/homebrew/Cellar/go/1.3/libexec/bin"
          "/usr/local/opt/go/libexec/bin/godoc"
          "/usr/local/sbin"
          "/usr/local/bin"
          "/usr/local/MacGPG2/bin"
          ))
        exec-path))
(setenv "TMPDIR" "/tmp")
(setenv "PATH"
        (mapconcat 'identity exec-path path-separator))
(setenv "PAGER" "cat")
(setenv "EDITOR" "emacsclient")
(setenv "ALTERNATE_EDITOR" "emacs")
(setenv "PROMPT_COMMAND" "")
(setenv "GPG_AGENT_INFO" nil)
(setenv "SSH_AUTH_SOCK" (expand-file-name "~/.ssh/agent.sock"))
(setenv "PS1" "${debian_chroot:+($debian_chroot)}\\u@\\h:\\w \\$ ")
(setenv "_JAVA_OPTIONS" "-Djava.awt.headless=true")
(setenv "MAN_WIDTH" "72")

(setenv "GIT_EDITOR" "emacsclient")
(setenv "GIT_COMMITTER_NAME" "Will Maier")
(setenv "GIT_COMMITTER_EMAIL" "wcmaier@m.aier.us")
(setenv "GIT_AUTHOR_NAME" "Will Maier")
(setenv "GIT_AUTHOR_EMAIL" "wcmaier@m.aier.us")

(setenv "GOPATH" (expand-file-name "~/go"))

(provide 'init-environment)
;;; init-environment ends here

