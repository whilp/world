# dotfiles

[whilp/home](https://hub.docker.com/r/whilp/home/) on hub.docker.com.

```
docker run -v /var/run/docker.sock:/var/run/docker.sock --rm -it whilp/home /usr/bin/emacs -nw
```

## TODO

- chmod .ssh/config
- mkdir .ssh/ctl
- ssh-agent -a ~/.ssh/agent.sock
- ssh keys/agent
- ssh known_hosts
- projectile-shell not working
- ispell
- control-p doesn't work
- whilp/git-get
- github/hub
- magit hub not present
- pastebuffer

## agents

- make an alpine package for spiped
- exec with-ssh-agent with-gpg-agent emacs-nw
- with-ssh-agent starts spipe client, creates sock, sets ENV vars, and execs next
- same for with-gpg-agent
