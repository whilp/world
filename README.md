dotfiles
========

[whilp/home](https://hub.docker.com/r/whilp/home/) on hub.docker.com.

```
docker run -v /var/run/docker.sock:/var/run/docker.sock --rm -it whilp/home /usr/bin/emacs -nw
```

TODO
----

-	magit hub not present
-	make a vaultproject.io container
-	extend envcrypt to talk to vault
-	add apks
	-	clojure
	-	lein
	-	racket
	-	scala
