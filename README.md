dotfiles
========

My dotfiles as a docker container.

```bash
# Build and load the container. 
bazel run image

# Run it.
cid=$(./image/run.sh -d)
port=$(docker port $cid 22)
ssh "root@localhost:${port##*:}"
```

Or connect using the [chrome secure shell app](https://chrome.google.com/webstore/detail/secure-shell-app/pnhechapfaindjhompbnflcldabbghjo?hl=en).
