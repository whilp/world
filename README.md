dotfiles
========

My dotfiles as a docker container.

```bash
# Build and load the container. 
bazel run image

# Run it.
./image/run.sh
```

The container can build itself. Other linux systems can likely build it, too, though MacOSX cannot (see limitations). The colorscheme used here also works well with [kitty](https://github.com/kovidgoyal/kitty), which can toggle among schemes like:

```bash
kitty @ set-colors ~/.config/kitty/dark.conf
```
