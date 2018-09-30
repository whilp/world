dotfiles
========

[![Build Status](https://travis-ci.org/whilp/dotfiles.svg?branch=master)](https://travis-ci.org/whilp/dotfiles)

A reproducible and complete environment for computering since 2005.

```
$ git rev-list --max-parents=0 --pretty HEAD
commit 165210edc61ec09e133b8e0af26d98e1e46de2ea
Author: will <devnull@localhost>
Date:   Sat Jun 11 21:26:33 2005 +0000

    [project @ 2005-06-11 16:26:33 by will]
    initial import into CVS
```

```bash
# Build and load the container. 
$ bazel run image

# Run it.
$ ./image/run.sh
ssh://root@localhost:32779
```

Then you can connect using the [chrome secure shell app](https://chrome.google.com/webstore/detail/secure-shell-app/pnhechapfaindjhompbnflcldabbghjo?hl=en).

Chrome
------

Speaking of Chrome, if you're on OSX you'll probably want to set the following flags:

1.	chrome://flags/#views-browser-windows (disabled, [here's](https://www.reddit.com/r/chrome/comments/9d1c9p/fullscreen_mode_tab_problem_chrome_69_for_mac/?sort=new) [why](https://productforums.google.com/forum/#!msg/chrome/29i65bB5VKM/lo8FfzkRGQAJ)\)
2.	chrome://flags/#enable-fullscreen-toolbar-reveal (disabled)
