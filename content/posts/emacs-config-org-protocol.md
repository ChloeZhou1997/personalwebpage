+++
title = "Configuring org-protocol in AMD build MacOS"
author = ["Chloe"]
date = 2022-11-04
lastmod = 2022-11-08T14:33:00-05:00
tags = ["config", "emacs"]
draft = false
+++

This is a post on configuring `org-protocol` for people using Emacs on
MacOS with AMD chips. The [documentary](https://orgmode.org/worg/org-contrib/org-protocol.html#org2f758f1) only contains configuration for
up to MacOs built on Intel chips. I will also include the steps I have
taken to figure the problem out with very limited programming and
computer background knowledge.

First, load `org-protocol.el` to the `init.el` configuration file:

```emacs-lisp
(server-start)
(add-to-list 'load-path "~/path/to/org/protocol/")
(require 'org-protocol)
```

Note that `org-protocol.el` comes with the `org-mode` so it's not in any
package archives. From the [repo](https://github.com/emacs-mirror/emacs/blob/master/lisp/org/org-protocol.el) of `org-protocol.el`, I know the file is
located under `.../lisp/org/org-protocol.el` so what I did is to
navigate into my `.emacs.d` and trying to figure out where the file
is. Another way to find the path is through command line  as following:

```shell
find ~/.emacs.d -name 'org-protocol.el'
```

The terminal will return the file-path and you just need to copy the
path up to where the actual `org.protocol.el` file is located to the
`'load-path` part.

Then comes to the Mac OS X setup section, the older link doesn't work,
but the [newer](https://github.com/xuchunyang/setup-org-protocol-on-mac) ones won't work on MacOS will AMD chip either. This is
because Homebrew installs software to path `/opt/homebrew/` instead of
older `/usr/bin` path. From the script, from the script provided, it
seems the `org-protocol.app` is trying to reach the `emacscleint`, so the
task comes down to find where the `emacscleint` in my computer actually
locates. Since I have several emacs version installed in my macbook
but I am only using the emacs-plus@28, I want to make sure that I am
calling the right `emacscelint` so I run

```shell
brew info emacs-plus@28
```

to get the potentially correct path the `emacscleint` might be, I simply
navigated into the folder but another way is again using command line
with `sudo` in front to gain permission.

After this step, simply follow the rest of the tutorial [here](https://github.com/xuchunyang/setup-org-protocol-on-mac), and
everything works just fine.
