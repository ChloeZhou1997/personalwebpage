+++
title = "Using Emacs for bookmark management"
author = ["Chloe"]
date = 2022-11-08T13:05:00-05:00
lastmod = 2022-11-08T14:21:10-05:00
tags = ["emacs", "config"]
categories = ["tutorial"]
draft = false
weight = 2001
+++

This post is inspired by Mike Zamansky's video channel on
[youtube](https://www.youtube.com/watch?v=gjr9mP01oWE)[^fn:1].Since I am now using [Arc](https://thebrowser.company/) as my main browser so usually I
don't usually need to manage my bookmark, sometimes when I saw
interesting blogs and information, I still want to have it somewhere
and preferably an annotation to tell me why I store it at the first
place, to have this sort of bookmark in my emacs comes in handy.

The idea is to basically capture the url from my browser and send it
to a file through emacsclient and export the file in `.html` form and
bookmark[^fn:2] the page in my browser.

Also, I don't want to export my bookmark every time when I add more
information to it, it would be nice that the org file will auto export
itself on saving.


## Prep {#prep}

-   Org-protocol
-   Org-capture (browser extension + templates)


## Org-protocol configuration {#org-protocol-configuration}

There are abundant org-protocol configurations information online, the
the only caveat is the link to the emacsclient file might be broken
(see [this blog post]({{< relref "emacs-config-org-protocol" >}})).


## Org capture {#org-capture}

Both chrome and firefox have the capture extension in the extension
store. The templates are setup as following:

```emacs-lisp
(setq org-capture-templates '(
															("p" "Protocol" entry (file+headline "~/Notes/captures.org" "Inbox")
															 "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
															("L" "Protocol Link" entry (file+headline "~/Notes/captures.org" "Link")
															 "* %? [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n")
															))
```

Be aware that, `%c` stands for the head of the **kill-ring**, therefore in
order to capture the link properly, copy the link in the browser first
then invoke the capture button if using the `Protocol` template.

The captured information will be stored in `captures.org` file, under
**Inbox** and **Link** headings.

Open the `captures.org` file, copy-paste the following configuration at
the end of the file:

```text
# Local Variables:
# org-confirm-babel-evaluate:nil
# eval: (add-hook 'after-save-hook 'org-html-export-to-html t t)
# End:
```

This allows the corresponding html to be updated every time on saving.

[^fn:1]: The video also introduces another way of using emacsclient,
    which seems more flexible than using the org-capture extension in
    browser. I might dig into this configuration later.
[^fn:2]: In Arc, the concept of bookmark is replaced with a pinned
    tab. If a tab is not pinned, the browser will close them after 24h. I
    pinned the bookmark page on top of my main space.