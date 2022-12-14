+++
title = "Org + Hugo + Cloudflare for personal blog"
author = ["Chloe"]
date = 2022-10-28
lastmod = 2022-10-28T11:06:26-04:00
tags = ["config", "hugo"]
draft = false
+++

## Hugo Quick setup {#hugo-quick-setup}


### Ox-Hugo {#ox-hugo}

**Source of documentation:**

-   [ox-hugo manual](https://ox-hugo.scripter.co/) for `init.el` Backlink

<!--listend-->

```emacs-lisp
(use-package ox-hugo
	:after ox)
```

**Handing in org.file**

I use one-page-per-post way to manage the post, the heading section
looks the following:

```text
#+TITLE:
#+AUTHOR: Chloe
#+DATE: 2022-10-28
#+HUGO_SECTION: posts
#+HUGO_BASE_DIR:~/Blog
#+HUGO_TAGS:
#+EXPORT_HUGO_CATEGORIES:
#+hugo_weight: auto
#+HUGO_DRAFT: true
#+hugo_auto_set_lastmod: t

```

I use `YASnipeet` to generate the head section, so the date will be
autogenerated to be the current date. Be aware that in ox-hugo,
`HUGO_BASE_DIR` is mandentory, it determines where the root directory
the exported `.md` file lies. By setting both `BASE_DIR` and `HUGO_SECTION`,
the generated `.md` file will be exported to `~/Blog/content/posts`.

Add following snippet at the end the `org` file will enabling the
autosave to the target location

```text
# Local Variables:
# eval: (org-hugo-auto-export-mode)
# End:
```


### Hugo {#hugo}

The setup of hugo is as easy as what's shown in [the official
guidance](https://gohugo.io/getting-started/quick-start/). In my case, I set the New Site name to be `Blog`, so that the
`.md` file will be generated in `~/Blog/content/posts` as what I
expected. In this case I can skip the Step 4 in the official guidance
as some `.md` files with hugo [front matter](https://gohugo.io/content-management/front-matter/) are already in place.

The front matter generated from the snippet typically looks like the
following:

```quote
+++
title = "Org + Hugo + Cloudflare for personal blog "
author = ["Chloe"]
date = 2022-10-28
lastmod = 2022-10-28T08:25:39-04:00
tags = ["config", "hugo"]
draft = true
+++
```

Start the server with the following code  where `-D` means enabling draft, and `ignoreCache`
will deisable the cache function, this will prevent some changes not
showing in the website on time.

```shell
hugo server -D --ignoreCache
```

Except for using theme [cactus](https://github.com/monkeyWzr/hugo-theme-cactus),  I also included a **backlink** section for
my blog, the configuation can be found [here](https://seds.nl/notes/export%5Forg%5Froam%5Fbacklinks%5Fwith%5Fgohugo/).


## Cloudflare {#cloudflare}

I use Cloudflare to deploy my personal website, for the site is free,
and allows for [continuous deployment with GitHub](https://gohugo.io/hosting-and-deployment/). The guidance of
deployment can be found [here](https://developers.cloudflare.com/pages/framework-guides/deploy-a-hugo-site/). Be aware with the **Deployment with
Cloudflare Page** section, if using the free version of cloudflare, the
build command need to be set as

```shell
hugo -b $CF_PAGES_URL
```

For the CSS to be built successfully in the final production. The
above code will modify the `baseURL` to be found in the `config.toml` file
(by default the section looks like `baseURL = 'http://example.org/'`),
which is why when using `hugo server` to preview the website everything
seems to work just fine. An easy way to investigate the effect of the
production version of the personal website, `cd /public` and run

```python
python -m http.server 8000
```

in the command line, open `localhost:8000` in the browser.