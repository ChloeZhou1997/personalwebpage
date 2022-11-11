+++
title = "Emacs Configuration"
author = ["Chloe"]
date = 2022-10-29
lastmod = 2022-11-11T09:11:14-05:00
tags = ["emacs", "config"]
draft = false
+++

Rewrite my config to get rid of the redunant functions and build the
system from stratch using `straight.el` for package management.


## Package System Setup {#package-system-setup}


### use-package {#use-package}

-   [GitRepo](https://github.com/jwiegley/use-package) for the documentations.
-   See [quick tutorial](https://ianyepan.github.io/posts/setting-up-use-package/) for how to use `use-package`

`use-package` is a way to organize the code neat but itself is not a
pckage manager.

Few most common keywords:

-   `:init` : executing the keywords **before** the code is executed.
-   `:config`: executing codes after the package is loaded
-   `:command`: autoload the command following the load of the package and
    do something about the loaded command (if you are autoloading
    **non-interactive function**, use `:autoload` instead.
    -   when using `:command` keyword, it creates autoloads for the commands
        and **defers** their loading before they are acutally used.
-   `:bind`: it's a two steps procedure, first create the autoload for the
    command being called, then bind a key to that command.

    -   using `:bind ("key-binding" . command)` is equivalent to,

    `:command function :init (bind-key "key-binding" 'function)`

    -   The keywords takes list of **conses**.
    -   speicial key needs to be incoperated in `<>` such as `<tab>`.
    -   the `:bind` can work in conjunction with `remap`, use `:bind([remap fill-paragraph] . unfill-toggle)` , the command `fill-paragraph` is
        rebinded with `unfill-toggle`
    -   other variant of key-binding macro can be found in the keybinding
        section of the [git repo](https://github.com/jwiegley/use-package#key-binding).
-   `:hook` : when `(use-package A :hook B)`, it mean hook B onto A, the
    extended version is `:command A, :init (add-hook 'B #'A)`
-   `:preface` : used to establish function and variable defintion that
    will make byte-complier happy and allow you to define code taht can
    be used in an `:if` test. It's evaluated both at load time and
    byte-compilation time.

<!--listend-->

```emacs-lisp

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(add-to-list 'package-archives
						 '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)
(unless package-archive-contents
	(package-refresh-contents))

;; Initialize use-package on non-linux platform
(unless (package-installed-p 'use-package)
	(package-install 'use-package))

(require 'use-package)

;; ensures the package to be download from the package archives
;; if they have not already been downloaded. This is the global
;; version of :ensure t.
(setq use-package-always-ensure t)

```

-   More explanations on the code:
    -   `require` : require will load the library **once**. Also when data were
        being updated, require checks whether it needs to do
        anything. [Check here for more information](https://emacs.stackexchange.com/questions/22717/what-does-require-package-mean-for-emacs-and-how-does-it-differ-from-load-fil).
    -   `(setq package-archieves)` - add more source by **asigning values** to
        the **variable** `package-archieve`
        -   potential debugging options:
            -   use `customize-set-variable` instead of `setq`
            -   use `(add-to-list 'package-archieves '("melpa". "http://melpa.org/packge/"))`
                -   Here, `add-to-list` takes first arguments as a **symbol**, so we
                    have to put a `'` in front of `package-archieves` to tell the
                    program to treat it as a symbol.[^fn:1]
    -   `（package-initialize)` is used when running into undefined
        functions or variables, so as to set up the **load-path** and
        autoloads for installed package. Afterwards, using `(require
            'package-name)` to fully load the package.
    -   Use `use-package-always-ensure` to save trouble from including
        `:ensure t` line for all the individual packages.

To keep all the package up to date, use `auto-package-update` option:

```emacs-lisp
(use-package auto-package-update
	:config
	(setq auto-package-update-delete-old-versions t)
	(setq auto-package-update-hide-results t)
	(auto-package-update-maybe))
```


### straigt.el <span class="tag"><span class="download">download</span><span class="emacs">emacs</span></span> {#straigt-dot-el}

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2022-11-03 Thu 21:38] </span></span> <br />
    Better way of organizing the package, according to system crafter,
    better switch to straight.el once and for all to avoice weird
    behavior. Make sure to backup the current init.el first.

`straight.el` needs to be bootstrapped into the system. There's
motivation in using it because it makes life easier for installing
package from Git repo.

```emacs-lisp
	(defvar bootstrap-version)
	(let ((bootstrap-file
	(expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
	(bootstrap-version 5))
		(unless (file-exists-p bootstrap-file)
			(with-current-buffer
		(url-retrieve-synchronously
		"https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
		'silent 'inhibit-cookies)
	(goto-char (point-max))
	(eval-print-last-sexp)))
		(load bootstrap-file nil 'nomessage))

	(straight-use-package 'use-package)

(setq straight-use-package-by-default t)
```


## General {#general}


### Some general settings {#some-general-settings}

Auto-revert, save history etc to improve general usage

```emacs-lisp
;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;;save what you enter into minibuffer prompts
(setq history-length 25)
(savehist-mode 1)

;; Remember and restore the last cursor location of opened files
(save-place-mode 1)

;; Turn on auto-fill-mode so the paragraph doesn't get super long
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; Turn of the electric indent mode
(electric-indent-mode -1)
```

Using shit to switch between windows:

```emacs-lisp
(windmove-default-keybindings)
```

Use ibuffer to nevigate the buffers:

```emacs-lisp
;;use ibuffer
(defalias 'list-buffers 'ibuffer-other-window) ;;open another buffer window
```


### UI setting {#ui-setting}


#### Most general setting {#most-general-setting}

Stuff like get rid of the tool-bar, splashing lines etc.

```emacs-lisp
;; Don't show the splash screen
(setq inhibit-splash-screen t)
;; Don't show startup message
(setq inhibit-startup-message t)
;; don't flash when the bell rings
(setq visible-bell nil)
;; hide the tool-bar-mode
(tool-bar-mode -1)
;;diable the scrool bar
(scroll-bar-mode -1)
;;short form of yes or no
(fset 'yes-or-no-p 'y-or-n-p)
;;when displaying picture, don't display actual size(they can be huge)
(setq org-image-actual-width nil)
;;show line number on the left of the window
(global-display-line-numbers-mode 1)
;;store the recently opened files in order
(recentf-mode 1)
;; Don't pop up UI dialogs when prompting
(setq use-dialog-box nil)
;; The the global scale tab-width
(setq-default tab-width 2)
```

By defult, Mac use option for `meta` key, and `Command` for super-key, I'd like to swap the functionality of it:

```emacs-lisp
;; Set the option, command key to corresponding emacs key
	(setq mac-command-modifier      'meta
				mac-option-modifier       'super
				mac-right-option-modifier 'hyper)
```

Org-mode code block related setting:

```emacs-lisp
(setq   org-src-tab-acts-natively t
				org-confirm-babel-evaluate nil
				org-edit-src-content-indentation 0)
```

To disable the auto indentation in org-mode


#### Misc {#misc}

```emacs-lisp
;; flash cursor lines when scroll
(use-package beacon
	:config
	(beacon-mode 2))
```


#### Face {#face}

<!--list-separator-->

-  Font

    The `:height` stands for the height of the font, which also determines the size of the font.

    ```emacs-lisp
    ;; Set the default face to larger font.
    (set-face-attribute 'default nil :font "Fira Code" :height 180)

    ;; Set the fixed pitch face
    (set-face-attribute 'fixed-pitch nil :font "Fira Code" :height 180)

    ;; Set the variable pitch face
    (set-face-attribute 'variable-pitch nil :font "Fira Code" :height 180 :weight 'regular)
    ```

    Define a `font-setup` function

    ```emacs-lisp
    (defun org-font-setup ()
    	;; Set faces for heading levels
    	(dolist (face '((org-level-1 . 1.2)
    									(org-level-2 . 1.1)
    									(org-level-3 . 1.05)
    									(org-level-4 . 1.0)
    									(org-level-5 . 0.8)
    									(org-level-6 . 0.8)
    									(org-level-7 . 0.8)
    									(org-level-8 . 0.8)))
    		(set-face-attribute (car face) nil :font "Fira Code" :weight 'regular :height (cdr face)))

    	;; Ensure that anything that should be fixed-pitch in Org files appears that way
    	(set-face-attribute 'org-block nil :foreground nil  :inherit 'fixed-pitch)
    	(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
    	(set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
    	(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
    	(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    	(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    	(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

    (add-hook 'org-mode-hook 'org-font-setup)
    ```

<!--list-separator-->

-  Beautify

    ```emacs-lisp
    ;;replace list hyphen with dot
    (font-lock-add-keywords 'org-mode
    												'(("^ *\\([-]\\) "
    													 (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
    ;;add emphasis markets at the end of the list
    (setq org-ellipsis " ▼"
    			org-hide-emphasis-markers t)

    (use-package org-bullets
    	:hook
    	(org-mode . (lambda () (org-bullets-mode 1)))
    	(org-mode . (lambda ()
    							"Beautify Org Checkbox Symbol"
    							(push '("[ ]" . "☐" ) prettify-symbols-alist)
    							(push '("[X]" . "☑" ) prettify-symbols-alist)
    							(push '("[-]" . "⊡" ) prettify-symbols-alist)
    							(prettify-symbols-mode))))
    ```

<!--list-separator-->

-  Theme

    Use `:ensure` to make sure the `all-the-icons` package will be autoinstalled when `doom-themes` is installed.

    ```emacs-lisp
    (use-package doom-themes
    		 :ensure  all-the-icons
    		 :config
    		 (load-theme 'doom-one t)
    		 ;; all-the-icons has to be installed, enabling custom neotree theme
    		 (doom-themes-neotree-config)
    		 ;; for treemacs user
    		 (setq doom-themes-treemacs-theme "doom-atom")
    		 (doom-themes-treemacs-config)
    		 ;;conrrect the org-mode's native fontification
    		 (doom-themes-org-config))
    ```

    Also, use `doom-modeline` to prettify the mode-line section

    ```emacs-lisp
    (use-package doom-modeline
    	:ensure t
    	:init (doom-modeline-mode 1)
    	:hook (after-init . doom-modeline-mode)
    	:custom
    	(doom-modeline-height 10)
    	(doom-modeline-enable-word-count nil)
    	(doom-modeline-minor-modes t))
    (minions-mode 1)
    ```


### Helpful {#helpful}

```emacs-lisp
(use-package helpful
	:bind
	("C-h f" . helpful-callable)
	("C-h v" . helpful-variable)
	("C-h k" . helpful-key)
	("C-h o" . helpful-symbol))
```


### Completion {#completion}


#### Vertico {#vertico}

Minimalistic auto completion setting: `vertico` + `savehist` + `marginalia`

```emacs-lisp
;;enable Vertico
(use-package vertico
	:init
	(vertico-mode)

	;; Different scroll margin
	;; (setq vertico-scroll-margin 0)

	;; Show more candidates
	;; (setq vertico-count 20)

	;; Grow and shrink the Vertico minibuffer
	;; (setq vertico-resize t)

	;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
	;; (setq vertico-cycle t)
	)

(define-key vertico-map "?" #'minibuffer-completion-help)
(define-key vertico-map (kbd "M-RET") #'minibuffer-force-complete)
(define-key vertico-map (kbd "M-TAB") #'minibuffer-complete)

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
	:init
	(savehist-mode))

;; Show info of files at the marginal
(use-package marginalia
	:after vertico
	:ensure t
	:custom
	(marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
	:init
	(marginalia-mode))

;; Optionally use the `orderless' completion style, so no need to worry about the
;; order of keywords when trying to search for command.
(use-package orderless
	:init
	;; Configure a custom style dispatcher (see the Consult wiki)
	;; (setq orderless-style-dispatchers '(+orderless-dispatch)
	;;       orderless-component-separator #'orderless-escapable-split-on-space)
	(setq completion-styles '(orderless basic)
				completion-category-defaults nil
				completion-category-overrides '((file (styles partial-completion)))))
```


#### Company {#company}

```emacs-lisp
(use-package company
	:config
	(setq company-minimum-prefix-length 2)
	(setq company-idle-delay 0.25)
	(setq company-backends '(company-capf company-semantic company-keywords company-etags company-dabbrev-code))
	(global-company-mode)
	:bind
	("M-/" . company-active-map)
	(:map company-active-map
				("C-n" . company-select-next)
				("C-p" . company-select-previous)))
```


#### Pair {#pair}

```emacs-lisp
(use-package smartparens
	:config
	(smartparens-global-mode t))
```


#### Which-key {#which-key}

This package offer all the possible completions for the prefix.

```emacs-lisp
(use-package which-key
	:config (which-key-mode))
```


#### Expansion {#expansion}

The basic expansion comes in handy by using `org-tempo`

```emacs-lisp
(require  'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("ja" . "src java"))
(add-to-list 'org-structure-template-alist '("quo" . "quote"))
(add-to-list 'org-structure-template-alist '("ex" . "example"))
```


### Movement and editing {#movement-and-editing}


#### Consult {#consult}

```emacs-lisp
;;define prefix C-s for search map
(define-prefix-command 'search-map)
(global-set-key (kbd "C-s") 'search-map)

(use-package consult
	:bind
	("C-x b" . consult-buffer)
	(:map search-map
				("s" . consult-line)
				("l" . consult-goto-line)
				("o" . consult-outline)
				("S" . consult-line-multi)))
```


#### Multi-editing {#multi-editing}

```emacs-lisp
(use-package iedit)
```


#### Misc {#misc}

```emacs-lisp
;;expand region basiced semantics
(use-package expand-region
	:bind
	("C-=" . er/expand-region))
```


### Snippet {#snippet}

```emacs-lisp
(defun my-org-mode-hook ()
	(setq-local yas-buffer-local-condition
							'(not (org-in-src-block-p t))))

(use-package yasnippet
	:init
	(yas-global-mode 1)
	:bind
	("<TAB>" . yas-expand)
	:config
	(add-hook 'org-mode-hook 'my-org-mode-hook))

(use-package yasnippet-snippets)
```


## Org-roam {#org-roam}


### Basic config {#basic-config}

The straight version of org is not working, using straight to make sure using the built-in version

```emacs-lisp
(use-package org
	:straight (
		org :type built-in
	)
	:hook ((org-mode . org-font-setup)
				 (org-mode . turn-on-visual-line-mode))
	:bind
	("C-c a" . org-agenda)
	("C-c l"   . 'org-store-link)
	("C-c C-l"  . 'org-insert-link))
```

```emacs-lisp
;;The official one has deprecated, use self-defined one instead.
(defun org-roam-node-insert-immediate (arg &rest args)
	(interactive "P")
	(let ((args (cons arg args))
				(org-roam-capture-templates (list (append (car org-roam-capture-templates)
																									'(:immediate-finish t)))))
		(apply #'org-roam-node-insert args)))



(use-package org-roam
	:after org
	:config
	(org-roam-setup)
	:custom
	(org-roam-directory "~/Notes/RoamNotes")
	(org-roam-completion-everywhere t)
	(org-roam-file-extensions '("org" "md"))
	(org-roam-completion-system 'vertico)
	:bind (("C-c n l" . org-roam-buffer-toggle)
				 ("C-c n f" . org-roam-node-find)
				 ("C-c n i" . org-roam-node-insert)
				 ("C-c n I" . org-roam-node-insert-immediate)
				 ("C-M-i" . completion-at-point)
				 ("C-c n t" . org-roam-tag-add)
				 ("C-c n a" . org-roam-alias-add)))

;;add tag in the node-find mini-buffer
(setq org-roam-node-display-template
			(concat "${title:*} "
							(propertize "${tags:10}" 'face 'org-tag)))
```


### Org-download {#org-download}

```emacs-lisp
(use-package org-download
	:custom
	(org-download-method 'directory)
	(org-download-image-dir "~/Notes/static/images")
	(org-download-heading-lvl 0)
	(org-download-timestamp "org_%Y%m%d-%H%M%S_")
	(org-image-actual-width 900)
	(org-download-screenshot-method "xclip -selection clipboard -t image/png -o > '%s'")
	:bind
	("C-M-y" . org-download-clipboard))
```


### Org-noter + Pdf-tool {#org-noter-plus-pdf-tool}

```emacs-lisp
(pdf-tools-install)

(use-package org-noter)

(use-package org-pdftools
	:hook (org-mode . org-pdftools-setup-link))

(use-package org-noter-pdftools
	:after org-noter
	:config
	;; Add a function to ensure precise note is inserted
	(defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
		(interactive "P")
		(org-noter--with-valid-session
		 (let ((org-noter-insert-note-no-questions (if toggle-no-questions
																									 (not org-noter-insert-note-no-questions)
																								 org-noter-insert-note-no-questions))
					 (org-pdftools-use-isearch-link t)
					 (org-pdftools-use-freepointer-annot t))
			 (org-noter-insert-note (org-noter--get-precise-info)))))

	;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
	(defun org-noter-set-start-location (&optional arg)
		"When opening a session with this document, go to the current location.
With a prefix ARG, remove start location."
		(interactive "P")
		(org-noter--with-valid-session
		 (let ((inhibit-read-only t)
					 (ast (org-noter--parse-root))
					 (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
			 (with-current-buffer (org-noter--session-notes-buffer session)
				 (org-with-wide-buffer
					(goto-char (org-element-property :begin ast))
					(if arg
							(org-entry-delete nil org-noter-property-note-location)
						(org-entry-put nil org-noter-property-note-location
													 (org-noter--pretty-print-location location))))))))
	(with-eval-after-load 'pdf-annot
		(add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))
```


### Org-bibtex {#org-bibtex}


#### bibtex completion system {#bibtex-completion-system}

```emacs-lisp
;;download  org-ref
(use-package org-ref)

;;download helm completion
(use-package helm-bibtex
	:custom
	(bibtex-completion-bibliography '("/Users/zhouqiaohui/Documents/MyLibrary.bib"))
	(bibtex-completion-library-path '("~/Notes/RoamNotes/Paper"))
	(bibtex-completion-pdf-field "File")
	(bibtex-completion-notes-path "~/Notes/RoamNotes"))
```


#### org-roam-bibtex {#org-roam-bibtex}

This allows roam like citation backlink

```emacs-lisp
(use-package org-roam-bibtex
	:after org-roam)
```


#### template {#template}

```emacs-lisp
(setq orb-preformat-keywords
			'("citekey" "title" "url" "author-or-editor" "keywords" "file")
			orb-process-file-keyword t
			orb-attached-file-extensions '("pdf"))

(setq org-roam-capture-templates
			'(("r" "bibliography reference" plain
				 (file "~/Notes/RoamNotes/Templates/cite_temp.org")
				 :target
				 (file+head "${citekey}.org" "#+title: ${title}\n"))
				("t" "thought" plain
				 (file "~/Notes/RoamNotes/Templates/thought_temp.org")
				 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
				 :unnarrowed t)
				("d" "default" plain
				 "%?"
				 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
				 :unnarrowed t)
				))
```


#### other-setting {#other-setting}

Set global key for `helm-bibtex` and `org-noter`

```emacs-lisp
(global-set-key (kbd "C-c h b") #'helm-bibtex)
(global-set-key (kbd "C-c n o") #'org-noter)
(global-set-key (kbd "C-c h i") #'org-ref-insert-helm)
```

For the PDF Scrapper, change the formate of the paper key:

```emacs-lisp
(setq orb-autokey-format "%a%T[3]%y")
```


### Org-roam-ui {#org-roam-ui}

```emacs-lisp
(use-package org-roam-ui
		:after org-roam
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
		:config
		(setq org-roam-ui-sync-theme t
					org-roam-ui-follow t
					org-roam-ui-update-on-save t
					org-roam-ui-open-on-start t))
```


## Org-protocol {#org-protocol}

```emacs-lisp
(server-start)
(add-to-list 'load-path "~/.dotfiles/.files/.emacs.d/src/org-mode/lisp")
(require 'org-protocol)

(setq org-directory "~/Notes/")

(defun transform-square-brackets-to-round-ones(string-to-transform)
	"Transforms [ into ( and ] into ), other chars left unchanged."
	(concat
	 (mapcar #'(lambda (c) (if (equal c ?[) ?\( (if (equal c ?]) ?\) c))) string-to-transform))
	)

(setq org-capture-templates '(
															("p" "Protocol" entry (file+headline "~/Notes/captures.org" "Inbox")
															 "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
															("L" "Protocol Link" entry (file+headline "~/Notes/captures.org" "Link")
															 "* %? [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n")
															))

(global-set-key (kbd "C-c c") 'org-capture)
```


## Agenda {#agenda}


### Basic Setup {#basic-setup}

```emacs-lisp
(setq org-agenda-files (list "~/Notes/Agenda/dailylife.org"
														 "~/.dotfiles/Emacs.org"
														 "~/Notes/blogideas.org"
														 "~/Notes/Questions.org"
														 "~/Notes/RoamNotes/readinglists.org"))
;;Add progress logging to the org-agenda file
(setq org-log-done 'note)

;;Add some captures related to agenda
(add-to-list 'org-capture-templates '("t" "Todo" entry (file+headline "~/Notes/Agenda/dailylife.org" "Task")
																			 "* TODO %?\n %i\n"))

(add-to-list 'org-capture-templates '("b" "Blog Idea" plain
																			 (file+headline "~/Notes/blogideas.org" "Inbox")
																			 (file "~/Notes/RoamNotes/Templates/blog_temp.org")
																			))

;;set todo keywords
(setq org-todo-keywords
			'((sequence "TODO(t)" "|" "DONE(d)")
				(sequence "TOREAD(t)" "READING(r)" "|" "CANCELLED(c)" "STALLED(s)" "DONE(d)")
				(sequence "|" "CANCELED(c)")))

;;set faces
(setq org-todo-keyword-faces
			'(("TODO" . (:foreground "#ff39a3" :weight bold))
				("READING" . "yellow")
				("QUE" . (:foreground "red" :background "white"))))
```


### Super-agenda {#super-agenda}

```emacs-lisp
(use-package org-ql)
(use-package org-super-agenda
	:hook org-agenda-mode)

(setq org-super-agenda-groups
			'((:name "Priority"
							 :tag "priority")
				(:name "Reading List"
							 :file-path "~/Notes/RoamNotes/readinglists.org"
							 :todo "READING")
				(:name "Research"
							 :tag "research")
				(:name "Questions to answer"
							 :todo "QUE")
				(:name "Project"
							 :children t)))
```


## Editing {#editing}


### Grammar Check {#grammar-check}

```emacs-lisp
(use-package lsp-grammarly
	:hook (text-mode . (lambda ()
											 (require 'lsp-grammarly)
											 (lsp))))  ; or lsp-deferred
```


### Syntax Highlighting {#syntax-highlighting}

```emacs-lisp
(use-package rainbow-delimiters
	:hook (prog-mode . rainbow-delimiters-mode))
```


### Undo Tree {#undo-tree}

```emacs-lisp
(use-package undo-tree
:ensure t
:init
(global-undo-tree-mode))
```


### Magit {#magit}

```emacs-lisp
(use-package magit)
```


## Others {#others}


### Terminal {#terminal}


#### Eshell {#eshell}

```emacs-lisp

(use-package eshell-git-prompt)
(use-package eshell

:config
(eshell-git-prompt-use-theme 'powerline))
```


### Dired {#dired}

```emacs-lisp

(use-package all-the-icons-dired)
(use-package dired-rainbow
	:defer 2
	:config
	(dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
	(dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
	(dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
	(dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
	(dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
	(dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
	(dired-rainbow-define media "#de751f" ("mp3" "mp4" "mkv" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
	(dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
	(dired-rainbow-define log "#c17d11" ("log"))
	(dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
	(dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
	(dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
	(dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
	(dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
	(dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
	(dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
	(dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
	(dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
	(dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
	(dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*"))

(use-package dired-single
	:defer t)

(use-package dired-ranger
	:defer t)

(use-package dired-collapse
	:defer t)


(use-package dired-single)

(use-package all-the-icons-dired
	:hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
	:config
	;; Doesn't work as expected!
	;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
	(setq dired-open-extensions '(("png" . "feh")
																("mkv" . "mpv"))))
```


### Obsidian {#obsidian}

```emacs-lisp
(use-package obsidian
	:demand t
	:config
	(obsidian-specify-path "~/Library/Mobile Documents/com~apple~CloudDocs/Obsidian/Research")
	(global-obsidian-mode t)
	:custom
	;; This directory will be used for `obsidian-capture' if set.
	(obsidian-inbox-directory "Inbox")
	:bind (:map obsidian-mode-map
	;; Replace C-c C-o with Obsidian.el's implementation. It's ok to use another key binding.
	("C-c C-o" . obsidian-follow-link-at-point)
	;; Jump to backlinks
	("C-c C-b" . obsidian-backlink-jump)
	;; If you prefer you can use `obsidian-insert-link'
	("C-c C-l" . obsidian-insert-wikilink)))
```

link: <https://github.com/zzamboni/vita/>


### For File navigation {#for-file-navigation}

```emacs-lisp
(use-package deadgrep
	:bind
	("C-c n d" . deadgrep))
```


## Export {#export}


### Blogging with ox-hugo {#blogging-with-ox-hugo}

```emacs-lisp
(use-package ox-hugo
	:after ox)
(setq org-export-with-broken-links t)
```


### <span class="org-todo todo ONWATCH">ONWATCH</span> CV with Org-mode <span class="tag"><span class="download">download</span><span class="emacs">emacs</span></span> {#cv-with-org-mode}


## Keep .emacs.d clean {#keep-dot-emacs-dot-d-clean}

```emacs-lisp
;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
url-history-file (expand-file-name "url/history" user-emacs-directory))

;; Use no-littering to automatically set common paths to the new user-emacs-directory
(use-package no-littering)

;; Keep customization settings in a temporary file (thanks Ambrevar!)
(setq custom-file
(if (boundp 'server-socket-dir)
		(expand-file-name "custom.el" server-socket-dir)
	(expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)
```


## Auto Tangle {#auto-tangle}

```emacs-lisp
(defun joz/org-babel-tangle-config ()
	(when (string-equal (buffer-file-name)
		(expand-file-name "~/.dotfiles/Emacs_new.org"))
	(let ((org-confim-babel-evaluate t))
		(org-babel-tangle))))
```


## Personal Setting {#personal-setting}


### helper function {#helper-function}


#### Open configuration file {#open-configuration-file}

To quickly open my configuration org file. I have a alias setting in
my zshconfig too named `emacsconfig` which opens my `init.el` in VsCode,
allowing me to quickly edit my init files to open emacs correctly (I
am very bad at debug in emacs and I personally find this way easier).

```emacs-lisp
(defun joz/myconfig ()
	"open my personal config"
	(interactive)
	(switch-to-buffer (find-file-noselect "~/.dotfiles/Emacs.org")))
```


#### Open bookmark capture {#open-bookmark-capture}

Open captured information from browser:

```emacs-lisp
(defun joz/mycapture ()
	"Open my captued info from interent"
	(interactive)
	(switch-to-buffer (find-file-noselect "~/Notes/captures.org")))
```

[^fn:1]: `#'foo` and `'foo` are equivalent when `foo` is a symbol, but the
    former is prefered when `foo` is a function. `#'` is intended to be a
    function call. [More explanations here.](https://emacs.stackexchange.com/a/10943/36783)