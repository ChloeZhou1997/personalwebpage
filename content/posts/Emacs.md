+++
title = "Emacs Configuration"
author = ["Chloe"]
date = 2022-10-29
lastmod = 2022-11-08T14:40:23-05:00
tags = ["emacs", "config"]
draft = false
+++

This is the Emacs configuration built from scratch following [system crafter](https://www.youtube.com/watch?v=74zOY-vgkyw&list=PLEoMzSkcN8oPH1au7H6B7bBJ4ZO7BXjSZ&index=1&t=0s)
and [Mike Zamansky](https://www.youtube.com/watch?v=49kBWM3RQQ8&list=PL9KxKa8NpFxIcNQa9js7dQQIHc81b0-Xg)'s youtube videos. As someone with only limited
experience in running statistical models in R it's been a confusing
exprience to finally build vague understanding of what's going on
behind the scene.

This literal configuration, by the time the post was written is still
a patchwork of pieces I have taken from different places. My goal is
to taking notes of concepts and syntax along the way when reorganizing
the configuration file so that the documentation will be
self-contained and "dummy-friendly" for people like me who wants to
start using emacs to boost their productivities but have very little
idea of anything relates to programming at the beginning of their
journey.


## Package System Steup {#package-system-steup}


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


### <span class="org-todo todo ONWATCH">ONWATCH</span> straigt.el <span class="tag"><span class="_download">@download</span><span class="_emacs">@emacs</span></span> {#straigt-dot-el}

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2022-11-03 Thu 21:38] </span></span> <br />
    Better way of organizing the package, according to system crafter,
    better switch to straight.el once and for all to avoice weird
    behavior. Make sure to backup the current init.el first.


## General UI Improvement {#general-ui-improvement}


### Interface twick {#interface-twick}

Some easy changes like change the size of the welcome windows, set the
most common key-map, change annoying default settings and hide stuff
like tool bar and scroll bar.


#### Some general settings {#some-general-settings}

```emacs-lisp

;; Opening frame
(add-to-list 'default-frame-alist '(height . 200))
(add-to-list 'default-frame-alist '(width . 200))

;;set the option and command key to corresponding emacs key
(setq mac-command-modifier      'meta
			mac-option-modifier       'super
			mac-right-option-modifier 'hyper)


;; Don't show the splash screen
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
;; don't flash whent he bell rings
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


#### Indentation setting {#indentation-setting}

The electric indent-mode is a minor mode introduced after Emacs 24.1,
which will trigger reindentation by certain characters. This mode is
better used with care because it will break major mode such as <span class="underline">python</span>
and <span class="underline">org</span>. So turn the mode off by set the value to `-1`. Read more from
[here](https://emacsredux.com/blog/2013/03/29/automatic-electric-indentation/). Turn on `auto-fill-mode` so that the paragraph doesn't get super
long.

```emacs-lisp
(require 'org-indent)
(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))
(add-hook 'org-mode-hook 'turn-on-auto-fill)
```

More explanation on the code:

-   `fboundp` - querying the state of the lisp environment, asking whether
    a particular name is bound to a function or macro. [Read more from this post](https://blog.cneufeld.ca/2014/01/the-less-familiar-parts-of-lisp-for-beginners-fboundp/).


#### Face setting {#face-setting}

More on the **concept of face**

-   The <span class="underline">face attributes</span> determine the visual appearance of a face. A **face**

is a collection of graphical attributes for displaying text:
font. forground color, background color, optional underling etc. It
determins how **text is displayed in buffer**.

-   `set-face-attribute` is a face attribute function, which returns the

value of the _attribute_ (attribute for face on frame). It overrides the
face specs belonging to _face_.

-   The **standard faces** includes
    -   default : the face used for <span class="underline">ordinary text</span> that doesn't specify any
        face. It's color is used as frame's background color。
    -   fixed-pitch: this face forces use of a fixed-width font. Customize
        it to use a different fixed-width font.
    -   variable-pitch: forces use of a variable-width font
    -   There are also standard face such as `region`, `highlight`, `underline`
        which control the display of text.
-   Usually we use `defface` macro to define a face. The macro associate a

face name with a default _face spec_, a **construct** which specifies what
attributes a face should have on any given terminal. For example, a
face spec might specify one foreground color on high-color terminals
and a different forground color on low-color terminal.

```emacs-lisp

(set-face-attribute 'default nil :font "Fira Code" :height 180)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code" :height 180)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Fira Code" :height 180 :weight 'regular)

```


#### Mode line config {#mode-line-config}

A minimalist design for the modeline, see [repo](https://github.com/seagle0128/doom-modeline) for more informati

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


#### Add line number {#add-line-number}

```emacs-lisp

;;neivigating throught lines
(column-number-mode)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook))
	(add-hook 'mode (lambda ()(display-line-numbers-mode 0))))

```


#### Theme {#theme}

```emacs-lisp

(use-package doom-themes
	:ensure all-the-icons
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


### Functional twick {#functional-twick}


#### General Settings {#general-settings}

-   be able to view c source file
-   move customization variables to a seperate file and load it.
-   auto-revert-mode at global level
-   auto revert dired and other buffers
-   rememeber and restore the last cursor location of opened files

...

```emacs-lisp

;; Set the source-directory
(setq find-function-C-source-directory "~/emacs-28.2/src")

;; move customization variables to a separate file and load it
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;;save what you enter into minibuffer prompts
(setq history-length 25)
(savehist-mode 1)

;; Remember and restore the last cursor location of opened files
(save-place-mode 1)

```


#### Windows Nevigation {#windows-nevigation}

Be able to use shift to nevigate between different windows

```emacs-lisp
;;use shift left right up down to switch between windows
(windmove-default-keybindings)
```

Use `ace window` package so when calling M-o, can switch between windows
using number. ( This is a cool package but it's kind of redundant
because I won't open that many window at the same time anyway.)

```emacs-lisp
;; (Use-package ace-window
;; 	:ensure t
;; 	:init
;; 	(global-set-key [remap other-window] 'ace-window)
;; 	(custom-set-faces
;; 	 '(aw-leading-char-face
;; 		 ((t (:inherit ace-jump-face-foreground :height 3.0)))))
;; 	:config
;; 	(global-set-key (kbd "M-o") 'ace-window))
```

-   Both `:inherit` and `:height` are face attributes
-   The `:inherit` attribute determines the name of the face to be
    inherited from
    -   the inherited value will merge into the face like the underlying
        face do but have higher priority.


#### Buffer Nevigation {#buffer-nevigation}

Enabling dired like buffer management.

```emacs-lisp
;;ibuffer
(defalias 'list-buffers 'ibuffer-other-window) ;;open another buffer window
```

enabling **ido mode:**

-   Add flex match
-   be able to search files and buffer by typing key-words and hit &lt;TAB&gt;

<!--listend-->

```emacs-lisp
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
```

There are other completion system which will be configured later.


## Global function improvement {#global-function-improvement}


### Helpful {#helpful}

```emacs-lisp
	(use-package helpful)

;; Note that the built-in `describe-function' includes both functions
;; and macros. `helpful-function' is functions only, so we provide
;; `helpful-callable' as a drop-in replacement.
(global-set-key (kbd "C-h f") #'helpful-callable)

(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-h o") #'helpful-symbol)

(setq counsel-describe-function-function #'helpful-callable)
(setq counsel-describe-variable-function #'helpful-variable)
```


### Yasnippet {#yasnippet}


#### Basic setup {#basic-setup}

-   [Setting from Repo](https://github.com/MooersLab/configorg/blob/main/config.org)
    ```emacs-lisp
    (use-package yasnippet
    	:ensure t
    	:init
    	(yas-global-mode 1))

    ```


#### Insert snippet {#insert-snippet}

```emacs-lisp

(global-set-key "\C-o" 'yas-expand)

```


#### Tab trigger in org code blocks {#tab-trigger-in-org-code-blocks}

```emacs-lisp
(setq   org-src-tab-acts-natively t
				org-confirm-babel-evaluate nil
				org-edit-src-content-indentation 0)

```


#### Turn off org-mode snippets in code blocks {#turn-off-org-mode-snippets-in-code-blocks}

```emacs-lisp
(defun my-org-mode-hook ()
	(setq-local yas-buffer-local-condition
							'(not (org-in-src-block-p t))))
'my-org-mode-hook
(add-hook 'org-mode-hook `my-org-mode-hook)
```


#### Snippet pop up manue {#snippet-pop-up-manue}

```emacs-lisp
(use-package popup
	:ensure t)

;; add some shotcuts in popup menu mode
(define-key popup-menu-keymap (kbd "M-n") 'popup-next)
(define-key popup-menu-keymap (kbd "TAB") 'popup-next)
(define-key popup-menu-keymap (kbd "<tab>") 'popup-next)
(define-key popup-menu-keymap (kbd "<backtab>") 'popup-previous)
(define-key popup-menu-keymap (kbd "M-p") 'popup-previous)

(defun yas/popup-isearch-prompt (prompt choices &optional display-fn)
	(when (featurep 'popup)
		(popup-menu*
		 (mapcar
			(lambda (choice)
				(popup-make-item
				 (or (and display-fn (funcall display-fn choice))
						 choice)
				 :value choice))
			choices)
		 :prompt prompt
		 ;; start isearch mode immediately
		 :isearch t
		 )))
(setq yas/prompt-functions '(yas/popup-isearch-prompt yas/no-prompt))
```


### Keys Bindings {#keys-bindings}


#### Which-key {#which-key}

which-key is  a useful UI panel  that appears when you  start pressing
any key binding in Emacs to offer you all possible completions for the
prefix. For  example, if  you press  C-c (hold  control and  press the
letter c), a  panel will appear at the bottom  of the frame displaying
all of the bindings under that prefix and which command they run. This
is very useful  for learning the possible key bindings  in the mode of
your current buffer.

```emacs-lisp
(use-package which-key
	:config (which-key-mode))
```


### Misc packages {#misc-packages}

```emacs-lisp
	; Becon mode
	; flashes the cursor's line when you scroll
	(use-package beacon
		:ensure t
		:config
		(beacon-mode 2)
	; this color looks good for the zenburn theme but not for the one
	; I'm using for the videos
	; (setq beacon-color "#666600")
	)

	; Hungty Deleteo Mode
	; deletes all the whitespace when you hit backspace or delete
;;   (use-package hungry-delete
;;     :ensure t
;;     :config
;;     (global-hungry-delete-mode))


	; expand the marked region in semantic increments (negative prefix to reduce region)
	(use-package expand-region
		:ensure t
		:config
		(global-set-key (kbd "C-=") 'er/expand-region))

```


## Org-mode {#org-mode}

Org mode buffer need Font Lock to be turned on.


### Org-mode face setting {#org-mode-face-setting}


#### Org mode activation {#org-mode-activation}

Recall pacakge `use-package`, when setting key-binding using `:bind` in
conjunction with `:map`, which only binds the key locally when the
package has already been loaded. The key binding before `:map` are
global key bindings.

```emacs-lisp
(use-package org
	:hook ((org-mode . org-font-setup)
				 (org-mode . turn-on-visual-line-mode))
	:mode ("\\.org" . org-mode)
	:bind (("C-c a"   . 'org-agenda)
				 ("C-c b"   . 'org-switchb)
				 ("C-s-s"   . 'org-save-all-org-buffers)
				 ("C-c l"   . 'org-store-link)
				 ("C-c C-l"  . 'org-insert-link)
				 :map org-mode-map
				 ("s-."     . 'org-todo)
				 ("M-p"     . 'org-set-property)))

```


#### Beautify org roam {#beautify-org-roam}

```emacs-lisp
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


#### Font and List {#font-and-list}

The org-font-setup setup the font and also the list style at the end.

```emacs-lisp
(defun org-font-setup ()
	;; Replace list hyphen with dot
	(font-lock-add-keywords 'org-mode
													'(("^ *\\([-]\\) "
														 (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

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
	(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
	(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
	(set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
	(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
	(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
	(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
	(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

	(setq org-ellipsis " ▼"
				org-hide-emphasis-markers t))

(add-hook 'org-mode-hook 'org-font-setup)
```


### Babel Setting {#babel-setting}

```emacs-lisp

(setq org-babel-load-languages
			'((awk        . t)
				(calc       . t)
				(css        . t)
				(ditaa      . t)
				(emacs-lisp . t)
				(gnuplot    . t)
				(haskell    . t)
				(js         . t)
				(lisp       . t)
				(org        . t)
				(plantuml   . t)
				(python     . t)
				(scheme     . t)
				(shell      . t)
				(sql        . t)
				(java				. t)))

;; Activate Babel languages
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)

;; Cancel Confirmation
(setq org-confirm-babel-evaluate nil
			org-src-fontify-natively t
			org-src-tab-acts-natively t)

```


#### Python autocompletion {#python-autocompletion}

```emacs-lisp
(use-package jedi
	:ensure t
	:init
	(add-hook 'python-mode-hook 'jedi:setup)
	(add-hook 'python-mode-hook 'jedi:ac-setup))
```


#### Strcture Template {#strcture-template}

```emacs-lisp

;;quick parser
;;be aware here use-pacakges won't work
(require  'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("ja" . "src java"))
(add-to-list 'org-structure-template-alist '("quo" . "src quote"))
(add-to-list 'org-structure-template-alist '("ex" . "src example"))
```


### Org-roam {#org-roam}

-   Org-roam v2 doesn't recognize `file:` link but only recognizes files
    and headings with ID.
-   `org-roam-mode` is no longer a global minor mode


#### Basic Config {#basic-config}

-   The template property:
-   `:immediate-finish` : do not offer to edit the information, just file
    it away immediately. Makes sense if the template only needs
    information that can be added automatically.
    ```emacs-lisp
    			(use-package org-roam
    				:after org
    				:config
    				(org-roam-setup)
    				:custom
    				(org-roam-directory "~/Notes/RoamNotes")
    				(org-roam-completion-everywhere nil)
    				:bind (("C-c n l" . org-roam-buffer-toggle)
    							 ("C-c n f" . org-roam-node-find)
    							 ("C-c n i" . org-roam-node-insert)
    							 ("C-c n I" . org-roam-node-insert-immediate)
    							 :map org-mode-map
    							 ("C-M-i" . completion-at-point)
    							 ("C-c n t" . org-roam-tag-add)
    							 ("C-c n a" . org-roam-alias-add)))

    	 (setq org-roam-completion-system 'ivy)

    ;;The official one has deprecated, use self-defined one instead.
    (defun org-roam-node-insert-immediate (arg &rest args)
    	(interactive "P")
    	(let ((args (cons arg args))
    				(org-roam-capture-templates (list (append (car org-roam-capture-templates)
    																									'(:immediate-finish t)))))
    		(apply #'org-roam-node-insert args)))

    ;;add tag in the node-find mini-buffer
    (setq org-roam-node-display-template
    			(concat "${title:*} "
    							(propertize "${tags:10}" 'face 'org-tag)))
    ```


#### Org-download {#org-download}

I use org-download to copy paste images online and show in org-mode,
in doing so, download the `pngpaste` from Homebrew and then bind the
`org-download-clipboard` to `C-M-y`. Except for that, the
`org-download-screeshot-method` won't work as expected. The solution is
taken from [here](https://github.com/abo-abo/org-download/issues/131#issuecomment-702236082).

```emacs-lisp
(use-package org-download
	:after org
	:defer nil
	:custom
	(org-download-method 'directory)
	(org-download-image-dir "~/Notes/img")
	(org-download-heading-lvl 0)
	(org-download-timestamp "org_%Y%m%d-%H%M%S_")
	(org-image-actual-width 900)
	(org-download-screenshot-method "xclip -selection clipboard -t image/png -o > '%s'")
	:bind
	("C-M-y" . org-download-clipboard)
	:config
	(require 'org-download))
```


#### Org-noter {#org-noter}

Pre-requisite: `pdf-tools`

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


#### Org-roam-bibitex {#org-roam-bibitex}

In order to make all the functionality work, need three packages to
coordinate: `Org-roam`, `bibtex-completion (help-bibtex & ivy-bibtex)`,
`org-ref`.

<!--list-separator-->

-  Installing org-roam-bibtex and hard dependencies

    -   `Bibtex-completion` allows one to access your reference from anywhere
    -   Org-ref allows one to insert `'cite:'` links into the Org-mode buffer.

    <!--listend-->

    ```emacs-lisp
    (use-package helm-bibtex)
    (use-package org-ref)
    ```

    The minimalist configuration

    ```emacs-lisp
    (setq bibtex-completion-bibliography
    			'("/Users/zhouqiaohui/Documents/MyLibrary.bib"))
    ```

    In bibtex, the bibtexcompletion will search for pdf files that have
    the suffix same as the BibTex key entry.

    ```emacs-lisp
    (setq bibtex-completion-library-path '("~/Notes/RoamNotes/Paper"))
    (setq bibtex-completion-pdf-field "File")
    ```

    If one file per publication is preferred, bibtex-completion-notes-path
    should point to the directory used for storing the notes files:

    ```emacs-lisp
    (setq bibtex-completion-notes-path "~/Notes/RoamNotes")
    ```

<!--list-separator-->

-  Installing soft dependencies

    -   Citar (Yet I don't think it's used...)

    This package provides a completing-read front-end to browse and act on
    BibTeX, BibLaTeX, and CSL JSON bibliographic data, and LaTeX,
    markdown, and org-cite editing support.

    When used with vertico, embark, and marginalia, it provides similar
    functionality to helm-bibtex and ivy-bibtex: quick filtering and
    selecting of bibliographic entries from the minibuffer, and the option
    to run different commands against them.

    See the [repo](https://github.com/emacs-citar/citar) here.

    ```emacs-lisp
    (use-package citar
    	:no-require
    	:custom
    	(org-cite-global-bibliography '("/Users/zhouqiaohui/Documents/MyLibrary.bib"))
    	(org-cite-insert-processor 'citar)
    	(org-cite-follow-processor 'citar)
    	(org-cite-activate-processor 'citar)
    	(citar-bibliography org-cite-global-bibliography)
    	;; optional: org-cite-insert is also bound to C-c C-x C-@
    	:bind
    	(:map org-mode-map :package org ("C-c b" . #'org-cite-insert)))
    ```

    This integrate directly with Org-Roam:

    -   multiple reference per note,
    -   multiple reference notes per file
    -   query note citation by reference
    -   live aupdating Citar UI for presence of notes

    <!--listend-->

    ```emacs-lisp
    (use-package citar-org-roam
    	:after citar org-roam
    	:no-require
    	:config (citar-org-roam-mode))
    ```

<!--list-separator-->

-  Installing org-roam-bibtex itself

    ```emacs-lisp
    (use-package org-roam-bibtex
    	:after org-roam
    	:config
    	(require 'org-ref)) ; optional: if using Org-ref v2 or v3 citation links
    ```

<!--list-separator-->

-  Templates integrating with bibtex

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

    Note action interface:

    ```emacs-lisp
    (setq orb-note-actions-interface 'helm)
    ```

<!--list-separator-->

-  Other Global Setting

    Up to this point, all the citations and backlink are correctly set up.

    ```emacs-lisp
    (org-roam-db-autosync-mode 1)
    (org-roam-bibtex-mode 1)
    ```

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


### Org-protocol {#org-protocol}

First configuring `org-protocol` and download [chrome extension](https://github.com/sprig/org-capture-extension).

```emacs-lisp
(server-start)
(add-to-list 'load-path "~/.dotfiles/.files/.emacs.d/src/org-mode/lisp")
(require 'org-protocol)
```

Then configuring capture template accordingly

```emacs-lisp
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
```


#### CANCELED Org-protocol-capture-html <span class="tag"><span class="ARCHIVE">ARCHIVE</span><span class="_download">@download</span></span> {#canceled-org-protocol-capture-html}


### Agenda {#agenda}


#### Basic Setup {#basic-setup}

```emacs-lisp
(setq org-agenda-files (list "~/Notes/Agenda/dailylife.org"
														 "~/.dotfiles/Emacs.org"
														 "~/Notes/blogideas.org"))
;;Add progress logging to the org-agenda file
(setq org-log-done 'note)

;;Add some captures related to agenda
(add-to-list 'org-capture-templates '("t" "Todo" entry (file+headline "~/Notes/Agenda/dailylife.org" "Task")
																			 "* TODO %?\n %i\n"))

(add-to-list 'org-capture-templates '("b" "Blog Idea" plain
																			 (file+headline "~/Notes/blogideas.org" "Inbox")
																			 (file "~/Notes/RoamNotes/Templates/blog_temp.org")
																			))
```


#### Super-agenda {#super-agenda}

<!--list-separator-->

- <span class="org-todo todo REVIEW">REVIEW</span>  Config super-agenda according to [git repo](https://github.com/alphapapa/org-super-agenda#screenshots) to make it looks nicer. <span class="tag"><span class="_emacs">@emacs</span><span class="_review">@review</span></span>

    ```emacs-lisp
    (use-package org-super-agenda)
    ```


### Html or Markdown Preview {#html-or-markdown-preview}


#### `.html` preview {#dot-html-preview}

[GitHub Repo](https://github.com/jakebox/org-preview-html)

```emacs-lisp

(use-package org-preview-html)

;; Set the default browser to xwidget
(setq-default org-preview-html-viewer 'xwidget)
(setq-default org-preview-html-refresh-configuration 'save)
```


#### `.md` Preview {#dot-md-preview}

[Github Repo](https://github.com/seagle0128/grip-mode/tree/e1e8ee952f75cdca93327b6e7dcd79244ca66bc0#limitations)

```emacs-lisp
(use-package grip-mode)
```


### For File navigation {#for-file-navigation}

I saw good reviews of deadgrep the other day so want to give it a
try... ( but I don't know how to use this yet )

```emacs-lisp
(use-package deadgrep)
(global-set-key (kbd "<f2>") #'deadgrep)
```


### <span class="org-todo todo ONWATCH">ONWATCH</span> CV with Org-mode <span class="tag"><span class="_download">@download</span><span class="_emacs">@emacs</span></span> {#cv-with-org-mode}

link: <https://github.com/zzamboni/vita/>


## Editor {#editor}


### Completion {#completion}


#### Ivy and counsel {#ivy-and-counsel}

```emacs-lisp

(use-package ivy
	:diminish
	:bind (("C-s" . swiper)
				 :map ivy-minibuffer-map
				 ("TAB" . ivy-alt-done)
				 ("C-l" . ivy-alt-done)
				 ("C-j" . ivy-next-line)
				 ("C-k" . ivy-previous-line)
				 :map ivy-switch-buffer-map
				 ("C-k" . ivy-previous-line)
				 ("C-l" . ivy-done)
				 ("C-d" . ivy-switch-buffer-kill)
				 :map ivy-reverse-i-search-map
				 ("C-k" . ivy-previous-line)
				 ("C-d" . ivy-reverse-i-search-kill))
	:config
	(ivy-mode 1))

(use-package ivy-rich
	:init
	(ivy-rich-mode 1))

(use-package counsel
	:bind (("C-M-j" . 'counsel-switch-buffer)
				 :map minibuffer-local-map
				 ("C-r" . 'counsel-minibuffer-history))
	:config
	(counsel-mode 1))
```

Swiper makes in-files search easier:

```emacs-lisp
(use-package swiper
	:config
		(ivy-mode)
		(setq ivy-use-virtual-buffers t)
		(setq enable-recursive-minibuffers t)
		;; enable this if you want `swiper' to use it
		;; (setq search-default-mode #'char-fold-to-regexp)
		:bind
		("\C-s" . swiper)
		("C-c C-r" . ivy-resume)
		("M-x" . counsel-M-x))


```


#### Vertico {#vertico}

light-weighted, integrating with built in emacs completion engine

```emacs-lisp
(use-package vertico
	:ensure t
	:bind (:map vertico-map
							("C-j" . vertico-next)
							("C-k" . vertico-previous)
							("C-f" . vertico-exit)
							:map minibuffer-local-map
							("M-h" . backward-kill-word))
	:custom
	(vertico-cycle t)
	:init
	(vertico-mode))

(use-package savehist
	:init
	(savehist-mode))

(use-package marginalia
	:after vertico
	:ensure t
	:custom
	(marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
	:init
	(marginalia-mode))
```


#### Avy - jump to a word {#avy-jump-to-a-word}

```emacs-lisp

;; another powerful search tool
(use-package avy
	:ensure t
	:bind ("M-s" . avy-goto-char))

```


#### Auto-completion {#auto-completion}

```emacs-lisp
;;auto-completion
(use-package auto-complete
	:ensure t
	:init
	(ac-config-default)
	:config
	(global-auto-complete-mode t)
	(setq ac-auto-show-menu 0.5))

(set-face-underline 'ac-candidate-face "darkgray")

```


#### Multiple editing {#multiple-editing}

Using Iedit: This package includes Emacs minor modes (iedit-mode and
iedit-rectangle-mode) based on a API library (iedit-lib) and allows
you to alter one occurrence of some text in a buffer (possibly
narrowed) or region, and simultaneously have other occurrences changed
in the same way, with visual feedback as you type.

```emacs-lisp
(use-package iedit)
```


#### (experiment)Blink {#experiment--blink}

[see repo](https://github.com/manateelazycat/blink-search)

```emacs-lisp
(add-to-list 'load-path "~/.dotfiles/.files/.emacs.d/blink-search")
(require 'blink-search)
```


### Flycheck {#flycheck}

```emacs-lisp
(use-package flycheck
	:ensure t
	:init
	(global-flycheck-mode t))
```


### Grammar Check {#grammar-check}

```emacs-lisp
(use-package lsp-grammarly
	:hook (text-mode . (lambda ()
											 (require 'lsp-grammarly)
											 (lsp))))  ; or lsp-deferred
```


#### <span class="org-todo done DONE">DONE</span> lsp-grammarly <span class="tag"><span class="_download">@download</span><span class="_emacs">@emacs</span></span> {#lsp-grammarly}

-   State "DONE"       from "TODO"       <span class="timestamp-wrapper"><span class="timestamp">[2022-11-06 Sun 12:37] </span></span> <br />
    `s-l a a` - select from options of actions

link:<https://github.com/emacs-grammarly/lsp-grammarly>


### Syntax Highlighting {#syntax-highlighting}

```emacs-lisp
(use-package rainbow-delimiters
	:hook (prog-mode . rainbow-delimiters-mode))
```


### Terminal {#terminal}


#### Eshell {#eshell}

```emacs-lisp

(use-package eshell-git-prompt)
(use-package eshell

:config
(eshell-git-prompt-use-theme 'powerline))
```


### Undo Tree {#undo-tree}

```emacs-lisp
(use-package undo-tree
:ensure t
:init
(global-undo-tree-mode))
```


### Markdown mode {#markdown-mode}

```emacs-lisp

(use-package markdown-mode
	:ensure t
	:mode ("README\\.md\\'" . gfm-mode)
	:init (setq markdown-command "multimarkdown"))

```


## File management {#file-management}


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


## Export {#export}


### Blogging with ox-hugo {#blogging-with-ox-hugo}

```emacs-lisp
(use-package ox-hugo
	:after ox)
(setq org-export-with-broken-links t)
```


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
(defun efs/org-babel-tangle-config ()
	(when (string-equal (buffer-file-name)
		(expand-file-name "~/.dotfiles/Emacs.org"))
	(let ((org-confim-babel-evaluate t))
		(org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))
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