+++
title = "Emacs Configuration"
author = ["Chloe"]
date = 2022-10-29
lastmod = 2022-10-30T02:09:49-04:00
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


## General Improvement of UI {#general-improvement-of-ui}


### Interface twick {#interface-twick}

Some easy changes like change the size of the welcome windows, set the
most common key-map, change annoying default settings and hide stuff
like tool bar and scroll bar.

```emacs-lisp

;; Opening frame
(add-to-list 'default-frame-alist '(height . 200))
(add-to-list 'default-frame-alist '(width . 200))

;;set the option and command key to corresponding emacs key
(setq mac-command-modifier      'meta
			mac-option-modifier       'alt
			mac-right-option-modifier 'alt)


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

(require 'org-indent)
(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))
(add-hook 'org-mode-hook 'turn-on-auto-fill)



```


### Theme {#theme}

```emacs-lisp

;;dependencies: all-the-icons
(use-package all-the-icons)

(use-package doom-themes
	:ensure t
	:config
	(load-theme 'doom-one t)

	(doom-themes-neotree-config)
	(setq doom-themes-treemacs-theme "doom-atom")
	(doom-themes-treemacs-config)
	(doom-themes-org-config))

```


### Mode line Config {#mode-line-config}


#### Doom-modeline {#doom-modeline}

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


### Line number {#line-number}

```emacs-lisp

;;neivigating throught lines
(column-number-mode)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook))
	(add-hook 'mode (lambda ()(display-line-numbers-mode 0))))

```


### Buffer improvement {#buffer-improvement}


#### Enabling using buffer like dired {#enabling-using-buffer-like-dired}

```emacs-lisp
;;ibuffer
(defalias 'list-buffers 'ibuffer-other-window)
```


#### ido mode {#ido-mode}

-   Add flex match
-   be able to search files and buffer by typing key-words and hit &lt;TAB&gt;

<!--listend-->

```emacs-lisp
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
```


### Windows nevigation {#windows-nevigation}

```emacs-lisp
;;use shift left right up down to switch between windows
(windmove-default-keybindings)
```


#### Ace window {#ace-window}

Switch window option, when calling M-o, can switch between windows
using number.

```emacs-lisp
(use-package ace-window
	:ensure t
	:init
	(progn
		(global-set-key [remap other-window] 'ace-window)
		(custom-set-faces
		 '(aw-leading-char-face
			 ((t (:inherit ace-jump-face-foreground :height 3.0)))))
		))

(global-set-key (kbd "M-o") 'ace-window)

```


## Important Global Steup {#important-global-steup}

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


## Org-mode setting {#org-mode-setting}


### Theme and UI setting {#theme-and-ui-setting}


#### Org mode list changes {#org-mode-list-changes}

```emacs-lisp
(use-package org
	:config
	(setq org-ellipsis " ▼"
				org-hide-emphasis-markers t))
```


#### Bullet heading {#bullet-heading}

```emacs-lisp
(use-package org-bullets
	:ensure t
	:config
	(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
```


#### Level heading font size {#level-heading-font-size}

```emacs-lisp

		 (set-face-attribute 'default nil :font "Fira Code" :height 180)

		 ;; Set the fixed pitch face
		 (set-face-attribute 'fixed-pitch nil :font "Fira Code" :height 180)

		 ;; Set the variable pitch face
		 (set-face-attribute 'variable-pitch nil :font "Fira Code" :height 180 :weight 'regular)


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
									(org-level-5 . 1.1)
									(org-level-6 . 1.1)
									(org-level-7 . 1.1)
									(org-level-8 . 1.1)))
		(set-face-attribute (car face) nil :font "Fira Code" :weight 'regular :height (cdr face)))

	;; Ensure that anything that should be fixed-pitch in Org files appears that way
	(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
	(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
	(set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
	(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
	(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
	(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
	(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

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


### Strcture Template {#strcture-template}

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


#### <span class="org-todo todo TODO">TODO</span> remove all the invocation of `org-roam-mode` in the source block, use `org-roam-db-auto-mode` to toggle the org-roam-mode {#remove-all-the-invocation-of-org-roam-mode-in-the-source-block-use-org-roam-db-auto-mode-to-toggle-the-org-roam-mode}


#### Basic Config {#basic-config}

```emacs-lisp
(use-package org-roam
	:init
	(setq org-roam-v2-ack t)
	(setq org-roam-mode-section-functions
				(list #'org-roam-backlinks-section
							#'org-roam-reflinks-section
							;; #'org-roam-unlinked-references-section
							))
	:custom
	(org-roam-directory "~/Notes")
	(org-roam-db-location "~/Notes/org-roam.db")
	(org-roam-completion-everywhere t)
	(org-roam-capture-templates
	 '(("d" "default" plain "%?"
			:if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
			:unnarrowed t)
		 ("b" "book notes" plain
			(file "~/Notes/Roam/Templates/BookNote.org")
			:if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
			:unnarrowed t)
		 ("p" "programming notes" plain
			(file "~/Notes/Roam/Templates/ProgrammingNotes.org")
			:if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Programming")
			:unnarrowed t)
		 ("l" "literature notes" plain
			(file "~/Notes/Roam/Templates/LiteratureNotes.org")
			:if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Programming")
			)
		 ))
	:bind (("C-c n l" . org-roam-buffer-toggle)
				 ("C-c n f" . org-roam-node-find)
				 ("C-c n i" . org-roam-node-insert)
				 ("C-c n g"   . org-roam-graph)
				 :map org-mode-map
				 ("C-M-i"   . completion-at-point)
				 ("C-c n I" . org-roam-node-insert-immediate))
	:config
	(org-roam-setup))

(defun org-roam-node-insert-immediate (arg &rest args)
	(interactive "P")
	(let ((args (cons arg args))
				(org-roam-capture-templates (list (append (car org-roam-capture-templates)
																									'(:immediate-finish t)))))
		(apply #'org-roam-node-insert args)))
```


#### For publishing {#for-publishing}

```emacs-lisp
(defun roam-sitemap (title list)
	(concat "#+OPTIONS: ^:nil author:nil html-postamble:nil\n"
					"#+SETUPFILE: ./simple_inline.theme\n"
					"#+TITLE: " title "\n\n"
					(org-list-to-org list) "\nfile:sitemap.svg"))

(setq my-publish-time 0)   ; see the next section for context
(defun roam-publication-wrapper (plist filename pubdir)
	(org-roam-graph)
	(org-html-publish-to-html plist filename pubdir)
	(setq my-publish-time (cadr (current-time))))

(setq org-publish-project-alist
	'(("roam"
		 :base-directory "~/Notes/Roam"
		 :auto-sitemap t
		 :sitemap-function roam-sitemap
		 :sitemap-title "Roam notes"
		 :publishing-function roam-publication-wrapper
		 :publishing-directory "~/roam-export"
		 :section-number nil
		 :table-of-contents nil
		 :style "<link rel=\"stylesheet\" href=\"../other/mystyle.cs\" type=\"text/css\">")))
```


### Html or Markdown Preview {#html-or-markdown-preview}

<!--list-separator-->

-  `.html` preview

    [GitHub Repo](https://github.com/jakebox/org-preview-html)

    ```emacs-lisp

    (use-package org-preview-html)

    ;; Set the default browser to xwidget
    (setq-default org-preview-html-viewer 'xwidget)
    (setq-default org-preview-html-refresh-configuration 'save)
    ```

<!--list-separator-->

-  `.md` Preview

    [Github
    Repo](https://github.com/seagle0128/grip-mode/tree/e1e8ee952f75cdca93327b6e7dcd79244ca66bc0#limitations)

    ```emacs-lisp
    (use-package grip-mode)
    ```


### For Research {#for-research}


#### Org-Brain {#org-brain}

[More Setting Availiable in GitRepo](https://github.com/Kungsgeten/org-brain)

```emacs-lisp
(use-package org-brain
	:ensure t
	:init
	(setq org-brain-path "~/Notes/")
	:config
	(bind-key "C-c b" "~/Notes")
	(setq org-id-track-globally t)
	(setq org-id-locations-file "~/.emacs.d/.org-id-locations")
	(add-hook 'before-save-hook 'org-brain-ensure-ids-in-buffer)
	(setq org-brain-visualize-default-choices 'all)
	(setq org-brain-title-max-length 12)
	(setq org-brain-include-file-entries nil
	org-brain-file-entries-use-title nil))

;; Allows you to edit entries directly from org-brain-visualize
(use-package polymode
	:config
	(add-hook 'org-brain-visualize-mode-hook #'org-brain-polymode))
```


## Keys Bindings {#keys-bindings}


### Global Key Bindings {#global-key-bindings}

```emacs-lisp
	(global-set-key (kbd "<f5>") 'revert-buffer)
	(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
	(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)

;;org-mode related

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c C-l") 'org-insert-link)

```


### Which-key {#which-key}

which-key is  a useful UI panel  that appears when you  start pressing
any key binding in Emacs to offer you all possible completions for the
prefix. For  example, if  you press  C-c (hold  control and  press the
letter c), a  panel will appear at the bottom  of the frame displaying
all of the bindings under that prefix and which command they run. This
is very useful  for learning the possible key bindings  in the mode of
your current buffer.

```emacs-lisp
(use-package which-key
	:ensure t
	:config (which-key-mode))
```


### General (not using) {#general--not-using}

```emacs-lisp
;; (use-package general
;; 	:ensure t
;; 	:config
;; 	(general-create-definer rune/leader-keys
;; 		:keymaps '(normal insert visual emacs)
;; 		:prefix "SPC"
;; 		:global-prefix "C-SPC")
;; 	(rune/leader-keys
;; 		"t" '(:ignore t :which-key "toglles")
;; 		"tt" '(counsel-load-theme :which-key "choose theme")))
```


## HashTag {#hashtag}

```emacs-lisp
(use-package deft
	:commands (deft)
	:config (setq deft-directory "~/Notes/Roam"
		deft-recursive t
		deft-extensions '("md" "org"))
	:bind
	("C-c n d" . deft)
	:custom
	(deft-recursive t)
	(deft-use-filter-string-for-filename t)
	(deft-default-extension "org")
	(deft-directory org-roam-directory))
```


## Editor {#editor}


### Flycheck {#flycheck}

```emacs-lisp
(use-package flycheck
	:ensure t
	:init
	(global-flycheck-mode t))
```


### Syntax Highlighting {#syntax-highlighting}

```emacs-lisp
(use-package rainbow-delimiters
	:hook (prog-mode . rainbow-delimiters-mode))
```


### Auto-completion {#auto-completion}

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


### Terminal {#terminal}


#### Eshell {#eshell}

```emacs-lisp

(use-package eshell-git-prompt)

(use-package eshell

:config
(eshell-git-prompt-use-theme 'powerline))
```


### Searching {#searching}


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

counsel, ivy and swiper usually come tgh, and is a useful completion framework.

```emacs-lisp
(use-package swiper
	:ensure try
	:config
	(progn
		(ivy-mode)
		(setq ivy-use-virtual-buffers t)
		(setq enable-recursive-minibuffers t)
		;; enable this if you want `swiper' to use it
		;; (setq search-default-mode #'char-fold-to-regexp)
		(global-set-key "\C-s" 'swiper)
		(global-set-key (kbd "C-c C-r") 'ivy-resume)
		(global-set-key (kbd "<f6>") 'ivy-resume)
		(global-set-key (kbd "M-x") 'counsel-M-x)
		(global-set-key (kbd "C-x C-f") 'counsel-find-file)
		(global-set-key (kbd "<f1> f") 'counsel-describe-function)
		(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
		(global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
		(global-set-key (kbd "<f1> l") 'counsel-find-library)
		(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
		(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
		(global-set-key (kbd "C-c g") 'counsel-git)
		(global-set-key (kbd "C-c j") 'counsel-git-grep)
		(global-set-key (kbd "C-c k") 'counsel-ag)
		(global-set-key (kbd "C-x l") 'counsel-locate)
		(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
		(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
		))


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


## Yasnippet {#yasnippet}

-   [Setting from Repo](https://github.com/MooersLab/configorg/blob/main/config.org)
    ```emacs-lisp
    (use-package yasnippet
    	:ensure t
    	:init
    	(yas-global-mode 1))

    ```


### Insert snippet {#insert-snippet}

```emacs-lisp

(global-set-key "\C-o" 'yas-expand)

```


### Tab trigger in org code blocks {#tab-trigger-in-org-code-blocks}

```emacs-lisp
(setq   org-src-tab-acts-natively t
				org-confirm-babel-evaluate nil
				org-edit-src-content-indentation 0)

```


### Turn off org-mode snippets in code blocks {#turn-off-org-mode-snippets-in-code-blocks}

```emacs-lisp
(defun my-org-mode-hook ()
	(setq-local yas-buffer-local-condition
							'(not (org-in-src-block-p t))))
'my-org-mode-hook
(add-hook 'org-mode-hook `my-org-mode-hook)
```


### Snippet pop up manue {#snippet-pop-up-manue}

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


## Reveal.js {#reveal-dot-js}

```emacs-lisp
(use-package ox-reveal
	:ensure ox-reveal)

(setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
(setq org-reveal-mathjax t)

(use-package htmlize
	:ensure t)
```


## Package Management {#package-management}


### Try {#try}

```emacs-lisp
(use-package try
	:ensure t)
```


## Misc packages {#misc-packages}

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
(use-package hungry-delete
	:ensure t
	:config
	(global-hungry-delete-mode))


; expand the marked region in semantic increments (negative prefix to reduce region)
(use-package expand-region
	:ensure t
	:config
	(global-set-key (kbd "C-=") 'er/expand-region))


(use-package iedit
	:ensure t)

```


## Project Management {#project-management}

```emacs-lisp

(use-package projectile
	:diminish projectile-mode
	:config (projectile-mode)
	:custom ((projectile-completion-system 'ivy))
	:bind-keymap
	("C-c p" . projectile-command-map)
	:init
	;; NOTE: Set this to the folder where you keep your Git repos!
	(when (file-directory-p "~/Projects/Code")
		(setq projectile-project-search-path '("~/Projects/Code")))
	(setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
	:config (counsel-projectile-mode))

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


## Blogging with ox-hugo {#blogging-with-ox-hugo}

```emacs-lisp
(use-package ox-hugo
	:ensure t            ;Auto-install the package from Melpa (optional)
	:after ox)

(setq org-export-with-broken-links t)
```


## Helpful {#helpful}

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


## Auto Tangle {#auto-tangle}

```emacs-lisp
(defun efs/org-babel-tangle-config ()
	(when (string-equal (buffer-file-name)
		(expand-file-name "~/.dotfiles/Emacs.org"))
	(let ((org-confim-babel-evaluate nil))
		(org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))
```


## Personal Setting {#personal-setting}


### dotcrafter {#dotcrafter}

```emacs-lisp

(load-file "/Users/zhouqiaohui/.dotfiles/.files/.emacs.d/dotcrafter.el")
(require 'dotcrafter)

```

[^fn:1]: `#'foo` and `'foo` are equivalent when `foo` is a symbol, but the
    former is prefered when `foo` is a function. `#'` is intended to be a
    function call. [More explanations here.](https://emacs.stackexchange.com/a/10943/36783)