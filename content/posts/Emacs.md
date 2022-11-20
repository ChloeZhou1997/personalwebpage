+++
title = "Emacs Configuration"
author = ["Chloe"]
date = 2022-10-29
lastmod = 2022-11-19T13:12:21-05:00
tags = ["emacs", "config"]
draft = false
+++

Rewrite my config to get rid of the redunant functions and build the
system from stratch using `straight.el` for package management.


## <span class="org-todo todo TODO">TODO</span> Emacs configurations <code>[4/7]</code> {#emacs-configurations}

-   [ ] [ox-json](https://github.com/jlumpe/ox-json) + [pyorg](https://github.com/jlumpe/pyorg) to obtain `json` data from org files, check [org ele
    API](https://orgmode.org/worg/dev/org-element-api.html#attributes) also
-   [ ] Install [blink-search](https://github.com/manateelazycat/blink-search) and explore it.
-   [X] Check on `embark` to see if it's an alternative to `which-key`
-   [X] Install `Projectile`
-   [ ] rewrite the super-agenda to make it nicer.
-   [X] improvement of performance of emacs (not really... but I tried...)
-   [X] reconfiuring eshell


## Setup before everything else {#setup-before-everything-else}

```emacs-lisp
(setq package-enable-at-startup nil)
(setq fill-column 2000)
```


## Performance Monitoring {#performance-monitoring}

Define a function that will monitor how long it takes for Emacs to
startup:

-   Also checks [The startup steps summary](https://www.gnu.org/software/emacs/manual/html_node/elisp/Startup-Summary.html).

<!--listend-->

```emacs-lisp
(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                   (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)
```


## Keep .emacs.d clean {#keep-dot-emacs-dot-d-clean}

```emacs-lisp
    ;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
;; NOTE: If you want to move everything out of the ~/.emacs.d folder
;; reliably, set `user-emacs-directory` before loading no-littering!
;(setq user-emacs-directory "~/.cache/emacs")

;; (use-package no-littering)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
;; (setq auto-save-file-name-transforms
;; 			`((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

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


### Auto Tangle {#auto-tangle}

```emacs-lisp
(defun joz/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
    (expand-file-name "~/.dotfiles/Emacs.org"))
  (let ((org-confim-babel-evaluate t))
    (org-babel-tangle))))
```


## Package System Setup {#package-system-setup}


### use-package {#use-package}

-   [GitRepo](https://github.com/jwiegley/use-package) for the documentations.
-   See [quick tutorial](https://ianyepan.github.io/posts/setting-up-use-package/) for how to use `use-package`

`use-package` is a way to organize the code neat and also by defer
loading of packages it will improve the start up time of
emacs. Typical pacakges that implies defering loading of packages
includes: `:command` , `:bind`, `:hook`, `:defer`, if one package is needed to
be run at the start up time, using `:demand` to make sure the package is
get loaded.


### straigt.el {#straigt-dot-el}

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2022-11-03 Thu 21:38] </span></span> <br />
    Better way of organizing the package, according to system crafter,
    better switch to straight.el once and for all to avoice weird
    behavior. Make sure to backup the current init.el first.

`straight.el` needs to be bootstrapped into the system. There's
motivation in using it because it makes life easier for installing
package from Git repo.


#### Bootstrap {#bootstrap}

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

(straight-use-package 'org)
(straight-use-package 'use-package)

(setq straight-use-package-by-default t)
```

Upgrade straight package using `straight-pull-all`.


#### Setup {#setup}

```emacs-lisp
;; From this point on we should be able to use `use-package
(use-package straight
  :config
  (setq straight-host-usernames '((github . "ChloeZhou1997"))) ; Move to personal information?
  ;; Make sure packages do not pull in internal org, we pregister org from straight.el
  (straight-register-package 'org)
  (straight-register-package 'org-contrib)
  :custom (straight-use-package-by-default t))

(use-package use-package-ensure-system-package)   ; because we are in use-package config
(use-package bind-key)                  ; Load early, but see section [[Key bindings]]
;; (setq straight-use-package-by-default t)
```


## Genseral {#genseral}


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
    ;; (set-face-attribute 'default nil :font "Fira Code" :height 100)

    ;;Set the fixed pitch face
    ;; (set-face-attribute 'fixed-pitch nil :font "Fira Code" :height 100)

    ;; Set the variable pitch face
    ;; (set-face-attribute 'variable-pitch nil :font "Fira Code" :height 1 :weight 'regular)
    ```

    Define a `font-setup` function

    ```emacs-lisp
    ;; (defun org-font-setup ()
    ;; 	;; Set faces for heading levels
    ;; 	(dolist (face '((org-level-1 . 1.2)
    ;; 									(org-level-2 . 1.1)
    ;; 									(org-level-3 . 1.05)
    ;; 									(org-level-4 . 1.0)
    ;; 									(org-level-5 . 0.8)
    ;; 									(org-level-6 . 0.8)
    ;; 									(org-level-7 . 0.8)
    ;; 									(org-level-8 . 0.8)))
    ;; 		(set-face-attribute (car face) nil :font "Fira Code" :weight 'regular :height (cdr face)))

      ;; Ensure that anything that should be fixed-pitch in Org files appears that way
    ;; 	(set-face-attribute 'org-block nil :foreground nil  :inherit 'fixed-pitch)
    ;; 	(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
    ;; 	(set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
    ;; 	(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
    ;; 	(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    ;; 	(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    ;; 	(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

    ;; (add-hook 'org-mode-hook 'org-font-setup)
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

    <!--list-separator-->

    -  Doom bundled

        Use `:ensure` to make sure the `all-the-icons` package will be autoinstalled when `doom-themes` is installed.

        ```emacs-lisp

        ;; (use-package treemacs)
        ;; (use-package all-the-icons)
        ;; (use-package minions
        ;; 	:config (minions-mode 1))

        ;;dometheme

        ;; (use-package doom-themes
        ;; 	:config
        ;; 	(load-theme 'doom-one t)
        ;; 	;; all-the-icons has to be installed, enabling custom neotree theme
        ;; 	(doom-themes-neotree-config)
        ;; 	;; for treemacs user
        ;; 	(setq doom-themes-treemacs-theme "doom-atom")
        ;; 	(doom-themes-treemacs-config)
        ;; 	;;conrrect the org-mode's native fontification
        ;; 	(doom-themes-org-config))


        ```

        Also, use `doom-modeline` to prettify the mode-line section

        ```emacs-lisp
        ;; (use-package doom-modeline
        ;; 	:ensure t
        ;; 	:init (doom-modeline-mode 1)
        ;; 	:hook (after-init . doom-modeline-mode)
        ;; 	:custom
        ;; 	(doom-modeline-height 10)
        ;; 	(doom-modeline-enable-word-count nil)
        ;; 	(doom-modeline-minor-modes t))
        ```

    <!--list-separator-->

    -  Nano theme

        ```emacs-lisp
        ;; nano writer theme
        (setq nano-font-family-monospaced "Roboto Mono")
        (setq nano-font-family-proportional nil)
        (setq nano-font-size 17)

        (use-package nano-base-colors
          :straight (nano-emacs :host github :repo "rougier/nano-emacs")
          :config
          ;; (require 'nano-theme-dark)
          ;; (require 'nano-theme)
          (require 'nano-faces)
          (require 'nano-modeline)
          (require 'nano-layout)
          (require 'nano-session)
          (require 'nano-colors)
          (require 'nano-writer)
          (require 'nano-help))

        (use-package nano-theme
          :straight (nano-theme :host github :repo "rougier/nano-theme"))

        (nano-mode)
        (nano-faces)

        ;; (nano-theme-set-dark)
        ;; (nano-theme)
        (nano-modeline)
        ```


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


## Org-roam {#org-roam}


### Basic config {#basic-config}

The straight version of org is not working, using straight to make sure using the built-in version

```emacs-lisp
(use-package org
  ;; :straight (
  ;; 	org :type built-in
  ;; )
  :mode ("\\.org" . org-mode)
  :hook ((org-mode . turn-on-visual-line-mode)
         (org-mode . company-mode))
  :bind
  ("C-c a" . org-agenda)
  ("C-c l"   . 'org-store-link)
  ("C-c C-l"  . 'org-insert-link))

;;load babel after org has loaded
(with-eval-after-load 'org
  (org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
      (python . t))))

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


### Consult-roam {#consult-roam}

```emacs-lisp
(use-package consult-org-roam
   :after org-roam
   :init
   ;; Activate the minor mode
   (consult-org-roam-mode)
   :custom
   ;; Use `ripgrep' for searching with `consult-org-roam-search'
   (consult-org-roam-grep-func #'consult-ripgrep)
   ;; Display org-roam buffers right after non-org-roam buffers
   ;; in consult-buffer (and not down at the bottom)
   (consult-org-roam-buffer-after-buffers t)
   :config
   ;; Eventually suppress previewing for certain functions
   (consult-customize
    consult-org-roam-forward-links
    :preview-key (kbd "M-."))
   :bind
   ;; Define some convenient keybindings as an addition
   ("C-c n e" . consult-org-roam-file-find)
   ("C-c n b" . consult-org-roam-backlinks)
   ("C-c n l" . consult-org-roam-forward-links)
   ("C-c n r" . consult-org-roam-search))
```


### Org-download {#org-download}

```emacs-lisp
(use-package org-download
  :custom
  (org-download-method 'directory)
  (org-download-image-dir "~/Notes/static/images")
  (org-download-heading-lvl 0)
  (org-download-timestamp "org_%Y%m%d-%H%M%S_")
  (org-image-actual-width 400)
  (org-download-screenshot-method "xclip -selection clipboard -t image/png -o > '%s'")
  (org-download-image-html-width 400)
  (org-download-image-org-width 400)
  :bind
  ("C-M-y" . org-download-clipboard))
```


### Org-noter + Pdf-tool {#org-noter-plus-pdf-tool}

```emacs-lisp
(use-package pdf-tools)
(pdf-tools-install) ;;it has to be called otherwise org-noter won't integrate.
(use-package org-noter
  :bind
  ("C-c n o" . org-noter))

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
  :bind
  ("C-c h b" . helm-bibtex)
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
  :after org-roam
  :hook org-mode)
```


#### template {#template}

-   `${slug}` is by default the title of the note, it's the text passed
    into the template system from the search. Also according to
    wikipedia,[it's the human readable part of the url](https://en.wikipedia.org/wiki/Clean_URL#Slug).

<!--listend-->

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
         :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
         :unnarrowed t)
        ))
```


#### other-setting {#other-setting}

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
(setq org-directory "~/Notes/")

(defun transform-square-brackets-to-round-ones(string-to-transform)
  "Transforms [ into ( and ] into ), other chars left unchanged."
  (concat
   (mapcar #'(lambda (c) (if (equal c ?[) ?\( (if (equal c ?]) ?\) c))) string-to-transform))
  )

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
;; (setq org-log-done 'note)
(setq org-log-done t)
;;Add captures template
(setq org-capture-templates '(
                              ("p" "Protocol" entry
                               (file+headline "~/Notes/captures.org" "Inbox")
                               "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
                              ("L" "Protocol Link" entry
                               (file+headline "~/Notes/captures.org" "Link")
                               "* %? [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n")
                              ("t" "Todo" entry
                               (file+headline "~/Notes/Agenda/dailylife.org" "Task")
                               "* TODO %?\n %i\n")
                              ("b" "Blog Idea" plain
                               (file+headline "~/Notes/blogideas.org" "Inbox")
                               (file "~/Notes/RoamNotes/Templates/blog_temp.org")
                               )
                              ("f" "emacs problem" plain
                               (file+headline "~/Notes/captures.org" "Emacs Problems")
                               "- [ ] %?\n")
                              ))

;;set todo keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)")
        (sequence "TOREAD(t)" "READING(r)" "|" "CANCELLED(c)" "STALLED(s)" "DONE(d)")
        (sequence "|" "CANCELED(c)")))

;;set faces
(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "white" :background "#238B22" :weight bold))
        ("READING" . "yellow")
        ("QUE" . (:foreground "red" :background "white" :weight bold))
        ("SOMEDAY" . (:forground "#CABCB5" :underline t))))
```


### Super-agenda {#super-agenda}

```emacs-lisp
(use-package org-ql)
(use-package org-super-agenda
  :hook org-agenda-mode)

(setq org-super-agenda-groups
      '((:name "Priority"
               :tag "priority"
               :face (:foreground "#E1B896"  :underline t))
        (:name "Reading List"
               :file-path "~/Notes/RoamNotes/readinglists.org"
               :todo "READING")
        (:name "Research"
               :and (
                     :tag "research"
                          :not (
                                :todo ("SOMEDAY" "PROJECT"))))
        (:name "Learning"
               :and(:tag "learning"
                         :not (:todo ("PROJECT" "NEXT"))))
        (:name "Questions to answer"
               :todo "QUE")
        (:name "Long term plan"
               :and(:todo "PROJECT"
                          :children t))))
```


## Editing {#editing}


### Grammar Check {#grammar-check}

```emacs-lisp
(use-package lsp-grammarly
  :hook (text-mode . (lambda ()
                       (require 'lsp-grammarly)
                       (lsp))))  ; or lsp-deferred

(define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)
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
(use-package magit
  :defer t)
```


## Programming {#programming}


### Exec-path-from-shell {#exec-path-from-shell}

```emacs-lisp
(use-package exec-path-from-shell)
(setq exec-path-from-shell-variables '("PATH"))
  (exec-path-from-shell-initialize)
```


### Python {#python}

```emacs-lisp
(use-package python-mode
  :custom
  (python-shell-interpreter "/opt/homebrew/opt/python@3.10/bin/python3.10"))
(setq python-shell-completion-native-enable nil)
(setq org-babel-python-command "python3")
```


## Others {#others}


### Completion {#completion}


#### Miniframe (with vertico + Nono) {#miniframe--with-vertico-plus-nono}

```emacs-lisp
;; Nicolas .P Rougier emacs configuration - mini-frame configuration
;; ---------------------------------------------------------------------
(use-package vertico)
(use-package mini-frame)

(defun minibuffer-setup ()

  ;; This prevents the header line to spill over second line
  (let ((inhibit-message t))
    (toggle-truncate-lines 1))

  (setq enable-recursive-minibuffers t)

  ;; This allows to have a consistent full width (fake) header like
  (setq display-table (make-display-table))
  (set-display-table-slot display-table
                          'truncation (make-glyph-code ?\  'nano-subtle))
  (set-display-table-slot display-table
                          'wrap (make-glyph-code ?\  'nano-subtle))
  (setq buffer-display-table display-table)

  (cursor-intangible-mode)
  (face-remap-add-relative 'default :foreground "black")
  (face-remap-add-relative 'completions-first-difference :foreground "black")
  (let* ((left  (concat (propertize " "
                                    'face '(nano-subtle)
                                    'display '(raise +0.20))
                        (propertize " Minibuffer"
                                    'face 'nano-subtle)
                        (propertize " "
                                    'face 'nano-subtle
                                    'display '(raise -0.30))))
         (right (propertize "C-g: abort"
                            'face '(:inherit (nano-faded nano-subtle)
                                    :weight light)))
         (spacer (propertize (make-string (- (window-width)
                                             (length left)
                                             (length right)
                                             1) ?\ )
                             'face 'nano-subtle))
         (header (concat left spacer right " "))
         (overlay (make-overlay (+ (point-min) 0) (+ (point-min) 0))))
    (overlay-put overlay 'before-string
        (concat
         (propertize " " 'display header)
         "\n"
         ;; This provides a vertical gap (half a line) above the prompt.
         (propertize " " 'face `(:extend t)
                     'display '(raise .33)
                     'read-only t 'cursor-intangible t)))))

 (add-hook 'minibuffer-setup-hook #'minibuffer-setup)


;; (defun minibuffer-exit ())
;; (add-hook 'minibuffer-exit-hook #'minibuffer-exit)

;; Prefix/Affix the current candidate. From
;; https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
(defun minibuffer-format-candidate (orig cand prefix suffix index _start)
  (let ((prefix (if (= vertico--index index)
                    "  " "   ")))
    (funcall orig cand prefix suffix index _start)))

(advice-add #'vertico--format-candidate
            :around #'minibuffer-format-candidate)

(with-eval-after-load 'vertico
  (setq completion-styles '(basic substring partial-completion flex))
  (setq vertico-count 10)
  (setq vertico-count-format nil)
  (setq vertico-grid-separator
        #("  |  " 2 3 (display (space :width (1))
                               face (:background "#ECEFF1"))))
  (define-key vertico-map (kbd "<backtab>") #'minibuffer-complete)
  (set-face-attribute 'vertico-current nil
                      :inherit '(nano-strong nano-subtle))
  (set-face-attribute 'completions-first-difference nil
                      :inherit '(nano-default))
  (set-face-attribute 'minibuffer-prompt nil
                      :inherit '(nano-default nano-strong))
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))

  (defun vertico--prompt-selection ()
    "Highlight the prompt"
    (let ((inhibit-modification-hooks t))
      (set-text-properties (minibuffer-prompt-end) (point-max)
                           '(face (nano-strong nano-salient))))))

(with-eval-after-load 'marginalia
  (setq truncate-string-ellipsis "…")
  (setq marginalia--ellipsis "…")
  (setq marginalia-align 'right)
  (setq marginalia-align-offset -1))


(with-eval-after-load 'mini-frame
  (set-face-background 'child-frame-border (face-foreground 'nano-faded))
  (setq mini-frame-default-height vertico-count)
  (setq mini-frame-create-lazy t)
  (setq mini-frame-show-parameters 'mini-frame-dynamic-parameters)
  (setq mini-frame-ignore-commands
        '("edebug-eval-expression" debugger-eval-expression))
  (setq mini-frame-internal-border-color (face-foreground 'nano-subtle-i))
  ;; (setq mini-frame-resize 'grow-only) ;; -> buggy as of 01/05/2021
  ;; (setq mini-frame-resize 'not-set)
  ;; (setq mini-frame-resize nil)
  (setq mini-frame-resize t)
  (setq mini-frame-resize-min-height 3)


  (defun mini-frame-dynamic-parameters ()
    (let* ((edges       (window-pixel-edges))      ;; (left top right bottom)
           (body-edges  (window-body-pixel-edges)) ;; (left top right bottom)
           (left   (nth 0 edges))      ;; Take margins into account
           (top    (nth 1 edges)) ;; Drop header line
           (right  (nth 2 edges))      ;; Take margins into account
           (bottom (nth 3 body-edges)) ;; Drop header line
           (left   (if (eq left-fringe-width 0)
                       left
                     (- left (frame-parameter nil 'left-fringe))))
           (right  (nth 2 edges))
           (right  (if (eq right-fringe-width 0)
                       right
                     (+ right (frame-parameter nil 'right-fringe))))
           (fringe-left 0)
           (fringe-right 0)
           (border 1)
           ;; (width (- (frame-pixel-width) (* 2 (+ fringe border))))
           (width (- right left fringe-left fringe-right (* 0 border)))
           (y (- top border)))
    `((left . ,(- left border))
      (top . ,y)
      (alpha . 1.0)
      (width . (text-pixels . ,width))
      (left-fringe . ,fringe-left)
      (right-fringe . ,fringe-right)
      (child-frame-border-width . ,border)
      (internal-border-width . ,border)
      (foreground-color . ,(face-foreground 'nano-default))
      (background-color . ,(face-background 'highlight)))))
  )

(custom-set-variables
 '(mini-frame-show-parameters
   '((top . 10)
     (width . 0.7)
     (left . 0.5))))

(mini-frame-mode)
```


#### Vertico {#vertico}

Minimalistic auto completion setting: `vertico` + `savehist` + `marginalia`
Reference to [this tutorial](https://kristofferbalintona.me/posts/202202211546/).

```emacs-lisp
;;enable Vertico
(use-package vertico
  :custom
  (vertico-count 13)
  (vertico-resize t)
  (vertico-cycle nil)
  ;; Extensions
  (vertico-grid-separator "       ")
  (vertico-grid-lookahead 50)
  (vertico-buffer-display-action '(display-buffer-reuse-window))
  (vertico-multiform-categories
   '((file reverse)
     (consult-grep buffer)
     (consult-location)
     (imenu buffer)
     (library reverse indexed)
     (org-roam-node reverse indexed)
     (t reverse)
     ))
  (vertico-multiform-commands
   '(("flyspell-correct-*" grid reverse)
     (org-refile grid reverse indexed)
     (consult-yank-pop indexed)
     (consult-lsp-diagnostics)
     ))
  :bind
  (:map vertico-map
        ( "?" . minibuffer-completion-help)
        ("M-RET" . minibuffer-force-complete)
        ("M-TAB" . minibuffer-complete)
        ("C-M-n" . vertico-next-group)
        ("C-M-p" . vertico-previous-group)
        )
  :hook ((rfn-eshadow-update-overlay . vertico-directory-tidy) ; Clean up file path when typing
         ))

(vertico-mode)
(nano-dark)

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Show info of files at the marginal
(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

;;icon's completion in minibuffer
(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

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


#### Embark {#embark}

```emacs-lisp
(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-:" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
```


#### Company {#company}

```emacs-lisp
(use-package company
  :init
  (global-company-mode)
  :custom
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.25)
  (company-backends '((company-capf company-semantic company-keywords company-etags company-dabbrev-code company-yasnippet)))
  (company-files-exclusions '(".git/" ".DS_Store"))
  :bind
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


#### Snippet {#snippet}

```emacs-lisp
(defun my-org-mode-hook ()
  (setq-local yas-buffer-local-condition
              '(not (org-in-src-block-p t))))

(use-package yasnippet
  :init
  (yas-global-mode 1)
  :custom
  (yas-snippet-dirs '("~/.emacs.d/snippets"
                      "~/.emacs.d/straight/repos/yasnippet-snippets/snippets"))
  :bind
  ("\C-o" . yas-expand)
  :config
  (add-hook 'org-mode-hook 'my-org-mode-hook))

;;download snippets lib
(use-package yasnippet-snippets)

;;integration with consult
(use-package consult-yasnippet)
```


### Terminal {#terminal}


#### Eshell {#eshell}

Some links about eshell configuration and tutorial: [Eshell Aliases,
Prompt and colors](https://olddeuteronomy.github.io/post/eshell-aliases-and-prompt/), [shell setup gitrepo](https://github.com/mclear-tools/dotemacs/blob/117f7738df7c76cd453fdd0154b0ffa654a39744/setup-config/setup-shell.el#L175).

```emacs-lisp
(defun efs/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt)

(use-package eshell
  :bind
  (:map eshell-mode-map
        ("C-r" . consult-history)
        ("<home>" . eshell-bol))
  :hook (eshell-first-time-mode . efs/configure-eshell)
  :config
  (eshell-git-prompt-use-theme 'powerline))

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode))

```


### Obsidian {#obsidian}

```emacs-lisp
(use-package obsidian
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


### Svg-subject {#svg-subject}

```emacs-lisp
(use-package svg)
(use-package svg-tag-mode)
(use-package svg-lib)

(defun svg-font-lock-tag (label)
  (svg-lib-tag label nil :margin 0))

(defun svg-font-lock-todo ()
  (svg-lib-tag "TODO" nil :margin 0
               :font-family "Roboto Mono" :font-weight 500
               :foreground "#FFFFFF" :background "#673AB7"))

(defun svg-font-lock-done ()
  (svg-lib-tag "DONE" nil :margin 0
               :font-family "Roboto Mono" :font-weight 400
               :foreground "#B0BEC5" :background "white"))

(defun svg-font-lock-progress_percent (value)
  (svg-image (svg-lib-concat
              (svg-lib-progress-bar (/ (string-to-number value) 100.0)
                                nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 12)
              (svg-lib-tag (concat value "%")
                           nil :stroke 0 :margin 0)) :ascent 'center))

(defun svg-font-lock-progress_count (value)
  (let* ((seq (mapcar #'string-to-number (split-string value "/")))
         (count (float (car seq)))
         (total (float (cadr seq))))
  (svg-image (svg-lib-concat
              (svg-lib-progress-bar (/ count total) nil
                                :margin 0 :stroke 2 :radius 3 :padding 2 :width 12)
              (svg-lib-tag value nil
                           :stroke 0 :margin 0)) :ascent 'center)))

(defvar svg-font-lock-keywords
  `(("TODO"
     (0 (list 'face nil 'display (svg-font-lock-todo))))
    ("\\:\\([0-9a-zA-Z]+\\)\\:"
     (0 (list 'face nil 'display (svg-font-lock-tag (match-string 1)))))
    ("DONE"
     (0 (list 'face nil 'display (svg-font-lock-done))))
    ("\\[\\([0-9]\\{1,3\\}\\)%\\]"
     (0 (list 'face nil 'display (svg-font-lock-progress_percent (match-string 1)))))
    ("\\[\\([0-9]+/[0-9]+\\)\\]"
     (0 (list 'face nil 'display (svg-font-lock-progress_count (match-string 1)))))))

;; Activate
(push 'display font-lock-extra-managed-props)
(font-lock-add-keywords 'org-mode svg-font-lock-keywords)
(font-lock-flush (point-min) (point-max))
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


### Movement and editing {#movement-and-editing}


#### Consult {#consult}

```emacs-lisp
;;define prefix C-s for search map
(define-prefix-command 'search-map)
(global-set-key (kbd "C-s") 'search-map)

(use-package consult
  :bind
  ("C-x b" . consult-buffer)
  ("M-y" . consult-yank-pop)
  (:map search-map
        ("s" . consult-line)
        ("l" . consult-goto-line)
        ("o" . consult-outline)
        ("S" . consult-line-multi)))
```


#### Multi-editing {#multi-editing}

```emacs-lisp
(use-package iedit
  :bind
  ("C-;" . iedit-mode))

(use-package multiple-cursors
  :bind
  ("C-x C-;" . mc/mark-all-like-this)
  ("C-x r l" . mc/edit-lines))
```


#### Misc {#misc}

```emacs-lisp
;;expand region basiced semantics
(use-package expand-region
  :bind
  ("C-=" . er/expand-region))
```


### Project {#project}

```emacs-lisp
(use-package projectile
  :config
  (setq projectile-project-search-path '("~/Blogs" "~/Desktop/ZeroToMastery"))
  (setq projectile-switch-project-action #'projectile-dired)
  :bind
  (:map projectile-mode-map
        ("s-p" . projectile-command-map)
        ("C-c p" . projectile-command-map)))
```

Integration with consult:

```emacs-lisp
(use-package consult-projectile
  :straight (consult-projectile :type git :host gitlab :repo "OlMon/consult-projectile" :branch "master")
  :after projectile)
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


## Export {#export}


### Blogging with ox-hugo {#blogging-with-ox-hugo}

```emacs-lisp
(use-package ox-hugo
  :after ox)
(setq org-export-with-broken-links t)
```


### CV with Org-mode {#cv-with-org-mode}


#### moderncv {#moderncv}

```emacs-lisp
(use-package ox-altacv
  :straight (org-cv :type git :host gitlab :repo "ChloeZhou1997/org-cv"))

(use-package ox-awesomecv
  :straight (org-cv :type git :host gitlab :repo "ChloeZhou1997/org-cv"))
```


## Email {#email}

```emacs-lisp
(use-package mu4e
  :straight (:host github
                    :files ("build/mu4e/*.el")
                    :branch "master"
                    :repo "djcb/mu"
                    :pre-build (("meson" "build")
                                ("ninja" "-C" "build"))))

(require 'smtpmail)

;; we installed this with homebrew
(setq mu4e-mu-binary (executable-find "mu"))

;; this is the directory we created before:
(setq mu4e-maildir "~/.maildir")

;; this command is called to sync imap servers:
(setq mu4e-get-mail-command "mbsync -a")
;; how often to call it in seconds:
(setq mu4e-update-interval 300)

;; save attachment to desktop by default
;; or another choice of yours:
(setq mu4e-attachment-dir "~/Desktop")

;; rename files when moving - needed for mbsync:
(setq mu4e-change-filenames-when-moving t)

(setq mu4e-drafts-folder "/[Gmail]/Drafts")
(setq mu4e-sent-folder "/[Gmail]/Sent Mail")
(setq mu4e-refile-folder "/[Gmail]/All Mail")
(setq mu4e-refile-folder "/[Gmail]/Trash")

;; list of your email adresses:
(setq mu4e-user-mail-address-list '("zhouqiaohui97@gmail.com"))

;; check your ~/.maildir to see how the subdirectories are called
;; for the generic imap account:
;; e.g `ls ~/.maildir/example'
(setq   mu4e-maildir-shortcuts
        '(("/INBOX" . ?g)
          ("/[Gmail]/Sent Mail" . ?G)))

;; the following is to show shortcuts in the main view.
;; (add-to-list 'mu4e-bookmarks
;;              (mu4e-bookmark-define
;; 							"Inbox - Gmail"
;;               "maildir:/gmail/INBOX"
;; 							"?g"))

;;context
;; (setq mu4e-contexts
;;       `(,(make-mu4e-context
;;           :name "gmail"
;;           :enter-func
;;           (lambda () (mu4e-message "Enter zhouqiaohui97@gmail.com context"))
;;           :leave-func
;;           (lambda () (mu4e-message "Leave zhouqiaohui97@gmail.com context"))
;;           :match-func
;;           (lambda (msg)
;;             (when msg
;;               (mu4e-message-contact-field-matches msg
;;                                                   :to "zhouqiaohui97@gmail.com")))
;;           :vars '((user-mail-address . "zhouqiaohui97@gmail.com")
;;                   (user-full-name . "qiaohui zhou")
;;                   (mu4e-drafts-folder . "/[Gmail]/Drafts")
;;                   (mu4e-sent-folder . "/[Gmail]/Sent Mail")
;;                   (mu4e-trash-folder . "/[Gmail]/Trash")))))

(setq mu4e-context-policy 'pick-first) ;; start with the first (default) context;
(setq mu4e-compose-context-policy 'ask) ;; ask for context if no context matches;

;; gpg encryptiom & decryption:
;; this can be left alone
(require 'epa-file)
(epa-file-enable)
(setq epa-pinentry-mode 'loopback)
(auth-source-forget-all-cached)

;; don't keep message compose buffers around after sending:
(setq message-kill-buffer-on-exit t)

;; send function:
(setq send-mail-function 'sendmail-send-it
      message-send-mail-function 'sendmail-send-it)

;; send program:
;; this is exeranal. remember we installed it before.
(setq sendmail-program (executable-find "msmtp"))

;; select the right sender email from the context.
(setq message-sendmail-envelope-from 'header)

;; chose from account before sending
;; this is a custom function that works for me.
;; well I stole it somewhere long ago.
;; I suggest using it to make matters easy
;; of course adjust the email adresses and account descriptions
(defun timu/set-msmtp-account ()
  (if (message-mail-p)
      (save-excursion
        (let*
            ((from (save-restriction
                     (message-narrow-to-headers)
                     (message-fetch-field "from")))
             (account
              (cond
               ((string-match "zhouqiaohui97@gmail.com" from) "gmail"))))
          (setq message-sendmail-extra-arguments (list '"-a" account))))))

(add-hook 'message-send-mail-hook 'timu/set-msmtp-account)

;; mu4e cc & bcc
;; this is custom as well
(add-hook 'mu4e-compose-mode-hook
          (defun timu/add-cc-and-bcc ()
            "My Function to automatically add Cc & Bcc: headers.
    This is in the mu4e compose mode."
            (save-excursion (message-add-header "Cc:\n"))
            (save-excursion (message-add-header "Bcc:\n"))))

;; mu4e address completion
(add-hook 'mu4e-compose-mode-hook 'company-mode)

;;optional
;; store link to message if in header view, not to header query:
(setq org-mu4e-link-query-in-headers-mode nil)
;; don't have to confirm when quitting:
(setq mu4e-confirm-quit nil)
;; number of visible headers in horizontal split view:
(setq mu4e-headers-visible-lines 20)
;; don't show threading by default:
(setq mu4e-headers-show-threads nil)
;; hide annoying "mu4e Retrieving mail..." msg in mini buffer:
(setq mu4e-hide-index-messages t)
;; customize the reply-quote-string:
(setq message-citation-line-format "%N @ %Y-%m-%d %H:%M :\n")
;; M-x find-function RET message-citation-line-format for docs:
(setq message-citation-line-function 'message-insert-formatted-citation-line)
;; by default do not show related emails:
(setq mu4e-headers-include-related nil)
;; by default do not show threads:
(setq mu4e-headers-show-threads nil)
```


### Set mu4e theme {#set-mu4e-theme}

```emacs-lisp
;; (use-package mu4e-thread-folding
;; 	:straight(mu4e-thread-folding :host github :repo "rougier/mu4e-thread-folding"))

;; (use-package mu4e-dashboard
;; 	:straight(mu4e-dashboard :host github :repo "rougier/mu4e-dashboard"))

;; (use-package nano-mu4e
;; 	:straight (nano-emacs :host github :repo "rougier/nano-emacs"))
```
