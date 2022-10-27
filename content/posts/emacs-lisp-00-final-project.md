+++
title = "Lisp Project - Dotcrafter"
author = ["Chloe"]
date = 2022-10-27
lastmod = 2022-10-27T12:11:55-04:00
tags = ["emacs", "config"]
draft = false
+++

Use `move-folder-to-config-files` to move config folders into the
`/.dotfiles`

Use `move-file-to-config-files` to move config files into the `/.dotfiles`

Use `link-config-files` to create symblink between the config files in
the `/.dotfiles` and the `home-directory`


## From `function.org` {#from-function-dot-org}

See: [Interactive function]({{< relref "emacs-lisp-01-functions" >}})

Define a command that automaticaly tangle the \`.org\` files in your
dotfiles folder (this is used in the last section of the file.

**(Not in actual production code)**

```emacs-lisp
(defun
		dotfiles-tangle-org-file(&optional org-file)
	"Tangles a single .org file relative to the path in
dotfiles-folder. If no file is specified, tangle the current
file if it is an org-mode buffer inside of dotfiles-folder"
	(interactive "F")
	(message "File: %s" org-file)
	;;suppress prompts and messages
	(let ((org-confirm-babel-evaluate nil)
	(message-log-max nil)
	(inhibit-message t))
	(org-babel-tangle-file (expand-file-name org-file dotfiles-folder))))

(defun dotfiles-tangle-org-files()
	(interactive)
	(dolist (org-file dotfiles-org-files)
		(dotfiles-tangle-org-file org-file)))
```


## From `variable-and-scoping.org` {#from-variable-and-scoping-dot-org}

```emacs-lisp

(defcustom dotcrafter-dotfiles-folder "~/.dotfiles"
	"The folder where dotfiles and org-mode configuration files are stored."
	:type 'directory
	:group 'dotfiles)

(defcustom dotfiles-org-files '()
	"The list of org-mode files under the `dotfiles-folder' which
contain configuration files that should be tangled"
	:type '(list string)
	:group 'dotfiles)

;; (defun dotfiles-tangle-org-file (&optional org-file)
;;   "Tangles a single .org file relative to the path in
;; dotfiles-folder.  If no file is specified, tangle the current
;; file if it is an org-mode buffer inside of dotfiles-folder."
;;   (interactive)
;;  ;; Suppress prompts and messages
;;   (let ((org-confirm-babel-evaluate nil)
;;         (message-log-max nil)
;;         (inhibit-message t))
;;     (org-babel-tangle-file (expand-file-name org-file dotfiles-folder))))

(defun dotcrafter-tangle-org-files ()
	"Tangles all of the .org files in the paths specified by the variable dotfiles-folder"
	(interactive)
	(dolist (org-file dotfiles-org-files)
		(dotfiles-tangle-org-file org-file))
	(message "Dotfiles are up to date!"))

```


## From `reading-and-writing-buffers.org` {#from-reading-and-writing-buffers-dot-org}

[More information]({{< relref "emacs-lisp-03-reading-and-writing-buffers" >}})


### Getting the buffer for our configuration Org files {#getting-the-buffer-for-our-configuration-org-files}

```emacs-lisp

;; (dolist (org-file dotfiles-org-files)
;; 	(with-current-buffer (get-file-buffer (expand-file-name org-file
;; 																													dotfiles-folder))
;; 		(message "File: %s" (buffer-file-name)))
;; 	(message "The current buffer is %s" (current-buffer)))
```

we can use these functions to find **where a file inside of the dotfiles folder should be linked in the home
directory.**

-   find the relative path of a file under `/.dotfile` relative to the
    actual folder
-   find the relative path of the `/.dotfile/file` against the output
    directory, (usually the home directory).

<!--listend-->

```emacs-lisp

;;; Code:
		(defcustom dotcrafter-output-directory "~"
			"The directory where dotcrafter.el will write out your dotfiles.
		This ia typically set to the home directory but can be changed for
		testing purposes."
			:type 'string
			:group 'dotfiles)

		(defcustom dotcrafter-config-files-directory ".files"
			"The directory path inside of '~/.dotfiles' where configuration
		files should be symbolically linked are stored"
			:type 'string
			:group 'dotfiles)

		(defun dotcrafter--resolve-config-files-path()
			"The function gives the full path of the given file relative to
	the ~/.dotfile folder"
						(expand-file-name dotcrafter-config-files-directory
															dotcrafter-dotfiles-folder))

		(dotcrafter--resolve-config-files-path)

		(defun resolve-config-file-target(config-file)
			"Get the path of each each directory and file in the
	relative to the corresponding ~/.dotfiles location"
			(expand-file-name
			 (file-relative-name
				 (expand-file-name config-file)
				 (resolve-config-files-path))
			 output-directory))

		(resolve-config-file-target "init.el")

		;; expanded files
		(file-relative-name "~/Notes/Emacs/init.el" "~/.dotfiles/.files")
```


### Creating expected directories before linking {#creating-expected-directories-before-linking}

When we start to create symbolic links into the home directory (where
the config file should be, usually), one caveat is when creating
symlink too close to the home directory and commonly used in unix
system (e.g. `~/.config`). (If these folders do not prexist, the program
will create symbolic link for these folders as well...?)

Solution: create these files at first to avoid the problem all at
once.

```emacs-lisp

(defcustom dotcrafter-ensure-output-directories '(".config" ".zshrc" ".emacs.d")
	"list of directories in the output folder that should be created
 before linking configuration files."
	:type '(list string)
	:group 'dotfiles)

(defun dotcrafter-ensure-output-directories ()
	;; Ensure that the expected output directories are already
	;; created so that links will be created inside
	(dolist (dir dotcrafter-ensure-output-directories)
		(make-directory (expand-file-name dir output-directory) t)))

(dotcrafter-ensure-output-directories)
```


### Finding the list of all configuration files to be linked {#finding-the-list-of-all-configuration-files-to-be-linked}

Goal: mirror the configuration files in `~/.dotfiles` into the home
folder using symbolic link.

Solution: Based on , list all the linkable
files under `~/.dotfiles`

```emacs-lisp

(defun find-all-files-to-link()
	(let ((file-to-link
				 (directory-files-recursively
					(resolve-config-files-path)
					"")))
		(progn
			(message "file-to-link: %s" file-to-link)
			(dolist (file file-to-link)
				(message "File:%s\n			- %s" file (resolve-config-file-target file))))))

(find-all-files-to-link)

```


### Migrating config files to the dotfiles folder {#migrating-config-files-to-the-dotfiles-folder}

-   Migrating folders or files under home dir (`output-directory`) to the
    `.dotfiles` diretory.
-   Move the file to the corresponding location under the config path
-   parameter `D` allows for pass folder as input argument, `F` allows for
    passing files as input argument.
    -   [Code Character for interactive](https://www.gnu.org/software/emacs/manual/html%5Fnode/elisp/Interactive-Codes.html)
-   [] in the tutorial, `F` is enough
    to allow both folder path and
    file path to be input, not in
    my case through ... have to use
    both `D` and `F` in order for the
    function to work.

<!--listend-->

```emacs-lisp

(defun dotcrafter-move-folder-to-config-files (&optional source-path)
	(interactive "DConfiguration path to move:")
	(let*
			((relative-path (file-relative-name (expand-file-name source-path)
																					output-directory))
			 (dest-path (expand-file-name relative-path
																		(resolve-config-files-path)))
			 (dest-path (if (string-suffix-p "/" dest-path)
											(substring dest-path 0 -1)
										dest-path)))
		(when(string-prefix-p ".." relative-path)
			(error "Copied path is not inside of config output directory :%s" output-directory))
		(when(file-exists-p dest-path)
			(error "Can't copy path because it already exists in the config directory: %s" dest-path))

		(make-directory (file-name-directory dest-path) t)
		(rename-file source-path dest-path)))

(defun dotcrafter-move-file-to-config-files (&optional source-path)
	(interactive "fConfiguration path to move:")
	(let*
			((relative-path (file-relative-name (expand-file-name source-path)
																					output-directory))
			 (dest-path (expand-file-name relative-path
																		(resolve-config-files-path)))
			 (dest-path (if (string-suffix-p "/" dest-path)
											(substring dest-path 0 -1)
										dest-path)))
		(when(string-prefix-p ".." relative-path)
			(error "Copied path is not inside of config output directory :%s" output-directory))
		(when(file-exists-p dest-path)
			(error "Can't copy path because it already exists in the config directory: %s" dest-path))

		(make-directory (file-name-directory dest-path) t)
		(rename-file source-path dest-path)))
```


### Creating symblink for all config files {#creating-symblink-for-all-config-files}

Create symblink at the optimal level in home dir so no need to create
a link for every single file 0.0.


#### Procedure {#procedure}

1.  Recursively looping over the `~/.dotfiles/`
2.  File any given file, break the path into pieces (identifier "`/`")
3.  Check whether each piece exists (iteratively)
4.  Check if a symblink exists for each piece, pointing to the `output-directory`
5.  Create the symblink if it doesn't exists


#### Code {#code}

-   `dotcrafter-link-config-files` : link the whole config dir
-   `link-config-file` : link every inidividual files (inside of the dir)

<!--listend-->

```emacs-lisp

(defun dotcrafter--link-config-file (config-file)
	(let* ((path-parts
					(split-string (file-relative-name (expand-file-name config-file)
																						(dotcrafter--resolve-config-files-path))
												"/" t))
				 (current-path nil))

		(while path-parts
			(setq current-path (if current-path
														 (concat current-path "/" (car path-parts))
													 (car path-parts)))
			(setq path-parts (cdr path-parts))


			;; Whether need to create a symblink between the current source path to the target path
			(let ((source-path (expand-file-name (concat config-files-directory "/" current-path)
																					 dotfiles-folder))
						(target-path (expand-file-name current-path output-directory)))

				;; First, if the file exists, if it's a symblink
				(if (file-symlink-p target-path)
						(progn
							(message "The source path is a string %s" source-path)
							;;check if the symblink target to the source path
							(if (string-equal source-path (file-truename target-path))
									;;stop looping
									(setq path-parts '())
								(error "The targeted file/folder %s is a symblink of a different source file" target-path)))
					;; if the file/folder exists, but doesn't have a symblink
					;; if it's a file, creat symblink
					;; if it's a folder, keep looping
					(when (not (file-directory-p target-path))
						(make-symbolic-link source-path target-path)
						(setq path-parts '())))))))


(defun dotcrafter-link-config-files()
	(interactive)
	(let ((config-files
				 (directory-files-recursively (dotcrafter--resolve-config-files-path) "")))
		;;ensure the expected output folders are created;
		(dolist (dir dotcrafter-ensure-output-directories)
			(make-directory (expand-file-name dir dotcrafter-output-directory) t))

		(dolist (file config-files)
			(dotcrafter--link-config-file file))))

(dotcrafter-link-config-files)
```


## From `creating-minor-mode.org` {#from-creating-minor-mode-dot-org}

Goal: creatingminor mode to

-   automatically tangle and update configuration target files for ANY
    Org Mode file lives inside of the dotfiles folder
-   the tangle setting in my init config.

<!--listend-->

```emacs-lisp

(defcustom dotcrafter-keymap-prefix "C-c C-,"
	"The prefix for dotcrafter-mode key bindings."
	:type 'string
	:group 'dotfiles)

(defcustom dotcrafter-tangle-on-save t
	"When t, automatically tangle Org files on save"
	:type 'boolean
	:group 'dotfiles)


(defun dotcrafter-tangle-org-file (&optional org-file)
	"Tangle .org file relative to the path in dotfiles-folder.
If no file is specified, tangle the current file if it's an
org-mode buffer inside of dotfile-folder"
	(interactive)
	(let ((org-confirm-babel-evaluate nil)
				(message-log-max nil)
				(inhibit-message-t))
		(org-babel-tangle-file (expand-file-name org-file dotfiles-folder))
		(dotcrafter-link-config-files)))

(defun dotcrafter--org-mode-hook ()
	(add-hook 'after-save-hook #'dotcrafter--after-save-handler nil t))

(defun dotcrafter--after-save-handler ()
	(when (and dotcrafter-mode
						 dotcrafter-tangle-on-save
						 (member (file-name-nondirectory buffer-file-name) dotfiles-org-files)
						 (string-equal (directory-file-name (file-name-directory (buffer-file-name)))
													 (directory-file-name (expand-file-name dotfiles-folder))))
		(message "Tangling %s..." (file-name-nondirectory buffer-file-name))
		(dotcrafter-tangle-org-file buffer-file-name)))

(defun dotcrafter--key (key)
	(kbd (concat dotcrafter-keymap-prefix " " key)))

(define-minor-mode dotcrafter-mode
	"Toggles global dotcrafter-mode"
	nil
	:global t
	:group 'dotfiles
	:lighter " dotcrafter"
	:keymap
	(list (cons (dotcrafter--key "t") #'dotcrafter-tangle-org-file)
				(cons (dotcrafter--key "u") #'dotcrafter-update-dotfiles))
	(if dotcrafter-mode
			(add-hook 'org-mode-hook #'dotcrafter--org-mode-hook)
		(remove-hook 'org-mode-hook #'dotcrafter--org-mode-hook)))
```