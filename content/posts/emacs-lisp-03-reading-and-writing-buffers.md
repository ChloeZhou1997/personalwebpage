+++
title = "Learning List 03 Reading and Writing Buffer in Emacs"
author = ["Chloe"]
date = 2022-10-27
lastmod = 2022-10-27T12:14:26-04:00
tags = ["emacs", "lisp"]
draft = false
+++

## Basic API for Emacs Buffers {#basic-api-for-emacs-buffers}

Buffers may be visible or not.


### Manipulating Buffer {#manipulating-buffer}


#### Current buffer {#current-buffer}

```emacs-lisp
(current-buffer)
```


#### Get buffer by name or create one (`get-buffer-create`) {#get-buffer-by-name-or-create-one--get-buffer-create}

```emacs-lisp

(get-buffer "*scratch*")
```

Create a buffer if it doesn't already exist:

```emacs-lisp

(get-buffer-create "Hello World")

```


#### Changing the current buffer (`set-buffer`) {#changing-the-current-buffer--set-buffer}

```emacs-lisp
(progn
	(set-buffer (get-buffer "*scratch*"))
	(current-buffer))

;;equivalently
(progn
	(set-buffer "*scratch*")
	(current-buffer))

```

-   `progn` is an expression that allows you to run multiple expression inside of the body.


#### Changing the current buffer safely (`save-current-buffer`) {#changing-the-current-buffer-safely--save-current-buffer}

The `set-buffer` command may affect the code run afterwards: (the
command that's supposed to run in other buffer now run in the
_**scratch**_ buffer because that's what set by the `set-buffer` function.

Solution: using `save-current-buffer`

```emacs-lisp
(progn
	(save-current-buffer
		(set-buffer "*scratch*")
		(message "Current buffer: %s" (current-buffer)))
	(current-buffer))

```

Alternatively,

```emacs-lisp
(progn
	(with-current-buffer "*scratch*"
		(message "Current buffer: %s" (current-buffer)))
	(current-buffer))
```


### Manipulating Files {#manipulating-files}


#### File of the current buffer (`buffer-file-name`) {#file-of-the-current-buffer--buffer-file-name}

use `buffer-file-name` to find/obatin the file name and the full path of the
file the buffer represented:

```emacs-lisp

(buffer-file-name)

```

Find the _existing_ buffer that is represented by the file path (provide
as much information as possible about the file path) using
`get-file-buffer`.

-   If the file is not currently open in the buffer the function will
    return `nil`

<!--listend-->

```emacs-lisp

(get-file-buffer "~/Notes/Emacs/reading-and-writing-buffers.org")
```


#### Load files into a buffer (`find-file-noselect`) {#load-files-into-a-buffer--find-file-noselect}

```emacs-lisp
(find-file-noselect "function.org")
```

-   If running the command more than once the file will be loaded into
    the same buffer and sometimes it's not what we want (the buffer may
    not refect the most recent change of the file)
-   send `t` to the second argument to prevent a prompt when the file is
    modified and differ from the buffer

<!--listend-->

```emacs-lisp
(find-file-noselect "function.org" t)
```


#### Manipulating file paths (prefix `file-name`) {#manipulating-file-paths--prefix-file-name}

In Emacs, file paths are decomposed into two parts: 1. directory
part; 2. file name and extension.

Function prefix : `file-name`.

-   `-directory` - get the directory part of a file path
-   `-nondirectory` - get the filename of the file path
-   `-extension` - get the extension of the file without the leading
    period
-   `-sans-extension` - get the path without extension
-   `-base` - get the file name without path or etension
-   `-as-directory` - turn the file name into a directory name

An interesting example

```emacs-lisp
(file-name-as-directory
 (file-name-sans-extension (buffer-file-name))) ;; turn the current file name into another path

```


#### Resolving file paths (`file-name-absolute-p` etc) {#resolving-file-paths--file-name-absolute-p-etc}

-   `file-name-absolute-p`
-   `file-relative-name` - give the path of a file relative to another
    path
-   `expand-file-name` - return the absolute path for a file under a
    specified directory

<!--listend-->

```emacs-lisp

(file-name-absolute-p (buffer-file-name)) ;;t

(file-relative-name (buffer-file-name) "~/Notes")
(file-relative-name (buffer-file-name) "~/.dotfiles")

(expand-file-name "function.org")

(expand-file-name "Emacs.org" "~/.dotfiles")

```

If resolving files that contains **Environmental Variable**,
`expand-file-name` won't work, use `substitute-in-file-name` instead

```emacs-lisp
(substitute-in-file-name "$HOME/.emacs.d")

```


#### Check if file exists {#check-if-file-exists}

`file-exists-p` - check if the file or directory exsits;

```emacs-lisp
(file-exists-p "~/.dotfiles/.files/.emacs.d")
```

Other options:

-   `file-readable-p`
-   `file-executable-p`
-   `file-writable-p`


### Manipulating Directories {#manipulating-directories}


#### Creating directories (`make-directory`) {#creating-directories--make-directory}

```emacs-lisp

(make-directory "~/Notes/Emacs/Test")

;;create directory with path that doesn't exist
;;add t as the second parameter

(make-directory "~/Notes/Emacs/test1/test2" t)

```


#### Listing files in directories {#listing-files-in-directories}

Using `directory-files` and `directory-files-recursively`.

```emacs-lisp

(directory-files "~/.dotfiles") ;;return a list of files names with extension
(directory-files "~/.dotfiles" t) ;;return the full path of files in the list
(directory-files "~/.dotfiles" t ".org") ;; Get all file containing '.org'

;; listing all the subfolders file
(directory-files-recursively "~/.dotfiles" "\\.el$") ;; the last argument is a simple regexp
(directory-files-recursively "~/.dotfiles/.files" "")
(directory-files-recursively "~/.dotfiles" "" t) ;;return the folder as well as the files

;; forth parameter is a lambda function that can ba passed onto the function to restrict the path the doc can search into (this one is very tricky!)
(directory-files-recursively "~/.dotfiles" "" t
														 (lambda (dir)
															 (progn
																 (message "The dir is: %s" dir)
																 (string-equal dir "~/.dotfiles/.files"))))

;; The fifth parameter is used to specify whether the path should follow symbolic link
```


#### Copying, moving and deleting files and dir {#copying-moving-and-deleting-files-and-dir}

-   `Copy-file`: copy the contents of one file to another
-   `Copy-directory` : copy the contents including the subdirectories into
    another dir

**Caveat**: elisp treat `/folder` and `/folder/` differently, if you want to
present the symbol as a folder, be sure to include `/` at the end of it.

```emacs-lisp
(copy-file "~/.emacs.d/init.el" "~/exp/") ;; if using "~/exp" will throw err
;; (directory-files "~/exp")
;;to ignore that the file already exists
(copy-file "~/.emacs.d/init.el" "~/exp/" t)

;; copy directory

;; (directory-files-recursively "~/.emacs.d" "" t
;; 														 (lambda (dir)
;; 															 (string-equal dir "~/.emacs.d/lisp"))) ;; no directory called lisp

(copy-directory "~/.emacs.d/snippets" "/tmp")
```


#### Renaming/Moving {#renaming-moving}

-   `rename-file` : rename a file or directory

<!--listend-->

```emacs-lisp

(copy-file "~/.emacs.d/init.el" "/tmp/")
(rename-file "/tmp/init.el" "/tmp/init-rename.el")

```


#### Deleting {#deleting}

-   `delete-file` : Delete a file. (optionally moving it to eh trash
    folder)
-   `delete-directory` : Delete a directory, including files if disired

<!--listend-->

```emacs-lisp

(delete-file "/tmp/init-rename.el")

;;delete folder (optionally, including the contents)

(delete-directory "~/exp") ;; return err message because the folder is not empty
(delete-directory "~/exp" t)

```


### Creating Symblink {#creating-symblink}

-   `make-symbolic-link` : create symbolic link, set the third arg to `t`,
    so it won't through error even if the symblink already exists

<!--listend-->

```emacs-lisp

(make-symbolic-link "~/.dotfiles/.zshrc" "~/.zshrc" t)

```

-   `file-symlink-p`: check if the file is a symblink file
-   `file-truename`: return the fule resolved path of the symblink file

<!--listend-->

```emacs-lisp

(file-symlink-p "~/.zshrc")
(file-truename "~/.zshrc")

```