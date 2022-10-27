+++
title = "Learning Elisp 02 Variable and Scope"
author = ["Chloe"]
date = 2022-10-27
lastmod = 2022-10-27T12:14:16-04:00
tags = ["emacs", "lisp"]
draft = false
+++

## Variables {#variables}

-   Set vairbales and <span class="underline">Define</span> vairables are different
-   <span class="underline">Define</span> **Buffer-local** variables
    -   Understanding variable scopes
-   Creating **Variable scope** with `let`
-   Defining and setting customization variables

Variable is a <span class="underline">symbol</span> that binds with a <span class="underline">value</span> : `'tab-width`-&gt; `4`


### Set variables {#set-variables}

**Variables are symbols binds to values**, what `setq` means is "set
 quote", a convient way to write

```emacs-lisp

(set 'tab-width 4)

;; The variable doesn't have to exist or be pre-defined

(set 'i-dont-exist 5)

```


#### Documenting Variables {#documenting-variables}

`defvar` allows one to assign documentations to the created variables

**Remark:**

1.  defvar will can be assigned to variables that doesn't exist yet
2.  if the variable has already created and has been assigned with a
    default value, the assigned defult value from the `defvar` function
    won't work **(don't seem to apply in my case...)**

<!--listend-->

```emacs-lisp

(defvar i-dont-exist 100
"This is a variable to test whether the default value is the existing
one or the newly assigned one")

```


#### Set buffer-local value for variables {#set-buffer-local-value-for-variables}

```emacs-lisp

(setq some-value 2)

(setq-local some-value 5)

;; after using setq-local, the setq will only set the buffer-local binding
(setq some-value 4)

```


#### Making a variable local for all buffers {#making-a-variable-local-for-all-buffers}

```emacs-lisp

(setq not-local-yet t)
(make-variable-buffer-local 'not-local-yet)

```


#### Setting default values {#setting-default-values}

```emacs-lisp
(setq-default not-local-yet nil)
```


## Scope {#scope}

Define a local scope using `let`, the region inside that `let` is
another scope.


### Define a locals cope with `let` {#define-a-locals-cope-with-let}

If defining a loop and set the variable in global scope, the function

will only be able to run once:

```emacs-lisp

(setq x 0)

(defun do-the-loop ()
	(message "Starting the loop at %d" x)
	(while (< x 5)
		(message "Loop index is %d" x)
		(cl-incf x))  ;; (equivalent to (setq x (1+ x))
	(message "DONE!"))

(do-the-loop)
```

To avoid the problem:

```emacs-lisp

(defun do-the-loop ()
	(let ((x 0))
		(message "Starting the loop at %d" x)
		(while (< x 5)
(message "Loops index: %d" x)
(cl-incf x))
		(message "DONE Looping!")))

(do-the-loop)

```

-   Why `(())`? Because `let` is a list, and the argument pass onto the `list`
    is also a `list` of (sort of) _key-value_ pair.

<!--listend-->

```emacs-lisp

(let ((x 5)
		(y 7))
	(* x y))

```

-   What if you want to refer to `y` in another variable, say `z`?
    -   `(let ((x 10) (z (+ y 5))) (* x y))` will throw error message: `symbol y is void`
    -   The right solution is to use `let*` instead

<!--listend-->

```emacs-lisp

(let* ((y 5)
		(z (+ y 5)))
	(* y z))

```

-   `let*` is in essence a nested `let`

<!--listend-->

```emacs-lisp

(let ((y 5)) (let ((z (+ y 5))) (* z y)))

```


### Dynamic scope {#dynamic-scope}

-   The value associated with a variable might change depending on where
    an expression gets evaluated
-   If the function is executed in a local scope predicated by `let`, the variable will be evaluated based on the value inside the scope of `let`

<!--listend-->

```emacs-lisp

(setq x 5)

(defun do-math (y)
	(+ x y))

(do-math 10) ;;15

(let ((x 100)) (do-math 10)) ;; 110

```


## Defining customization variables {#defining-customization-variables}

[Check for more details](https://github.com/daviwil/emacs-from-scratch/blob/master/show-notes/Emacs-Lisp-04.org#using-defcustom)

Customizable variables is user-facing setting so people can set up
those variables without coding.

Using `defcustom`


### Set customizable variable (correctly) {#set-customizable-variable--correctly}

The variables defined to be customized and `setq` might not trigger the
behavior! The correct way to set those customized variable is to use
`cutomize-set-variable`

```emacs-lisp

(customize-set-variable 'tab-width 2)
(customize-set-variable 'org-directory "~/Notes")


```

If using `use-package`, use `:custom` section

```emacs-lisp

(use-package emacs
	:custom
	(tab-width 2))

```

Use `C-h v` - _describe varibale_ to check whether a variable is
customizable or not; Another way is to use `custom-variable-p` to
evaluate on the variable symbol.

```emacs-lisp

(custom-variable-p 'tab-width)

```