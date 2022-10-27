+++
title = "Learning Elisp 01 Functions"
author = ["Chloe"]
date = 2022-10-27
lastmod = 2022-10-27T12:14:06-04:00
tags = ["emacs", "lisp"]
draft = false
+++

## Define a function {#define-a-function}

Specify the \`&amp;optional\` or \`&amp;rest\` argument in the arguments

```emacs-lisp

(defun multiply-many (x &optional y &rest operands)
	"Multiplying the result of math expression on the arguments X, Y and OPERANDS"
			(dolist (i operands)
				(when i
					(setq x (* x
							 (or y 1)
							 i))))
			x)

(multiply-many 2 3 8 19)

```


## Function wihtout names {#function-wihtout-names}

Lambda function - a function without a name

-   pass a functionto another function
-   don't want to define for the function

<!--listend-->

```emacs-lisp

;;call an lambda function directly
((lambda (x y)
	 (+ 100 x y))
 10 20)

```


## Invoking Function {#invoking-function}

Be aware that when invoking the function by **passing symbol**, \`'\` cannot
be ignored, otherwise the gimmie-function will assume \`named-version\`
is a variable instead of a function and tries to search for it in the
<span class="underline">variable space</span> instead of the <span class="underline">function space</span>.

```emacs-lisp

;;calling the function by symbol
(funcall '+ 2 2)

;; define a function that acceptsa function

(defun gimmie-function (fun x)
	(message "Function: %s -- Result: %d"
		 fun
		 (funcall fun x)))

;; store a lambda in a variable
(setq function-in-variable (lambda (arg) (+ arg 1)))

;; the equivalence of the above
(defun named-version (arg)
	(+ arg 1))

;; Invoke lamda from parameter
(gimmie-function (lambda (arg) (+ arg 1)) 5)

;; Invoke lambda stored in variable (same as above)
(gimmie-function function-in-variable 5)

;; Invoke function by passing sybol
(gimmie-function 'named-version 5)

```

If having a <span class="underline">list</span> of values that you want to pass to a function; use
\`apply\` instead:

```emacs-lisp
(apply '+ '(2 2))

(apply 'multiply-many '(1 2 3 4 5))
```


## Interactive function (Command) {#interactive-function--command}

-   They show up in M-x command list
-   Can be used in key-bindings
-   Can have parameters sent via prefix arguments \`C-u\`

<!--listend-->

```emacs-lisp

(defun my-first-command ()
	(interactive)
	(message "Hey it's worked"))

```

Interactive function allows you to pass argument to the
function. The parameter says what type of arguments can be pass onto
these interactive functions.

N - prompt for numbers or unmeric prefix argument
p - Use numeric prefix without prompting (only prefix arguments)
M - prompt for a string
i - skip an "irrelevant" argument

Making the first example interactive:

```emacs-lisp

(defun multiply-many (x &optional y &rest operands)
	"Multiplying the result of math expression on the arguments X, Y and OPERANDS"
	(interactive "Nx:\nNy(optional):\nNAny more arguments:")
				 (dolist (i operands)
					 (when i
						 (setq x (* x
						(or y 1)
						i))))
				 (message "The result is %d" x))


```

Bind \`multiply-many\` to C-c z

```emacs-lisp

(global-set-key (kbd "C-c z") 'multiply-many)

```