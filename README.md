<h1> ElispCheatSheet </h1>

Quick reference to the core language of Emacs &#x2014;Editor MACroS.

( Much Emacs Lisp was utilised in making my blog
<https://alhassy.github.io> )

**The listing sheet, as PDF, can be found
[here](<https://github.com/alhassy/ElispCheatSheet/blob/master/CheatSheet.pdf>)**, 
while below is an unruly html rendition.

This reference sheet is built around the system
<https://github.com/alhassy/CheatSheet>.


# Table of Contents

1.  [Functions](#org93777fc)
2.  [Variables](#orgc310d6c)
3.  [Block of Code](#org994feb6)
4.  [List Manipulation](#orga2352b0)
5.  [Conditionals](#orgb2ce15b)
6.  [Reads](#org23e3f6d)
7.  [Loops](#org215ff9d)
8.  [Hooks](#org5fdb874)













*Everything is a list!*

-   To find out more about `name` execute `(describe-symbol 'name)`!
    -   After the closing parens invoke `C-x C-e` to evaluate.
-   To find out more about a key press, execute `C-h k` then the key press.


<a id="org93777fc"></a>

# Functions

-   Function invocation: `(f x₀ x₁ … xₙ)`. E.g., `(+ 3 4)` or `(message "hello")`.
    -   After the closing parens invoke `C-x C-e` to execute them.
    -   Only prefix invocations means we can use `-,+,*` in *names*
        since `(f+*- a b)` is parsed as applying function `f+*-` to arguments `a, b`.
        
        E.g., `(1+ 42) → 43` using function *named* `1+` &#x2013;the ‘successor function’.

-   Function definition:
    
        ;; “de”fine “fun”ctions
        (defun my-fun (arg₀ arg₁ … argₖ)         ;; header, signature
          "This functions performs task …"       ;; documentation, optional
          …sequence of instructions to perform…  ;; body
        )
    
    -   The return value of the function is the result of the last expression executed.
    -   The documentation string may indicate the return type, among other things.

-   Anonymous functions: `(lambda (arg₀ … argₖ) bodyHere)`.
    
        ;; make and immediately invoke
        ((lambda (x y) (message (format "x, y ≈ %s, %s" x y))) 1 2)
        
        ;; make then way later invoke
        (setq my-func (lambda (x y) (message (format "x, y ≈ %s, %s" x y))))
        (funcall my-func 1 2)
        ;; (my-func 1 2) ;; invalid!
    
    The last one is invalid since `(f x0 … xk)` is only meaningful for functions
    `f` formed using `defun`.

-   Recursion and IO:
    `(defun sum (n) (if (<= n 0) 0 (+ n (sum (- n 1)))))`
    -   Now `(sum 100) → 5050`.

-   IO: `(defun make-sum (n) (interactive "n") (message-box (format "%s" (sum n))))`
    -   The `interactive` option means the value of `n` is queried to the user; e.g.,
        enter 100 after executing `(execute-extended-command "" "make-sum")`
        or `M-x make-sum`.
    -   In general `interactive` may take no arguments.
        The benefit is that the function can be executed using `M-x`,
        and is then referred to as an interactive function.

\newpage


<a id="orgc310d6c"></a>

# Variables

-   Global Variables: `(setq name value)`; e.g., `(setq my-list '(1 2 3))`.
    -   This creates brand-new variables; generally: `(setq name₀ value₀ ⋯ nameₖ valueₖ)`.
-   Local Scope: `(let ((name₀ val₀) … (nameₖ valₖ)) …use nameᵢ here… )`.

-   Elisp is dynamically scoped: The caller's stack is accessible by default!
    
        (defun woah () 
          "If any caller has a local ‘work’, they're in for a nasty bug
           from me!"
          (setq work 666))
        
        (defun add-one (x)
          "Just adding one to input, innocently calling library method ‘woah’."
          (let ((work (+ 1 x)))
            (woah) ;; May change ‘work’!
            work
          )
        )
        
        ;; (add-one 2) ⇒ 666

-   Quotes: `'x` refers to the *name* rather than the *value* of `x`.
    -   This is superficially similar to pointers:
        Given `int *x = …`, `x` is the name (address)
        whereas `*x` is the value.
    -   The quote simply forbids evaluation; it means *take it literally as you see it*
        rather than looking up the definition and evaluating.

    (setq this 'hello)
    (setq that this)
    
    ;;  this  → hello
    ;; 'this  → this
    ;;  that  → hello
    ;; 'that  → that

Note: `'x ≈ (quote x)`.


<a id="org994feb6"></a>

# Block of Code

Use the `progn` function to evaluate multiple statements. E.g.,

    (progn
      (message "hello")
      (setq x  (if (< 2 3) 'two-less-than-3))
      (sleep-for 0 500)
      (message (format "%s" x))
      (sleep-for 0 500)
      23    ;; Explicit return value
    )

This' like curly-braces in C or Java. The difference is that the last expression is considered
the ‘return value’ of the block.

Herein, a ‘block’ is a number of sequential expressions which needn't be wrapped with a `progn` form.

-   Lazy conjunction and disjunction:
    
    -   Perform multiple statements but stop when any of them fails, returns `nil`: `(and s₀ s₁ … sₖ)`.
        -   Maybe monad!
    -   Perform multiple statements until one of them succeeds, returns non-`nil`: `(or s₀ s₁ … sₖ)`.
    
    We can coerce a statement `sᵢ` to returning non-`nil` as so: (`progn sᵢ t)`.
    Likewise, coerce failure by `(progn sᵢ nil)`.

-   Jumps, Control-flow transfer: Perform multiple statements and decide when and where you would like to stop.
    
    -   `(catch 'my-jump bodyBlock)` where the body may contain `(throw 'my-jump returnValue)`;
    
    the value of the catch/throw is then `returnValue`.
    
    -   Useful for when the `bodyBlock` is, say, a loop.
        Then we may have multiple `catch`'s with different labels according to the nesting of loops.
        -   Possibly informatively named throw symbol is `'break`.
    -   Using name `'continue` for the throw symbol and having such a catch/throw as *the body of a loop*
        gives the impression of continue-statements from Java.
    -   Using name `'return` for the throw symbol and having such a catch/throw as the body of a function
        definition gives the impression of, possibly multiple, return-statements from Java
        &#x2013;as well as ‘early exits’.
    -   Simple law: `(catch 'it s₀ s₁ … sₖ (throw 'it r) sₖ₊₁ ⋯ sₖ₊ₙ) ≈ (progn s₀ s₁ ⋯ sₖ r)`.
        -   Provided the `sᵢ` are simple function application forms.


<a id="orga2352b0"></a>

# List Manipulation

-   Produce a syntactic, un-evaluated list, we use the single quote:
    `'(1 2 3)`.

-   Construction: `(cons 'x₀ '(x₁ … xₖ)) → (x₀ x₁ … xₖ)`.
-   Head, or *contents of the address part of the register*:
    `(car '(x₀ x₁ … xₖ)) → x₀`.
-   Tail, or *contents of the decrement part of the register*:
    `(cdr '(x₀ x₁ … xₖ)) → (x₁ … xₖ)`.
-   Deletion: `(delete e xs)` yields `xs` with all instance of `e` removed.
    -   E.g., `(delete 1 '(2 1 3 4 1)) → '(2 3 4)`.

E.g., `(cons 1 (cons "a" (cons 'nice nil))) ≈ (list 1 "a" 'nice) ≈ '(1 "a" nice)`.


<a id="orgb2ce15b"></a>

# Conditionals

-   Booleans: `nil`, the empty list `()`, is considered *false*, all else
    is *true*. 
    -   Note: `nil ≈ () ≈ '() ≈ 'nil`.
    -   (Deep structural) equality: `(equal x y)`.
    -   Comparisons: As expected; e.g., `(<= x y)` denotes *x ≤ y*.

-   `(if condition thenExpr optionalElseBlock)`
    -   Note: `(if x y) ≈ (if x y nil)`; better: `(when c thenBlock) ≈ (if c (progn thenBlock))`.
    -   Note the else-clause is a ‘block’: Everything after the then-clause is considered to be part of it.

-   Avoid nested if-then-else clauses by using a `cond` statement &#x2013;a generalisation of switch statements.  
    
        (cond
          (test₀
            actionBlock₀)
          (test₁
            actionBlock₁)
          …
          (t                       ;; optional
            defaultActionBlock))
    
    Sequentially evaluate the predicates `testᵢ` and perform only the action of the first true test;
    yield `nil` when no tests are true.

-   Make choices by comparing against *only* numbers or symbols &#x2013;e.g., not strings!&#x2013; with less clutter by using `case`:
    
        (case 'boberto
          ('bob 3)
          ('rob 9)
          ('bobert 9001)
          (otherwise "You're a stranger!"))
    
    With case you can use either `t` or `otherwise` for the default case, but it must come last.


<a id="org23e3f6d"></a>

# Reads

-   [How to Learn Emacs: A Hand-drawn One-pager for Beginners / A visual tutorial](http://sachachua.com/blog/wp-content/uploads/2013/05/How-to-Learn-Emacs-v2-Large.png)
-   [An Introduction to Programming in Emacs Lisp](https://www.gnu.org/software/emacs/manual/html_node/eintr/index.html#Top)
-   [GNU Emacs Lisp Reference Manual](https://www.gnu.org/software/emacs/manual/html_node/elisp/index.html#Top)


<a id="org215ff9d"></a>

# Loops

Sum the first `10` numbers:

    (let ((n 100) (i 0) (sum 0))
      (while (<= i n)
        (setq sum (+ sum i))
        (setq i   (+ i   1))
      )
      (message (number-to-string sum))
    )

Essentially a for-loop:

    (dotimes (x   ;; refers to current iteration, initally 0
              n   ;; total number of iterations
    	  ret ;; optional: return value of the loop
    	 )
      …body here, maybe mentioning x…  	 
    )
    
    ;; E.g., sum of first n numbers
    (let ((sum 0) (n 100))
      (dotimes (i (1+ n) sum) (setq sum (+ sum i))))

A for-each loop: Iterate through a list.
Like `dotimes`, the final item is the expression value at the end of the loop.

    (dolist (elem '("a" 23 'woah-there) nil)
      (message (format "%s" elem))
      (sleep-for 0 500)
    )

`(describe-symbol 'sleep-for)` ;-)

Loop essentials:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left">C</td>
<td class="org-left">Elisp</td>
</tr>


<tr>
<td class="org-left">`x += y`</td>
<td class="org-left">`(incf x y)`</td>
</tr>


<tr>
<td class="org-left">`x--`</td>
<td class="org-left">`(decf x)`</td>
</tr>


<tr>
<td class="org-left">`x++`</td>
<td class="org-left">`(incf x)`</td>
</tr>
</tbody>
</table>

    (defun my/cool-function (N D)
      "Sum the numbers 0..N that are not divisible by D"
      (catch 'return
        (when (< N 0) (throw 'return 0)) ;; early exit
        (let ((counter 0) (sum 0))
          (catch 'break
            (while 'true
              (catch 'continue
                (incf counter)
                (cond
                  ((equal counter N)     (throw 'break sum))
                  ((zerop (% counter D)) (throw 'continue nil))
        	      ('otherwise            (incf sum counter))
                  )))))))
    
    (my/cool-function  100 3)  ;; ⇒ 3267
    (my/cool-function  100 5)  ;; ⇒ 4000
    (my/cool-function -100 7)  ;; ⇒ 0

Note that we could have had a final redundant `throw 'return`:
Redundant since the final expression in a block is its return value.

The special `loop` constructs provide immensely many options to form
nearly any kind of imperative loop. E.g., Python-style ‘downfrom’ for-loops
and Java do-while loops. I personally prefer functional programming, so wont
look into this much.


<a id="org5fdb874"></a>

# Hooks

Hooks are lists of functions that are called from Emacs Lisp in order to modify the behaviour of something. For example, different modes have their own hooks so that you can add functions that will run when that mode is initialised.

E.g.,
let's add the `go` function to the list of functions when a buffer 
is initialised with org-mode.

    (describe-symbol 'org-mode-hook)
    
    (defun go () (message-box "It worked!"))
    
      (add-hook 'org-mode-hook 'go)
    ≈ (add-to-list 'org-mode-hook 'go)
    
    ;; Now execute: (revert-buffer) to observe “go” being executed.
    ;; Later remove this silly function from the list:
    (remove-hook 'org-mode-hook 'go)

