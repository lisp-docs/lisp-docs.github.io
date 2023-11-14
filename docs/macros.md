# Macros

One of the most distinctive features of Common Lisp is its macros. In most other languages, code is strutured according to a fairly complex syntax, which is parsed to generate an abstract syntax tree to be processed further by the compiler or interpreter and is not generally available to the programmer. Lisp code is already structed as a tree of s-expressions, read in as a simple cons structure which can be manipulated in the same way as any other data. This equivalance between code and data is described by the Greek word "homoiconicity" and it makes Lisp macros particularly powerful and easy to write.

## Examples

Macros have multiple uses. They can introduce new syntax to the language, control when code is evaluated and how many times, elminate repetitive boilerplate or make programs more efficient by doing computation at compile time.

### AND

Suppose we have some lisp forms and we want to check they all evaluate to true. The obvious way to write this is (and a b c), where a b and c are some arbitrary lisp forms. We could implement this as a function, using rest parameters and recursion to operate on an arbitrary number of values before returning either the final value, or nil if any of the other values are nil.

```lisp
(defun and (&rest values)
  (when (car values)
    (if (cdr values)
        (and (cdr values))
        (car values))))
```

```lisp
* (and (< 1 2) 3)
3
* (and (> 1 2) 3)
nil
```

This seems to work, but suppose we are writing a control system for a rocket silo. We want to perform a sequence of operations, checking each stage returns a true value to indicate it has been successfully completed.

```lisp
* (and (open-doors) (prime-fuel-tanks) (launch-rocket))
```

Why did our silo just blow up?

These are all functions with side effects. Calling open-doors sends a signal to open the doors and then returns t if sensors indicate the doors have opened correctly or nil otherwise. When a function is called all of its arguments are all evaluated first, followed by the body of the function. This means that our `and` expression will try to open the doors, prime the fuel tanks and launch the rocket, and only afterwards check the doors opened successfully.

With a macro we can control exactly when the evaluation happens

```lisp
(defmacro %and (&rest forms)
  (list 'when (car forms)
     (if (cddr forms) 
      (cons '%and (cdr forms))
      (cadr forms))))
```

Our missile silo controller is expanded at compile time to

```lisp
(if (open-doors)
  (if (prime-fuel-tanks)
    (launch-rocket)))
```

If an argument evaluates to nil, the chain of `if` statements is broken and none of the remaining arguments are evaulated.

### Backquote

Common Lisp's backquote syntax is very useful tool for constructing code in macros and complex data structures generally. Instead of constructing a data structure with functions like `cons` `list` and `append`, one can simple write a quoted template and insert values into it by unquoting with commas.
For instance, instead of

```lisp
(append '(there will be) (incf n) '(green bottles standing on the wall))
```

With backquote we can write

```lisp
* (defvar n 10)
N
* `(there will be ,(decf n) green bottles standing on the wall)
(there will be 9 green bottles standing on the wall)
* `(there will be ,(decf n) green bottles standing on the wall)
(there will be 8 green bottles standing on the wall)
```

The comma unquotes the expression following it so it is evaluated as if it were outside the quote. n is decremented and the result is inserted into the quoted structure.

Lists can also be unquoted with `,@` which splices the contents of the list into the surrounding structure. Compare

```lisp
* (defvar object '(green bottle))
OBJECT
* `(there will be 1 ,object standing on the wall)
(there will be 1 (green bottle) standing on the wall)
* `(there will be 1 ,@object standing on the wall)
(there will be 1 green bottle standing on the wall)
```

Backquote is extremely useful for generating code in macros. Subsequent examples will make heavy use of it.

### Comparing numbers

Quite a program needs to compare numbers and do something different depending on which is larger. In Lisp we could use a `cond` form like this

```lisp
(cond ((< x y) (y-is-bigger))
      ((= x y) (both-equal))
      (t       (x-is-bigger)))
```

This is quite long winded for such a common pattern. It would be clearer and more convenient if we could write something like `(compare (x y) (y-is-bigger) (x-is-bigger) (both-equal))`. Luckily with macros we can.

```lisp
(defmacro compare ((a b) < = >)
 `(cond ((< ,a ,b) ,<)
        ((= ,a ,b) ,=)
        (t        ,>)))
```

```lisp
* (let ((x 100)
        (y 10))
    (compare (x y) 'y-is-bigger 'both-equal 'x-is-bigger))
X-IS-BIGGER
```

This appears to work but there is a problem. If the first test, `(< ,a ,b)`, fails, a and b are evaluated again in the second test where we have `(= ,a ,b)`. Arguments to a macro are not evaluated until after the macro is expanded, so if we insert `a` twice, it will be evaluated twice. For functions which have side effects or require a lot of computation, this is a problem.

We can solve this problem by generating code to evaluating a and b and bind them to variables before comparing them.

```lisp
(defmacro compare ((a b) < = >)
  `(let ((a ,a)
         (b ,b))
     (cond ((< a b) ,<)
           ((= a b) ,=)
           (t       ,>))))
```

```lisp
* (let ((x 11)
        (y 10))
    (compare ((decf x) y) 'b-is-bigger 'both-equal 'a-is-bigger))
BOTH-EQUAL
```

Good, now `(decf x)` is only done once and the result is equal to y. Have we finished? How about this?

```lisp
* (let ((x 10)
        (y 100)
        (a 'y-is-bigger)
        (b 'both-equal)
        (c 'x-is-bigger))
    (compare (x y) a b c))
10
```

What has happened here? We expected to get back the value of a: `'y-is-bigger`. Instead of which the value of x has leaked through, replacing the value we supplied. Let's look at the expansion of this macro (we can get the result of expanding a macro with `macroexpand`)

```lisp
* (macroexpand '(compare (x y) a b c))
(LET ((A X) (B Y))
  (COND ((< A B) A) ((= A B) B) (T C)))
```

Looking at the expanded macro it's quite obvious what is going wrong. `x` is bound to `a` in the code produced by the macro which overrides the outer binding of a to 'y-is-bigger. We could get around this by coming up with more obscure names for bindings in macros, but there is a better solution. The function `gensym` returns a new uninterned symbol. Gensyms are guarenteed to be unique because no uninterned symbol is eq to any other, so if we bind a value to a gensym, we can be certain no other variables will be accidentally captured.

```lisp
(defmacro compare ((a b) < = >)
  (let ((asym (gensym))
        (bsym (gensym)))
    `(let ((,asym ,a)
           (,bsym ,b))
        (cond ((< ,asym ,bsym) ,<)
              ((= ,asym ,bsym) ,=)
              (t       ,>)))))
```

This can be expressed far more elegantly using the `once-only` macro, originally written by Peter Norvig and available in the Alexandria utility library. `once-only` automatically introduces gensym bindings for expressions in exactly the same way as the previous example.

```lisp
(require 'alexandria)
(import alexandria:once-only)

(defmacro compare ((a b) < = >)
  (once-only (a b)
    `(cond ((< ,a ,b) ,<)
           ((= ,a ,b) ,=)
           (t         ,>))))
```

## Understanding Macros

### Time of Evaluation

\\ <!-- The below use of the <code> tag is because of an issue with rendering the backtick https://github.com/facebook/docusaurus/issues/2004 -->
### The `,` Comma, <code>&#096;</code> Back Quote, and `'` Quote operators

### The `@` Splice operator

### Declaring Variables with GENSYM

## Variable Capture

## Classic Macro Pitfalls

## Reader Macros
