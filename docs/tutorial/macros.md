---
sidebar_position: 9
---

# Macros

:::warning
This article **needs to be checked!**.
Please help contribute by checking it and making changes in [our repository](https://github.com/lisp-docs/lisp-docs.github.io) or by clicking on the "Edit this page" link below.
:::

## Introduction to Macros in Common Lisp

Macros are a powerful metaprogramming feature in Common Lisp that allow you to extend the language itself. Unlike functions, which operate on evaluated arguments at runtime, macros operate on the *unevaluated* code (the abstract syntax tree or AST) at compile time. This allows you to perform code transformations, create new syntactic forms, and implement domain-specific languages within Lisp.

This tutorial will introduce you to the fundamental concepts of macros, including how they work, how to define them, and how to use them effectively. We'll also cover related concepts like backquote, comma, and macro expansion.

**Key Concepts:**

* **Code as Data:** Lisp code is represented as lists, which are also Lisp data structures. This allows macros to manipulate code as data.
* **Macro Expansion:** The process of transforming macro calls into Lisp code.
* **`defmacro`:** The special form used to define macros.
* **Backquote (`) and Comma (,):** Special operators used for constructing Lisp code within macros.
* **Macro Arguments:** The unevaluated forms passed to a macro.
* **Hygiene:** Preventing unintended variable capture in macros.

**Table of Contents:**

**1. Introduction to Macros:**

* What are macros?
* Macros vs. functions.
* The macro expansion process.

**2. Defining Macros with `defmacro`:**

* Basic `defmacro` syntax.
* Macro parameters.
* The macro body.

**3. Backquote (`) and Comma (,):**

* Backquote for quoting code templates.
* Comma for unquoting expressions within backquoted templates.
* Comma-at (`,@`) for splicing lists.

**4. Macro Expansion:**

* `macroexpand-1`: Expanding a macro call once.
* `macroexpand`: Fully expanding a macro call.

**5. Macro Arguments:**

* Required, optional, and rest parameters in macros.
* Destructuring in macro parameters.

**6. Macro Hygiene and `gensym`:**

* The problem of variable capture.
* Using `gensym` to create unique symbols.

**7. Examples and Advanced Techniques:**

* Creating control flow macros.
* Implementing domain-specific languages (DSLs).
* Debugging macros.

This tutorial will provide a comprehensive introduction to macros in Common Lisp, equipping you with the knowledge to extend the language and write more expressive and powerful code.

## 1. Introduction to Macros

Macros are a powerful feature of Common Lisp that allow you to extend the syntax of the language. They operate on code at compile time, transforming it before it is evaluated. This section introduces the basic concepts of macros.

### 1.1 What are Macros?

Macros are essentially code transformers. They take Lisp code as input (in its unevaluated form) and produce new Lisp code as output. This output is then evaluated by the Lisp interpreter or compiler.

Think of macros as functions that operate on *code* rather than *data*. While a function receives evaluated arguments and returns a value, a macro receives unevaluated code forms (lists, symbols, etc.) and returns a new code form.

### 1.2 Macros vs. Functions

Here's a comparison highlighting the key differences between macros and functions:

| Feature          | Macro                                       | Function                                      |
| ---------------- | ------------------------------------------ | -------------------------------------------- |
| Evaluation Time | Compile time (before evaluation)           | Runtime (during evaluation)                 |
| Arguments       | Unevaluated code forms (syntax trees)     | Evaluated values                             |
| Return Value    | A new code form (to be evaluated)         | A value                                      |
| Purpose         | Code transformation, syntax extension       | Computation, data manipulation             |
| How it works    | Transforms code before it is interpreted/compiled | Executes code to compute a result based on arguments |

#### Example

Let's imagine we want a shorthand for a simple `if` statement that checks if a number is positive:

**Function approach (less ideal for syntax extension):**

```lisp
(defun is-positive-then (number then-form)
  (if (> number 0)
      (eval then-form) ; Avoid using eval in most cases
      nil))

(is-positive-then 5 '(print "Positive!")) ; Prints "Positive!"
(is-positive-then -1 '(print "Positive!")) ; Returns NIL
```

This function takes the `then-form` as a list and uses `eval` to evaluate it. This is generally discouraged due to performance and scoping issues.

**Macro approach (more powerful):**

```lisp
(defmacro when-positive (number &body body)
  `(if (> ,number 0)
       (progn ,@body)))

(when-positive 5 (print "Positive!") (print "Another action!")) ; Prints "Positive!" and "Another action!"
(when-positive -1 (print "Positive!")) ; Does nothing
```

This macro takes the `body` as a list of forms and uses backquote and comma (explained in the next section) to construct the `if` expression. The key advantage here is that the `body` forms are *not* evaluated by the macro itself; they are inserted directly into the resulting code, which is then evaluated by Lisp.

### 1.3 The Macro Expansion Process

The macro expansion process happens before the code is evaluated or compiled. It consists of the following steps:

1. **Macro Call:** The Lisp system encounters a form where the first element is a macro name (e.g., `when-positive`).
2. **Macro Expansion:** The macro function associated with the macro name is called. The unevaluated arguments of the macro call are passed to the macro function.
3. **Code Transformation:** The macro function performs its code transformation and returns a new Lisp form.
4. **Result Evaluation/Compilation:** The resulting form (the *macro expansion*) replaces the original macro call in the code. This expanded code is then evaluated or compiled as usual.

#### Example of Macro Expansion

Let's trace the expansion of the `when-positive` macro:

```lisp
(when-positive 5 (print "Positive!"))
```

1. **Macro Call:** The Lisp system sees `(when-positive 5 (print "Positive!"))`.
2. **Macro Expansion:** The `when-positive` macro function is called with the arguments `5` and `(print "Positive!")`.
3. **Code Transformation:** The macro function returns the following code form:

    ```lisp
    (if (> 5 0)
        (progn (print "Positive!")))
    ```

4. **Result Evaluation:** This expanded form is now evaluated by Lisp, printing "Positive!".

The `macroexpand-1` and `macroexpand` functions (covered later) are crucial for inspecting the results of macro expansion.

Understanding the macro expansion process is key to understanding how macros work and how to write them effectively. The next section will cover the `defmacro` special form, which is used to define macros.

## 2. Defining Macros with `defmacro`

The `defmacro` special form is used to define macros in Common Lisp. This section explains the syntax of `defmacro` and how to use it to create your own macros.

### 2.1 Basic `defmacro` Syntax

The basic syntax of `defmacro` is as follows:

```lisp
(defmacro macro-name (parameter-list)
  "Optional documentation string"
  body)
```

* **`macro-name`**: The name of the macro (a symbol).
* **`parameter-list`**: A list of parameters that the macro accepts.
* **"Optional documentation string"**: A string that describes the macro's purpose. This is good practice for documenting your macros.
* **`body`**: The Lisp code that performs the macro's transformation. This code should return a new Lisp form.

### 2.2 Macro Parameters

The `parameter-list` in `defmacro` can contain several types of parameters:

* **Required parameters:** These are parameters that must be provided when the macro is called. They are represented by symbols.
* **Optional parameters:** These parameters are optional and can have default values. They are specified using `&optional`.
* **Rest parameter:** This parameter collects any remaining arguments into a list. It is specified using `&rest`.
* **Keyword parameters:** These parameters are passed using keywords (e.g., `:key1 value1`). They are specified using `&key`.
* **Auxiliary variables:** These are local variables used within the macro body. They are specified using `&aux`.

#### Examples of Parameter Lists

* `(x y)`: Two required parameters, `x` and `y`.
* `(x &optional y)`: One required parameter `x` and one optional parameter `y` (default value is `nil`).
* `(x &optional (y 10))`: One required parameter `x` and one optional parameter `y` with a default value of `10`.
* `(x &rest rest)`: One required parameter `x` and a rest parameter `rest` (which will be a list of any remaining arguments).
* `(&key name age)`: Two keyword parameters `name` and `age`.
* `(&key (name "default") (age 20))` Keyword parameters with default values.
* `(x &aux (z (+ x 1)))`: One required parameter x and an auxiliary variable `z` initialized to `(+ x 1)`.

### 2.3 The Macro Body

The `body` of a `defmacro` form contains the Lisp code that performs the macro's transformation. This code should return a new Lisp form, which will replace the original macro call.

The most common way to construct the new Lisp form is to use *backquote* (`) and *comma* (,).

* **Backquote (`)**: Creates a template for the new code form. Expressions within the backquoted template are not evaluated unless they are preceded by a comma.
* **Comma (,):** Unquotes an expression within a backquoted template. The expression is evaluated, and its result is inserted into the template.
* **Comma-at (`,@`):** Splices a list into a backquoted template. The elements of the list are inserted directly into the template, without creating a sublist.

#### Example: A Simple Macro

```lisp
(defmacro my-add (x y)
  `(+ ,x ,y))

(my-add 2 3) ; Expands to (+ 2 3), which evaluates to 5
```

In this example:

1. The `defmacro` form defines a macro named `my-add`.
2. The parameter list is `(x y)`.
3. The body is `` `(+ ,x ,y) ``.
4. The backquote creates the template `(+ ,x ,y)`.
5. The commas before `x` and `y` cause their values to be inserted into the template.

#### Example with `&rest` and `,@`

```lisp
(defmacro my-sum (&rest numbers)
  `(+ ,@numbers))

(my-sum 1 2 3 4) ; Expands to (+ 1 2 3 4), which evaluates to 10
```

In this example:

1. The `&rest numbers` parameter collects all arguments into a list named `numbers`.
2. The `,@numbers` splices the elements of the `numbers` list into the `+` form.

#### Example with `&key`

```lisp
(defmacro make-point (&key (x 0) (y 0))
  `(list :x ,x :y ,y))

(make-point :x 10 :y 20) ; Expands to (LIST :X 10 :Y 20)
(make-point :x 5)      ; Expands to (LIST :X 5 :Y 0)
(make-point)           ; Expands to (LIST :X 0 :Y 0)
```

This macro creates a point represented as a list with `:x` and `:y` keys. It uses keyword parameters with default values.

Understanding `defmacro`, parameter lists, backquote, comma, and comma-at is essential for writing effective macros in Common Lisp. The next section will cover macro expansion in more detail.

## 3. Backquote (`) and Comma (,)

Backquote (`) and comma (,) are special operators in Common Lisp that are essential for writing macros. They provide a concise and powerful way to construct Lisp code templates.

### 3.1 Backquote for Quoting Code Templates

The *backquote* character (`) is a shorthand for the`quote` special form, but with the added ability to *unquote* expressions within the quoted form. It creates a template for Lisp code, where most parts are treated literally, but some parts can be dynamically inserted.

**Basic Usage:**

```lisp
`(a b c) ; Equivalent to (quote (a b c))
        ; Evaluates to (A B C)

`(1 2 3) ; Equivalent to (quote (1 2 3))
        ; Evaluates to (1 2 3)
```

In these simple cases, backquote behaves exactly like `quote`. However, its power comes into play when combined with the comma.

### 3.2 Comma for Unquoting Expressions within Backquoted Templates

The *comma* character (,) is used to *unquote* an expression within a backquoted template. The expression following the comma is evaluated, and its result is inserted into the template.

#### Example

```lisp
(let ((x 10))
  `(a b ,x c)) ; Evaluates to (A B 10 C)
```

Here's how it works:

1. The backquote creates a template `(a b ,x c)`.
2. The comma before `x` causes the value of `x` (which is 10) to be inserted into the template.
3. The resulting list `(A B 10 C)` is returned.

**More Examples:**

```lisp
(let ((name "Alice"))
  `(hello ,name)) ; Evaluates to (HELLO "Alice")

(let ((operation '+))
  `(,operation 5 3)) ; Evaluates to (+ 5 3), which then evaluates to 8

(let ((list '(1 2 3)))
    `(a ,list b)) ; Evaluates to (A (1 2 3) B)
```

Without the comma, the `x` in `` `(a b x c) `` would be treated as a literal symbol `X`, not the value of the variable `x`.

### 3.3 Comma-at (`,@`) for Splicing Lists

The *comma-at* operator (`,@`) is used to *splice* a list into a backquoted template. This means that the *elements* of the list are inserted directly into the template, rather than the list itself being inserted as a sublist.

#### Example

```lisp
(let ((numbers '(1 2 3)))
  `(a ,@numbers b)) ; Evaluates to (A 1 2 3 B)
```

Here's the difference between comma and comma-at:

* `,` inserts the *value* of the expression.
* `,@` inserts the *elements* of the list that is the value of the expression.

**More Examples:**

```lisp
(let ((names '("Alice" "Bob")))
    `(hello ,@names and everyone else)) ; Evaluates to (HELLO "Alice" "Bob" AND EVERYONE ELSE)

(let ((forms '((+ 1 2) (* 3 4))))
    `(progn ,@forms)) ; Evaluates to (PROGN (+ 1 2) (* 3 4))
```

**Common Use Case in Macros:**

Comma-at is commonly used with `&rest` parameters in macros to insert the collected arguments into the generated code:

```lisp
(defmacro my-list (&rest items)
  `(list ,@items))

(my-list 1 2 3) ; Expands to (LIST 1 2 3)
```

In this macro, `&rest items` collects all arguments into the list `items`. The `,@items` then splices the elements of `items` into the `list` form.

#### Example showing the difference between , and ,@

```lisp
(let ((items '(1 2 3)))
  `(list ,items)) ; Evaluates to (LIST (1 2 3)) ; A sublist

(let ((items '(1 2 3)))
  `(list ,@items)) ; Evaluates to (LIST 1 2 3)     ; Splices the elements
```

Understanding backquote, comma, and comma-at is crucial for writing effective and concise macros. They provide a powerful way to construct and manipulate Lisp code within macros. The next section will cover macro expansion in more detail.

## 4. Macro Expansion

Understanding macro expansion is crucial for debugging and understanding how macros work. Common Lisp provides two functions for inspecting the result of macro expansion: `macroexpand-1` and `macroexpand`.

### 4.1 `macroexpand-1`: Expanding a Macro Call Once

The `macroexpand-1` function expands a macro call *once*. If the given form is not a macro call, it simply returns the form unchanged.

The syntax is:

```lisp
(macroexpand-1 form &optional environment)
```

* `form`: The Lisp form to expand.
* `environment`: An optional environment to use for expansion (usually not needed for simple cases).

`macroexpand-1` returns two values:

1. The expanded form.
2. A boolean indicating whether the form was expanded (T) or not (NIL).

#### Examples

Let's use the `when-positive` macro from a previous example:

```lisp
(defmacro when-positive (number &body body)
  `(if (> ,number 0)
       (progn ,@body)))

(macroexpand-1 '(when-positive 5 (print "Positive!")))
; Returns:
; (IF (> 5 0) (PROGN (PRINT "Positive!")))
; T
```

Here, `macroexpand-1` expands the `when-positive` macro call into its corresponding `if` form. The second return value `T` indicates that an expansion occurred.

If the form is not a macro call:

```lisp
(macroexpand-1 '(+ 1 2))
; Returns:
; (+ 1 2)
; NIL
```

In this case, `macroexpand-1` returns the form unchanged, and the second return value is `NIL`.

#### Example with nested macros

If you have nested macros, `macroexpand-1` only expands the outermost macro:

```lisp
(defmacro my-print (x) `(print ,x))
(defmacro my-when (condition &body body) `(if ,condition (progn ,@body)))

(macroexpand-1 '(my-when (> 5 0) (my-print "Hello")))
; Returns:
; (IF (> 5 0) (PROGN (MY-PRINT "Hello")))
; T
```

Only `my-when` is expanded. `my-print` is left as is.

### 4.2 `macroexpand`: Fully Expanding a Macro Call

The `macroexpand` function fully expands a macro call, recursively expanding any nested macros until no more expansions are possible.

The syntax is:

```lisp
(macroexpand form &optional environment)
```

* `form`: The Lisp form to expand.
* `environment`: An optional environment to use for expansion (usually not needed for simple cases).

`macroexpand` also returns two values:

1. The fully expanded form.
2. A boolean indicating whether the form was expanded (T) or not (NIL).

#### Example using the nested macros from above

```lisp
(macroexpand '(my-when (> 5 0) (my-print "Hello")))
; Returns:
; (IF (> 5 0) (PROGN (PRINT "Hello")))
; T
```

This time, `macroexpand` expands both `my-when` *and* `my-print`, resulting in the fully expanded form.

#### Example with no macros

```lisp
(macroexpand '(+ 1 2))
; Returns:
; (+ 1 2)
; NIL
```

If the form contains no macros, `macroexpand` returns the form unchanged, just like `macroexpand-1`.

**Using `macroexpand-1` and `macroexpand` for Debugging:**

These functions are invaluable for debugging macros. By inspecting the expanded code, you can see exactly what your macro is generating and identify any errors in your macro definition.

#### Example

If you have a macro that's not behaving as expected, you can use `macroexpand-1` or `macroexpand` to see the generated code:

```lisp
(defmacro buggy-macro (x) `(setf y ,x)) ; Intended to set a local variable

(macroexpand-1 '(buggy-macro 10))
; Returns:
; (SETF Y 10)
; T
```

This reveals that the macro is setting the *global* variable `y`, not a local variable. To fix this, you would need to use `gensym` (as explained in the next section) to create a unique local variable name.

Understanding `macroexpand-1` and `macroexpand` is essential for debugging and understanding how macros transform code. They provide a powerful way to inspect the inner workings of macros. The next section will cover macro arguments in more detail.

## 5. Macro Arguments

This section covers how to handle arguments passed to macros, including required, optional, rest, and keyword parameters, as well as destructuring.

### 5.1 Required, Optional, and Rest Parameters in Macros

Macro parameter lists support the same kinds of parameters as function parameter lists: required, optional, and rest parameters.

* **Required parameters:** These parameters *must* be provided when the macro is called.
* **Optional parameters:** These parameters are optional and can have default values. They are introduced by `&optional`.
* **Rest parameter:** This parameter collects any remaining arguments into a list. It is introduced by `&rest`.

#### Examples

**1. Required parameters:**

```lisp
(defmacro my-times (x y)
  `(* ,x ,y))

(my-times 2 3) ; Expands to (* 2 3)
```

`x` and `y` are required parameters.

**2. Optional parameters:**

```lisp
(defmacro my-increment (x &optional (by 1))
  `(+ ,x ,by))

(my-increment 5)    ; Expands to (+ 5 1)
(my-increment 5 3)  ; Expands to (+ 5 3)
```

`by` is an optional parameter with a default value of `1`.

**3. Rest parameter:**

```lisp
(defmacro my-print-all (&rest items)
  `(progn ,@(mapcar #'(lambda (item) `(print ,item)) items)))

(my-print-all 1 "hello" 'foo)
; Expands to:
; (PROGN (PRINT 1) (PRINT "hello") (PRINT 'FOO))
```

`items` is a rest parameter that collects all remaining arguments into a list. `mapcar` is used to generate a list of `print` forms, which are then spliced into the `progn` form using `,@`.

**4. Keyword parameters:**

```lisp
(defmacro make-person (&key name (age 0))
    `(list :name ,name :age ,age))

(make-person :name "John" :age 30) ; Expands to (LIST :NAME "John" :AGE 30)
(make-person :name "Jane")       ; Expands to (LIST :NAME "Jane" :AGE 0)
(make-person :age 25)           ; Expands to (LIST :NAME NIL :AGE 25)
(make-person)                   ; Expands to (LIST :NAME NIL :AGE 0)
```

`name` and `age` are keyword parameters with default values.

### 5.2 Destructuring in Macro Parameters

*Destructuring* allows you to match complex data structures in macro parameter lists. This is particularly useful when you want to handle list arguments with specific structures.

#### Example

```lisp
(defmacro set-point (point value)
  `(setf (aref ,point 0) (car ,value)
         (aref ,point 1) (cdr ,value)))

(let ((p (make-array 2)))
  (set-point p '(10 20))
  p) ; Returns #(10 20)
```

This macro expects the `value` argument to be a cons cell (a list of two elements).

**Destructuring with nested lists:**

```lisp
(defmacro set-point ((x y) value)
  `(progn
     (setf ,x (car ,value))
     (setf ,y (cadr ,value))))

(let ((x 0) (y 0))
    (set-point (x y) '(10 20))
    (list x y)) ; Returns (10 20)
```

Here, the parameter list `(x y)` destructures the first argument. If you call the macro with `(set-point (a b) '(10 20))`, `a` will be bound to `10`, and `b` will be bound to `20`. This is much more concise and readable than manually accessing the elements of the list.

**Combining destructuring with `&rest`:**

```lisp
(defmacro my-map (function &rest lists)
  `(mapcar ,function ,@lists))

(my-map #'+ '(1 2 3) '(4 5 6)) ; Expands to (MAPCAR #'+ '(1 2 3) '(4 5 6))
```

This macro uses `&rest` to collect multiple lists and then splices them into the `mapcar` form.

Destructuring is a powerful feature that makes macros more expressive and easier to use when dealing with structured data. It greatly simplifies code transformation within macros. The next section will cover macro hygiene and `gensym`.

## 6. Macro Hygiene and `gensym`

*Macro hygiene* is a crucial concept in Lisp macro programming. It refers to the property that macros should not inadvertently capture variables in the code where they are used. This section explains the problem of variable capture and how to solve it using `gensym`.

### 6.1 The Problem of Variable Capture

*Variable capture* occurs when a macro introduces a variable name that clashes with a variable name already present in the code where the macro is used. This can lead to unexpected behavior and hard-to-debug errors.

#### Example of Variable Capture

```lisp
(defmacro my-loop (n &body body)
  `(let ((i 0)) ; Introduces a variable i
     (loop repeat ,n
           do (progn ,@body)
              (incf i))))

(let ((i 100)) ; Outer i
  (my-loop 5 (print i))) ; Prints 0, 1, 2, 3, 4, not 100

```

In this example, the `my-loop` macro introduces a variable `i` for its internal loop counter. However, the code where the macro is used also has a variable named `i`. This causes the macro's `i` to *capture* the outer `i`, resulting in incorrect behavior. The outer `i` is shadowed by the inner `i` of the macro.

**Another Example:**

```lisp
(defmacro with-temp-list (list-var &body body)
  `(let ((temp-list nil))
     (setf temp-list ,list-var)
     ,@body
     temp-list))

(let ((temp-list '(1 2 3)))
  (with-temp-list '(4 5 6) (print temp-list)))
;Output: (4 5 6)
;instead of (1 2 3)
```

Here, the `with-temp-list` macro introduces `temp-list`, which unintentionally shadows the outer `temp-list`.

### 6.2 Using `gensym` to Create Unique Symbols

The solution to variable capture is to use `gensym` (generate symbol) to create unique, uninterned symbols for internal macro variables. These symbols are guaranteed not to clash with any user-defined variables.

`gensym` creates a new symbol that is guaranteed to be different from any other symbol in the system.

```lisp
(gensym)     ; Returns a unique symbol like #:G123
(gensym "X-") ; Returns a unique symbol with a prefix like #:X-456
```

**Fixing the `my-loop` macro:**

```lisp
(defmacro my-loop (n &body body)
  (let ((i (gensym))) ; Create a unique symbol
    `(let ((,i 0))
       (loop repeat ,n
             do (progn ,@body)
                (incf ,i)))))

(let ((i 100))
  (my-loop 5 (print i))) ; Now correctly prints 100 five times
```

By using `(gensym)` to create the `i` variable, we ensure that it does not clash with any other variables in the surrounding code.

**Fixing the `with-temp-list` macro:**

```lisp
(defmacro with-temp-list (list-var &body body)
  (let ((temp-list (gensym)))
    `(let ((,temp-list nil))
       (setf ,temp-list ,list-var)
       ,@body
       ,temp-list)))

(let ((temp-list '(1 2 3)))
  (with-temp-list '(4 5 6) (print temp-list)))
;Output: (1 2 3)
```

Now the outer `temp-list` is not shadowed.

**Best Practice:**

It's a best practice to *always* use `gensym` for any variables that are introduced by your macros. This will prevent variable capture and make your macros more robust.

By understanding macro hygiene and using `gensym`, you can write macros that are safe and predictable. This is essential for building complex Lisp systems and extending the language effectively. The next section will cover examples and advanced macro techniques.

## 7. Examples and Advanced Techniques

This section demonstrates some practical applications of macros, including creating control flow macros, implementing simple domain-specific languages (DSLs), and debugging macros.

### 7.1 Creating Control Flow Macros

Macros are well-suited for creating custom control flow constructs.

**1. A `unless` macro:**

The standard Common Lisp provides `when` and `unless`; here's an example of how to create `unless`:

```lisp
(defmacro unless (condition &body body)
  `(when (not ,condition)
     ,@body))

(let ((x 5))
  (unless (< x 0)
    (print "x is not negative"))) ; Prints "x is not negative"
```

This macro simply inverts the condition of a `when` form.

**2. A `for-each` macro:**

```lisp
(defmacro for-each ((var list) &body body)
  (let ((glist (gensym)))
    `(dolist (,var ,glist)
       (let ((,glist ,list))
         ,@body))))

(for-each (x '(1 2 3 4 5)) (print x))
; Prints 1, 2, 3, 4, 5
```

This macro iterates over the elements of a list, binding each element to the specified variable. The use of `gensym` prevents variable capture if the user's code already uses a variable with the same name as the list variable.

**3. A `dotimes-indexed` macro:**

This macro provides an index in addition to the value in each loop iteration.

```lisp
(defmacro dotimes-indexed ((index var sequence) &body body)
  (let ((gseq (gensym))
        (gindex (gensym)))
    `(let ((,gseq ,sequence))
       (loop for ,gindex from 0 below (length ,gseq)
             for ,var across ,gseq
             do (let ((,index ,gindex))
                  ,@body)))))

(dotimes-indexed (i x #(a b c))
  (format t "Element ~a at index ~a~%" x i))
;Prints:
;Element A at index 0
;Element B at index 1
;Element C at index 2
```

### 7.2 Implementing Domain-Specific Languages (DSLs)

Macros are extremely useful for creating small, embedded DSLs within Lisp.

#### Example: A simple HTML DSL

```lisp
(defmacro html (&rest body)
  `(concatenate 'string ,@body))

(defmacro tag (name &rest content)
  `(format nil "<~a>~{~a~^ ~}</~a>" ,name ,content ,name))

(defmacro bold (&rest content)
  `(tag "b" ,@content))

(defmacro paragraph (&rest content)
  `(tag "p" ,@content))

(html (tag "html"
           (tag "head" (tag "title" "My Page"))
           (tag "body"
                (paragraph (bold "Hello") " world!"))))

;Expands to:
;(concatenate 'string
; (format nil "<~a>~{~a~^ ~}</~a>" "html"
;         (format nil "<~a>~{~a~^ ~}</~a>" "head" (format nil "<~a>~{~a~^ ~}</~a>" "title" "My Page"))
;         (format nil "<~a>~{~a~^ ~}</~a>" "body" (format nil "<~a>~{~a~^ ~}</~a>" "p" (format nil "<~a>~{~a~^ ~}</~a>" "b" "Hello") " world!")))

;Evaluates to:
;"<html><head><title>My Page</title></head><body><p><b>Hello</b> world!</p></body></html>"
```

This example defines macros for creating HTML tags, making it easier to write HTML within Lisp code.

### 7.3 Debugging Macros

Debugging macros can be tricky because they operate at compile time. However, the `macroexpand-1` and `macroexpand` functions are invaluable for this purpose.

**1. Using `macroexpand-1` and `macroexpand`:**

As explained previously, these functions allow you to see the expanded code generated by a macro.

```lisp
(macroexpand-1 '(my-loop 3 (print "Hello"))) ; See one level of expansion
(macroexpand '(my-loop 3 (print "Hello")))   ; See the fully expanded code
```

**2. `trace`:**

The `trace` macro can be used to trace the execution of functions, including macro functions. This can be helpful for understanding the steps involved in macro expansion.

```lisp
(trace my-loop)
(my-loop 2 (print "Test")) ; Shows the macro expansion process

(untrace my-loop) ; Stop tracing
```

**3. Breakpoints in Macros:**

Some Lisp development environments (like SLIME) allow you to set breakpoints *within* macro definitions. This allows you to step through the macro expansion process and inspect the values of variables at each step.

By using these debugging techniques, you can effectively debug your macros and ensure they are generating the correct code.

This section provided examples of creating control flow macros, implementing DSLs, and debugging macros. With these techniques, you can leverage the power of macros to extend Common Lisp and write more expressive code. This concludes the tutorial on macros.
