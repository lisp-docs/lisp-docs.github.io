---
sidebar_position: 4
---

# Control Flow

:::warning
This article is a stub. Please help contribute in [our repository](https://github.com/lisp-docs/lisp-docs.github.io) or by clicking on the "Edit this page" link below
:::

## Introduction to Control Flow in Common Lisp

Control flow refers to the order in which statements in a program are executed. It determines the path the program takes based on conditions and other factors. Common Lisp provides a rich set of control flow forms, offering flexibility and expressiveness in structuring your code. Unlike some languages that rely heavily on structural indentation, Common Lisp uses parentheses `()` to define code blocks, making the logical structure explicit.

This tutorial will introduce you to the fundamental control flow concepts in Common Lisp and guide you through the most important control flow forms. We will cover conditional execution, looping, and non-local exits, providing examples and explanations to help you understand how to effectively use these tools.

**Key Control Flow Concepts:**

* **Conditional Execution:** Executing different code blocks based on whether a condition is true or false.
* **Iteration (Looping):** Repeating a block of code multiple times, either a fixed number of times or until a certain condition is met.
* **Non-Local Exits:** Mechanisms for transferring control to a different part of the program, bypassing the normal sequential execution.

**Table of Contents:**

Here's a breakdown of the control flow forms we will explore:

### 1. Conditional Forms

* **`if`**: Basic conditional execution.
* **`when`**: Execute a block only if a condition is true.
* **`unless`**: Execute a block only if a condition is false.
* **`cond`**: General conditional execution with multiple clauses.
* **`case`**: Conditional execution based on comparing a key to specific values.
* **`typecase`**: Conditional execution based on the type of a value.
* **`ecase`**: Error-signaling variant of `case`.
* **`etypecase`**: Error-signaling variant of `typecase`.

### 2. Looping Forms

* **`loop`**: The most versatile looping construct, offering various clauses for control.
  * Simple iteration
  * `for` clauses (iterating over sequences, numbers, etc.)
  * `while` and `until` clauses (conditional termination)
  * `collect`, `sum`, `count`, `minimize`, `maximize` (accumulation)
* **`do`**: Parameterized iteration with variable updates.
* **`dotimes`**: Iterating a fixed number of times.
* **`dolist`**: Iterating over the elements of a list.

### 3. Non-Local Exits

* **`block` and `return-from`**: Named blocks for exiting from nested expressions.
* **`catch` and `throw`**: Dynamic non-local exits, transferring control to a `catch` block.

### 4. Other Control Flow Constructs

* **`go` and `tagbody`**: Low-level control flow (generally discouraged in favor of higher-level forms).

By the end of this tutorial, you will be able to write more complex and structured Common Lisp programs using the appropriate control flow mechanisms. We will provide clear examples and explanations for each form, allowing you to quickly grasp these essential concepts.

## 1. Conditional Forms in Common Lisp

Conditional forms allow you to execute different parts of your code based on whether certain conditions are true or false. Common Lisp offers several conditional forms, each suited for different situations.

### 1.1 `if`: Basic Conditional Execution

The `if` form is the most fundamental conditional. It takes three arguments:

* **Test:** An expression that is evaluated to determine truth.
* **Then:** An expression to be evaluated if the test is true.
* **Else:** An optional expression to be evaluated if the test is false.

```lisp
(if (< 5 10) ; Test: Is 5 less than 10?
    (print "5 is less than 10") ; Then: Execute this if true
    (print "5 is not less than 10")) ; Else: Execute this if false
```

Output:

```lisp
"5 is less than 10"
```

If the `else` clause is omitted and the test is false, `if` returns `nil`.

```lisp
(if (> 5 10)
    (print "5 is greater than 10")) ; No else clause
```

Evaluation result:

```lisp
NIL
```

### 1.2 `when`: Execute a Block if True

The `when` form is a simplified `if` that executes a block of code only if the test is true. It takes a test and any number of expressions to be executed if the test is true. If the test is false, `when` returns `nil`.

```lisp
(let ((x 15))
  (when (> x 10)
    (print "x is greater than 10")
    (print (* x 2)))) ; Multiple expressions in the 'then' block
```

Output:

```lisp
"x is greater than 10"
30
```

### 1.3 `unless`: Execute a Block if False

`unless` is the opposite of `when`. It executes a block of code only if the test is *false*.

```lisp
(let ((x 5))
  (unless (> x 10)
    (print "x is not greater than 10")
    (print (* x 2))))
```

Output:

```lisp
"x is not greater than 10"
10
```

### 1.4 `cond`: General Conditional Execution

The `cond` form provides a more general way to handle multiple conditions. It takes a list of *clauses*, where each clause has a test and a sequence of expressions to be executed if the test is true. The first clause whose test evaluates to true is executed, and the rest are skipped.

```lisp
(let ((grade 85))
  (cond ((>= grade 90) (print "A"))
        ((>= grade 80) (print "B"))
        ((>= grade 70) (print "C"))
        ((>= grade 60) (print "D"))
        (t (print "F")))) ; The 't' clause acts as a default (else)
```

Output:

```lisp
"B"
```

The `t` clause is often used as the last clause to provide a default action if none of the previous tests are true.

### 1.5 `case`: Conditional Execution Based on a Key

The `case` form compares a *key* to a set of *keylists*. It's useful when you want to check for equality against specific values.

```lisp
(let ((fruit 'apple))
  (case fruit
    ((apple) (print "It's an apple!"))
    ((banana orange) (print "It's a banana or an orange!"))
    (otherwise (print "It's some other fruit."))))
```

Output:

```lisp
"It's an apple!"
```

If the key matches a key in a keylist, the corresponding expressions are executed. The `otherwise` keyword acts as a default case.

### 1.6 `typecase`: Conditional Execution Based on Type

The `typecase` form checks the *type* of a value and executes the corresponding code.

```lisp
(let ((value 10))
  (typecase value
    (integer (print "It's an integer!"))
    (string (print "It's a string!"))
    (t (print "It's something else."))))
```

Output:

```lisp
"It's an integer!"
```

### 1.7 `ecase` and `etypecase`: Error-Signaling Variants

`ecase` and `etypecase` are similar to `case` and `typecase`, respectively, but they signal an error if the key or type doesn't match any of the provided clauses. This is useful for ensuring that all possible cases are handled.

```lisp
(let ((fruit 'grape))
  (ecase fruit
    ((apple) (print "It's an apple!"))
    ((banana) (print "It's a banana!"))))
```

This will signal an error because `grape` is not in any of the keylists. Similarly, `etypecase` will signal an error if the value's type does not match any of the specified types.

These conditional forms provide a comprehensive set of tools for controlling the flow of execution in your Common Lisp programs. Choosing the right form depends on the specific logic you need to implement. `if`, `when`, and `unless` are suitable for simple conditions. `cond` is more general for multiple conditions. `case` and `typecase` provide efficient ways to check against specific values or types, with `ecase` and `etypecase` adding error checking for robustness.

## 2. Looping Forms in Common Lisp

Looping constructs allow you to repeat a block of code multiple times. Common Lisp provides several powerful looping forms, with `loop` being the most versatile.

### 2.1 `loop`: The Versatile Looping Construct

The `loop` macro is extremely flexible and can handle a wide variety of looping needs. It uses various *clauses* to control the loop's behavior.

#### 2.1.1 Simple Iteration

The simplest form of `loop` creates an infinite loop. You must provide a way to exit the loop using `return` or a similar construct.

```lisp
(loop
  (print "This will print forever unless we stop it!")
  (return)) ; Exit the loop immediately
```

This will print the message only once because of the `return`.

#### 2.1.2 `for` Clauses: Iteration with Variables

The `for` clause introduces loop variables and specifies how they are updated in each iteration.

* **Iterating over a range of numbers:**

```lisp
(loop for i from 0 to 4 ; Iterate from 0 to 4 (inclusive)
      do (print i))
```

Output:

```lisp
0
1
2
3
4
```

You can use `from`, `to`, `below` (exclusive upper bound), `by` (step), `downfrom`, `downto` for different iteration patterns.

```lisp
(loop for i from 10 downto 1 by 2
      do (print i))
```

Output:

```lisp
10
8
6
4
2
```

* **Iterating over a list:**

```lisp
(loop for item in '(a b c d) ; Iterate over the elements of the list
      do (print item))
```

Output:

```lisp
A
B
C
D
```

#### 2.1.3 `while` and `until` Clauses: Conditional Termination

`while` continues the loop as long as a condition is true. `until` continues the loop until a condition becomes true.

```lisp
(let ((x 0))
  (loop while (< x 5) ; Loop while x is less than 5
        do (print x)
           (incf x))) ; Increment x
```

Output:

```lisp
0
1
2
3
4
```

```lisp
(let ((x 0))
  (loop until (> x 5) ; Loop until x is greater than 5
        do (print x)
           (incf x)))
```

Output:

```lisp
0
1
2
3
4
5
```

#### 2.1.4 Accumulation Clauses: `collect`, `sum`, `count`, `minimize`, `maximize`

These clauses allow you to accumulate results during the loop.

* **`collect`**: Collects the results into a list.

```lisp
(loop for i from 1 to 5
      collect (* i i)) ; Collect the squares of numbers from 1 to 5
```

Output:

```lisp
(1 4 9 16 25)
```

* **`sum`**: Sums the results.

```lisp
(loop for i from 1 to 5
      sum i) ; Sum the numbers from 1 to 5
```

Output:

```lisp
15
```

* **`count`**: Counts how many times a condition is true.

```lisp
(loop for i from 1 to 10
      count (evenp i)) ; Count the even numbers from 1 to 10
```

Output:

```lisp
5
```

* **`minimize` and `maximize`**: Find the minimum or maximum value.

```lisp
(loop for i in '(3 1 4 1 5 9 2 6)
      minimize i)
```

Output:

```lisp
1
```

### 2.2 `do`: Parameterized Iteration

The `do` form provides more explicit control over loop variables and their updates. It takes a list of variable specifications, a termination test, and a body of code.

```lisp
(do ((i 0 (1+ i)) ; Initialize i to 0, update it by adding 1 in each iteration
     (j 10 (- j 2))) ; Initialize j to 10, update it by subtracting 2
    ((> i 5) (print "Loop finished!")) ; Termination test: exit when i > 5
  (format t "i: ~d, j: ~d~%" i j))
```

Output:

```lisp
i: 0, j: 10
i: 1, j: 8
i: 2, j: 6
i: 3, j: 4
i: 4, j: 2
i: 5, j: 0
"Loop finished!"
```

### 2.3 `dotimes`: Iterating a Fixed Number of Times

`dotimes` is a convenient form for iterating a specific number of times.

```lisp
(dotimes (i 5) ; Iterate 5 times, i will take values from 0 to 4
  (format t "Iteration ~d~%" i))
```

Output:

```lisp
Iteration 0
Iteration 1
Iteration 2
Iteration 3
Iteration 4
```

### 2.4 `dolist`: Iterating over a List

`dolist` provides a simple way to iterate over the elements of a list.

```lisp
(dolist (item '(apple banana cherry)) ; Iterate over the list
  (format t "Fruit: ~a~%" item))
```

Output:

```lisp
Fruit: APPLE
Fruit: BANANA
Fruit: CHERRY
```

These looping forms offer different levels of control and are suited for various situations. `loop` is the most powerful and flexible, while `do`, `dotimes`, and `dolist` provide more specialized and often more concise ways to express common looping patterns.

## 3. Non-Local Exits in Common Lisp

Non-local exits provide a way to transfer control from one part of a program to another, bypassing the usual sequential flow. Common Lisp offers two primary mechanisms for this: `block`/`return-from` for lexical exits and `catch`/`throw` for dynamic exits.

### 3.1 `block` and `return-from`: Lexical Exits

`block` creates a named block of code. `return-from` exits that block, returning a specified value. These are *lexically scoped*, meaning `return-from` can only exit a `block` that is lexically visible (within the same function or a containing function).

```lisp
(defun my-function (x)
  (block my-block ; Define a block named my-block
    (loop for i from 1 to 10
          do (if (> (* x i) 50)
                 (return-from my-block (* x i))) ; Exit the block if the condition is met
          (print (* x i)))
    "Loop completed without exceeding 50")) ; This is returned if the loop finishes normally

(print (my-function 5)) ; Output: 50
(print (my-function 2)) ; Output: "Loop completed without exceeding 50"
```

In the first call to `my-function` (with `x = 5`), the loop reaches `i = 10`, where `(* x i)` becomes 50. The `return-from` exits the `my-block` and the function immediately returns 50.

In the second call (with `x = 2`), the loop completes without the condition in the `if` being met. The function then proceeds to the last expression in the `block` and returns the string "Loop completed without exceeding 50".

You can also use `return` as a shorthand for `(return-from nil ...)` which will return from the innermost enclosing `block` with the name `nil` (which is implicitly created by many forms like `loop`, `progn`, `let`, etc.):

```lisp
(loop
  (print "Inside the loop")
  (return "Exiting the loop")) ; Equivalent to (return-from nil "Exiting the loop")

```

### 3.2 `catch` and `throw`: Dynamic Exits

`catch` establishes a *catch point* with a specific *tag*. `throw` transfers control to the nearest dynamically enclosing `catch` with a matching tag, unwinding the call stack as needed. These are *dynamically scoped*, meaning `throw` can exit any `catch` that is currently active on the call stack, regardless of the lexical structure.

```lisp
(defun nested-calculations (x y)
  (catch 'my-tag ; Establish a catch point with the tag 'my-tag
    (print "Starting calculations...")
    (let ((result (* x (nested-helper y))))
      (print "Calculations completed.") ; This might not be reached
      result)))

(defun nested-helper (z)
  (if (< z 0)
      (throw 'my-tag "Negative input not allowed!") ; Throw to the 'my-tag catch
      (* z 2)))

(print (nested-calculations 5 3)) ; Output: 30, "Starting calculations...", "Calculations completed."
(print (nested-calculations 5 -1)) ; Output: "Starting calculations...", "Negative input not allowed!"
```

In the first call to `nested-calculations`, `nested-helper` returns 6, and the multiplication and final print occur.

In the second call, `nested-helper` is called with `-1`. The `throw` form is executed, transferring control directly to the `catch` form in `nested-calculations`, with the value "Negative input not allowed!". The "Calculations completed." print is skipped.

`catch` and `throw` provide a powerful way to handle exceptional situations or implement complex control flow patterns, but they should be used judiciously, as excessive use can make code harder to understand.

## 4. Other Control Flow Constructs

### 4.1 `go` and `tagbody`: Low-Level Control Flow

`tagbody` establishes a block of code with labels (tags). `go` transfers control to a specific tag within the same `tagbody`. These are the most primitive control flow mechanisms in Common Lisp and are generally discouraged in favor of higher-level constructs like `loop`, `do`, `cond`, and `block`/`return-from`.

```lisp
(tagbody
 start
  (print "Starting...")
  (let ((x (read)))
    (if (zerop x)
        (go end))) ; Go to the 'end' tag if x is zero
  (print "Not zero!")
  (go start) ; Go back to the 'start' tag
 end
  (print "Ending."))
```

This code creates a loop that prompts the user for input. If the input is zero, the program jumps to the `end` tag and exits. Otherwise, it prints "Not zero!" and loops back to the beginning.

While `go` and `tagbody` provide a basic level of control, they can easily lead to spaghetti code that is difficult to read and maintain. The higher-level control flow forms provide better structure and readability, so they should be preferred in most cases.

This concludes the introduction to control flow in Common Lisp. By understanding these forms, you can write more complex and well-structured programs. Remember to use the most appropriate control flow construct for each situation to keep your code clear and maintainable.
