---
sidebar_position: 8
---

# Condition System

:::warning
This article **needs to be checked!**.
Please help contribute by checking it and making changes in [our repository](https://github.com/lisp-docs/lisp-docs.github.io) or by clicking on the "Edit this page" link below.
:::

## Introduction to the Condition System and Error Handling in Common Lisp

The Common Lisp Condition System is a powerful and flexible mechanism for handling exceptional situations, including errors, warnings, and other events that may disrupt the normal flow of program execution. Unlike simpler error handling mechanisms like exceptions in other languages, the Condition System allows for more sophisticated control over how these situations are handled, including the possibility of *recovering* from errors and continuing execution.

This tutorial will introduce you to the core concepts of the Condition System, including conditions, handlers, restarts, and how to define and signal your own conditions. You'll learn how to use these tools to write robust and maintainable Lisp programs.

**Key Concepts:**

* **Conditions:** Objects representing exceptional situations.
* **Handlers:** Functions that are invoked when a condition is signaled.
* **Restarts:** Points in the execution where processing can resume after a condition is handled.
* **Signaling Conditions:** Using `signal`, `error`, `warn`, and `cerror`.
* **Handling Conditions:** Using `handler-case`, `handler-bind`, and `ignore-errors`.

**Table of Contents:**

**1. Conditions:**

* What are conditions?
* Condition types and inheritance.
* Standard condition classes.
* Defining custom condition classes using `define-condition`.

**2. Signaling Conditions:**

* `signal`: Signaling a condition.
* `error`: Signaling an error (a serious condition).
* `warn`: Signaling a warning (a less serious condition).
* `cerror`: Signaling a correctable error (offering restarts).

**3. Handling Conditions:**

* `handler-case`: Handling specific condition types.
* `handler-bind`: Dynamically binding handlers.
* `ignore-errors`: Ignoring errors and continuing execution.

**4. Restarts:**

* What are restarts?
* Defining restarts using `restart-case`.
* Invoking restarts using `invoke-restart`, `invoke-restart-interactively`, and `find-restart`.

**5. Controlling the Debugger:**

* The Lisp debugger and its interaction with the Condition System.
* Using restarts in the debugger.

**6. Example: Implementing a Robust Input Function:**

* A practical example demonstrating how to use the Condition System to handle invalid user input.

This tutorial will provide clear explanations and practical examples for each topic, helping you understand how to use the Condition System effectively in your Common Lisp programs. You'll learn how to write code that can gracefully handle unexpected situations and provide meaningful feedback to the user.

## 1. Conditions in Common Lisp

The foundation of Common Lisp's robust error handling system is the concept of *conditions*. Conditions represent exceptional situations that may occur during program execution, such as errors, warnings, or other notable events.

### 1.1 What are Conditions?

In essence, a condition is a Lisp object that encapsulates information about an exceptional circumstance. This information might include:

* A descriptive message explaining the situation.
* Relevant data related to the condition (e.g., the invalid input that caused an error).
* Information about the context in which the condition occurred (e.g., the call stack).

Conditions are not simply error messages; they are structured data that can be inspected and handled programmatically.

### 1.2 Condition Types and Inheritance

Conditions are organized into a hierarchy of classes, allowing for categorization and specialized handling. This hierarchy is based on standard object-oriented inheritance using `defclass`.

The root of the condition hierarchy is the class `condition`. All other condition classes inherit from it, either directly or indirectly. This inheritance structure allows you to handle conditions at different levels of specificity. For example, you could handle all errors (by handling the `error` condition) or handle a specific type of error (by handling a subclass of `error`).

### 1.3 Standard Condition Classes

Common Lisp defines several standard condition classes that represent common exceptional situations. Some of the most important ones include:

* **`condition`**: The root of the condition hierarchy. All conditions inherit from this class.
* **`serious-condition`**: Represents conditions that typically require intervention. `error` inherits from this class.
* **`error`**: Represents errors that halt normal program execution unless handled.
* **`simple-error`**: A subclass of `error` that allows you to provide a simple error message.
* **`warning`**: Represents warnings that indicate potential problems but don't necessarily halt execution.
* **`simple-warning`**: A subclass of `warning` that allows you to provide a simple warning message.
* **`type-error`**: Signaled when a type mismatch occurs.
* **`arithmetic-error`**: Signaled when an arithmetic operation results in an error (e.g., division by zero).
* **`file-error`**: Signaled when an error occurs during file I/O.

This hierarchy allows for precise handling of different types of exceptional situations. You can handle a specific type of error, a more general category of errors, or even all conditions.

### 1.4 Defining Custom Condition Classes using `define-condition`

You can define your own custom condition classes using the `define-condition` macro. This allows you to create conditions that are specific to your application's needs.

The syntax of `define-condition` is:

```lisp
(define-condition condition-name (parent-condition-classes)
  ((slot-name1 :initarg :initarg1 :reader reader1)
   (slot-name2 :initarg :initarg2 :accessor accessor2)
   ...)
  (:report report-function))
```

* **`condition-name`**: The name of the new condition class.
* **`(parent-condition-classes)`**: A list of parent condition classes (usually `error`, `warning`, or `condition`).
* **`((slot-name1 ...) ...)`**: A list of slot specifications, similar to `defclass`.
* **`(:report report-function)`**: An optional function that is used to generate a human-readable report of the condition.

**Example:**

```lisp
(define-condition invalid-input-error (error)
  ((input :initarg :input :reader invalid-input-error-input))
  (:report (lambda (condition stream)
             (format stream "Invalid input: ~a" (invalid-input-error-input condition)))))

(defun process-input (input)
  (unless (integerp input)
    (error 'invalid-input-error :input input))
  (format t "Processing input: ~a~%" input))

(process-input 10) ; Prints "Processing input: 10"
(process-input "abc") ; Signals an INVALID-INPUT-ERROR with the message "Invalid input: abc"
```

In this example, we define a custom condition `invalid-input-error` that inherits from `error`. It has a slot `input` to store the invalid input. The `:report` option defines how the error message is generated.

This section introduced the concept of conditions in Common Lisp, including the condition hierarchy, standard condition classes, and how to define custom condition classes. Understanding conditions is essential for using the Condition System effectively to handle exceptional situations in your programs. The next sections will cover how to signal and handle conditions.

## 2. Signaling Conditions in Common Lisp

After defining condition classes, you need a way to *signal* them, indicating that an exceptional situation has occurred. Common Lisp provides several functions for this purpose, each with a slightly different behavior.

### 2.1 `signal`: Signaling a Condition

The `signal` function is the most general way to signal a condition. It takes a condition object as its argument. This object can be created using `make-condition` or by directly specifying the condition type and initargs.

```lisp
(define-condition my-condition (condition)
  ((message :initarg :message :reader my-condition-message)))

(signal (make-condition 'my-condition :message "Something happened."))

(signal 'my-condition :message "Another event.") ; more common way
```

When `signal` is called, it searches for applicable *handlers* (which we'll discuss in the next section). If no suitable handler is found, the debugger is typically invoked.

`signal` is generally used for non-error conditions or conditions that you expect to be handled by a specific handler.

### 2.2 `error`: Signaling an Error (a Serious Condition)

The `error` function is used to signal errors that typically halt normal program execution. It takes a format string and arguments, similar to `format`, to generate an error message. It signals a condition of type `error` (or a more specific subclass of `error`).

```lisp
(error "This is an error message.")

(let ((x 10))
  (error "The value of x is ~a, which is not allowed." x))
```

When `error` is called, it signals an error condition. If no handler for `error` (or a superclass of the signaled error) is found, the Lisp debugger is invoked.

`error` is the most common way to signal errors that should interrupt the normal flow of the program.

### 2.3 `warn`: Signaling a Warning (a Less Serious Condition)

The `warn` function is used to signal warnings, which indicate potential problems but don't necessarily halt execution. It also takes a format string and arguments. It signals a condition of type `warning` (or a more specific subclass of `warning`).

```lisp
(warn "This is a warning message.")

(let ((y nil))
  (when (null y)
    (warn "Y is nil. This might cause problems later.")))
```

When `warn` is called, it prints the warning message to `*error-output*` and then continues execution. It does *not* invoke the debugger unless a handler specifically for warnings is defined that does so.

`warn` is useful for providing feedback to the user about potential issues without interrupting the program's operation.

### 2.4 `cerror`: Signaling a Correctable Error (Offering Restarts)

The `cerror` function is used to signal errors that can be *corrected* by the user or by a handler. It takes a restart name, a format string for the error message, and any arguments for the format string.

```lisp
(defun divide (x y)
  (if (zerop y)
      (cerror "Return 1 instead." "Division by zero!")
      (/ x y)))

(print (divide 10 0)) ; This will invoke the debugger and offer a restart.
```

When `cerror` is called:

1. It signals an error condition.
2. It establishes a *restart* with the given name (in this case, "Return 1 instead.").
3. If no handler handles the error, the debugger is invoked. The debugger will present the user with the available restarts.

If the user chooses the "Return 1 instead." restart, the `divide` function will return 1, and execution will continue.

`cerror` is a powerful tool for creating interactive and robust programs that can recover from errors. We'll discuss restarts in more detail in a later section.

**Summary of Signaling Functions:**

* **`signal`**: Signals a general condition.
* **`error`**: Signals a serious error that usually halts execution.
* **`warn`**: Signals a warning that does not halt execution.
* **`cerror`**: Signals a correctable error and establishes a restart.

This section covered the different functions for signaling conditions in Common Lisp. Understanding how to use `signal`, `error`, `warn`, and `cerror` is essential for building robust and user-friendly applications. The next section will cover how to handle these conditions.

## 3. Handling Conditions in Common Lisp

After signaling a condition, you need a way to *handle* it. Handling a condition means executing specific code in response to the condition being signaled. Common Lisp provides several mechanisms for this, including `handler-case`, `handler-bind`, and `ignore-errors`.

### 3.1 `handler-case`: Handling Specific Condition Types

The `handler-case` macro is used to handle specific types of conditions. Its syntax is:

```lisp
(handler-case expression
  (condition-type1 (variable1)
    body1)
  (condition-type2 (variable2)
    body2)
  ...)
```

* **`expression`**: The code that might signal a condition.
* **`condition-type`**: The type of condition to handle (a condition class).
* **`variable`**: A variable that will be bound to the condition object if it is caught.
* **`body`**: The code to execute if the condition is caught.

**Example:**

```lisp
(defun safe-divide (x y)
  (handler-case (/ x y)
    (division-by-zero (c)
      (format t "Caught division by zero: ~a~%" c)
      0)))

(print (safe-divide 10 2))  ; Prints 5
(print (safe-divide 10 0))  ; Prints "Cau< division by zero>lt;DIVISION-BY`-ZERO ...>"` and then `0
```

In this example, `handler-case` attempts to evaluate `(/ x y)`. If a `division-by-zero` condition is signaled, the handler associated with `division-by-zero` is executed. The condition object is bound to the variable `c`, and the handler prints a message and returns 0. If no `division-by-zero` is signaled, the division result is returned.

You can also use `otherwise` clause that acts like a default case if no other handler is suitable.

```lisp
(defun process-input (input)
  (handler-case (parse-integer input)
    (type-error (c)
      (format t "Invalid input: ~a~%" input)
      nil)
    (otherwise (c)
      (format t "An unexpected error occurred: ~a~%" c)
      nil)))

(process-input "123") ; parses the integer 123
(process-input "abc") ; prints "Invalid input: abc" and returns nil
(process-input '(1 2 3)) ; print<An unexpected error occurre><TYPE-`ERROR ...>"` and returns ni`l
```

### 3.2 `handler-bind`: Dynamically Binding Handlers

The `handler-bind` macro allows you to dynamically bind handlers for specific conditions within a given scope. Its syntax is:

```lisp
(handler-bind ((condition-type1 handler-function1)
               (condition-type2 handler-function2)
               ...)
  body)
```

* **`condition-type`**: The type of condition to handle.
* **`handler-function`**: A function that will be called if the condition is signaled. The function should accept one argument (the condition object).
* **`body`**: The code within which the handlers are bound.

**Example:**

```lisp
(defun my-function ()
  (handler-bind ((division-by-zero #'(lambda (c)
                                      (format t "Handling division by zero in my-function: ~a~%" c)
                                      (return-from my-function 0)))) ; Return from the function
    (/ 10 0)
    (print "This will not be printed.")))

(print (my-function)) ; Prints "Handling division by zero i<y-function: #>DIVISION-BY-`ZERO ...>" and` then `0
```

In this example, `handler-bind` binds a handler for `division-by-zero` within the `my-function` function. If a `division-by-zero` condition is signaled, the handler function is called, which prints a message and then uses `return-from` to return 0 from `my-function`.

`handler-bind` is more flexible than `handler-case` as the handlers are functions and can be computed dynamically.

### 3.3 `ignore-errors`: Ignoring Errors and Continuing Execution

The `ignore-errors` macro provides a quick way to ignore errors and continue execution. Its syntax is:

```lisp
(ignore-errors expression)
```

If `expression` signals an error, `ignore-errors` catches the error and returns two values: `nil` and the condition object. If `expression` does not signal an error, it returns the value of `expression` and `nil`.

**Example:**

```lisp
(multiple-value-bind (result error) (ignore-errors (/ 10 0))
  (if error
      (format t "An error occurred: ~a~%" error)
      (format t "The result is: ~a~%" result)))

(multiple-value-bind (result error) (ignore-errors (/ 10 2))
  (if error
      (format t "An error occurred: ~a~%" error)
      (format t "The result is: ~a~%" result)))
```

This will first print `"An error occurred: #<DIVISION-BY-ZERO ...>"` and then `"The result is: 5"`.

`ignore-errors` should be used with caution, as it can mask important errors and make debugging more difficult. It's best used in situations where you expect errors to occur occasionally and want to handle them gracefully without interrupting the program's main flow.

This section covered the primary mechanisms for handling conditions in Common Lisp: `handler-case`, `handler-bind`, and `ignore-errors`. Using these tools effectively is crucial for writing robust and reliable Lisp programs. The next section will cover restarts, which provide a powerful way to recover from errors and continue execution.

## 4. Restarts in Common Lisp

Restarts are a powerful feature of the Common Lisp Condition System that allow you to define points in your code where execution can resume after a condition has been handled. They provide a way to recover from errors or other exceptional situations in a controlled manner.

### 4.1 What are Restarts?

A restart is essentially a named action that can be invoked to continue execution from a specific point. When a condition is signaled, handlers can choose to invoke a restart, which then transfers control back to the point where the restart was established.

Restarts provide a more structured and flexible alternative to simply returning a default value or unwinding the stack. They allow the code that handles a condition to make decisions about how to continue execution, potentially based on user input or other factors.

### 4.2 Defining Restarts using `restart-case`

The `restart-case` macro is used to define restarts. Its syntax is:

```lisp
(restart-case expression
  (restart-name1 (&rest arguments)
    body1)
  (restart-name2 (&rest arguments)
    body2)
  ...)
```

* **`expression`**: The code that might signal a condition.
* **`restart-name`**: A symbol that names the restart.
* **`arguments`**: A list of arguments that will be passed to the restart's body.
* **`body`**: The code to execute when the restart is invoked.

**Example:**

```lisp
(defun divide (x y)
  (restart-case (/ x y)
    (return-one ()
      (format t "Returning 1 instead of dividing by zero.~%")
      1)
    (use-new-divisor (new-y)
      (format t "Using new divisor: ~a~%" new-y)
      (/ x new-y))))

(print (divide 10 0)) ; This will invoke the debugger.
```

If you evaluate `(divide 10 0)`, a `division-by-zero` error will be signaled. Because the call to `/` is wrapped in `restart-case`, the debugger will be invoked, and it will present the available restarts: `return-one` and `use-new-divisor`.

### 4.3 Invoking Restarts

There are several ways to invoke restarts:

* **`invoke-restart`**: Invokes a restart by name. It takes the restart name and any arguments for the restart's body.

    ```lisp
    (invoke-restart 'return-one) ; Will execute the body of the RETURN-ONE restart.
    (invoke-restart 'use-new-divisor 5) ; Will execute the body of the USE-NEW-DIVISOR restart with 5 bound to new-y.
    ```

* **`invoke-restart-interactively`**: Invokes a restart by prompting the user for any required arguments. This is typically used in the debugger.

* **`find-restart`**: Returns the restart object associated with a given name. This is useful if you need to manipulate the restart object directly.

**Example with `cerror`:**

The most common way to use restarts in user code is with `cerror`:

```lisp
(defun safe-divide (x y)
  (if (zerop y)
      (cerror "Use 1 as the result." "Division by zero!")
      (/ x y)))

(print (safe-divide 10 0)) ; Triggers the error.
```

When `(safe-divide 10 0)` is evaluated, the `cerror` function signals an error and establishes a restart named `continue`. If the user chooses to continue (by selecting the available restart in the debugger or by using `invoke-restart 'continue`), execution continues normally, and the `cerror` form returns. However, in this example, the `/` form will still signal the `division-by-zero` error and the debugger will be entered once again, because no handler was defined to handle the error.

Let's modify the above example to include a handler:

```lisp
(defun safe-divide (x y)
  (handler-case
      (if (zerop y)
          (cerror "Use 1 as the result." "Division by zero!")
          (/ x y))
    (division-by-zero () 1)))

(print (safe-divide 10 0)) ; Triggers the error, the handler returns 1, and 1 gets printed.
```

Now, the `handler-case` handles the `division-by-zero` error, and returns 1. The restart is not needed anymore.

Here is an example where the restart is useful:

```lisp
(defun safe-divide (x y)
  (handler-case
      (if (zerop y)
          (cerror "Use new divisor." "Division by zero!")
          (/ x y))
    (division-by-zero () 1)))

(defun interactive-divide (x)
    (loop
        (let ((divisor (read)))
            (handler-case (safe-divide x divisor)
                (division-by-zero ()
                    (format t "Please enter a non-zero divisor:~%")
                    nil)
                (t (result) (return result))))))

(print (interactive-divide 10))
```

Now the user is prompted to enter a new divisor until a valid one is entered.

Restarts are a powerful mechanism for error recovery and interactive programming in Common Lisp. They allow you to create more robust and user-friendly applications. This section covered the basics of defining and invoking restarts. The next section will briefly discuss how the debugger interacts with the Condition System.

## 5. Controlling the Debugger in Common Lisp

The Common Lisp debugger is an interactive environment that helps you diagnose and fix errors in your code. It's tightly integrated with the Condition System, providing powerful tools for inspecting the state of your program when a condition is signaled.

### 5.1 The Lisp Debugger and its Interaction with the Condition System

When an unhandled error is signaled (i.e., no suitable handler is found), the Lisp system enters the debugger. The debugger provides you with information about the error, including:

* The condition object that was signaled.
* A backtrace (the call stack), showing the sequence of function calls that led to the error.
* Available restarts, if any were established using `cerror` or `restart-case`.

The debugger allows you to:

* Inspect variables and data structures.
* Evaluate Lisp expressions in the context of the error.
* Invoke restarts to continue execution from a specific point.
* Step through the code to trace the execution flow.

The Condition System plays a crucial role in the debugger's functionality. When an error is signaled, the system searches for handlers. If no handler is found, the debugger is invoked as a *last-resort handler*. The debugger itself then becomes the active handler.

### 5.2 Using Restarts in the Debugger

The interaction between the debugger and restarts is one of the most powerful aspects of the Condition System. When a condition is signaled within a `restart-case` or when `cerror` is used, the debugger presents the available restarts to the user.

Let's revisit the `divide` example from the previous section:

```lisp
(defun divide (x y)
  (restart-case (/ x y)
    (return-one ()
      (format t "Returning 1 instead of dividing by zero.~%")
      1)
    (use-new-divisor (new-y)
      (format t "Using new divisor: ~a~%" new-y)
      (/ x new-y))))

(divide 10 0)
```

When you evaluate `(divide 10 0)`, the `division-by-zero` error is signaled. The debugger is invoked, and you'll typically see something like this (the exact output may vary depending on your Lisp implementation):

```lisp
Division by zero.
   [Condition of type DIVISION-BY-ZERO]

Restarts:
 0: [RETURN-ONE] Returning 1 instead of dividing by zero.
 1: [USE-NEW-DIVISOR] Use new divi<
 2: [ABORT] Return to top level.

Debug>
```

Here, the debugger shows the error message and lists the available restarts. You can then choose a restart by entering its number.

* If you enter `0`, the `return-one` restart is invoked, and the `divide` function returns 1.
* If you enter `1`, the debugger will prompt you for a value for `new-y`, and then invoke the `use-new-divisor` restart with that value.
* If you enter `2` (or use a debugger command for aborting), the program will return to the top level.

**Example with `cerror`:**

```lisp
(defun my-function (x)
    (if (< x 0)
        (cerror "Use the absolute value." "X cannot be negative")
        x))

(my-function -5)
```

The debugger output will be similar to this:

```lisp
X cannot be negative
   [Condition of type SIMPLE-ERROR]

Restarts:
 0: [CONTINUE] Use the absolute va<
 1: [ABORT] Return to top level.

Debug>
```

If you choose the `CONTINUE` restart, the `cerror` form will return, and the function will continue execution using the original, negative value of `x`. Because of this, it is common to wrap the `cerror` in a `handler-case` as seen in the previous section.

**Debugger Commands:**

Debuggers provide various commands for inspecting the state of the program, such as:

* Inspecting variable values.
* Moving up and down the call stack.
* Evaluating Lisp expressions.
* Stepping through the code.

The specific commands available depend on your Lisp implementation. Consult your implementation's documentation for details.

By understanding how the debugger interacts with the Condition System and how to use restarts, you can significantly improve your ability to debug and handle errors in your Lisp programs. This combination of tools allows you to create more robust, user-friendly, and maintainable applications.

## 6. Example: Implementing a Robust Input Function

This example demonstrates how to use the Condition System to create a robust function for reading integer input from the user, handling various potential errors gracefully.

```lisp
(define-condition invalid-integer-input (error)
  ((input :initarg :input :reader invalid-input-error-input))
  (:report (lambda (condition stream)
             (format stream "Invalid integer input: ~a" (invalid-input-error-input condition)))))

(defun get-integer-input (&optional prompt)
  (loop
    (when prompt
      (format t "~a" prompt))
    (force-output) ; Ensure the prompt is displayed

    (handler-case (let ((input (read)))
                     (typecase input
                       (integer input)
                       (t (error 'invalid-integer-input :input input))))
      (end-of-file ()
        (format t "End of input reached.~%")
        (return nil)) ; Handle end of input
      (invalid-integer-input (c)
        (format t "~a Please enter an integer.~%" c)
        nil)
      (error (c) ; Catches other errors that might occur during input
        (format t "An unexpected error occurred: ~a. Please try again.~%" c)
        nil))))

(defun main ()
    (loop
        (let ((input (get-integer-input "Enter a number (or Ctrl-D to exit): ")))
            (if input
                (format t "You entered: ~a~%" input)
                (return)))))

(main)
```

**Explanation:**

1. **`invalid-integer-input` Condition:** We define a custom condition `invalid-integer-input` to represent the case where the user enters input that cannot be parsed as an integer. This condition stores the invalid input in its `input` slot.

2. **`get-integer-input` Function:** This function is responsible for reading and validating the user's input.
    * It uses a `loop` to repeatedly prompt the user until valid input is provided.
    * The `force-output` function ensures that the prompt is displayed immediately.
    * **`handler-case`:** This is the core of the error handling. It attempts to read input using `read` and then checks if it is an integer using `typecase`. If it is not an integer it signals `invalid-integer-input`.
        * **`end-of-file` handler:** This handles the case where the user presses Ctrl-D (or the equivalent end-of-file signal). It prints a message and returns `nil`, which will cause the main loop to exit.
        * **`invalid-integer-input` handler:** This handles our custom condition. It prints an informative message using the `:report` function of the condition and then continues the loop, prompting the user again.
        * **`error` handler:** This is a catch-all handler for any other errors that might occur during input (e.g., syntax errors). It prints a generic error message and continues the loop.
    * If the input is a valid integer, the function returns the integer.

3. **`main` function:** This function simply calls `get-integer-input` in a loop and prints the result. If `get-integer-input` returns `nil` (due to end-of-file), the loop terminates.

**How it demonstrates robustness:**

* **Handles invalid input:** If the user enters non-integer input (e.g., "abc", "(1 2 3)"), the program prints an appropriate error message and prompts the user again.
* **Handles end of input:** If the user presses Ctrl-D, the program exits gracefully.
* **Handles unexpected errors:** The catch-all `error` handler ensures that the program does not crash due to unexpected input errors.
* **Clear error messages:** The custom condition and its `:report` function provide clear and informative error messages to the user.

This example demonstrates how the Condition System can be used to create robust and user-friendly input functions that handle various potential errors gracefully. This is a crucial aspect of writing real-world applications in Common Lisp. This concludes the tutorial on the Common Lisp Condition System.
