---
sidebar_position: 5
---

# Functions

## Introduction to Functions in Common Lisp

Functions are fundamental building blocks in any programming language, and Common Lisp is no exception. They allow you to encapsulate reusable blocks of code, making your programs more modular, organized, and easier to maintain. In Common Lisp, functions are first-class citizens, meaning they can be passed as arguments to other functions, returned as values, and assigned to variables. This flexibility is a key aspect of Lisp's expressive power.

This tutorial will guide you through the creation and use of functions in Common Lisp, including the important concept of lambda forms, which allow you to create anonymous functions. We will cover defining named functions, understanding function arguments, using local variables, and working with higher-order functions.

**Key Concepts:**

* **Function Definition:** Creating a named function with a specific set of parameters and a body of code.
* **Function Application:** Calling or invoking a function to execute its code.
* **Lambda Forms:** Creating anonymous functions (functions without names).
* **Function Arguments:** Passing data to functions.
  * Required arguments
  * Optional arguments
  * Rest arguments
  * Keyword arguments
* **Local Variables:** Defining variables within the scope of a function.
* **Higher-Order Functions:** Functions that take other functions as arguments or return functions as values.

**Table of Contents:**

Here's a breakdown of the topics we will cover:

**1. Defining Named Functions:**

* `defun`: The primary form for defining named functions.
  * Basic function definition.
  * Documenting functions with docstrings.

**2. Function Application (Calling Functions):**

* Basic function calls.
* Evaluating function calls.

**3. Lambda Forms (Anonymous Functions):**

* `lambda`: Creating anonymous functions.
* Using lambda forms directly.
* Assigning lambda forms to variables.

**4. Function Arguments:**

* **Required Arguments:** Arguments that must be provided when calling the function.
* **Optional Arguments:** Arguments that can be omitted, with default values.
* **Rest Arguments:** Collecting a variable number of arguments into a list.
* **Keyword Arguments:** Arguments passed by name, allowing for flexible argument passing.
* Mixing argument types.
* `&optional`, `&rest`, `&key`, `&allow-other-keys`, `&aux`.

**5. Local Variables within Functions:**

* `let`: Creating local variables within a function's scope.
* `let*`: Creating local variables sequentially.

**6. Higher-Order Functions:**

* Passing functions as arguments.
* Returning functions as values.
* `funcall`: Calling a function object.
* `apply`: Applying a function to a list of arguments.
* `mapcar`, `mapc`, `maplist`, `mapcon`: Mapping functions over lists.

By the end of this tutorial, you will have a solid understanding of how to define, use, and work with functions in Common Lisp, including the powerful concept of lambda forms and higher-order functions. This knowledge will enable you to write more modular, reusable, and expressive code.

## 1. Defining Named Functions in Common Lisp

Functions are the core building blocks of Common Lisp programs. They encapsulate reusable blocks of code, promoting modularity and organization. The primary way to define named functions in Common Lisp is using the `defun` macro.

### 1.1 `defun`: The Primary Function Definition Form

The `defun` macro takes the following general form:

```lisp
(defun function-name (parameter-list)
  "Optional documentation string (docstring)"
  body)
```

* **`function-name`**: The name you want to give to your function (a symbol).
* **`parameter-list`**: A list of parameter names (symbols) that the function accepts as input.
* **`"Optional documentation string (docstring)"`**: A string literal that describes the function's purpose. This is highly recommended for good code documentation.
* **`body`**: One or more Lisp expressions that define the function's behavior. The value of the last expression evaluated in the body is returned as the function's result.

#### 1.1.1 Basic Function Definition

Let's start with a simple example: a function that adds two numbers.

```lisp
(defun add-numbers (x y)
  (+ x y))
```

This defines a function named `add-numbers` that takes two parameters, `x` and `y`, and returns their sum.

To call or *apply* this function, you use parentheses:

```lisp
(add-numbers 5 3)  ; Evaluates to 8
(add-numbers 10 -2) ; Evaluates to 8
```

Another example, a function that greets a person:

```lisp
(defun greet (name)
  (format t "Hello, ~a!~%" name))
```

Calling it:

```lisp
(greet "World") ; Prints "Hello, World!" to the standard output and returns NIL
```

Note that `format` with `t` as the first argument prints to standard output and returns `nil`. If you wanted to return the greeting string you would use `(format nil "Hello, ~a!~%" name)`.

A function can have an empty parameter list, meaning it takes no arguments:

```lisp
(defun say-hello ()
  (print "Hello!"))
```

Calling it:

```lisp
(say-hello) ; Prints "Hello!"
```

#### 1.1.2 Documenting Functions with Docstrings

It's crucial to document your functions using docstrings. A docstring is a string literal placed immediately after the parameter list in a `defun` form. It describes the function's purpose, arguments, and return value.

```lisp
(defun square (x)
  "Returns the square of X.

Args:
  X: A number.

Returns:
  The square of X (a number)."
  (* x x))
```

You can access the docstring using the `documentation` function:

```lisp
(documentation 'square 'function) ; Returns the docstring of the square function
```

Output:

```lisp
"Returns the square of X.

Args:
  X: A number.

Returns:
  The square of X (a number)."
```

Docstrings are extremely valuable for code maintainability and understanding. They are used by documentation generators and interactive development environments to provide helpful information about your functions.

Here is a more complex example showing the use of local variables inside a function using `let`.

```lisp
(defun calculate-average (x y z)
  "Calculates the average of three numbers.

Args:
  X: The first number.
  Y: The second number.
  Z: The third number.
Returns:
  The average of X, Y, and Z.
"
  (let ((sum (+ x y z)))
    (/ sum 3)))
```

Calling it:

```lisp
(calculate-average 10 20 30) ; Returns 20
```

This example demonstrates how to define a function with a docstring and use local variables with `let` to perform calculations within the function. This style of clear documentation and well-structured code is highly encouraged in Common Lisp programming.

## 2. Function Application (Calling Functions) in Common Lisp

In Common Lisp, calling a function is referred to as *function application*. It involves providing the function's name and any required arguments within parentheses.

### 2.1 Basic Function Calls

The general syntax for a function call is:

```lisp
(function-name argument1 argument2 ...)
```

* **`function-name`**: The name of the function you want to call (a symbol).
* **`argument1`, `argument2`, ...**: The values you want to pass to the function as input.

Let's revisit the `add-numbers` function from the previous section:

```lisp
(defun add-numbers (x y)
  (+ x y))
```

To call this function with the arguments 5 and 3, you would write:

```lisp
(add-numbers 5 3)
```

The Lisp system evaluates this expression and returns the result, which is 8.

Here are some more examples:

```lisp
(+ 2 3)      ; Returns 5
(- 10 4)     ; Returns 6
(* 6 7)      ; Returns 42
(/ 15 3)     ; Returns 5
(expt 2 4)   ; Returns 16 (2 raised to the power of 4)
(mod 17 5)   ; Returns 2 (remainder of 17 divided by 5)
(max 1 5 2)  ; Returns 5 (the maximum of the given numbers)
(min 1 5 2)  ; Returns 1 (the minimum of the given numbers)
```

You can also nest function calls, meaning the result of one function call can be used as an argument to another:

```lisp
(+ (* 2 3) 4)    ; Returns 10 (equivalent to (6 + 4))
(/ (- 10 2) (+ 1 3)) ; Returns 2 (equivalent to (8 / 4))
```

### 2.2 Evaluating Function Calls

When the Lisp system encounters a function call, it follows these steps:

1. **Evaluate the function name:** In most cases, the function name is a symbol that directly represents the function. However, as we will see later with higher-order functions, the function part can be any expression that evaluates to a function object.
2. **Evaluate the arguments:** Each argument expression is evaluated from left to right.
3. **Apply the function:** The function is called with the evaluated arguments. The function's body is executed, and the result of the last expression in the body is returned.

Let's illustrate this with the nested call `(/ (- 10 2) (+ 1 3))`:

1. The function name `/` is identified (division).
2. The first argument `(- 10 2)` is evaluated:
    * The function name `-` is identified (subtraction).
    * The arguments 10 and 2 are evaluated (they are already values).
    * The subtraction is performed: 10 - 2 = 8.
3. The second argument `(+ 1 3)` is evaluated:
    * The function name `+` is identified (addition).
    * The arguments 1 and 3 are evaluated.
    * The addition is performed: 1 + 3 = 4.
4. The division is performed: 8 / 4 = 2.

Therefore, the entire expression `(/ (- 10 2) (+ 1 3))` evaluates to 2.

This evaluation process is fundamental to understanding how Lisp programs work. The fact that function calls are themselves expressions that can be nested makes Lisp code very concise and expressive.

It is important to understand that Lisp uses *applicative order* evaluation, which means that the arguments to a function are evaluated *before* the function is called. This is the standard evaluation strategy in most programming languages.

This concludes the basic introduction to function application in Common Lisp. Understanding how to call functions and how these calls are evaluated is essential for writing and understanding Lisp programs.

## 3. Lambda Forms (Anonymous Functions) in Common Lisp

Lambda forms, also known as anonymous functions, are functions without a name. They are a powerful feature in Common Lisp that allows you to create functions on the fly and use them in various contexts, such as passing them as arguments to other functions or returning them as values.

### 3.1 `lambda`: Creating Anonymous Functions

The `lambda` special form is used to create anonymous functions. Its syntax is similar to `defun`, but without the function name:

```lisp
(lambda (parameter-list)
  body)
```

* **`lambda`**: The keyword that indicates the creation of an anonymous function.
* **`parameter-list`**: A list of parameter names (symbols) that the lambda form accepts.
* **`body`**: One or more Lisp expressions that define the function's behavior.

For example, a lambda form that adds two numbers would be:

```lisp
(lambda (x y) (+ x y))
```

This creates a function that takes two arguments, `x` and `y`, and returns their sum.

### 3.2 Using Lambda Forms Directly

You can use lambda forms directly by enclosing them in parentheses and then providing the arguments:

```lisp
((lambda (x y) (+ x y)) 5 3) ; Evaluates to 8
```

Here, `(lambda (x y) (+ x y))` is the function, and `5` and `3` are the arguments. The Lisp system evaluates the lambda form to a function object and then applies it to the given arguments.

Another example:

```lisp
((lambda (name) (format t "Hello, ~a!~%" name)) "Lambda World") ; Prints "Hello, Lambda World!"
```

### 3.3 Assigning Lambda Forms to Variables

While lambda forms are anonymous, you can assign them to variables, effectively giving them a name. This is often useful when you want to reuse a lambda form multiple times.

```lisp
(setf add-numbers (lambda (x y) (+ x y)))
```

Now, `add-numbers` is a variable that holds the lambda function. You can call it just like a regular named function defined with `defun`:

```lisp
(add-numbers 5 3) ; Evaluates to 8
```

This is functionally equivalent to defining the function with `defun`:

```lisp
(defun add-numbers (x y) (+ x y))
```

The difference is that with `defun`, the function is defined globally, while with `setf` and `lambda`, you are creating a function object and assigning it to a variable.

Here's an example showing the power of assigning lambda forms:

```lisp
(let ((operations (list (cons '+ (lambda (x y) (+ x y)))
                       (cons '- (lambda (x y) (- x y)))
                       (cons '* (lambda (x y) (* x y)))
                       (cons '/ (lambda (x y) (/ x y))))
      (operator '+)
      (a 10)
      (b 5))
  (funcall (cdr (assoc operator operations)) a b))
```

This example creates a list of operations, where each element is a cons cell containing an operator symbol (e.g., `'+`) and a corresponding lambda function. It then uses `assoc` to find the correct function based on the `operator` variable and `funcall` to apply it to `a` and `b`. This showcases how lambda forms can be stored in data structures and used dynamically.

Lambda forms are essential for working with higher-order functions in Common Lisp, which we will cover in a later section. They provide a concise and flexible way to create and use functions within your code.

## 4. Function Arguments in Common Lisp

Common Lisp offers a rich system for handling function arguments, providing flexibility and control over how data is passed to functions. This section covers the different types of arguments and the special lambda list keywords used to define them.

### 4.1 Required Arguments

Required arguments are those that *must* be provided when calling a function. They are listed first in the parameter list of a `defun` or `lambda` form.

```lisp
(defun subtract (x y)
  (- x y))

(subtract 10 3) ; x = 10, y = 3, returns 7
```

In this example, `x` and `y` are required arguments. Calling `subtract` with only one argument or no arguments will result in an error.

### 4.2 Optional Arguments

Optional arguments can be omitted when calling a function. They are specified using the `&optional` keyword in the parameter list. You can also provide default values for optional arguments.

```lisp
(defun greet (name &optional (greeting "Hello"))
  (format t "~a, ~a!~%" greeting name))

(greet "Alice")           ; name = "Alice", greeting = "Hello", prints "Hello, Alice!"
(greet "Bob" "Good day") ; name = "Bob", greeting = "Good day", prints "Good day, Bob!"
```

Here, `greeting` is an optional argument with a default value of `"Hello"`. If you don't provide a value for `greeting`, the default value is used.

You can also check if an optional argument was supplied by using a supplied-p variable:

```lisp
(defun greet (name &optional (greeting "Hello" greeting-supplied-p))
  (if greeting-supplied-p
      (format t "~a, ~a! (Explicitly supplied)~%" greeting name)
      (format t "~a, ~a! (Using default)~%" greeting name)))

(greet "Alice")       ; Prints "Hello, Alice! (Using default)"
(greet "Bob" "Hi")   ; Prints "Hi, Bob! (Explicitly supplied)"
```

### 4.3 Rest Arguments

Rest arguments allow a function to accept a variable number of arguments. They are specified using the `&rest` keyword. All extra arguments are collected into a list and bound to the rest parameter.

```lisp
(defun sum-numbers (&rest numbers)
  (apply '+ numbers))

(sum-numbers 1 2 3)     ; numbers = (1 2 3), returns 6
(sum-numbers 1 2 3 4 5) ; numbers = (1 2 3 4 5), returns 15
(sum-numbers) ; numbers = nil, returns 0
```

`apply` is used here to apply the `+` function to the list of numbers.

### 4.4 Keyword Arguments

Keyword arguments are passed by name, allowing for more flexible and readable function calls, especially when dealing with many optional parameters. They are specified using the `&key` keyword.

```lisp
(defun describe-person (name &key (age 0) (occupation "unknown"))
  (format t "Name: ~a~%" name)
  (format t "Age: ~d~%" age)
  (format t "Occupation: ~a~%" occupation))

(describe-person "Charlie")                       ; Uses default values for age and occupation
(describe-person "David" :age 30)                 ; Specifies age
(describe-person "Eve" :occupation "Programmer") ; Specifies occupation
(describe-person "Frank" :age 25 :occupation "Teacher") ; Specifies both
```

When calling a function with keyword arguments, you use keywords (symbols starting with a colon `:`) to specify the argument names.

Similar to optional arguments, you can use supplied-p variables with keyword arguments:

```lisp
(defun describe-person (name &key (age 0 age-supplied-p))
  (format t "Name: ~a~%" name)
  (if age-supplied-p
      (format t "Age: ~d (Supplied)~%" age)
      (format t "Age: ~d (Default)~%" age)))

(describe-person "Grace") ; Age: 0 (Default)
(describe-person "Heidi" :age 40) ; Age: 40 (Supplied)
```

### 4.5 Mixing Argument Types

You can mix different types of arguments in a function definition:

```lisp
(defun format-data (label value &optional (unit "") &key (precision 2))
  (format t "~a: ~,~vf ~a~%" label value precision unit))

(format-data "Temperature" 25.5 :unit "°C")          ; Uses default precision
(format-data "Distance" 123.4567 :precision 3 :unit "km") ; Overrides precision
```

### 4.6 Lambda List Keywords: `&optional`, `&rest`, `&key`, `&allow-other-keys`, `&aux`

Here's a summary of the lambda list keywords:

* **`&optional`**: Introduces optional arguments.
* **`&rest`**: Introduces a rest argument that collects remaining arguments into a list.
* **`&key`**: Introduces keyword arguments.
* **`&allow-other-keys`**: Allows the function to accept additional keyword arguments that it doesn't explicitly handle. These extra keywords are accessible via `apply`.
* **`&aux`**: Introduces auxiliary variables that are bound within the function's scope but are not part of the argument list. They are initialized before the function body is executed.

Example of `&allow-other-keys` and `&aux`:

```lisp
(defun process-data (data &key &allow-other-keys &aux (verbose (getf data :verbose t)))
  (when verbose (print "Processing data..."))
  (format t "Data: ~a~%" data))

(process-data '(:a 1 :b 2 :verbose nil :c 3)) ; verbose is explicitly NIL
(process-data '(:a 1 :b 2 :c 3)) ; verbose takes default value T
```

Understanding these different argument types and lambda list keywords is essential for writing flexible and robust functions in Common Lisp. They enable you to create functions that can handle a wide range of input scenarios and improve the readability and maintainability of your code.

## 5. Local Variables within Functions in Common Lisp

Local variables are variables that are only accessible within a specific scope, typically within a function. Common Lisp provides two primary forms for creating local variables: `let` and `let*`.

### 5.1 `let`: Parallel Binding

The `let` special form creates local variables and binds them to initial values. The bindings happen *in parallel*, meaning the initial values are evaluated in an unspecified order, and none of the new variables are in scope during the evaluation of the initial values.

The general syntax of `let` is:

```lisp
(let ((variable1 value1)
      (variable2 value2)
      ...)
  body)
```

* **`variable1`, `variable2`, ...**: The names of the local variables (symbols).
* **`value1`, `value2`, ...**: Expressions that are evaluated to provide the initial values for the corresponding variables.
* **`body`**: One or more Lisp expressions that form the body of the `let` form. The value of the last expression in the body is returned as the result of the `let` form.

Here's an example:

```lisp
(let ((x 10)
      (y 20))
  (+ x y)) ; Returns 30
```

In this example, `x` is bound to 10 and `y` is bound to 20. The `+` expression is then evaluated within this scope, resulting in 30.

A key characteristic of `let` is that the initial value expressions are evaluated *before* any of the variables are bound. This means you cannot use a variable declared earlier in the `let` form to initialize a later one:

```lisp
(let ((x 10)
      (y (+ x 5))) ; Error: x is not yet bound when evaluating (+ x 5)
  (print (+ x y)))
```

This will result in an *unbound variable* error because `x` is not yet bound when `(+ x 5)` is evaluated to initialize `y`.

If you don't provide an initial value for a variable in a `let` form, it is initialized to `nil`:

```lisp
(let ((z))
  (print z)) ; Prints NIL
```

### 5.2 `let*`: Sequential Binding

The `let*` special form is similar to `let`, but the bindings happen *sequentially*. This means that each variable is bound immediately after its initial value is evaluated, and subsequent variables can use the values of previously bound variables.

The syntax of `let*` is the same as `let`:

```lisp
(let* ((variable1 value1)
       (variable2 value2)
       ...)
  body)
```

Using `let*`, we can correct the previous example that caused an error with `let`:

```lisp
(let* ((x 10)
       (y (+ x 5))) ; Now this works: x is already bound to 10
  (print (+ x y))) ; Prints 25
```

Here, `x` is bound to 10 first. Then, `y` is bound to the result of `(+ x 5)`, which is 15, because `x` is now in scope.

Another illustrative example:

```lisp
(let* ((a 5)
       (b (* a 2))     ; b is 10
       (c (+ a b 1))) ; c is 16
  (format t "a: ~a, b: ~a, c: ~a~%" a b c))
```

Output:

```lisp
a: 5, b: 10, c: 16
```

**Choosing between `let` and `let*`:**

* Use `let` when the initial values of the local variables are independent of each other. This is generally preferred for clarity and efficiency.
* Use `let*` when the initial value of one variable depends on the value of a previously bound variable.

In summary, `let` and `let*` provide essential mechanisms for creating local variables within functions, controlling scope, and managing data within specific code blocks. Understanding the difference between parallel and sequential binding is crucial for writing correct and efficient Lisp code.

## 6. Higher-Order Functions in Common Lisp

Higher-order functions are functions that can take other functions as arguments or return functions as results. This is a powerful concept that allows for more abstract and flexible programming. Common Lisp, with its first-class functions, fully supports higher-order functions.

### 6.1 Passing Functions as Arguments

You can pass functions as arguments to other functions. This is typically done using the `#'` (function) operator, which returns the function object associated with a symbol.

```lisp
(defun apply-twice (f x)
  (funcall f (funcall f x))) ; Applies f to x twice

(defun increment (n)
  (+ n 1))

(print (apply-twice #'increment 5)) ; Applies increment twice to 5: (increment (increment 5)) = 7
```

In this example, `apply-twice` takes a function `f` and a value `x` as arguments. It then applies `f` to `x` twice using `funcall` (explained below). We pass the `increment` function as an argument using `#'increment`.

### 6.2 Returning Functions as Values

Functions can also return other functions as values. This is often done using `lambda` forms to create the function to be returned.

```lisp
(defun make-adder (n)
  (lambda (x) (+ x n))) ; Returns a function that adds n to its argument

(let ((add-5 (make-adder 5))) ; add-5 is now a function that adds 5
  (print (funcall add-5 10))) ; Applies the returned function to 10, resulting in 15

(let ((add-10 (make-adder 10)))
  (print (funcall add-10 20))) ; Applies the returned function to 20, resulting in 30
```

`make-adder` returns a new function (a lambda form) that "remembers" the value of `n` from the environment where it was created. This is called a *closure*.

### 6.3 `funcall`: Calling a Function Object

`funcall` is used to call a function object. It takes the function object as its first argument and any additional arguments to be passed to the function.

```lisp
(funcall #'+ 2 3)        ; Equivalent to (+ 2 3), returns 5
(funcall #'print "Hello") ; Equivalent to (print "Hello"), prints "Hello"
```

`funcall` is essential when you have a function object stored in a variable or when you are working with higher-order functions. In the examples above with `apply-twice` and `make-adder` `funcall` is used to call the function passed as an argument or created by the function.

### 6.4 `apply`: Applying a Function to a List of Arguments

`apply` is similar to `funcall`, but it takes a list of arguments instead of individual arguments.

```lisp
(apply #'+ '(1 2 3))      ; Equivalent to (+ 1 2 3), returns 6
(apply #'max '(1 5 2 8 3)) ; Equivalent to (max 1 5 2 8 3), returns 8
```

The last argument to `apply` *must* be a list.

`apply` is useful when you have the arguments to a function stored in a list and you want to call the function with those arguments.

### 6.5 Mapping Functions over Lists: `mapcar`, `mapc`, `maplist`, `mapcon`

Common Lisp provides several functions for applying a function to each element of one or more lists.

* **`mapcar`**: Applies a function to each element of one or more lists and returns a *new list* containing the results.

```lisp
(mapcar #'1+ '(1 2 3 4)) ; Applies 1+ (increment) to each element, returns (2 3 4 5)
(mapcar #'* '(1 2 3) '(4 5 6)) ; Applies * to corresponding elements of two lists, returns (4 10 18)
```

* **`mapc`**: Similar to `mapcar`, but it *does not* return a list of the results. It is used for side effects (like printing). It returns the first list argument.

```lisp
(mapc #'print '(1 2 3)) ; Prints each element, returns (1 2 3)
```

* **`maplist`**: Similar to `mapcar`, but it passes the *rest of the list* to the function in each call.

```lisp
(maplist #'(lambda (x) x) '(1 2 3)) ; Returns ((1 2 3) (2 3) (3))
```

* **`mapcon`**: Similar to `maplist` but it uses `nconc` to combine the results of the function calls.

```lisp
(mapcon #'(lambda (x) x) '(1 2 3)) ; Returns (1 2 3 2 3 3)
```

`mapcar` is the most commonly used mapping function.

Higher-order functions are a powerful tool in Common Lisp, enabling you to write more concise, reusable, and expressive code. They are particularly useful for working with collections of data and for creating more abstract control structures.
