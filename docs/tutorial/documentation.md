---
sidebar_position: 14
---

# Documenting your Code

## Introduction to Documenting Code in Common Lisp

Documenting code is a crucial aspect of software development, ensuring maintainability, collaboration, and understanding. Common Lisp provides excellent built-in facilities for documenting your code directly within the source files. These facilities allow you to embed documentation strings, which can then be extracted by various tools to generate documentation in different formats.

This tutorial will introduce you to the core concepts and forms for documenting Common Lisp code, covering how to attach documentation to various program entities and how to access this documentation programmatically.

**Key Concepts:**

* **Documentation Strings:** Strings attached to program entities (functions, variables, classes, etc.) that describe their purpose and usage.
* **`documentation` Function:** Accessing documentation strings programmatically.
* **Documenting Different Entities:** How to document functions, macros, variables, classes, packages, and more.
* **Style and Best Practices:** Guidelines for writing effective documentation.

**Table of Contents:**

**1. Documentation Strings:**

* What are documentation strings?
* Syntax for attaching documentation strings.
* Placement of documentation strings.

**2. The `documentation` Function:**

* Basic usage of `documentation`.
* Specifying the documentation type.
* Handling missing documentation.

**3. Documenting Different Entities:**

* Documenting functions (`defun`).
* Documenting macros (`defmacro`).
* Documenting variables (`defvar`, `defparameter`, `defconstant`).
* Documenting classes (`defclass`).
* Documenting packages (`defpackage`).
* Documenting generic functions (`defgeneric`).
* Documenting methods (`defmethod`).
* Documenting structures (`defstruct`).

**4. Style and Best Practices:**

* Writing clear and concise documentation.
* Using consistent formatting.
* Documenting parameters, return values, and side effects.
* Examples of good documentation.

**5. Tools and Utilities:**

* Brief overview of documentation generators.

This tutorial will provide a comprehensive guide to documenting your Common Lisp code effectively, enabling you to create well-documented and maintainable projects.

## 1. Documentation Strings

This section introduces the concept of documentation strings in Common Lisp, explaining what they are, how to attach them to code, and where they should be placed.

### 1.1 What are Documentation Strings?

Documentation strings (docstrings) are strings embedded within your Lisp code that serve as documentation for various program entities. They provide a way to describe the purpose, usage, and other relevant information about functions, macros, variables, classes, packages, and more.

Docstrings are not just comments; they are stored by the Lisp system and can be accessed programmatically using the `documentation` function (covered in the next section). This makes them invaluable for generating documentation, providing help within the REPL, and enabling interactive development tools.

### 1.2 Syntax for Attaching Documentation Strings

The syntax for attaching a documentation string depends on the specific construct you are documenting. However, the general principle is that the docstring should appear as a string literal immediately after the name of the entity being defined.

Here are some common examples:

* **Functions (`defun`):**

    ```lisp
    (defun greet (name)
      "Greets the given name." ; Docstring
      (format t "Hello, ~a!~%" name))
    ```

* **Macros (`defmacro`):**

    ```lisp
    (defmacro when-positive (number &body body)
      "Executes the body if the number is positive." ; Docstring
      `(if (> ,number 0)
           (progn ,@body)))
    ```

* **Variables (`defvar`, `defparameter`, `defconstant`):**

    ```lisp
    (defvar *my-global-variable* 42
      "A global variable used for important calculations.") ; Docstring

    (defparameter *debug-mode* nil
      "A flag to enable debug output.") ; Docstring

    (defconstant +pi+ 3.14159
      "An approximation of pi.") ; Docstring
    ```

* **Classes (`defclass`):**

    ```lisp
    (defclass person ()
      ((name :initarg :name :accessor person-name)
       (age :initarg :age :accessor person-age))
      (:documentation "Represents a person with a name and age.")) ; Docstring
    ```

* **Packages (`defpackage`):**

    ```lisp
    (defpackage :my-utils
      (:use :common-lisp)
      (:export :my-function :my-variable)
      (:documentation "A package containing utility functions.")) ; Docstring
    ```

* **Generic Functions (`defgeneric`):**

    ```lisp
    (defgeneric area (object)
      (:documentation "Computes the area of an object.")) ; Docstring
    ```

* **Methods (`defmethod`):**

    ```lisp
    (defmethod area ((r rectangle))
      "Computes the area of a rectangle." ; Docstring
      (* (rectangle-width r) (rectangle-height r)))
    ```

* **Structures (`defstruct`):**

    ```lisp
    (defstruct point
      (x 0)
      (y 0)
      (:documentation "Represents a point in 2D space.")) ; Docstring
    ```

### 1.3 Placement of Documentation Strings

The correct placement of documentation strings is crucial for them to be recognized by the Lisp system.

* For functions, macros, and generic functions, the docstring should be the *first* form inside the definition, immediately after the name of the entity.
* For variables, the docstring should follow the initial value (if any).
* For classes and structures, the docstring should be placed within the options list (using the `:documentation` keyword).
* For packages, the docstring is also placed within the options list using the `:documentation` keyword.

**Example showing correct and incorrect placement:**

```lisp
; Correct:
(defun my-function (x)
  "This is the correct placement."
  (+ x 1))

; Incorrect:
(defun my-function (x)
  (+ x 1)
  "This is incorrect.") ; This string will be evaluated but not stored as documentation

; Correct:
(defvar *my-variable* 10
  "Correct docstring placement")

; Incorrect:
(defvar *my-variable*
  "Incorrect docstring placement" ; This will cause an error
  10)
```

By following these rules, you can ensure that your documentation strings are correctly associated with the corresponding program entities and can be accessed programmatically. The next section will cover the `documentation` function, which is used to retrieve these docstrings.

## 2. The `documentation` Function

The `documentation` function is the primary way to programmatically access documentation strings in Common Lisp. This section explains how to use it, how to specify the documentation type, and how to handle cases where documentation is missing.

### 2.1 Basic Usage of `documentation`

The `documentation` function takes two arguments:

```lisp
(documentation symbol doc-type)
```

* **`symbol`:** The symbol representing the entity you want to get documentation for (e.g., a function name, variable name, class name, package name).
* **`doc-type`:** A keyword specifying the type of documentation you want to retrieve (e.g., `:function`, `:variable`, `:class`, `:package`).

`documentation` returns the documentation string associated with the given symbol and documentation type, or `NIL` if no documentation is found.

**Examples:**

Using the examples from the previous section:

```lisp
(defun greet (name)
  "Greets the given name."
  (format t "Hello, ~a!~%" name))

(documentation 'greet 'function) ; Returns "Greets the given name."

(defvar *my-global-variable* 42
  "A global variable used for important calculations.")

(documentation '*my-global-variable* 'variable) ; Returns "A global variable used for important calculations."

(defclass person ()
  ((name :initarg :name :accessor person-name)
   (age :initarg :age :accessor person-age))
  (:documentation "Represents a person with a name and age."))

(documentation 'person 'class) ; Returns "Represents a person with a name and age."

(defpackage :my-utils
  (:use :common-lisp)
  (:export :my-function :my-variable)
  (:documentation "A package containing utility functions."))

(documentation :my-utils 'package) ; Returns "A package containing utility functions."
```

### 2.2 Specifying the Documentation Type

The `doc-type` argument to `documentation` specifies which kind of documentation you are requesting. The most common doc-types are:

* `:function`: For functions and generic functions.
* `:macro`: For macros.
* `:variable`: For variables (including `defvar`, `defparameter`, and `defconstant`).
* `:class`: For classes.
* `:package`: For packages.
* `:type`: For types defined with `deftype`.
* `:struct`: For structures defined with `defstruct`.
* `:method`: For methods of generic functions.

It's important to use the correct `doc-type`, as a symbol can have different documentation strings for different types. For example, a symbol might be both a function and a variable, each with its own docstring.

**Example:**

```lisp
(defun my-function ()
  "This is the function's documentation.")

(defvar my-function 10
  "This is the variable's documentation.")

(documentation 'my-function 'function) ; Returns "This is the function's documentation."
(documentation 'my-function 'variable) ; Returns "This is the variable's documentation."
```

### 2.3 Handling Missing Documentation

If you call `documentation` with a symbol and `doc-type` for which no documentation string exists, it returns `NIL`. It's good practice to handle this case gracefully in your code.

**Example:**

```lisp
(defun my-function-without-docs ())

(if (documentation 'my-function-without-docs 'function)
    (print (documentation 'my-function-without-docs 'function))
    (print "No documentation found.")) ; Prints "No documentation found."
```

A more concise way to do this is using `or`:

```lisp
(print (or (documentation 'my-function-without-docs 'function)
           "No documentation found.")) ; Prints "No documentation found."
```

This uses the fact that `or` returns the first non-`NIL` value.

**Example with methods:**

Methods are a bit different, as they are associated with generic functions and specialized on parameter types. To get the documentation of a method, you need to provide a list of the specialized parameter types:

```lisp
(defgeneric my-generic (x))

(defmethod my-generic ((x integer))
  "This is the integer method.")

(documentation `(my-generic ,(find-class 'integer)) 'method) ; Returns "This is the integer method."
```

Here, `(find-class 'integer)` returns the class object for `integer`, and the backquoted expression creates the method specializer list.

Understanding how to use the `documentation` function is essential for working with documentation strings programmatically. It allows you to create tools that generate documentation, provide online help, and perform other documentation-related tasks. The next section will cover style and best practices for writing effective documentation.

## 3. Documenting Different Entities

This section provides specific examples of how to document various entities in Common Lisp, including functions, macros, variables, classes, packages, generic functions, methods, and structures.

### 3.1 Documenting Functions (`defun`)

The documentation string for a function should describe its purpose, arguments, return values, and any side effects.

```lisp
(defun factorial (n)
  "Computes the factorial of a non-negative integer N.
Returns an integer representing the factorial of N.
Signals an error if N is negative."
  (if (< n 0)
      (error "N must be non-negative.")
      (if (zerop n)
          1
          (* n (factorial (1- n))))))
```

Key elements to include in function documentation:

* **Purpose:** A brief, clear description of what the function does.
* **Arguments:** Describe each argument, including its type and purpose.
* **Return values:** Describe what the function returns and under what conditions.
* **Side effects:** Mention any side effects the function has (e.g., modifying global variables, performing I/O).
* **Exceptions/Errors:** Document any errors or exceptions that the function might signal.

### 3.2 Documenting Macros (`defmacro`)

Documenting macros is especially important because they perform code transformations. The documentation should explain what the macro does, how it transforms code, and what its arguments represent.

```lisp
(defmacro when-positive (number &body body)
  "Executes the BODY forms if NUMBER is positive.
NUMBER is an expression that evaluates to a number.
BODY is a sequence of forms to be executed if NUMBER > 0."
  `(if (> ,number 0)
       (progn ,@body)))
```

Key elements to include in macro documentation:

* **Purpose:** A concise description of the macro's function.
* **Arguments:** Explain what each argument represents *in terms of code forms* (e.g., "a form that evaluates to a number," "a list of forms").
* **Expansion:** Briefly describe how the macro transforms the code.

### 3.3 Documenting Variables (`defvar`, `defparameter`, `defconstant`)

Documentation for variables should describe their purpose, usage, and any special characteristics.

```lisp
(defvar *debug-mode* nil
  "A flag to enable or disable debug output.
Setting this to T will enable verbose logging.")

(defparameter *default-name* "Anonymous"
  "The default name used when no name is provided.")

(defconstant +maximum-connections+ 100
  "The maximum number of allowed network connections.")
```

Key elements to include in variable documentation:

* **Purpose:** What the variable is used for.
* **Scope:** Whether it's a global variable, special variable, or lexical variable.
* **Meaning of values:** What different values of the variable represent.

### 3.4 Documenting Classes (`defclass`)

Class documentation should describe the purpose of the class, its slots (instance variables), and any important behavior.

```lisp
(defclass person ()
  ((name :initarg :name :accessor person-name)
   (age :initarg :age :accessor person-age))
  (:documentation "Represents a person with a name and age.
The NAME slot stores the person's name as a string.
The AGE slot stores the person's age as an integer."))
```

Key elements to include in class documentation:

* **Purpose:** A description of what the class represents.
* **Slots:** Describe each slot, including its name, type (if applicable), and purpose.
* **Inheritance:** Mention any superclasses.

### 3.5 Documenting Packages (`defpackage`)

Package documentation should describe the purpose of the package and what functionality it provides.

```lisp
(defpackage :my-utils
  (:use :common-lisp)
  (:export :my-function :my-variable)
  (:documentation "A package containing utility functions for string manipulation and data processing."))
```

Key elements to include in package documentation:

* **Purpose:** A high-level description of the package's purpose.
* **Functionality:** A summary of the main features and functionalities provided by the package.

### 3.6 Documenting Generic Functions (`defgeneric`)

Generic function documentation should describe the function's purpose and the general meaning of its arguments. The specific behavior for different argument types is documented in the methods.

```lisp
(defgeneric area (object)
  (:documentation "Computes the area of an OBJECT.
OBJECT can be any geometric shape."))
```

Key elements to include in generic function documentation:

* **Purpose:** A general description of what the function does.
* **Arguments:** Describe the general meaning of each argument.

### 3.7 Documenting Methods (`defmethod`)

Method documentation should describe the specific behavior of the method for the given argument types.

```lisp
(defmethod area ((r rectangle))
  "Computes the area of a RECTANGLE by multiplying its width and height."
  (* (rectangle-width r) (rectangle-height r)))
```

Key elements to include in method documentation:

* **Specific behavior:** Describe what the method does for the given argument types.
* **Specializations:** Clearly indicate the types of the specialized arguments.

### 3.8 Documenting Structures (`defstruct`)

Structure documentation should describe the purpose of the structure and the meaning of its fields.

```lisp
(defstruct point
  (x 0)
  (y 0)
  (:documentation "Represents a point in 2D space.
The X field stores the x-coordinate.
The Y field stores the y-coordinate."))
```

Key elements to include in structure documentation:

* **Purpose:** A description of what the structure represents.
* **Fields:** Describe each field, including its name, type (if applicable), and purpose.

By following these guidelines, you can write clear, concise, and informative documentation for your Common Lisp code. This will make your code easier to understand, maintain, and use. The next section will cover style and best practices for writing effective documentation.

## 4. Style and Best Practices

This section outlines style guidelines and best practices for writing effective documentation strings in Common Lisp. Consistent and well-written documentation significantly improves code maintainability and usability.

### 4.1 Writing Clear and Concise Documentation

* **Be brief and to the point:** Avoid unnecessary jargon or overly verbose explanations. Get straight to the purpose of the documented entity.
* **Use simple language:** Use clear and straightforward language that is easy to understand, even for those who are not intimately familiar with the code.
* **Focus on the "what" and "why":** Explain what the entity does and why it exists. Don't just rephrase the code.
* **Use complete sentences:** Write grammatically correct and complete sentences. This improves readability and professionalism.
* **Avoid ambiguity:** Be precise in your descriptions to avoid any confusion or misinterpretations.

### 4.2 Using Consistent Formatting

Consistent formatting makes documentation easier to read and parse.

* **Indentation:** Indent docstrings consistently with the surrounding code.
* **Line breaks:** Use line breaks to improve readability, especially for longer docstrings. Aim for lines no longer than 80 characters.
* **Capitalization:** Capitalize the first letter of the first sentence of a docstring.
* **Punctuation:** Use proper punctuation, including periods at the end of sentences.

### 4.3 Documenting Parameters, Return Values, and Side Effects

* **Parameters:** Clearly describe each parameter, including its type (if applicable), purpose, and any constraints or special considerations. Use a consistent format for parameter descriptions. A common convention is to mention parameter types in parentheses.

    ```lisp
    (defun process-data (data (filename string))
      "Processes the given DATA.
    DATA (a list) is the data to be processed.
    FILENAME (a string) is the name of the file to write the results to."
      ; ...
    )
    ```

* **Return values:** Explicitly state what the function or macro returns and under what conditions. If multiple values are returned, describe each one.

    ```lisp
    (defun divide (x y)
      "Divides X by Y.
    Returns two values: the quotient and the remainder.
    Signals an error if Y is zero."
      (if (zerop y)
          (error "Division by zero.")
          (values (floor x y) (rem x y))))
    ```

* **Side effects:** Document any side effects the entity has, such as modifying global variables, performing I/O, or changing the state of objects.

    ```lisp
    (defun write-to-log (message)
      "Writes MESSAGE to the log file.
    Side effects: Appends MESSAGE to the log file."
      ; ...
    )
    ```

### 4.4 Examples of Good Documentation

Here are some more examples of well-written documentation strings:

**Function:**

```lisp
(defun string-upcase-first (string)
  "Converts the first character of STRING to uppercase.
STRING (a string) is the input string.
Returns a new string with the first character uppercased.
Returns STRING unchanged if it is empty."
  (if (zerop (length string))
      string
      (concatenate 'string (string-upcase (subseq string 0 1)) (subseq string 1))))
```

**Macro:**

```lisp
(defmacro with-open-file-for-output ((stream pathname &rest options) &body body)
  "Opens a file for output, executes BODY forms, and then closes the file.
STREAM (a symbol) is the variable to which the file stream will be bound.
PATHNAME (a string or pathname) is the path to the file.
OPTIONS are keyword arguments passed to OPEN.
Expands to a WITH-OPEN-FILE form."
  `(with-open-file (,stream ,pathname :direction :output ,@options)
     ,@body))
```

**Class:**

```lisp
(defclass point ()
  ((x :initarg :x :accessor point-x)
   (y :initarg :y :accessor point-y))
  (:documentation "Represents a point in a 2D Cartesian coordinate system.
The X slot stores the x-coordinate (a number).
The Y slot stores the y-coordinate (a number)."))
```

By adhering to these style guidelines and best practices, you can create high-quality documentation that will make your Common Lisp code more accessible and maintainable. The next section will briefly mention some tools and utilities that can be used for generating documentation from these docstrings.

## 5. Tools and Utilities

This section provides a brief overview of tools and utilities that can be used to generate documentation from the docstrings you've written in your Common Lisp code.

### 5.1 Available Systems for Generating Documentation

There are different options to choose. Please check the [Awesome-CL](https://github.com/CodyReichert/awesome-cl?tab=readme-ov-file#documentation-builders) section on Documentation Builders for the latest recommendations.

* Staple - a tool to generate documentation pages using an HTML template. Uses the existing README, adds docstrings, crossreferences and links to the CLHS. zlib.
* mgl-pax - Exploratory programming environment and documentation generator. one may accomplish similar effects as with Literate Programming, but documentation is generated from code, not vice versa. Code is first, code must look pretty, documentation is code. MIT.
* sphinxcontrib-cldomain - Extending Sphinx to cover Common Lisp. To build documentation with the same ease as sphinx would a Python project. GPL3 crossreferences, links to the CLHS, symbol index, search, and all Sphinx features.
* Codex - A beautiful documentation system for Common Lisp. MIT.
* See also the [Literate Programming](https://github.com/CodyReichert/awesome-cl?tab=readme-ov-file#literate-programming) section in the [Awesome-CL](https://github.com/CodyReichert/awesome-cl) page.
