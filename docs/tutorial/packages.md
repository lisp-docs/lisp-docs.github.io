---
sidebar_position: 8
---

# Packages

:::warning
This article **needs to be checked!**.
Please help contribute by checking it and making changes in [our repository](https://github.com/lisp-docs/lisp-docs.github.io) or by clicking on the "Edit this page" link below.
:::

## Quickstart

If you are looking to just hit the ground running and learn Common Lisp I recommend just [installing SBCL](https://www.sbcl.org/getting.html) and [going ahead](/docs/tutorial/informal-introduction-to-lisp) with the tutorial.

If you are on a linux/debian type system, you can run

```bash
$ sudo apt-get install sbcl
```

For macOS

```bash
$ brew install sbcl
```

For Arch Linux

```bash
$ sudo pacman -S sbcl
```

For other systems, Windows, or to just get the latest version instead of the one in your package manager, you can [download a binary here](https://www.sbcl.org/platform-table.html) (click on the green cell in the table corresponding to your system, they are actually links). Then follow these instructions: [SBCL's Getting Started](https://www.sbcl.org/getting.html).

For a more detailed explanation, read ahead.

## Introduction to Packages, Protocols, and Code in Different Files in Common Lisp

As Lisp programs grow in size and complexity, it becomes essential to organize code into manageable units. Common Lisp provides *packages* as a mechanism for namespace management, preventing name clashes and promoting modularity. Furthermore, the concept of *protocols* (often implemented using generic functions and CLOS) allows for defining interfaces and structuring interactions between different parts of a system. Finally, organizing code into separate files is crucial for maintainability and collaboration.

This tutorial will introduce you to these essential concepts, explaining how to define and use packages, how to implement protocols, and how to structure your Lisp code across multiple files.

**Key Concepts:**

* **Packages:** Namespaces that prevent symbol collisions.
* **Symbols and Interning:** How symbols are associated with packages.
* **`defpackage`:** Defining new packages.
* **`in-package`:** Switching to a different package.
* **`use-package`:** Importing symbols from other packages.
* **`export` and `import`:** Controlling symbol visibility.
* **Protocols (using Generic Functions and CLOS):** Defining interfaces for interacting with objects.
* **Loading Code from Files:** Using `load` and `require` to load code from separate files.
* **ASDF System Definitions:** Using ASDF to manage multi-file projects and dependencies.

**Table of Contents:**

**1. Packages:**

* What are packages and why are they needed?
* The `COMMON-LISP` and `KEYWORD` packages.
* `defpackage`: Defining a package.
* `in-package`: Switching to a package.
* `use-package`: Importing symbols.
* `export`: Making symbols visible to other packages.
* `import`: Accessing external symbols.
* Package nicknames.

**2. Symbols and Interning:**

* What is a symbol?
* The concept of interning.
* Package prefixes (e.g., `package:symbol`).

**3. Protocols (with Generic Functions and CLOS):**

* Defining protocols using generic functions.
* Implementing protocols with methods.
* Benefits of using protocols.
* Example of a simple protocol.

**4. Loading Code from Files:**

* `load`: Evaluating Lisp code from a file.
* `require`: Loading a file only once.

**5. Organizing Code into Systems with ASDF:**

* ASDF basics (recap from previous tutorial, focusing on multi-file aspects).
* Defining systems with multiple components (files).
* Dependencies between systems.

**6. Best Practices and Examples:**

* Structuring a multi-file project.
* Example of a project using packages, protocols, and ASDF.

This tutorial aims to provide a practical guide to organizing and managing Common Lisp code in larger projects. By the end, you should be able to create and use packages, define protocols, and structure your code across multiple files effectively.

## 1. Packages

Packages in Common Lisp provide a way to organize code into namespaces, preventing naming conflicts and promoting modularity. This section explains the basics of packages and how to use them.

### 1.1 What are Packages and Why are They Needed?

In any large software project, it's common to have multiple developers or libraries contributing code. Without a mechanism for managing namespaces, it's easy to encounter naming collisions, where two different parts of the code use the same name for different things (e.g., two different functions named `calculate-area`).

Packages solve this problem by providing separate namespaces. Each symbol (variable, function, class, etc.) belongs to a specific package. This allows you to have symbols with the same name in different packages without them interfering with each other.

### 1.2 The `COMMON-LISP` and `KEYWORD` Packages

Two special packages are always present in a Common Lisp environment:

* **`COMMON-LISP` (or `CL`):** This package contains all the standard Common Lisp functions, macros, and other symbols. When you start a Lisp REPL, you are usually in this package.
* **`KEYWORD`:** This package contains keywords, which are symbols that evaluate to themselves (e.g., `:foo`, `:bar`). Keywords are commonly used as named arguments to functions and macros.

### 1.3 `defpackage`: Defining a Package

The `defpackage` macro is used to define a new package. Its basic syntax is:

```lisp
(defpackage package-name
  (:use package1 package2 ...)
  (:export symbol1 symbol2 ...)
  (:nicknames nickname1 nickname2 ...))
```

* **`package-name`:** The name of the package (a symbol or a string).
* **`(:use package1 package2 ...)`:** Specifies which other packages this package *uses*. Using a package imports the external symbols of that package.
* **`(:export symbol1 symbol2 ...)`:** Specifies which symbols from this package are *exported* (made visible to other packages).
* **`(:nicknames nickname1 nickname2 ...)`:** Specifies alternative names (nicknames) for the package.

**Example:**

```lisp
(defpackage my-utils
  (:use common-lisp)
  (:export my-function my-variable)
    (:nicknames mu))
```

This defines a package named `my-utils` that:

* Uses the `COMMON-LISP` package (so it can use standard Lisp functions).
* Exports the symbols `my-function` and `my-variable`.
* Has the nickname `mu`.

### 1.4 `in-package`: Switching to a Package

The `in-package` macro is used to switch the current package. When you evaluate code, the symbols you type are interned (associated) with the current package.

```lisp
(in-package :my-utils) ; Switch to the my-utils package

(defun my-function (x) (* x 2)) ; Defines my-function in my-utils

(defvar my-variable 10) ; Defines my-variable in my-utils
```

Now, `my-function` and `my-variable` belong to the `my-utils` package.

### 1.5 `use-package`: Importing Symbols

The `use-package` function imports all external symbols from another package into the current package. This makes it easier to use those symbols without having to qualify them with the package name.

```lisp
(in-package :another-package)

(use-package :my-utils) ; Imports my-function and my-variable

(my-function 5)     ; Now you can use my-function directly
(print my-variable) ; And my-variable
```

### 1.6 `export`: Making Symbols Visible to Other Packages

The `export` function (or the `:export` option in `defpackage`) makes symbols in a package visible to other packages. Only exported symbols can be imported using `use-package`.

```lisp
(in-package :my-utils)

(export 'my-function) ; Makes my-function visible
(export '(my-variable my-other-function)) ; Export multiple symbols
```

### 1.7 `import`: Accessing External Symbols

The `import` function allows you to access individual symbols from other packages without using `use-package`. This is useful when you only need a few symbols from a package or when using `use-package` would cause name conflicts.

```lisp
(in-package :yet-another-package)

(import 'my-utils:my-function) ; Imports only my-function

(my-utils:my-variable) ; You still need to qualify my-variable
(my-function 10) ; You can use my-function directly
```

### 1.8 Package Nicknames

Package nicknames provide shorter, more convenient ways to refer to packages.

```lisp
(defpackage my-long-package-name
  (:nicknames m-l-p))

(in-package :m-l-p) ; You can now use the nickname
```

**Example summarizing package usage:**

```lisp
(defpackage :my-application
  (:use :common-lisp :my-utils))

(in-package :my-application)

(my-function 20) ; Uses my-function from my-utils
(print my-variable) ; Uses my-variable from my-utils

(defun application-function ()
  (print "Application function"))

(export 'application-function) ; Exports the application function
```

This section provided a basic introduction to packages in Common Lisp. Understanding packages is essential for organizing larger projects and preventing naming conflicts. The next section will cover symbols and interning in more detail.

## 2. Symbols and Interning

This section delves into the concept of symbols and interning, which are fundamental to how packages work in Common Lisp.

### 2.1 What is a Symbol?

In Common Lisp, a *symbol* is a data object that represents a name. Symbols are used as identifiers for variables, functions, classes, and other program entities. Unlike strings, which represent sequences of characters, symbols are atomic objects with unique identities.

Key characteristics of symbols:

* **Name:** A symbol has a name, which is a string.
* **Package:** A symbol belongs to a specific package.
* **Value:** A symbol can have a value (if it's a variable).
* **Function definition:** A symbol can have a function definition (if it's a function name).
* **Property list:** A symbol can have a property list, which is a list of key-value pairs that can be used to store arbitrary information about the symbol.

**Examples:**

`foo`, `BAR`, `my-variable`, `+`, `-` are all symbols.

### 2.2 The Concept of Interning

*Interning* is the process of associating a symbol's name with a unique object in a specific package. When you type a symbol in the Lisp REPL or in your code, the Lisp system *interns* that symbol in the current package. This means that if you type the same symbol name again, you will get the *same* symbol object.

**Example:**

```lisp
(eq 'foo 'foo) ; Returns T (because 'foo refers to the same symbol object)

(string= (symbol-name 'foo) "FOO") ; Returns T (symbol names are case insensitive by default)
```

Here's how interning works:

1. When the Lisp reader encounters a symbol name (e.g., `foo`), it checks if a symbol with that name already exists in the current package.
2. If a symbol with that name exists, the reader returns that existing symbol object.
3. If a symbol with that name does not exist, the reader creates a new symbol object, associates it with the name, and *interns* it in the current package.

**Uninterned Symbols:**

It's also possible to create *uninterned* symbols using `gensym` (which we saw in the macros tutorial). Uninterned symbols are not associated with any package and are guaranteed to be unique.

```lisp
(eq (gensym) (gensym)) ; Returns NIL (because each call to gensym creates a new, distinct symbol)
```

### 2.3 Package Prefixes (e.g., `package:symbol`)

To refer to a symbol in a different package, you can use a *package prefix*. The syntax is:

```lisp
package-name:symbol-name
```

or

```lisp
package-nickname:symbol-name
```

**Example:**

Let's say you have a package `my-utils` that exports a function `my-function`:

```lisp
(defpackage my-utils
    (:use common-lisp)
    (:export my-function)
    (:nicknames mu))

(in-package :my-utils)

(defun my-function (x) (* x 2))
```

To call `my-function` from another package, you can use the package prefix:

```lisp
(in-package :cl-user) ; Switch to the CL-USER package

(my-utils:my-function 5) ; Calls my-function from the my-utils package, returns 10
(mu:my-function 10) ; Using the nickname, returns 20
```

If you have used a package (with `use-package`) you don't need the package prefix:

```lisp
(in-package :cl-user)

(use-package :my-utils)

(my-function 15) ; Calls my-function from my-utils without the prefix, returns 30
```

**Double Colon `::`:**

You can also use a double colon `::` to refer to a symbol in another package, even if it is *not* exported. However, this is generally discouraged as it breaks encapsulation and makes your code more fragile.

```lisp
(in-package :cl-user)

(my-utils::internal-function) ; Accesses an internal (unexported) function (if it exists)
```

Using package prefixes and understanding interning is crucial for managing namespaces and writing modular Common Lisp code. It allows you to organize your code into logical units and prevent naming collisions. The next section will cover protocols (using generic functions and CLOS).

## 3. Protocols (with Generic Functions and CLOS)

In Common Lisp, *protocols* are often implemented using generic functions and the Common Lisp Object System (CLOS). A protocol defines a set of operations (functions) that objects of different classes should support. This allows for polymorphism and extensibility.

### 3.1 Defining Protocols Using Generic Functions

A protocol is defined by declaring a set of generic functions. A generic function is a function that can have different behaviors (methods) depending on the types of its arguments.

```lisp
(defgeneric area (object)
  (:documentation "Computes the area of an object."))

(defgeneric perimeter (object)
  (:documentation "Computes the perimeter of an object."))
```

Here, we define two generic functions: `area` and `perimeter`. These functions constitute our "geometric shape" protocol. They don't have any specific implementation yet; they simply declare the operations that should be supported.

### 3.2 Implementing Protocols with Methods

To implement a protocol for a specific class, you define methods for the generic functions. Methods are specialized on the types of their arguments.

```lisp
(defclass rectangle ()
  ((width :initarg :width :accessor rectangle-width)
   (height :initarg :height :accessor rectangle-height)))

(defmethod area ((r rectangle))
  (* (rectangle-width r) (rectangle-height r)))

(defmethod perimeter ((r rectangle))
  (* 2 (+ (rectangle-width r) (rectangle-height r))))

(defclass circle ()
  ((radius :initarg :radius :accessor circle-radius)))

(defmethod area ((c circle))
  (* pi (expt (circle-radius c) 2)))

(defmethod perimeter ((c circle))
  (* 2 pi (circle-radius c)))
```

Here, we define two classes, `rectangle` and `circle`, and provide methods for `area` and `perimeter` for each class. Now, objects of these classes implement the "geometric shape" protocol.

### 3.3 Benefits of Using Protocols

* **Polymorphism:** You can write code that works with objects of different classes as long as they implement the same protocol.

    ```lisp
    (defun describe-shape (shape)
      (format t "Area: ~a, Perimeter: ~a~%" (area shape) (perimeter shape)))

    (let ((rect (make-instance 'rectangle :width 5 :height 10))
          (circ (make-instance 'circle :radius 3)))
      (describe-shape rect) ; Calls the rectangle methods
      (describe-shape circ)) ; Calls the circle methods
    ```

* **Extensibility:** You can easily add new classes that implement the protocol without modifying existing code.

    ```lisp
    (defclass square (rectangle)
      ())

    (defmethod area ((s square))
        (expt (rectangle-width s) 2))

    (defmethod perimeter ((s square))
        (* 4 (rectangle-width s)))

    (describe-shape (make-instance 'square :width 4))
    ```

* **Abstraction:** Protocols define an interface, separating the implementation from the usage. This makes code more modular and easier to maintain.

### 3.4 Example of a Simple Protocol

Let's consider a simple "printable" protocol:

```lisp
(defgeneric print-object-nicely (object stream)
  (:documentation "Prints an object in a user-friendly format to a stream."))

(defmethod print-object-nicely ((object t) stream)
  (print-object object stream)) ; Default method: use the standard print-object

(defclass person ()
  ((name :initarg :name :accessor person-name)
   (age :initarg :age :accessor person-age)))

(defmethod print-object-nicely ((p person) stream)
  (format stream "Person: ~a (Age: ~a)" (person-name p) (person-age p)))

(let ((person (make-instance 'person :name "Alice" :age 30)))
  (print-object-nicely person t)) ; Prints "Person: Alice (Age: 30)" to the standard output
```

This defines a `print-object-nicely` generic function. The default method simply uses the standard `print-object`. The method specialized on `person` provides a custom printing format.

This section explained how to define and implement protocols using generic functions and CLOS. Protocols are a powerful tool for writing extensible and maintainable code in Common Lisp. The next section will cover loading code from files.

## 4. Loading Code from Files

As your Lisp projects grow, you'll want to organize your code into multiple files. Common Lisp provides functions for loading code from files. This section covers `load` and `require`.

### 4.1 `load`: Evaluating Lisp Code from a File

The `load` function evaluates Lisp code from a file. Its basic syntax is:

```lisp
(load pathname &key verbose print compile if-does-not-exist external-format)
```

* **`pathname`:** The pathname of the file to load (a string, pathname object, or logical pathname).
* **`verbose`:** If true (default), prints messages indicating which files are being loaded.
* **`print`:** If true, prints the values returned by the top-level forms in the file.
* **`compile`:** If true, compiles the file before loading (if it's not already compiled).
* **`if-does-not-exist`:** Specifies what to do if the file does not exist (`:error` (default), `:ignore`, or a function to call).
* **`external-format`:** Specifies the character encoding of the file.

**Example:**

Let's say you have a file named `my-functions.lisp` with the following content:

```lisp
(in-package :my-utils)

(defun greet (name)
  (format t "Hello, ~a!~%" name))

(defvar *my-special-variable* 42)
```

You can load this file using `load`:

```lisp
(load "my-functions.lisp") ; Loads and evaluates the code in the file
```

After loading, you can use the functions and variables defined in the file:

```lisp
(in-package :cl-user)
(use-package :my-utils)

(greet "World") ; Prints "Hello, World!"
(print *my-special-variable*) ; Prints 42
```

**Pathnames:**

It's generally better to use pathname objects rather than strings for file names, as they are more portable. You can create pathname objects using `pathname` or `#p`:

```lisp
(load (pathname "my-functions.lisp"))
(load #p"my-functions.lisp")
```

You can also specify absolute or relative paths:

```lisp
(load #p"/path/to/my/file.lisp") ; Absolute path
(load #p"../another/file.lisp")   ; Relative path
```

**Compiling Files:**

You can compile a file using `compile-file`:

```lisp
(compile-file "my-functions.lisp") ; Creates a fasl file (compiled code)
```

Loading the compiled file is generally faster:

```lisp
(load "my-functions.fasl")
```

### 4.2 `require`: Loading a File Only Once

The `require` function is similar to `load`, but it ensures that a file is loaded only *once*. If the file has already been loaded, `require` does nothing. This is important to prevent redefinitions and potential errors.

The syntax is:

```lisp
(require module-name &optional pathname)
```

* **`module-name`:** A symbol representing the module (usually the file name without the extension).
* **`pathname`:** An optional pathname for the file. If omitted, `require` searches for a file named `module-name.lisp` or `module-name.fasl` in implementation dependent locations.

**Example:**

```lisp
(require :my-functions) ; Loads my-functions.lisp (or .fasl) if it hasn't been loaded yet
(require :my-functions) ; Does nothing because my-functions is already loaded
```

`require` is commonly used to load libraries and dependencies. It's good practice to use `require` instead of `load` whenever you want to ensure that a file is loaded only once.

**Example using pathname:**

```lisp
(require :my-functions #p"path/to/my-functions.lisp")
```

This will load the file specified by the pathname, regardless of the module name.

**Order of loading:**

When you have multiple files that depend on each other, the order in which you load them is important. You should load files in dependency order, starting with the files that have no dependencies and ending with the files that depend on others. ASDF (covered in the next section) helps manage this automatically.

This section explained how to load code from files using `load` and `require`. Using these functions is essential for organizing your Lisp code into manageable modules. The next section will cover ASDF, which is a standard build system that handles multi-file projects and dependencies.

## 5. Organizing Code into Systems with ASDF

ASDF (Another System Definition Facility) is the standard build system for Common Lisp. It provides a way to define software components (systems) and their dependencies, making it easier to build, load, and manage multi-file Lisp projects. This section focuses on how ASDF helps organize code across multiple files.

### 5.1 ASDF Basics (Recap Focusing on Multi-File Aspects)

As a quick recap (since this was discussed [here](/docs/tutorial/using-cl-implementation#4-build-tools-asdf)), ASDF uses system definition files (`.asd` files) to describe the structure of a project. A system is a collection of files (Lisp source code, data files, etc.) that make up a logical unit of software.


A basic system definition looks like this:

```lisp
(asdf:defsystem :my-project
  :description "My awesome project"
  :author "Your Name"
  :license "MIT"
  :version "1.0.0"
  :components ((:file "my-project")))
```

This simple example defines a system named `:my-project` with a single component: a Lisp source file named `my-project.lisp`.

For multi-file projects, the `:components` list becomes more important.

### 5.2 Defining Systems with Multiple Components (Files)

To define a system with multiple files, you list multiple components in the `:components` list. The order of the components is important, as ASDF uses this order to determine the loading and compilation order.

**Example:**

Let's say you have a project with three files: `utils.lisp`, `core.lisp`, and `main.lisp`. `main.lisp` depends on `core.lisp`, and `core.lisp` depends on `utils.lisp`. Your project directory would look like this:

```lisp
my-project/
├── my-project.asd
├── utils.lisp
├── core.lisp
└── main.lisp
```

Your `my-project.asd` file would look like this:

```lisp
(asdf:defsystem :my-project
  :description "A multi-file project"
  :author "Your Name"
  :license "MIT"
  :version "1.0.0"
  :components ((:file "utils")
               (:file "core")
               (:file "main")))
```

ASDF will ensure that `utils.lisp` is loaded before `core.lisp`, and `core.lisp` is loaded before `main.lisp`.

**Using Modules:**

For larger projects, you can use *modules* to organize components into subdirectories.

**Example:**

Let's reorganize the project:

```lisp
my-project/
├── my-project.asd
└── src/
    ├── utils.lisp
    ├── core.lisp
    └── main.lisp
```

The `my-project.asd` file would then be:

```lisp
(asdf:defsystem :my-project
  :description "A multi-file project with modules"
  :author "Your Name"
  :license "MIT"
  :version "1.0.0"
  :components ((:module "src"
                 :components ((:file "utils")
                              (:file "core")
                              (:file "main")))))
```

ASDF will now look for the files in the `src` subdirectory.

### 5.3 Dependencies Between Systems

One of the most powerful features of ASDF is its ability to manage dependencies between systems. If your project depends on other libraries or systems, you can specify these dependencies in the `:depends-on` list of your system definition.

**Example:**

Let's say your `main.lisp` file uses the `drakma` library for making HTTP requests. You would add `:drakma` to the `:depends-on` list:

```lisp
(asdf:defsystem :my-project
  :description "A project with dependencies"
  :author "Your Name"
  :license "MIT"
  :version "1.0.0"
  :depends-on (:drakma) ; Declares dependency on drakma
  :components ((:module "src"
                 :components ((:file "utils")
                              (:file "core")
                              (:file "main")))))
```

When you load `:my-project` using `asdf:load-system`, ASDF will automatically load `drakma` (if it's not already loaded) before loading your project's files. This greatly simplifies dependency management.

**Local Projects:**

If you are developing a library that you want to include as a dependency, you can use local projects. You put the library's `.asd` file into `~/.quicklisp/local-projects`.

**Example:**

You have a local library called `my-local-lib` in `~/.quicklisp/local-projects/my-local-lib.asd`.

```lisp
(asdf:defsystem :my-project
  :description "A project with local dependencies"
  :author "Your Name"
  :license "MIT"
  :version "1.0.0"
  :depends-on (:my-local-lib) ; Declares dependency on local project
  :components ((:module "src"
                 :components ((:file "utils")
                              (:file "core")
                              (:file "main")))))
```

ASDF will look for the `my-local-lib` system in the local projects directory.

By using ASDF, you can easily manage complex multi-file projects, handle dependencies, and ensure that your code is loaded and compiled in the correct order. This makes your Lisp development workflow much more efficient and organized. The next section will cover best practices and examples of structuring multi-file projects.

## 6. Best Practices and Examples

This section provides best practices for structuring multi-file projects in Common Lisp and presents a comprehensive example that integrates packages, protocols, and ASDF.

### 6.1 Structuring a Multi-File Project

Here are some recommended practices for organizing your Lisp projects:

* **One System per Project:** Typically, you'll have one ASDF system definition (`.asd` file) per project.
* **Directory Structure:** Use a clear and consistent directory structure. A common approach is:

```bash
my-project/
├── my-project.asd     ; ASDF system definition
├── src/               ; Source code
│   ├── package.lisp   ; Package definition
│   ├── utils.lisp     ; Utility functions
│   ├── core.lisp      ; Core logic
│   └── main.lisp      ; Main entry point
└── tests/             ; Unit tests (optional)
    └── test-suite.lisp
```

* **Package Definitions:** Define your packages in a separate `package.lisp` file within your `src` directory. This makes it clear which symbols are exported.
* **Dependency Order:** Ensure that your components in the `.asd` file are listed in the correct dependency order. Files defining packages should generally be loaded first.
* **Use Modules for Subsystems:** For larger projects, use modules to group related components into subdirectories.
* **Separate Tests:** Keep your unit tests in a separate `tests` directory.

### 6.2 Example of a Project Using Packages, Protocols, and ASDF

Let's create a simple project that simulates different types of animals.

**Directory Structure:**

```lisp
animal-simulator/
├── animal-simulator.asd
└── src/
    ├── package.lisp
    ├── animal.lisp
    └── simulator.lisp
```

**src/package.lisp:**

```lisp
(defpackage :animal-simulator
  (:use :common-lisp)
  (:export :animal :make-animal :animal-name :animal-sound
           :simulate-round))
```

**src/animal.lisp:**

```lisp
(in-package :animal-simulator)

(defclass animal ()
  ((name :initarg :name :accessor animal-name)))

(defgeneric animal-sound (animal)
  (:documentation "Returns the sound an animal makes."))

(defmethod animal-sound ((animal animal))
  "Generic animal sound")

(defclass dog (animal) ())

(defmethod animal-sound ((dog dog))
  "Woof!")

(defclass cat (animal) ())

(defmethod animal-sound ((cat cat))
  "Meow!")

(defun make-animal (type name)
  (make-instance type :name name))
```

**src/simulator.lisp:**

```lisp
(in-package :animal-simulator)

(defun simulate-round (animals)
  (dolist (animal animals)
    (format t "~a says ~a~%" (animal-name animal) (animal-sound animal))))
```

**animal-simulator.asd:**

```lisp
(asdf:defsystem :animal-simulator
  :description "A simple animal simulator"
  :author "Your Name"
  :license "MIT"
  :version "1.0.0"
  :components ((:file "src/package")
               (:file "src/animal")
               (:file "src/simulator")))
```

**Using the Project:**

1. **Load the system:**

    ```lisp
    (ql:quickload :animal-simulator)
    ```

2. **Use the functions:**

    ```lisp
    (in-package :animal-simulator)

    (let ((animals (list (make-animal 'dog "Rover")
                         (make-animal 'cat "Whiskers")
                         (make-animal 'animal "Generic"))))
      (simulate-round animals))
    ; Output:
    ; Rover says Woof!
    ; Whiskers says Meow!
    ; Generic says Generic animal sound
    ```

This example demonstrates how to:

* Define a package (`animal-simulator`).
* Define a protocol (`animal-sound` as a generic function).
* Implement the protocol for different classes (`dog` and `cat`).
* Organize the code into multiple files.
* Use ASDF to manage the project.

By following these best practices and using the tools provided by Common Lisp, you can effectively structure and manage even complex projects. This concludes the tutorial on packages, protocols, and multi-file projects. You should now be equipped to organize your Lisp code in a modular and maintainable way.
