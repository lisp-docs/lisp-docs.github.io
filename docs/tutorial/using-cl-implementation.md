---
sidebar_position: 3
---

# Common Lisp Implementations

## Quickstart

If you are looking to just hit the ground running and learn Common Lisp I recommend just [installing SBCL](https://www.sbcl.org/getting.html) and [going ahead](/docs/tutorial/informal-introduction-to-lisp) with the tutorial.

If you are on a linux/debian type system, you can run

```bash
sudo apt-get install sbcl
```

For macOS

```bash
brew install sbcl
```

For Arch Linux

```bash
sudo pacman -S sbcl
```

For other systems, Windows, or to just get the latest version instead of the one in your package manager, you can [download a binary here](https://www.sbcl.org/platform-table.html) (click on the green cell in the table corresponding to your system, they are actually links). Then follow these instructions: [SBCL's Getting Started](https://www.sbcl.org/getting.html).

I recommend now to [continue](/docs/tutorial/informal-introduction-to-lisp) with the tutorial. Otherwise, for a more detailed explanation, read ahead.

## Introduction to Common Lisp Implementations

Common Lisp is defined by a standard, but it's the *implementations* that bring that standard to life. A Common Lisp implementation is a software system that provides a Lisp environment, including a compiler or interpreter, a runtime system, and often additional tools like debuggers, profilers, and development environments. Choosing the right implementation is important, as they can differ in performance, features, supported platforms, and available libraries.

This tutorial will introduce you to some of the most popular and actively maintained Common Lisp implementations, discuss their strengths and weaknesses, and cover associated concepts like package managers and build tools.

### Key Concepts

* **Implementation:** A concrete realization of the Common Lisp standard.
* **Compiler/Interpreter:** The component that translates Lisp code into executable instructions.
* **Runtime System:** The environment that supports the execution of Lisp programs.
* **Foreign Function Interface (FFI):** A mechanism for interacting with code written in other languages (like C).
* **Package Managers:** Tools for managing Lisp libraries and dependencies.
* **Build Tools:** Tools for building and distributing Lisp applications.

### Links to Implementations

There are quite  a a few implementations. We are only going to cover some in this document. Here is a list of links to implementation websites where you can read their documentation or download them.

#### Open Source Implementations

* [**SBCL**](https://www.sbcl.org/)
* [**CMUCL**](https://www.cons.org/cmucl/)
  * Since SBCL is a fork of CMUCL which became very popular, people tend to forget about CMUCL. Note that this implementation is actively developed, and as of today 2025/01/30 the last commit in [their repository](https://gitlab.common-lisp.net/cmucl/cmucl) was about 12 hours ago
* [**Clozure Common Lisp**](https://ccl.clozure.com/)
* [**Embeddable Common-Lisp**](https://ecl.common-lisp.dev/)
  * Very usful for writting embedded applications and interacting with C
* [**CLISP**](https://www.gnu.org/software/clisp/)
* [**ABCL**](https://abcl.org/): This implementation runs on the Java Virtual Machine (JVM)
* [**CLAS**](https://clasp-developers.github.io/)
  * Seamless Integration of Common Lisp and C++

### Commercial Implementations

* [**LispWorks**](https://www.lispworks.com/)
* [**Allegro Common Lisp**](https://franz.com/products/allegro-common-lisp/)

### Experimental / New Implementations Still In Development

* [**JSCL**](https://github.com/jscl-project/jscl)
  * A CL to JS compiler
* [**valtan**](https://github.com/cxxxr/valtan)
  * Another CL to JS compiler starting out
* [**WISP**](https://github.com/mbrock/wisp)

This tutorial aims to provide a practical overview of Common Lisp implementations and the ecosystem surrounding them. By the end, you should be able to choose an implementation suitable for your needs, manage libraries, and build Lisp projects effectively.

## 1. Popular Common Lisp Implementations

This section introduces some of the most widely used and actively developed Common Lisp implementations.

### 1.1 SBCL (Steel Bank Common Lisp)

* **Type:** Primarily a compiler, but also includes an interpreter.
* **License:** Open source (primarily a combination of public domain and a BSD-style license).
* **Platforms:** Runs on a wide range of platforms, including Linux, macOS, Windows, and various Unix-like systems, on x86, x86-64, ARM, PowerPC, and other architectures.
* **Strengths:** Known for its excellent performance, often considered one of the fastest Common Lisp implementations. It has a robust compiler that generates efficient native code. It also has a very active development community.
* **Weaknesses:** The focus on performance can sometimes make it less forgiving of non-standard code or certain edge cases.
* **Use Cases:** General-purpose Lisp development, performance-critical applications, systems programming.

SBCL is a great choice for users who prioritize performance and want a robust, open-source implementation.

### 1.2 CCL (Clozure Common Lisp)

* **Type:** Native compiler.
* **License:** Open source (LLGPL).
* **Platforms:** Excellent support for macOS, also runs on Linux, Windows, and other platforms. Supports x86, x86-64, PowerPC, and ARM architectures.
* **Strengths:** Very fast and generates high-quality native code. It has a long history and a strong focus on stability. It is known for its excellent support on macOS.
* **Weaknesses:** The development community is smaller compared to SBCL.
* **Use Cases:** macOS development, general-purpose Lisp development, applications requiring good performance.

CCL is a solid choice, particularly for macOS users.

### 1.3 ECL (Embeddable Common Lisp)

* **Type:** Bytecode compiler and interpreter.
* **License:** Open source (LGPL).
* **Platforms:** Highly portable, runs on many platforms, including embedded systems.
* **Strengths:** Designed to be easily embedded in other applications written in C or other languages. It has a small footprint and is very portable.
* **Weaknesses:** Performance is generally not as high as SBCL or CCL, as it uses a bytecode interpreter.
* **Use Cases:** Embedding Lisp functionality in other applications, scripting, small footprint systems.

ECL is an excellent choice when you need to integrate Lisp functionality into a larger system written in another language.

### 1.4 ABCL (Armed Bear Common Lisp)

* **Type:** Compiler that targets the Java Virtual Machine (JVM).
* **License:** Open source (MIT).
* **Platforms:** Runs on any platform with a JVM.
* **Strengths:** Excellent integration with Java libraries and the Java ecosystem. This allows you to leverage existing Java code and tools from Lisp.
* **Weaknesses:** Performance is dependent on the JVM and is typically not as high as native compilers like SBCL or CCL.
* **Use Cases:** Integrating Lisp with Java applications, using Java libraries from Lisp, cross-platform development where JVM portability is important.

ABCL is a good choice if you need to work closely with Java or require JVM portability.

### 1.5 LispWorks

* **Type:** Commercial implementation with both a compiler and an interpreter.
* **License:** Commercial.
* **Platforms:** Supports macOS, Windows, Linux, and other platforms.
* **Strengths:** Provides a comprehensive integrated development environment (IDE) with advanced features like a visual debugger, profiler, and GUI builder. It also offers excellent support and commercial-grade stability.
* **Weaknesses:** It is a commercial product, so it requires a license purchase.
* **Use Cases:** Large-scale commercial projects, applications requiring a full-featured IDE and commercial support.

LispWorks is a powerful choice for professional Lisp development, especially for large teams or projects requiring advanced tools and support.

**Summary Table:**

| Implementation | Type        | License      | Platforms                       | Strengths                                                                | Weaknesses                               | Use Cases                                                                   |
| -------------- | ----------- | ------------ | ------------------------------- | ------------------------------------------------------------------------ | ---------------------------------------- | --------------------------------------------------------------------------- |
| SBCL           | Compiler    | Open Source  | Many (Linux, macOS, Windows, etc.) | Excellent performance, robust compiler, active community              | Can be less forgiving of non-standard code | Performance-critical applications, general-purpose development              |
| CCL            | Compiler    | Open Source  | macOS, Linux, Windows, etc.       | Very fast, stable, excellent macOS support                               | Smaller community than SBCL               | macOS development, general-purpose development                                |
| ECL            | Bytecode/Int| Open Source  | Highly portable                 | Embeddable, small footprint                                               | Lower performance than native compilers   | Embedding Lisp, scripting                                                  |
| ABCL           | JVM-based   | Open Source  | Any platform with a JVM         | Java integration, JVM portability                                       | Performance dependent on JVM            | Java integration, cross-platform development                               |
| LispWorks      | Compiler/Int| Commercial   | macOS, Windows, Linux, etc.       | Comprehensive IDE, advanced features, commercial support, high stability | Requires a license                         | Large-scale commercial projects, applications requiring advanced tools and support |

This section provided an overview of some of the most popular Common Lisp implementations. The next sections will cover related concepts like package managers, build tools, and the Foreign Function Interface.

## 2. Choosing a Common Lisp Implementation

Selecting the right Common Lisp implementation depends on your specific needs and priorities. This section outlines key factors to consider when making your choice.

### 2.1 Performance Considerations

Performance is often a primary concern. Different implementations have different strengths and weaknesses when it comes to speed.

* **Native Compilers (SBCL, CCL):** Generally offer the best performance, as they compile Lisp code directly to native machine code. SBCL is often considered the fastest, but CCL is also very performant, especially on macOS. If raw speed is critical, these are usually the best choices.
* **Bytecode Compilers/Interpreters (ECL):** Trade some performance for portability and embeddability. ECL's performance is generally lower than that of native compilers, but it's often sufficient for many applications, especially when embedding is a key requirement.
* **JVM-based Implementations (ABCL):** Performance depends on the underlying Java Virtual Machine. While JVMs have improved significantly in performance, they generally don't match the raw speed of native compilers for computationally intensive Lisp code. However, ABCL provides seamless integration with Java libraries, which can be a significant advantage.

When evaluating performance, consider the type of application you're building. For CPU-bound tasks, native compilers are usually preferable. For applications that rely heavily on I/O or interaction with external systems, the performance differences might be less significant.

### 2.2 Platform Compatibility

Ensure the implementation you choose supports the platforms you need to target.

* **Cross-Platform Implementations (SBCL, CCL, ECL, ABCL):** These implementations support a variety of operating systems and architectures. SBCL, ECL and ABCL are good choices for cross-platform development. CCL has excellent support for macOS as well as other platforms.
* **Operating System Specific Implementations:** Some implementations might have stronger support for specific operating systems.

Consider both your development environment and the target deployment environment when evaluating platform compatibility.

### 2.3 Available Libraries and Tools

The availability of libraries and tools can significantly impact your development process.

* **Quicklisp:** A package manager that provides access to a large collection of open-source Lisp libraries. Most implementations support Quicklisp, making library management relatively consistent across implementations. However, some libraries may have better support or performance on specific implementations.
* **FFI (Foreign Function Interface):** The ability to interact with code written in other languages (like C) is crucial for many applications. Most major implementations provide a Foreign Function Interface (FFI). CFFI is a common and portable FFI library that works across multiple implementations.
* **IDEs and Development Tools:** SLIME (Superior Lisp Interaction Mode for Emacs) is a popular development environment for Common Lisp. It works well with most implementations. LispWorks provides its own comprehensive IDE.

Consider the libraries and tools you'll need for your project and ensure they are compatible with the implementation you choose.

### 2.4 Licensing and Support

Licensing and support are important factors, especially for commercial projects.

* **Open-Source Implementations (SBCL, CCL, ECL, ABCL):** These are free to use and distribute, making them attractive for open-source projects and hobbyists.
* **Commercial Implementations (LispWorks):** Offer commercial support, which can be valuable for large organizations or projects requiring guaranteed assistance.

Consider your project's licensing requirements and the level of support you might need when making your decision.

**Summary of Choosing Factors:**

| Factor            | Considerations                                                                                                        |
| ----------------- | --------------------------------------------------------------------------------------------------------------------- |
| Performance       | Native compilers (SBCL, CCL) are generally fastest. Consider the type of application (CPU-bound vs. I/O-bound).        |
| Platform          | Ensure compatibility with your development and deployment environments.                                               |
| Libraries/Tools   | Check for compatibility with Quicklisp and necessary libraries. Consider FFI and IDE support.                           |
| Licensing/Support | Open-source implementations are free. Commercial implementations offer support.                                        |

By considering these factors, you can make an informed decision about which Common Lisp implementation is best suited for your project. Often, SBCL is a good default choice due to its performance and active community, but other implementations might be more appropriate depending on your specific needs.

## 3. Package Managers in Common Lisp: Quicklisp

Managing external libraries and dependencies is crucial for any software project. In Common Lisp, Quicklisp is the de facto standard package manager, simplifying the process of finding, installing, and using libraries.

### 3.1 Quicklisp: A Widely Used Package Manager

Quicklisp is a package manager that provides access to a large collection of open-source Common Lisp libraries. It makes it easy to:

* **Install libraries:** Download and install libraries with a single command.
* **Manage dependencies:** Automatically handle library dependencies.
* **Update libraries:** Keep your installed libraries up to date.
* **Create local projects:** Manage dependencies for individual projects.

Quicklisp works by maintaining a central repository of library metadata (information about libraries, their dependencies, and where to download them). When you install a library, Quicklisp downloads the library's source code and any necessary dependencies.

### 3.2 Using Quicklisp to Install and Manage Libraries

Here's how to get started with Quicklisp and use it to manage libraries:

**1. Installation:**

The easiest way to install Quicklisp is to load it directly from its website. Start your Lisp REPL and evaluate the following form:

```lisp
(ql:quickload "quicklisp")
```

This will download and install Quicklisp. After the installation is complete, you'll need to load it again, and set it up locally:

```lisp
(load (merge-pathnames "setup.lisp" (user-homedir-pathname)))
```

This creates a local Quicklisp installation in your home directory (usually `~/.quicklisp/`).

**2. Loading Quicklisp in your Lisp environment:**

After the initial installation, you can load Quicklisp in your Lisp environment by evaluating:

```lisp
(ql:quickload "quicklisp")
```

You can add this form to your Lisp initialization file (e.g., `~/.sbclrc` for SBCL) so that Quicklisp is loaded automatically when you start your Lisp environment.

**3. Installing Libraries:**

To install a library using Quicklisp, use the `ql:quickload` function:

```lisp
(ql:quickload "drakma") ; Installs the Drakma HTTP client library
```

Quicklisp will download and install the specified library and any dependencies it requires. Once a library is quickloaded it is available in the current lisp session.

**4. Using Installed Libraries:**

Once a library is installed, you can use it in your code using `require` or `ql:quickload`. `require` is the traditional way to load a library, however it only works if the library is already loaded in the lisp environment. `ql:quickload` will load the library if it is not already loaded, therefore it is more convenient.

```lisp
(ql:quickload "drakma")
(drakma:http-request "http://www.google.com")
```

**5. Updating Quicklisp and Libraries:**

To update Quicklisp itself, use the `ql:update-dist` function:

```lisp
(ql:update-dist)
```

To update all installed libraries, use the `ql:update-all-dists` function:

```lisp
(ql:update-all-dists)
```

**6. Local Projects:**

Quicklisp also supports managing dependencies for individual projects. You can create a local projects directory and use a local-projects file to manage dependencies for each project independently.

First, create a `local-projects` directory inside your `~/.quicklisp` directory if it doesn't exist:

```bash
mkdir -p ~/.quicklisp/local-projects
```

Then, create a file named `<your-project-name>.asd` inside the `local-projects` directory with the following content:

```lisp
(asdf:defsystem :my-project
  :version "1.0.0"
  :description "My awesome project"
  :author "Your Name"
  :license "MIT"
  :depends-on ("drakma" "other-library"))
```

Then, in your Lisp environment, load your project:

```lisp
(ql:quickload :my-project)
```

This will load your project and all its dependencies.

Quicklisp is an essential tool for any Common Lisp developer. It simplifies library management and makes it easy to use external code in your projects. This section provided a basic introduction to Quicklisp. For more advanced features and options, refer to the Quicklisp documentation. The next section will cover ASDF, the standard build tool for Common Lisp.

### 3.3 Qlot

Qlot is basically a way to manage project specific dependencies and it can fetch those dependencies from a varierty of sources including:

* multiple quicklisp distributions
* git repositories
* http(s) files
* and others

It's extensible to be able to add different types of sources.

The tutorial in the repository is the best source of information. I personally use qlot for managing the code in my small company and it has been very useful.

[https://github.com/fukamachi/qlot](https://github.com/fukamachi/qlot)

### 3.4 OCICL

OCICL is a new alternative to quicklisp, and like qlot, it sets up project specific dependency management. Here's [a link](https://github.com/ocicl/ocicl) to the repository with its tutorial.

## 4. Build Tools: ASDF

ASDF (Another System Definition Facility) is the standard build system for Common Lisp. It provides a way to define software components (systems) and their dependencies, making it easier to build, load, and manage Lisp projects.

### 4.1 ASDF (Another System Definition Facility): A Standard Build System

ASDF provides a declarative way to describe software components (systems) and their dependencies. A *system* is a collection of files (Lisp source code, data files, etc.) that make up a logical unit of software. ASDF handles the details of compiling, loading, and linking these files in the correct order.

Key features of ASDF:

* **System definitions:** Describe the components of a software project and their dependencies.
* **Dependency management:** Automatically loads required libraries and systems.
* **Build operations:** Provides operations like compiling, loading, and testing.
* **Portability:** Works across different Common Lisp implementations.

### 4.2 Defining Systems and Dependencies with ASDF

System definitions are written in Lisp and typically stored in files with the `.asd` extension. A basic system definition looks like this:

```lisp
(asdf:defsystem :my-project
  :description "My awesome project"
  :author "Your Name"
  :license "MIT"
  :version "1.0.0"
  :depends-on ("cl-ppcre") ; Dependencies
  :components ((:file "my-project") ; Source file
               (:file "utils")))     ; Another source file
```

Let's break down the components of this definition:

* **`asdf:defsystem`**: Defines a new system.
* **:my-project**: The name of the system (a keyword).
* **:description**, **:author**, **:license**, **:version**: Metadata about the system.
* **:depends-on**: A list of other systems that this system depends on. ASDF will ensure that these dependencies are loaded before loading the current system.
* **:components**: A list of components that make up the system. In this case, we have two source files: `my-project.lisp` and `utils.lisp`.

**Component Types:**

ASDF supports various component types:

* **`:file`**: Represents a Lisp source file.
* **`:module`**: Represents a subdirectory containing other components.
* **`:static-file`**: Represents a file that should be copied as-is (e.g., data files).

**Example with Modules:**

```lisp
(asdf:defsystem :my-project
  :description "My awesome project"
  :author "Your Name"
  :license "MIT"
  :version "1.0.0"
  :depends-on ("cl-ppcre")
  :components ((:module "src" ; A module (subdirectory)
                 :components ((:file "src/my-project")
                              (:file "src/utils")))
               (:file "tests/my-project-tests"))) ; Test file
```

This example defines a module `src` containing the source files and a separate test file. The files would be located in the directory structure:

```bash
my-project/
├── my-project.asd
└── src/
    ├── my-project.lisp
    └── utils.lisp
└── tests/
    └── my-project-tests.lisp
```

### 4.3 Building and Loading Lisp Projects

Once you have defined a system, you can use ASDF to build and load it.

**1. Loading ASDF:**

If it is not already loaded by your lisp environment, you can load ASDF using quicklisp:

```lisp
(ql:quickload "asdf")
```

**2. Loading a System:**

To load a system, use the `asdf:load-system` function:

```lisp
(asdf:load-system :my-project)
```

ASDF will automatically compile the source files (if necessary) and load them in the correct order, resolving any dependencies.

**3. Compiling a System:**

You can explicitly compile a system using the `asdf:compile-system` function:

```lisp
(asdf:compile-system :my-project)
```

This will compile the source files without loading them. This is useful for creating compiled fasl files that can be loaded more quickly later.

**4. Other ASDF Operations:**

ASDF provides other useful operations, such as:

* `asdf:test-system`: Runs the tests associated with a system.
* `asdf:upgrade-system`: Upgrades a system to a newer version.

**Example Usage:**

Let's say you have the following `my-project.asd` file:

```lisp
(asdf:defsystem :my-project
  :version "1.0.0"
  :description "A simple project"
  :author "Your Name"
  :license "MIT"
  :components ((:file "my-project")))
```

And a corresponding `my-project.lisp` file:

```lisp
(defpackage :my-project
  (:use :cl)
  (:export :hello))

(in-package :my-project)

(defun hello ()
  (format t "Hello, world!~%"))
```

You can load and use the project like this:

```lisp
(asdf:load-system :my-project)
(my-project:hello) ; Prints "Hello, world!"
```

ASDF is an essential tool for managing Lisp projects. It provides a standardized and portable way to define systems, manage dependencies, and build your code. This section provided a basic introduction to ASDF. For more advanced features and options, refer to the ASDF documentation. The next section will cover the Foreign Function Interface (FFI).

## 5. Foreign Function Interface (FFI) in Common Lisp

The Foreign Function Interface (FFI) allows Common Lisp programs to interact with code written in other programming languages, most commonly C. This is crucial for accessing system libraries, using existing C codebases, or improving performance by offloading computationally intensive tasks to C. CFFI (C Foreign Function Interface) is a widely used and portable library that simplifies FFI interactions in Common Lisp.

### 5.1 Interacting with C code using CFFI

CFFI provides a high-level interface for defining foreign functions and data structures, making it easier to call C code from Lisp and vice versa. It handles the details of data type conversions and calling conventions, allowing you to focus on the logic of your interaction with the foreign code.

To use CFFI, you first need to load it using Quicklisp:

```lisp
(ql:quickload "cffi")
```

### 5.2 Calling C Functions from Lisp

To call a C function from Lisp, you need to declare it using `cffi:defcfun`. The syntax is:

```lisp
(cffi:defcfun c-function-name return-type
  (lisp-argument-name1 c-argument-type1)
  (lisp-argument-name2 c-argument-type2)
  ...)
```

* **`c-function-name`**: The name of the C function.
* **`return-type`**: The C type of the return value.
* **`lisp-argument-name`**: The name you'll use in Lisp to refer to the argument.
* **`c-argument-type`**: The C type of the argument.

**Example:**

Let's say you have a simple C function in `my_c_library.c`:

```c
#include <stdio.h>

int add(int a, int b) {
  return a + b;
}

void print_message(const char *message) {
    printf("%s\n", message);
}
```

And you compile it into a shared library `libmy_c_library.so` (on Linux/macOS) or `my_c_library.dll` (on Windows). You can then define the corresponding Lisp functions:

```lisp
(cffi:load-foreign-library "libmy_c_library") ; Load the shared library

(cffi:defcfun add (:int)
  (a :int)
  (b :int))

(cffi:defcfun print-message :void
    (message :string))

(add 10 20) ; Returns 30
(print-message "Hello from Lisp!") ; Prints "Hello from Lisp!" to standard output
```

CFFI provides a wide range of C types that you can use in `defcfun`, including `:int`, `:unsigned-int`, `:float`, `:double`, `:pointer`, `:string`, and many others.

### 5.3 Using Lisp Data Structures in C

CFFI also allows you to pass Lisp data structures to C functions and vice-versa. This is typically done using pointers and foreign memory.

**Example: Passing an array to C:**

Let's modify the C library to accept an array of integers:

```c
#include <stdio.h>

int sum_array(int *arr, int len) {
  int sum = 0;
  for (int i = 0; i < len; i++) {
    sum += arr[i];
  }
  return sum;
}
```

And the corresponding Lisp code:

```lisp
(cffi:load-foreign-library "libmy_c_library")

(cffi:defcfun sum-array :int
  (arr :pointer)
  (len :int))

(let ((lisp-array (vector 1 2 3 4 5)))
  (cffi:with-foreign-object (c-array :int (length lisp-array)) ; Allocate foreign memory
    (loop for i from 0 below (length lisp-array)
          do (setf (cffi:mem-aref c-array :int i) (aref lisp-array i))) ; Copy Lisp array to C array
    (sum-array c-array (length lisp-array)))) ; Call the C function
; Returns 15
```

Here's what's happening:

1. `cffi:with-foreign-object` allocates a block of foreign memory of the appropriate size to hold the C array.
2. `cffi:mem-aref` is used to access and set elements in the foreign memory as if it were a C array.
3. The pointer to the foreign memory is passed to the C function `sum_array`.

**Example: Returning a string from C:**

```c
#include <string.h>
#include <stdlib.h>

char* create_message(const char *name) {
    char *message = (char*) malloc(100 * sizeof(char)); // Allocate memory
    strcpy(message, "Hello, ");
    strcat(message, name);
    return message;
}
```

```lisp
(cffi:load-foreign-library "libmy_c_library")

(cffi:defcfun create-message :string
    (name :string))

(create-message "Lisp") ; Returns "Hello, Lisp"
```

CFFI handles the conversion of the C string to a Lisp string and also frees the memory allocated in C.

CFFI provides many more features for interacting with C, including defining structures, unions, callbacks, and more. This section provided a basic introduction to CFFI. For more advanced usage, consult the CFFI documentation. The next section will briefly cover some popular Lisp development environments.

## 6. Development Environments for Common Lisp

A good development environment can significantly improve your productivity when working with any programming language. This section introduces SLIME, the most popular development environment for Common Lisp, and briefly mentions other available options.

### 6.1 SLIME (Superior Lisp Interaction Mode for Emacs) (and Sly)

SLIME (Superior Lisp Interaction Mode for Emacs) is an Emacs mode that provides a powerful and interactive development environment for Common Lisp. It's the most widely used and highly regarded Lisp development environment.

Key features of SLIME:

* **REPL integration:** Provides a fully integrated REPL within Emacs, allowing you to evaluate Lisp code interactively.
* **Code completion:** Offers intelligent code completion for symbols, functions, and other Lisp constructs.
* **Debugging:** Provides powerful debugging features, including stepping through code, inspecting variables, and setting breakpoints.
* **Cross-referencing:** Allows you to easily navigate between function definitions, variable usages, and other code elements.
* **Documentation lookup:** Provides quick access to Lisp documentation.
* **Integration with Quicklisp and ASDF:** Seamlessly integrates with Quicklisp for library management and ASDF for project management.

**Setting up SLIME:**

1. **Install Emacs:** If you don't already have it, install Emacs.
2. **Install Quicklisp:** Follow the instructions in the previous section to install Quicklisp.
3. **Install SLIME via Quicklisp:** Within your Lisp REPL, evaluate:

    ```lisp
    (ql:quickload "slime")
    ```

4. **Configure Emacs:** Add the following lines to your Emacs initialization file (e.g., `~/.emacs` or `~/.emacs.d/init.el`):

    ```elisp
    (add-to-list 'load-path "~/.quicklisp/dists/quicklisp/software/slime-20231022/") ; Replace with correct path
    (require 'slime)
    (slime-setup '(sbcl)) ; Replace sbcl with your Lisp implementation of choice
    ```

    Make sure to replace the path with the actual location of your SLIME installation. You can find the correct path by evaluating `(ql:qmerge "slime/slime.el")` in your Lisp REPL. The path shown might be slightly different depending on the version of SLIME you installed.

5. **Start SLIME:** After restarting Emacs, you can start SLIME by running the command `M-x slime`. This will connect Emacs to your Lisp implementation.

**Basic SLIME Usage:**

* **`C-x C-e` (eval-last-expression):** Evaluates the Lisp expression before the cursor.
* **`C-c C-c` (compile-defun):** Compiles the top-level form (e.g., function, class) where the cursor is.
* **`M-.` (find-definition):** Jumps to the definition of the symbol at the cursor.
* **`C-c d d` (describe-symbol):** Displays information about the symbol at the cursor.
* **`C-c C-z` (switch-to-slime-repl):** Switches to the SLIME REPL buffer.
* **`C-c C-d` (slime-documentation-lookup):** Lookups the documentation for the symbol at the cursor.

SLIME provides many other powerful features that can greatly enhance your Lisp development workflow. Learning the basic commands and exploring the available features is highly recommended.

### 6.2 Other Lisp IDEs

While SLIME is the most popular choice, other Lisp IDEs and development tools are available:

* **LispWorks IDE:** As mentioned earlier, LispWorks provides its own comprehensive IDE with advanced features. It's a commercial product but offers a very polished and professional development experience.
* **Portacle:** A portable Common Lisp development environment that includes SBCL, SLIME, and other tools in a single package. It's a good option for beginners as it simplifies the setup process.
* **Lem:** A Common Lisp editor inspired by Emacs. While not strictly an IDE like SLIME, it offers Lisp-specific features and can be a good alternative for those who prefer a more modern editor experience. Lem is making quick strides with an incredibly active communty to a point where it seems the aim is to replace emacs altogether. They even have their own version of magit called legit, and they keep coming up with new improvements all the time. With that said, the documentation is not as extensive, and it assumes you know how to use emacs, since most of the key bindinds are similar. I would suggest beginners to use emacs instead.
* **Visual Studio Code:** has a slynk connector to a slime server. I personally started using this because of the high barrier to entry to lisp. However, once I finally made the jump to emacs, the improvement in the workflow was very big. While programming in CL in visual studio is still better than say python or any other language because of the REPL, it is still really behind using emacs. The emacs integration to the Common Lisp REPL programming workflow is unmatched.
* **Sublime Text:** also has a REPL package, which can sometimes be better than Visual Studio since the REPL is loaded as a new tab and it makes it much more convenient than Visual Studio (this is my personal experience).
* **Atom:** even though the editor itself is discontinued, the CL REPL integration is still pretty good and accessible for beginners.
* **climacs:** this is hopefully going to be the future once it's fully implemented. It integrates with CL in a way no other editor, including emacs, has. However, the actual editor functionality is not ready.

My personal recommendation if you are starting with Common Lisp and don't want to waste time learning emacs is to use either VSCode, Sublime Text, or Atom. This is **not** the standard recommendation. Once you decide to invest in Common Lisp and actually code any medium to large project, my recommendation is to jump ship immediately and invest in learning emacs. Most people will recommend just using emacs right away, which I agree is the right approach once you have commited to Common Lisp, however, I understand the frustration involved in the slowdown of having to learn a very different IDE than all the *modern* ones. I will say though that once you learn emacs, this by itself will be worth it and it may even become your standard editor for a lot of things. Emacs is very powerful and customizable, it's just that the steep learning curve makes it hard to start. I recommend to start using org mode right away to manage your todo lists even if you don't use it to program, that way you slowly get used to using emacs, and after a while of using even very basic org mode functionality, you will realize it's a better editor than basically anything else out there including Visual Studio Code, Sublime Text, Zed, and the rest, so that will then make it obvious to switch to programming with emacs, and will make the transition smoother. It's much easier to just have to learn how to use the Slime/Sly IDE features instead of having to learn text editing features and IDE features at the same time.
