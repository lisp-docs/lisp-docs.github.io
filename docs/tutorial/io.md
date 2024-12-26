---
sidebar_position: 7
---

# Input and Output

## Introduction to Input and Output in Common Lisp

Input and Output (I/O) are essential aspects of any programming language, allowing programs to interact with the external world, such as reading data from files, displaying information to the user, and communicating with other programs. Common Lisp provides a powerful and flexible I/O system that handles various types of input and output, from simple text streams to binary data and complex data structures.

This tutorial will introduce you to the fundamental concepts of I/O in Common Lisp, covering standard input and output streams, file I/O, and techniques for reading and writing Lisp data structures to and from files.

**Key Concepts:**

* **Streams:** Represent sources of input or destinations of output.
* **Standard Streams:** Predefined streams for standard input, standard output, and standard error.
* **File I/O:** Reading and writing data to and from files.
* **Formatted Output:** Using `format` for controlled output.
* **Reading Lisp Objects:** Using `read` to parse Lisp expressions from input.
* **Writing Lisp Objects:** Using `print`, `prin1`, `princ`, and `write` to output Lisp objects.

**Table of Contents:**

**1. Standard Input and Output:**

* `*standard-input*`: The standard input stream (usually the keyboard).
* `*standard-output*`: The standard output stream (usually the console).
* `*error-output*`: The standard error stream (for error messages).
* `read`: Reading Lisp objects from input.
* `print`, `prin1`, `princ`: Writing Lisp objects to output.
* `format`: Formatted output.

**2. File I/O:**

* `open`: Opening files for reading or writing.
  * File modes: `:input`, `:output`, `:io`, `:probe`.
  * Element types: `:element-type`.
* `close`: Closing files.
* `with-open-file`: A convenient macro for handling file I/O.
* `read-line`: Reading a line from a file.
* `write-line`: Writing a line to a file.
* `read-char`, `write-char`: Reading and writing individual characters.
* `read-byte`, `write-byte`: Reading and writing individual bytes (for binary files).

**3. Reading and Writing Lisp Data Structures:**

* `read`: Reading arbitrary Lisp objects from a stream.
* `print`, `prin1`, `princ`, `write`: Writing Lisp objects to a stream.
* `*print-pretty*`: Controlling pretty printing.
* `*print-circle*`: Handling circular structures.
* `*print-readably*`: Ensuring that output can be read back in.
* Using `with-standard-io-syntax` to control readtables.

**4. Binary I/O:**

* Using `:element-type '(unsigned-byte 8)` (or similar) with `open`.
* `read-byte`, `write-byte`: Reading and writing bytes.

**5. Pathnames:**

* Representing file and directory paths.
* Pathname functions: `pathname`, `pathnamep`, `namestring`, `merge-pathnames`.

This tutorial will provide practical examples for each topic, helping you understand how to use Common Lisp's I/O functions effectively. You'll learn how to handle different types of data, format output, and read and write complex Lisp data structures to persistent storage.

## 1. Standard Input and Output in Common Lisp

Standard input and output provide the most basic way for a Lisp program to interact with the user or other programs. Common Lisp defines special variables that represent these standard streams.

### 1.1 Standard Streams: `*standard-input*`, `*standard-output*`, `*error-output*`

* **`*standard-input*`**: This variable holds the standard input stream, which is usually connected to the keyboard. You read input from this stream using functions like `read`.
* **`*standard-output*`**: This variable holds the standard output stream, which is usually connected to the console or terminal. You write output to this stream using functions like `print`, `prin1`, `princ`, and `format`.
* **`*error-output*`**: This variable holds the standard error stream, which is typically also connected to the console but is used specifically for error messages. Using this separate stream allows error messages to be distinguished from regular output.

These variables are dynamically scoped, meaning their values can be temporarily rebound within specific code blocks. This is useful for redirecting input or output, which we will cover in the File I/O section.

### 1.2 `read`: Reading Lisp Objects from Input

The `read` function reads a Lisp object from an input stream (by default, `*standard-input*`). It parses the input according to Lisp's syntax and returns the corresponding Lisp object.

```lisp
(print "Enter a number:")
(let ((num (read)))
  (format t "You entered: ~a~%" num))

(print "Enter a list:")
(let ((my-list (read)))
  (format t "You entered the list: ~a~%" my-list))

(print "Enter a string:")
(let ((my-string (read)))
  (format t "You entered the string: ~a~%" my-string))
```

If you type `123` at the first prompt, `read` will return the integer `123`. If you type `(a b c)` at the second prompt, `read` will return the list `(A B C)`. If you type `"hello"` at the third prompt, `read` will return the string `"hello"`.

`read` will signal an error if the input is not a valid Lisp expression.

### 1.3 `print`, `prin1`, `princ`: Writing Lisp Objects to Output

These functions write Lisp objects to an output stream (by default, `*standard-output*`).

* **`print`**: Writes the object followed by a newline. The output is readable by `read`.

    ```lisp
    (print "Hello, world!") ; Prints "Hello, world!" followed by a newline.
    (print '(1 2 3)) ; Prints (1 2 3) followed by a newline.
    ```

* **`prin1`**: Writes the object without a trailing newline. The output is readable by `read`.

    ```lisp
    (prin1 "Hello") (prin1 " world!") ; Prints "Hello" " world!" (no newlines).
    ```

* **`princ`**: Writes the object without a trailing newline and in a more human-readable format (e.g., strings are printed without quotes). The output is *not* guaranteed to be readable by `read`.

    ```lisp
    (princ "Hello") (princ " world!") ; Prints Hello world!
    ```

Here's a comparison:

```lisp
(print "foo")   ; Prints "foo" and a newline.
(prin1 "foo")  ; Prints "foo"
(princ "foo")   ; Prints foo
```

### 1.4 `format`: Formatted Output

The `format` function provides powerful control over output formatting. It takes a destination, a format string, and any arguments to be formatted.

The destination can be:

* `t`: The standard output stream (`*standard-output*`).
* `nil`: Returns the formatted output as a string.
* A stream object (for file I/O, which we will cover later).

The format string contains format directives that control how the arguments are formatted. Here are some common directives:

* `~a`: Prints the argument using `princ`.
* `~s`: Prints the argument using `prin1`.
* `~d`: Prints the argument as a decimal integer.
* `~f`: Prints the argument as a floating-point number.
* `~%`: Prints a newline.
* `~~`: Prints a tilde character.

**Examples:**

```lisp
(format t "Hello, ~a!~%" "world") ; Prints "Hello, world!" and a newline.
(format t "The number is ~d.~%" 42) ; Prints "The number is 42." and a newline.
(format t "Pi is approximately ~f.~%" 3.14159) ; Prints "Pi is approximately 3.14159."
(format t "Pi is approximately ~,2f.~%" 3.14159) ; Prints "Pi is approximately 3.14." (2 decimal places)
(format nil "This will be returned as a string") ; returns "This will be returned as a string"
```

`format` is extremely versatile and can handle complex formatting scenarios. Consult the Common Lisp HyperSpec for a complete list of format directives.

This section covered the basics of standard input and output in Common Lisp. Understanding these functions is essential for interacting with the user and displaying information from your programs. The next section will cover file I/O.

## 2. File I/O in Common Lisp

File I/O allows your Lisp programs to interact with files on the file system, reading data from them and writing data to them.

### 2.1 `open`: Opening Files

The `open` function opens a file and returns a *stream object* that represents the connection to the file. It takes several keyword arguments:

* **`pathname`**: The path to the file (a string or a pathname object).
* **`:direction`**: The direction of the stream:
  * `:input`: Open for reading (the default).
  * `:output`: Open for writing (creates the file if it doesn't exist, overwrites if it does).
  * `:io`: Open for both reading and writing.
  * `:probe`: Checks if a file exists.
* **`:if-exists`**: What to do if the file already exists (only relevant for `:direction :output`):
  * `:supersede`: Overwrite the file (the default).
  * `:append`: Append to the end of the file.
  * `:new-version`: Create a new version of the file (implementation-dependent).
  * `:rename`: Rename the existing file.
  * `:error`: Signal an error.
  * `nil`: Return `nil`
* **`:if-does-not-exist`**: What to do if the file does not exist (only relevant for `:direction :input`):
  * `:error`: Signal an error (the default).
  * `nil`: Return `nil`.
  * `:create`: Create the file (only if `:direction` is `:io` or `:output`).
* **`:element-type`**: Specifies the type of elements to be read or written (e.g., `'character`, `'(unsigned-byte 8)` for binary files). The default is `character`.

**Examples:**

* Opening a file for reading:

    ```lisp
    (open "my-file.txt" :direction :input)
    ```

* Opening a file for writing, overwriting if it exists:

    ```lisp
    (open "output.txt" :direction :output)
    ```

* Opening a file for appending:

    ```lisp
    (open "log.txt" :direction :output :if-exists :append)
    ```

* Opening a binary file for writing:

    ```lisp
    (open "data.bin" :direction :output :element-type '(unsigned-byte 8))
    ```

### 2.2 `close`: Closing Files

It's crucial to close files when you're finished with them to release system resources. The `close` function closes a stream.

```lisp
(let ((stream (open "my-file.txt" :direction :input)))
  ; ... read from the file ...
  (close stream))
```

### 2.3 `with-open-file`: A Convenient Macro

The `with-open-file` macro simplifies file I/O by automatically opening and closing the file. Its syntax is:

```lisp
(with-open-file (stream pathname options)
  body)
```

* **`stream`**: A variable that will be bound to the stream object.
* **`pathname`**: The path to the file.
* **`options`**: Keyword arguments to `open`.
* **`body`**: The code that will operate on the file.

**Example:**

```lisp
(with-open-file (input "my-file.txt" :direction :input)
  (when input ; Check if the file was opened successfully
    (loop for line = (read-line input nil)
          while line
          do (print line))))
```

### 2.4 `read-line`: Reading a Line

The `read-line` function reads a line of text from a stream, returning the line as a string. It takes the stream as an argument. The second argument is an *eof-value* that is returned if end of file is reached.

```lisp
(with-open-file (input "my-file.txt" :direction :input)
  (loop for line = (read-line input nil)
        while line
        do (print line)))
```

### 2.5 `write-line`: Writing a Line

The `write-line` function writes a string to a stream followed by a newline.

```lisp
(with-open-file (output "output.txt" :direction :output :if-exists :supersede)
  (write-line "This is the first line." output)
  (write-line "This is the second line." output))
```

### 2.6 `read-char` and `write-char`: Reading and Writing Characters

`read-char` reads a single character from a stream. `write-char` writes a single character to a stream.

```lisp
(with-open-file (input "chars.txt" :direction :input)
  (loop for char = (read-char input nil)
        while char
        do (format t "Character: ~c Code: ~d~%" char (char-code char))))

(with-open-file (output "out-chars.txt" :direction :output :if-exists :supersede)
    (write-char #\A output)
    (write-char #\Newline output)
    (write-char #\B output))
```

### 2.7 `read-byte` and `write-byte`: Reading and Writing Bytes (Binary Files)

For binary files, you use `read-byte` and `write-byte`. You must open the file with an appropriate `:element-type`, such as `'(unsigned-byte 8)`.

```lisp
(with-open-file (output "data.bin" :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
    (write-byte 255 output)
    (write-byte 0 output)
    (write-byte 128 output))

(with-open-file (input "data.bin" :direction :input :element-type '(unsigned-byte 8))
  (loop for byte = (read-byte input nil)
        while byte
        do (format t "Byte: ~d~%" byte)))
```

This section covered the basics of file I/O in Common Lisp. You now know how to open, close, read from, and write to files, handling both text and binary data. The use of `with-open-file` is highly recommended for its conciseness and automatic resource management.

## 3. Reading and Writing Lisp Data Structures

This section covers how to read and write arbitrary Lisp data structures to and from streams (including files). This is crucial for saving and loading data, as well as for communicating with other Lisp programs.

### 3.1 `read`: Reading Arbitrary Lisp Objects

The `read` function is the primary way to parse Lisp expressions from an input stream. It can read any valid Lisp object, including numbers, symbols, strings, lists, vectors, and more.

```lisp
(with-input-from-string (input "(1 2 \"hello\" symbol)") ; Read from a string
  (print (read input)) ; Prints 1
  (print (read input)) ; Prints 2
  (print (read input)) ; Prints "hello"
  (print (read input))) ; Prints SYMBOL

(with-input-from-string (input "#(1 2 3)")
    (print (read input))) ; prints #(1 2 3)
```

As we've seen before, when used without an explicit stream argument, `read` reads from `*standard-input*`.

### 3.2 `print`, `prin1`, `princ`, `write`: Writing Lisp Objects

These functions write Lisp objects to an output stream.

* **`print`**: Writes the object followed by a newline. The output is *readable* by `read`. It prints the object using `prin1` and then prints a newline.

    ```lisp
    (print "This is a string.") ; Prints "This is a string." followed by a newline.
    (print '(a b c))          ; Prints (A B C) followed by a newline.
    ```

* **`prin1`**: Writes the object without a trailing newline. The output is *readable* by `read`.

    ```lisp
    (prin1 "This is a string.") ; Prints "This is a string."
    (prin1 '(a b c))          ; Prints (A B C)
    ```

* **`princ`**: Writes the object without a trailing newline and in a more human-readable format (e.g., strings are printed without quotes). The output is *not* guaranteed to be readable by `read`.

    ```lisp
    (princ "This is a string.") ; Prints This is a string.
    (princ '(a b c))          ; Prints (A B C)
    ```

* **`write`**: This is the most general printing function. It provides the most control over the output format through various keyword arguments. By default, it produces output that is readable by `read`, similar to `prin1`.

    ```lisp
    (write "This is a string.") ; Prints "This is a string."
    (write '(a b c))          ; Prints (A B C)
    ```

    `write` accepts many keyword arguments for controlling printing. Some of the most important are covered below using the special variables that control them.

### 3.3 `*print-pretty*`: Controlling Pretty Printing

The `*print-pretty*` variable controls whether lists and other complex data structures are printed in a nicely formatted, indented way.

```lisp
(let ((*print-pretty* t)) ; Enable pretty printing
  (print '(very very very long list (with many (nested sublists)) and some other stuff)))
; Output will be nicely indented.

(let ((*print-pretty* nil)) ; Disable pretty printing
  (print '(very very very long list (with many (nested sublists)) and some other stuff)))
; Output will be on a single line.
```

### 3.4 `*print-circle*`: Handling Circular Structures

If you have a circular data structure (where a part of the structure refers back to itself), `print` and `prin1` would normally enter an infinite loop. The `*print-circle*` variable prevents this by using `#n=` and `#n#` to represent circular references.

```lisp
(let ((x (list 1 2)))
  (setf (cdr x) x) ; Create a circular list
  (let ((*print-circle* t))
    (print x))) ; Prints #(1 . #1#)
```

### 3.5 `*print-readably*`: Ensuring Readability

The `*print-readably*` variable is crucial for ensuring that the output of `write` (and therefore `print` and `prin1`) can be read back in using `read`. When `*print-readably*` is `t`, `write` will escape special characters and use appropriate syntax to make the output unambiguous.

```lisp
(let ((*print-readably* t))
  (print "This string has \"quotes\".") ; Prints "This string has \"quotes\"."
  (print '|This symbol has spaces.|)) ; Prints |This symbol has spaces.|

(let ((*print-readably* nil))
  (print "This string has \"quotes\".") ; Prints This string has "quotes".
  (print '|This symbol has spaces.|)) ; Prints This symbol has spaces.
```

It is highly recommended to set `*print-readably*` to `t` when writing data to files that you intend to read back in using `read`.

### 3.6 Using `with-standard-io-syntax` to Control Readtables

The *readtable* controls how `read` parses input. The `with-standard-io-syntax` macro establishes a standard readtable, ensuring consistent behavior. It's good practice to use this macro when reading or writing data that needs to be portable.

```lisp
(with-standard-io-syntax
  (with-open-file (output "data.lisp" :direction :output :if-exists :supersede)
    (print '(a b c) output)))

(with-standard-io-syntax
    (with-open-file (input "data.lisp" :direction :input)
        (print (read input)))) ; prints (A B C)
```

This section covered how to read and write Lisp data structures using `read`, `print`, `prin1`, `princ`, and `write`, and how to control the output format using special variables like `*print-pretty*`, `*print-circle*`, and `*print-readably*`. Using `with-standard-io-syntax` is recommended for portability. These techniques are essential for saving and loading data and for exchanging data between Lisp programs.

## 4. Binary I/O in Common Lisp

Binary I/O deals with reading and writing raw bytes to and from files, as opposed to character-based I/O. This is essential for working with non-textual data like images, audio, or compiled code.

### 4.1 Using `:element-type '(unsigned-byte 8)` with `open`

To perform binary I/O, you must open the file in binary mode by specifying the `:element-type` keyword argument to `open`. The most common element type for binary files is `'(unsigned-byte 8)`, which represents unsigned 8-bit integers (bytes).

```lisp
(open "data.bin" :direction :output :element-type '(unsigned-byte 8)) ; Open for writing binary data
(open "data.bin" :direction :input :element-type '(unsigned-byte 8))  ; Open for reading binary data
```

### 4.2 `read-byte` and `write-byte`: Reading and Writing Bytes

* **`read-byte`**: Reads a single byte from a binary stream. It takes the stream as an argument. If the end of the file is reached, it returns `eof` if you pass `nil` as the second argument, otherwise an error will be signaled.

    ```lisp
    (with-open-file (input "data.bin" :direction :input :element-type '(unsigned-byte 8))
      (let ((byte1 (read-byte input nil)))
        (when byte1
            (format t "Byte 1: ~d~%" byte1))
        (let ((byte2 (read-byte input nil)))
            (when byte2
                (format t "Byte 2: ~d~%" byte2)))
        ))
    ```

* **`write-byte`**: Writes a single byte to a binary stream. It takes the byte (an integer between 0 and 255) and the stream as arguments.

    ```lisp
    (with-open-file (output "data.bin" :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-byte 255 output)
      (write-byte 0 output)
      (write-byte 128 output))
    ```

**Example: Copying a Binary File:**

This example demonstrates how to copy a binary file byte by byte:

```lisp
(defun copy-binary-file (input-filename output-filename)
  (with-open-file (input input-filename :direction :input :element-type '(unsigned-byte 8))
    (with-open-file (output output-filename :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (when (and input output)
        (loop for byte = (read-byte input nil)
              while byte
              do (write-byte byte output))))))

(copy-binary-file "input.jpg" "output.jpg") ; Copies input.jpg to output.jpg
```

This function opens both the input and output files in binary mode and then reads bytes from the input file and writes them to the output file until the end of the input file is reached.

## 5. Pathnames in Common Lisp

Pathnames are a standardized way to represent file and directory paths in Common Lisp. They provide a platform-independent way to work with files.

### 5.1 Representing File and Directory Paths

A pathname has several components:

* **`host`**: The file system or network host (e.g., "localhost", "my-server").
* **`device`**: The device or volume (e.g., "C:", "/dev/sda1").
* **`directory`**: A list of directory names.
* **`name`**: The file name.
* **`type`**: The file extension (e.g., "txt", "jpg", "lisp").
* **`version`**: A file version number (rarely used).

### 5.2 Pathname Functions

* **`pathname`**: Converts a string or another object to a pathname object.

    ```lisp
    (pathname "my-file.txt")
    (pathname "/path/to/my/file.txt")
    ```

* **`pathnamep`**: Tests if an object is a pathname.

    ```lisp
    (pathnamep (pathname "my-file.txt")) ; Returns T
    (pathnamep "my-file.txt")             ; Returns NIL
    ```

* **`namestring`**: Converts a pathname object to a string.

    ```lisp
    (namestring (pathname "my-file.txt")) ; Returns "my-file.txt" (or the appropriate platform-specific string)
    ```

* **`merge-pathnames`**: Merges two pathnames. This is useful for constructing relative paths.

    ```lisp
    (merge-pathnames "file.txt" "/path/to/directory/") ; Returns a pathname representing "/path/to/directory/file.txt"
    ```

* **`pathname-directory`**: Extracts the directory component of a pathname.
* **`pathname-name`**: Extracts the name component of a pathname.
* **`pathname-type`**: Extracts the type component of a pathname.
* **`pathname-host`**: Extracts the host component of a pathname.
* **`pathname-device`**: Extracts the device component of a pathname.

```lisp
(let ((my-pathname (pathname "/path/to/my/file.txt")))
    (print (pathname-directory my-pathname)) ; prints (:ABSOLUTE "path" "to" "my")
    (print (pathname-name my-pathname)) ; prints "file"
    (print (pathname-type my-pathname)) ; prints "txt"
)
```

**Example: Reading from a File in a Specific Directory:**

```lisp
(let ((directory-path "/my/data/"))
  (let ((file-pathname (merge-pathnames "input.txt" directory-path)))
    (with-open-file (input file-pathname :direction :input)
      (when input
        (loop for line = (read-line input nil)
              while line
              do (print line))))))
```

This example shows how to use `merge-pathnames` to construct a pathname from a directory path and a file name.

This section covered binary I/O and pathnames in Common Lisp. Binary I/O is essential for working with non-textual data, while pathnames provide a platform-independent way to handle file and directory paths. These are important tools for any Lisp programmer working with files.
