---
sidebar_position: 3
---

# Readtable

:::warning
This article **needs to be checked!**.
Please help contribute by checking it and making changes in [our repository](https://github.com/lisp-docs/lisp-docs.github.io) or by clicking on the "Edit this page" link below.
:::

## Introduction to Readtables and Associated Concepts in Common Lisp

The *readtable* in Common Lisp is a crucial component responsible for parsing textual input into Lisp objects. It defines the syntax of the language, determining how characters are interpreted and combined to form symbols, numbers, strings, lists, and other Lisp data structures. Understanding readtables allows you to customize the Lisp reader's behavior, enabling you to create domain-specific languages or handle non-standard input formats.

This tutorial will introduce you to the fundamental concepts of readtables, including character syntax, reader macros, and how to manipulate readtables to control the parsing process.

**Key Concepts:**

* **Readtable:** A data structure that maps characters to their syntax types and associated actions.
* **Character Syntax:** The interpretation of individual characters (e.g., whitespace, constituent, macro, terminator).
* **Reader Macros:** Functions associated with specific characters that perform custom parsing actions.
* **Standard Readtable:** The default readtable used by Common Lisp.
* **Customizing Readtables:** Creating and modifying readtables to change Lisp syntax.

**Table of Contents:**

**1. Introduction to Readtables:**

* What is a readtable?
* The role of the readtable in the Lisp reader.
* The current readtable (`*readtable*`).

**2. Character Syntax:**

* Constituent characters.
* Whitespace characters.
* Macro characters.
* Terminating macro characters.
* Non-terminating macro characters.
* Multiple escape characters.

**3. Reader Macros:**

* What are reader macros?
* Defining reader macros using `set-macro-character`.
* Examples of standard reader macros (`'`, `#`, `,`, etc.).
* Defining custom reader macros.

**4. Readtable Functions:**

* `copy-readtable`: Creating copies of readtables.
* `set-syntax-from-char`: Copying the syntax of one character to another.
* `get-macro-character`: Getting the macro function associated with a character.
* `set-macro-character`: Setting the macro function associated with a character.
* `read`: How read interacts with the readtable.

**5. Readtable Case:**

* `*read-base*`: The input base for numbers.
* `*read-default-float-format*`: The default float format.
* `*readtable-case*`: Controlling case conversion during reading.
* `with-standard-io-syntax`: Restoring standard readtable settings.

**6. Examples and Advanced Techniques:**

* Creating a simple domain-specific language.
* Handling different date formats.
* Implementing a custom comment syntax.

This tutorial will provide clear explanations and practical examples to help you understand readtables and their applications. You'll learn how to modify Lisp's syntax and create custom parsing rules for various purposes.

## 1. Introduction to Readtables

The *readtable* is a fundamental data structure in Common Lisp that governs how the Lisp reader parses textual input into Lisp objects. It's the key to Lisp's extensibility and its ability to support various syntaxes and domain-specific languages.

### 1.1 What is a Readtable?

A readtable is essentially a mapping from characters to their *syntax type* and associated *actions*. It tells the Lisp reader how to interpret each character it encounters during the reading process.

Each character in the character set has an associated syntax type within a given readtable. This syntax type determines how the reader treats the character. For instance, some characters are treated as whitespace, others as part of symbols, and still others trigger special parsing actions (these are called *macro characters*).

### 1.2 The Role of the Readtable in the Lisp Reader

The Lisp reader is the part of the Lisp system that takes textual input (a stream of characters) and transforms it into Lisp objects (symbols, numbers, lists, strings, etc.). The readtable is the primary source of information for this transformation.

Here's a simplified view of the reading process:

1. The reader reads a character from the input stream.
2. It looks up the character in the current readtable.
3. Based on the character's syntax type and any associated actions (reader macros), the reader performs the appropriate parsing steps.
4. This process continues until a complete Lisp object has been formed.
5. The reader returns the Lisp object.

Without a readtable, the Lisp reader wouldn't know how to interpret the characters it reads. The readtable provides the necessary context and rules for parsing.

### 1.3 The Current Readtable (`*readtable*`)

The special variable `*readtable*` holds the *current readtable*. This is the readtable that the Lisp reader uses by default. You can inspect its value:

```lisp
*readtable* ; Returns the current readtable object.
```

The standard readtable is usually what you'll be using. It defines the standard Lisp syntax that you're familiar with.

It's important to understand that `*readtable*` is a *dynamic variable*. This means that its value can be temporarily changed within a specific scope using `let` or other binding forms. This allows you to create local modifications to the readtable without affecting other parts of your program.

```lisp
(let ((*readtable* (copy-readtable nil))) ; Create a copy of the standard readtable
  ; Modify the copied readtable here...
  (read-from-string "#| This is a test |#") ; Will still work because the copy is based on the standard readtable
)
(read-from-string "#| This is a test |#") ; Will still work because *readtable* was not modified directly
```

`copy-readtable` is used to create a new readtable that is a copy of an existing one. If its argument is `nil` it will create a copy of the standard readtable. This is crucial when you want to modify the readtable, as modifying `*readtable*` directly would affect all subsequent reading operations in your Lisp session.

In summary, the readtable is a data structure that maps characters to their syntax types and associated actions, guiding the Lisp reader in parsing textual input. `*readtable*` holds the current readtable, and it's essential to create copies of the readtable before modifying it. The next section will delve into the different character syntax types.

## 2. Character Syntax

The readtable assigns a *syntax type* to each character, determining how the Lisp reader interprets it. This section describes the different character syntax types.

### 2.1 Constituent Characters

*Constituent characters* are used to form symbols and numbers. They include:

* Uppercase and lowercase letters (A-Z, a-z).
* Digits (0-9).
* Certain punctuation characters: `+`, `-`, `*`, `/`, `@`, `$`, `%`, `^`, `&`, `_`, `=`, `<`, `>`, `.`.

When the reader encounters a sequence of constituent characters, it forms a *token*. This token is then interpreted as either a symbol or a number, depending on its form. For example, `foo`, `Bar123`, and `+count` are all valid symbols. `123`, `-45`, and `3.14` are interpreted as numbers.

By default, the Lisp reader converts lowercase letters in symbols to uppercase. So, `foo`, `Foo`, and `FOO` are all read as the same symbol: `FOO`. This behavior is controlled by the `*readtable-case*` variable, which we'll discuss later.

### 2.2 Whitespace Characters

*Whitespace characters* are used to separate tokens. They are ignored by the reader except for their role as separators. The standard whitespace characters are:

* Space.
* Tab.
* Newline (line feed).
* Form feed.

Multiple whitespace characters are treated as a single separator.

### 2.3 Macro Characters

*Macro characters* trigger special parsing actions. They are associated with *reader macros*, which are functions that define how the character and the subsequent input should be interpreted.

There are two main types of macro characters:

* Terminating macro characters
* Non-terminating macro characters

### 2.4 Terminating Macro Characters

*Terminating macro characters* cause the reader to stop forming the current token and perform a specific action. They effectively terminate the current token.

Some standard terminating macro characters include:

* `(`: Starts a list.
* `)`: Ends a list.
* `"`: Starts and ends a string.
* `;`: Starts a comment (until the end of the line).

When the reader encounters a terminating macro character, it executes the associated reader macro function. This function might read further input, create a Lisp object, and return it.

For example, when the reader encounters `(`, the associated reader macro reads subsequent input until a matching `)` is found and constructs a list.

### 2.5 Non-Terminating Macro Characters

*Non-terminating macro characters* do not terminate the current token. They trigger a specific action but allow the reader to continue forming the token.

Some standard non-terminating macro characters include:

* `'`: The quote character. It's a shorthand for `(quote ...)`.
* `#`: The sharp sign. It's used for various purposes, including reading special constants, character literals, and reader macros.
* `,`: The comma. Used in conjunction with `#` for quasiquoting.

For example, when the reader encounters `'x`, the associated reader macro converts it to `(quote x)`. The reader then continues processing the rest of the input.

### 2.6 Multiple Escape Characters

The backslash character `\` is the *escape character*. It's used to include characters in symbols or strings that would otherwise have special meaning.

* Inside symbols, `\` allows you to include whitespace, macro characters, or lowercase letters. For instance, `My\ Variable` is read as the symbol `My Variable`, and `\|MixedCaseSymbol\|` is read as the symbol `MixedCaseSymbol`.
* Inside strings, `\` has similar uses, and also allows for escape sequences like `\n` (newline), `\t` (tab), `\"` (double quote), and `\\` (backslash).

Vertical bars `||` can also be used as escape characters. Everything between two vertical bars is read as a symbol, without case conversion.

```lisp
'My\ Symbol ; Reads as the symbol My Symbol
'|My Symbol| ; Reads as the symbol My Symbol (preserves case)

"This is a \"quoted\" string.\n" ; String with escaped quotes and newline
```

Understanding character syntax is fundamental to understanding how the Lisp reader works. The next section will focus on reader macros, which provide the mechanism for customizing the reader's behavior.

## 3. Reader Macros

*Reader macros* are functions associated with specific characters in the readtable. When the Lisp reader encounters a macro character, it invokes the corresponding reader macro function to perform custom parsing actions. This is the mechanism that allows you to extend and customize the Lisp syntax.

### 3.1 What are Reader Macros?

A reader macro is a function that takes two arguments:

1. The input stream from which characters are being read.
2. The character that triggered the macro (the macro character itself).

The reader macro function can then perform any necessary actions, such as:

* Reading more characters from the input stream.
* Creating Lisp objects.
* Returning the created object.

The reader then uses the returned object as part of the Lisp code being read.

### 3.2 Defining Reader Macros using `set-macro-character`

The `set-macro-character` function is used to associate a reader macro function with a character. Its syntax is:

```lisp
(set-macro-character char function &optional non-terminating-p readtable)
```

* **`char`**: The character to which the macro is being assigned.
* **`function`**: The reader macro function.
* **`non-terminating-p`**: A boolean indicating whether the macro character is non-terminating (defaults to `nil`, meaning terminating).
* **`readtable`**: The readtable to modify (defaults to `*readtable*`).

### 3.3 Examples of Standard Reader Macros

Here are some examples of standard reader macros and what they do:

* **`'` (Quote):** The quote character is a non-terminating macro character. Its reader macro transforms `'x` into `(quote x)`.

    ```lisp
    (read-from-string "'hello") ; Returns (QUOTE HELLO)
    ```

* **`#` (Sharp-sign):** The sharp-sign is a non-terminating macro character used for various purposes. It has many sub-macros.

  * `#\`: Used for character literals. `#\A` reads as the character object `#\A`.

```lisp
(read-from-string "#\A") ; Returns #\A
```

* `#(`: Used for vectors. `#(1 2 3)` reads as a vector containing the numbers 1, 2, and 3.

```lisp
(read-from-string "#(1 2 3)") ; Returns #(1 2 3)
```

* `#| ... |#`: Used for block comments. The reader ignores everything between `#|` and `|#`.

```lisp
(read-from-string "#| This is a block comment |# 123") ; Returns 123
```

* `#:symbol`: Creates an uninterned symbol (a *gensym*).

```lisp
(read-from-string "#:foo") ; Returns a freshly generated symbol.
```

* **,` (Comma):** The comma is used in conjunction with `#` for *quasiquoting*. It's a non-terminating macro character.

    ```lisp
    (let ((x 10))
        (read-from-string `(a b ,x c))) ; returns (A B 10 C)
    ```

### 3.4 Defining Custom Reader Macros

Here's an example of defining a custom reader macro that reads dates in the format YYYY-MM-DD:

```lisp
(defun read-date (stream char)
  (declare (ignore char)) ; We don't need the macro character itself
  (let ((year (read stream))
        (dash1 (read-char stream))
        (month (read stream))
        (dash2 (read-char stream))
        (day (read stream)))
    (when (and (eq dash1 #\-) (eq dash2 #\-))
      (list :year year :month month :day day))
    (error "Invalid date format.")))

(set-macro-character #\@ #'read-date) ; Set @ as the macro character

(read-from-string "@2024-10-27") ; Returns (:YEAR 2024 :MONTH 10 :DAY 27)
```

In this example:

1. `read-date` is the reader macro function. It reads the year, two dashes, the month and the day.
2. `set-macro-character` associates the `@` character with the `read-date` function.
3. Now, when the reader encounters `@2024-10-27`, it calls `read-date`, which parses the input and returns a list representing the date.

Another Example: Reading hexadecimal numbers prefixed by `$`:

```lisp
(defun read-hex-number (stream char)
  (declare (ignore char))
  (let ((token (read-token stream)))
    (parse-integer token :radix 16)))

(defun read-token (stream)
    (with-output-to-string (s)
        (loop for char = (peek-char nil stream nil)
            while (and char (digit-char-p char 16))
            do (write-char (read-char stream) s))))

(set-macro-character #\$ #'read-hex-number)

(read-from-string "$FF") ; Returns 255
(read-from-string "$1A") ; Returns 26
```

Reader macros are a powerful way to customize Lisp syntax. They allow you to define concise and expressive notations for your data and programs. This section covered how to define reader macros and provided examples of both standard and custom macros. The next section will cover readtable functions and other related concepts.

## 4. Readtable Functions

This section details functions for manipulating readtables, allowing you to create, modify, and inspect them.

### 4.1 `copy-readtable`: Creating Copies of Readtables

The `copy-readtable` function creates a new readtable that is a copy of an existing one. This is crucial when you want to modify a readtable without affecting other parts of your program or the standard Lisp syntax.

```lisp
(copy-readtable &optional from-readtable to-readtable)
```

* **`from-readtable`**: The readtable to copy (defaults to `*readtable*`).
* **`to-readtable`**: The readtable to copy to. If it is `nil` a new readtable is created, otherwise the `from-readtable` is copied to the `to-readtable` (defaults to `nil`).

```lisp
(let ((my-readtable (copy-readtable)))
  ; Modify my-readtable...
  (set-macro-character #\! #'(lambda (stream char) (declare (ignore stream char)) "Bang!") t my-readtable)
  (read-from-string "Test!" nil nil :start 0 :end nil :readtable my-readtable)) ; Returns "Bang!"

(read-from-string "Test!" nil nil :start 0 :end nil) ; Signals an error because ! is not a macro character in the default readtable
```

In this example, we create a copy of the current readtable and bind it to `my-readtable`. Then, we modify `my-readtable` by setting `!` as a macro character. The use of `read-from-string` with the keyword argument `:readtable` is necessary to use the modified readtable. The standard readtable remains unchanged.

### 4.2 `set-syntax-from-char`: Copying the Syntax of One Character to Another

The `set-syntax-from-char` function copies the syntax of one character to another within a given readtable.

```lisp
(set-syntax-from-char to-char from-char &optional readtable)
```

* **`to-char`**: The character to which the syntax will be copied.
* **`from-char`**: The character from which the syntax will be copied.
* **`readtable`**: The readtable to modify (defaults to `*readtable*`).

```lisp
(let ((my-readtable (copy-readtable)))
  (set-syntax-from-char #\$ #\% my-readtable) ; Make $ have the same syntax as %
  (read-from-string "10$") ; Will now signal an error because % is not a valid character in numbers
)
```

In this example, we make `$` have the same syntax as `%`. This is a quick way to make one character behave like another without having to define a new reader macro.

### 4.3 `get-macro-character`: Getting the Macro Function Associated with a Character

The `get-macro-character` function retrieves the macro function associated with a given character in a readtable.

```lisp
(get-macro-character char &optional readtable)
```

* **`char`**: The character for which to retrieve the macro function.
* **`readtable`**: The readtable to check (defaults to `*readtable*`).

It returns two values:

1. The macro function (or `nil` if the character is not a macro character).
2. A boolean indicating whether the character is a non-terminating macro character.

```lisp
(get-macro-character #\') ; Returns the quote macro function and T
(get-macro-character #\a) ; Returns NIL and NIL
```

### 4.4 `set-macro-character`: Setting the Macro Function Associated with a Character

We already saw this function in the previous section. The `set-macro-character` function associates a reader macro function with a character in a readtable.

```lisp
(set-macro-character char function &optional non-terminating-p readtable)
```

* **`char`**: The character to which the macro is being assigned.
* **`function`**: The reader macro function.
* **`non-terminating-p`**: A boolean indicating whether the macro character is non-terminating (defaults to `nil`, meaning terminating).
* **`readtable`**: The readtable to modify (defaults to `*readtable*`).

```lisp
(set-macro-character #\! #'(lambda (stream char) (declare (ignore stream char)) "Bang!") t)
(read-from-string "Test!") ; Returns "Bang!"
```

### 4.5 `read`: How `read` Interacts with the Readtable

The `read` function is the core function for parsing Lisp objects from an input stream. It uses the current readtable (`*readtable*`) to interpret the input.

When `read` encounters a character:

1. It looks up the character in the current readtable.
2. If the character is a whitespace character, it's skipped.
3. If the character is a constituent character, `read` starts forming a token (a symbol or number).
4. If the character is a macro character, the associated reader macro function is called. The reader macro function can then read more characters, create Lisp objects, and return them.
5. This process repeats until a complete Lisp object has been parsed.

`read` takes several optional arguments, including `:stream`, `:eof-error-p`, `:eof-value`, and `:recursive`. The `:readtable` keyword argument allows you to specify a different readtable than `*readtable*`.

```lisp
(let ((my-readtable (copy-readtable)))
    (set-macro-character #\! #'(lambda (stream char) (declare (ignore stream char)) "Bang!") t my-readtable)
    (read-from-string "Test!" nil nil :start 0 :end nil :readtable my-readtable)) ; Returns "Bang!"

(with-input-from-string (stream "Test!")
    (read stream nil nil :readtable (let ((my-readtable (copy-readtable)))
                                        (set-macro-character #\! #'(lambda (stream char) (declare (ignore stream char)) "Bang!") t my-readtable)
                                        my-readtable))) ; Returns "Bang!"
```

Understanding how `read` interacts with the readtable is essential for customizing Lisp syntax and creating domain-specific languages. This section covered the key readtable functions and explained how `read` uses the readtable to parse input. The next section will cover readtable case and related concepts.

## 5. Readtable Case and Related Variables

This section covers variables that influence the reading process, particularly how case is handled and how numbers are interpreted.

### 5.1 `*read-base*`: The Input Base for Numbers

The `*read-base*` special variable controls the radix (base) used when reading numbers. It can be set to any integer between 2 and 36 (inclusive).

```lisp
*read-base* ; Returns the current read base (usually 10).

(let ((*read-base* 2))
  (read-from-string "1011")) ; Returns 11 (binary 1011)

(let ((*read-base* 16))
  (read-from-string "FF")) ; Returns 255 (hexadecimal FF)

(let ((*read-base* 8))
  (read-from-string "77")) ; Returns 63 (octal 77)
```

It's important to use `let` to bind `*read-base*` locally, as changing it globally would affect all subsequent reading operations.

### 5.2 `*read-default-float-format*`: The Default Float Format

The `*read-default-float-format*` special variable determines the default floating-point format used when reading floating-point numbers without an explicit exponent marker. The possible values are:

* `short-float`
* `single-float` (the default)
* `double-float`
* `long-float`

```lisp
*read-default-float-format* ; Returns SINGLE-FLOAT

(let ((*read-default-float-format* 'double-float))
  (type-of (read-from-string "3.1415926535897932384626433832795"))) ; Returns DOUBLE-FLOAT

(type-of (read-from-string "3.1415926535897932384626433832795d0")) ; Returns DOUBLE-FLOAT (explicit exponent marker)

(let ((*read-default-float-format* 'short-float))
  (type-of (read-from-string "3.14"))) ; Returns SHORT-FLOAT
```

Again, it's crucial to bind this variable locally using `let` to avoid unintended side effects. If an exponent marker is used (e.g., `1.0d0`, `1.0e0`, `1.0f0`, `1.0s0`), the format specified by the marker will override the value of `*read-default-float-format*`.

### 5.3 `*readtable-case*`: Controlling Case Conversion During Reading

The `*readtable-case*` special variable controls how the Lisp reader handles the case of characters when reading symbols. It can have one of the following values:

* `:upcase` (the default): Converts all lowercase letters in symbols to uppercase.
* `:downcase`: Converts all uppercase letters in symbols to lowercase.
* `:preserve`: Preserves the case of letters in symbols.
* `:invert`: Inverts the case of letters in symbols (uppercase becomes lowercase, and vice versa).

```lisp
*readtable-case* ; Returns :UPCASE

(read-from-string "foo") ; Returns FOO
(read-from-string "Foo") ; Returns FOO
(read-from-string "FOO") ; Returns FOO

(let ((*readtable-case* :downcase))
  (read-from-string "foo")) ; Returns FOO
(let ((*readtable-case* :downcase))
  (read-from-string "Foo")) ; Returns foo
(let ((*readtable-case* :downcase))
  (read-from-string "FOO")) ; Returns foo

(let ((*readtable-case* :preserve))
  (read-from-string "foo")) ; Returns FOO
(let ((*readtable-case* :preserve))
  (read-from-string "Foo")) ; Returns Foo
(let ((*readtable-case* :preserve))
  (read-from-string "FOO")) ; Returns FOO

(let ((*readtable-case* :invert))
  (read-from-string "foo")) ; Returns foo
(let ((*readtable-case* :invert))
  (read-from-string "Foo")) ; Returns fOO
(let ((*readtable-case* :invert))
  (read-from-string "FOO")) ; Returns foo
```

Like the other variables, `*readtable-case*` should be bound locally using `let` to avoid unintended global changes.

### 5.4 `with-standard-io-syntax`: Restoring Standard Readtable Settings

The `with-standard-io-syntax` macro provides a convenient way to establish a standard I/O syntax, ensuring that the readtable and related variables are set to their standard values. This is especially useful when you've made modifications to the readtable or related variables and want to ensure consistent behavior in a specific part of your code.

```lisp
(let ((*read-base* 2)
      (*readtable-case* :downcase))
  (format t "Reading in non-standard syntax:~%")
  (format t "Binary 1011: ~a~%" (read-from-string "1011"))
  (format t "Symbol Foo: ~a~%" (read-from-string "Foo"))

  (with-standard-io-syntax
    (format t "~%Reading in standard syntax:~%")
    (format t "Decimal 1011: ~a~%" (read-from-string "1011"))
    (format t "Symbol Foo: ~a~%" (read-from-string "Foo"))))

; Output:
; Reading in non-standard syntax:
; Binary 1011: 11
; Symbol Foo: foo
;
; Reading in standard syntax:
; Decimal 1011: 1011
; Symbol Foo: FOO
```

`with-standard-io-syntax` is highly recommended when reading or writing data that needs to be portable or when interacting with external systems that expect standard Lisp syntax. It ensures that your code behaves consistently regardless of any modifications to the global I/O settings. This concludes the tutorial on readtables and associated concepts. You should now have a good understanding of how the Lisp reader works and how to customize it for various purposes.

## 6. Examples and Advanced Techniques with Readtables

This section demonstrates some practical applications of readtables and reader macros, including creating a simple domain-specific language (DSL), handling different date formats, and implementing a custom comment syntax.

### 6.1 Creating a Simple Domain-Specific Language

Reader macros are ideal for creating small DSLs within Common Lisp. This example creates a DSL for defining simple geometric points:

```lisp
(defun read-point (stream char)
  (declare (ignore char))
  (let ((x (read stream))
        (comma (read-char stream))
        (y (read stream))
        (close-paren (read-char stream)))
    (if (and (eql comma #\,) (eql close-paren #\)))
        (list :x x :y y)
        (error "Invalid point syntax"))))

(set-macro-character #\[ #'read-point)

(read-from-string "[10,20]") ; Returns (:X 10 :Y 20)
(read-from-string "[5, -3]") ; Returns (:X 5 :Y -3)
(read-from-string "[1,2,3]") ; Signals an error
```

Here's how it works:

1. `read-point` is the reader macro function. It reads an opening bracket `[`, the x-coordinate, a comma, the y-coordinate, and a closing parenthesis `)`.
2. `set-macro-character` associates the `[` character with the `read-point` function.
3. Now, when the reader encounters `[10,20]`, it calls `read-point`, which parses the input and returns a list representing the point.

This simple example demonstrates how you can create a concise and readable syntax for a specific domain.

### 6.2 Handling Different Date Formats

Reader macros can also be used to handle various date formats. This example handles dates in the YYYY/MM/DD format:

```lisp
(defun read-date (stream char)
  (declare (ignore char))
  (let ((year (read stream))
        (slash1 (read-char stream))
        (month (read stream))
        (slash2 (read-char stream))
        (day (read stream)))
    (if (and (eql slash1 #\/) (eql slash2 #\/))
        (encode-universal-time 0 0 0 day month year) ; Convert to universal time
        (error "Invalid date format"))))

(set-macro-character #\< #'read-date)

(read-from-string "<2024/10/27>") ; Returns a universal time representation of the date
```

This example is similar to the previous one, but it uses `/` as separators and converts the date to a universal time representation using `encode-universal-time`.

### 6.3 Implementing a Custom Comment Syntax

You can also use reader macros to implement custom comment syntaxes. This example implements comments that start with `;;;` and continue to the end of the line:

```lisp
(defun read-custom-comment (stream char)
  (declare (ignore char))
  (loop for char = (read-char stream nil) ; Read until end of line
        until (or (null char) (eql char #\Newline)))
  (values)) ; Return no values (effectively skipping the comment)

(set-macro-character #\; #'read-custom-comment nil) ; Make ; a non-terminating macro char

(read-from-string ";;; This is a custom comment\n123") ; Returns 123
(read-from-string ";;; This is a comment 456") ; Returns 456
```

Here's how it works:

1. `read-custom-comment` reads characters from the stream until it encounters a newline or the end of the file.
2. `set-macro-character` associates the `;` character with the `read-custom-comment` function and sets the third argument to `nil` to make it a terminating macro character.
3. Because the function returns no values, the reader effectively skips the comment.

This example demonstrates how to implement a comment syntax that's different from the standard single semicolon comment.

**Important Notes:**

* Modifying the standard readtable can have far-reaching consequences. It's almost always best to create a copy of the readtable using `copy-readtable` before making any changes.
* Reader macros can make code harder to read if overused. Use them judiciously and document them well.
* The `with-standard-io-syntax` macro is essential for ensuring standard syntax when needed.

These examples illustrate the power and flexibility of readtables and reader macros. They allow you to customize the Lisp reader to handle various input formats and even create small domain-specific languages. This concludes the tutorial on readtables and associated concepts.
