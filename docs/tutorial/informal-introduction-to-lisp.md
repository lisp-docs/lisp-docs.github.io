---
sidebar_position: 3.1
---

# An Informal Introduction to Common Lisp

This informal introduction is meant to be an example driven first introduction to interacting with a Common Lisp Read-Eval-Print-Loop (REPL). Please start a REPL in your terminal or IDE of choice and follow along. If you installed SBCL in the last tutorial, simply run `$ sbcl` in your terminal to start a REPL.

In the following examples, input and output are distinguished by the presence or absence of prompts.  To repeat the example, you must type everything after the prompt, when the prompt appears; lines that do not begin with a prompt are output from the Lisp system.  In Common Lisp, the standard interactive prompt is often `>` (though this can vary between implementations). We will use `>` here.

You can copy and paste the input lines into your Lisp environment (often called a REPL, for Read-Eval-Print Loop).

Many of the examples in this manual, even those entered at the interactive prompt, include comments. Comments in Common Lisp start with a semicolon `;` and extend to the end of the line. A single semicolon is typically used for short comments on the same line as code. Two semicolons `;;` are used for comments that form a paragraph on their own and describe a section of code. Three semicolons `;;;` are used for file-level comments introducing a file or a major section of code within a file. A comment may appear at the start of a line or following whitespace or code, but not within a string literal. A semicolon within a string literal is just a semicolon.

Since comments are to clarify code and are not interpreted by Lisp, they may be omitted when typing in examples.

Some examples:

```lisp
; this is the first comment
(defvar spam 1) ; and this is the second comment
          ;; ... and now a third!
(defvar text "; This is not a comment because it's inside quotes.")
```

As you may have guessed the form `defvar` above defines a variable. We'll delve into this a bit later.

## 1 Using Common Lisp as a Calculator

Let’s try some simple Common Lisp commands. Start your Lisp environment (often a REPL) and wait for the prompt, often `>`.

## 1.1 Numbers

The Lisp environment acts as a simple calculator: you can type an expression at it and it will print the value. Expression syntax uses prefix notation (also known as Polish notation): the operator comes before the operands. Parentheses `()` are used for grouping and function calls. For example:

```lisp
(+ 2 2)
```

Output:

```lisp
4
```

```lisp
(- 50 (* 5 6))
```

Output:

```lisp
20
```

```lisp
(/ (- 50 (* 5 6)) 4)
```

Output:

```lisp
5
```

```lisp
(/ 8 5) ; division by default yields a ratio or a float if appropriate
```

Output (depending on the implementation and settings):

```lisp
8/5  ; Ratio
```

or

```lisp
1.6 ; Float
```

Common Lisp has several numeric types: integers (e.g., 2, 4, 20), ratios (e.g. 8/5), and floating-point numbers (e.g., 1.6, 5.0).

To get an integer quotient (floor division) you can use `floor`:

```lisp
(floor 17 3)
```

Output:

```lisp
5
```

`floor` actually returns two values: the quotient and the remainder. To get just the remainder, use `mod`:

```lisp
(mod 17 3)
```

Output:

```lisp
2
```

To get both the quotient and remainder use `multiple-value-bind`

```lisp
(multiple-value-bind (quotient remainder) (floor 17 3)
  (format t "Quotient: ~a, Remainder: ~a~%" quotient remainder))
```

Output:

```lisp
Quotient: 5, Remainder: 2
```

```lisp
(+ (* 5 3) 2) ;floored quotient * divisor + remainder
```

Output:

```lisp
17
```

In Common Lisp, you can use `expt` to calculate powers:

```lisp
(expt 5 2) ; 5 squared
```

Output:

```lisp
25
```

```lisp
(expt 2 7) ; 2 to the power of 7
```

Output:

```lisp
128
```

The special operator `setf` is used to assign a value to a variable:

```lisp
(defvar width nil)
(setf width 20)
(defvar height nil)
(setf height (* 5 9))
(* width height)
```

Output:

```lisp
900
```

If a variable is not defined (assigned a value), trying to use it will signal an unbound-variable error:

```lisp
n ; try to access an undefined variable
```

This will signal an error similar to:

```lisp
The variable N is unbound.
```

Common Lisp has full support for floating-point numbers. Operations with mixed numeric types generally result in a floating-point number if one of the operands is a float:

```lisp
(- (* 4 3.75) 1)
```

Output:

```lisp
14.0
```

In the REPL, the value of the last evaluated expression is available through the special variable `*`. This is similar to `_` in Python's interactive mode.

```lisp
(defvar tax nil)
(setf tax (/ 12.5 100))
(defvar price nil)
(setf price 100.50)
(* price tax)
```

Output:

```lisp
12.5625
```

```lisp
(+ price *)
```

Output:

```lisp
113.0625
```

```lisp
(round * 2) ; Rounding to two decimal places
```

Output:

```lisp
113.06
```

You should treat `*` as read-only. Avoid assigning to it explicitly.

Common Lisp supports other numeric types, including complex numbers. The imaginary unit is represented as `#C(0 1)` or `0+1i`

```lisp
(* #C(0 1) #C(0 1)) ; i * i = -1
```

Output:

```lisp
-1
```

or

```lisp
#C(-1 0)
```

## 1.2. Text

Common Lisp can manipulate text (represented by the type `string`) as well as numbers. This includes characters “!”, words “rabbit”, names “Paris”, sentences “Got your back.”, etc. “Yay! :)”. They are enclosed in double quotes `"..."`.

```lisp
"spam eggs"
```

Output:

```lisp
"spam eggs"
```

```lisp
"Paris rabbit got your back :)! Yay!"
```

Output:

```lisp
"Paris rabbit got your back :)! Yay!"
```

```lisp
"1975" ; digits and numerals enclosed in quotes are also strings
```

Output:

```lisp
"1975"
```

Single quotes `'` are not string delimiters.

```lisp
"doesn't"
```

Output:

```lisp
"doesn't"
```

To include a double quote within a string, you need to escape it using a backslash `\`:

```lisp
"\"Yes,\" they said."
```

Output:

```lisp
"\"Yes,\" they said."
```

In the Lisp REPL, the string definition and output string are the same. The `print` function produces a more readable output, but for strings, it doesn't remove the quotes unless you use `princ`.

Special characters like `\n` in other languages are not interpreted as special characters in Lisp. (unless you use the interop library for this). The `format` directive has its own special characters, which will be discussed later on in this tutorial. For now, note that `~%` is the directive for a new line. The `t` in the example below means to print to the standrd output.

```lisp
CL-USER> (format t "First line.~%Second line.")
First line.
Second line.
NIL
```

Common Lisp doesn't have "raw strings" in the same way as Python. Backslashes are always interpreted as escape characters unless they are themselves escaped (e.g., `\\`).

String literals can span multiple lines without the need for any special syntax:

```lisp
(defvar long-string nil)
(setf long-string "Usage: thingy [OPTIONS]

     -h                        Display this usage message

     -H hostname               Hostname to connect to")
(print long-string)
```

Output:

```lisp
"Usage: thingy [OPTIONS]

     -h                        Display this usage message

     -H hostname               Hostname to connect to"
```

Strings can be concatenated (joined together) using `concatenate`:

```lisp
(concatenate 'string "un" "un" "un" "ium")
```

Output:

```lisp
"unununium"
```

There is no automatic concatenation of adjacent string literals in Common Lisp. You must always use `concatenate`.

```lisp
(concatenate 'string "Li" "sp")
```

Output:

```lisp
"Lisp"
```

```lisp
(defvar prefix nil)
(setf prefix "Li")
(concatenate 'string prefix "sp")
```

Output:

```lisp
"Lisp"
```

Strings can be accessed by index using `aref`. The first character has index 0:

```lisp
(defvar word nil)
(setf word "Lisp")
(aref word 0) ; character in position 0
```

Output:

```lisp
#\L
```

```lisp
(aref word 2) ; character in position 5
```

Output:

```lisp
#\s
```

Indices can't be negative in Common Lisp's `aref`.

To get a substring (slicing), use `subseq`:

```lisp
(subseq word 0 2) ; characters from position 0 (included) to 2 (excluded)
```

Output:

```lisp
"Li"
```

```lisp
(subseq word 2 3) ; characters from position 2 (included) to 3 (excluded)
```

Output:

```lisp
"s"
```

Slice indices have useful defaults; an omitted first index defaults to zero, and an omitted second index defaults to the length of the string:

```lisp
(subseq word 0)   ; characters from the beginning to the end
```

Output:

```lisp
"Lisp"
```

```lisp
(subseq word 2)   ; characters from position 4 (included) to the end
```

Output:

```lisp
"sp"
```

Common Lisp strings *are* mutable. You can change individual characters using `(setf (aref string index) new-character)`:

```lisp
(setf word "List")
(setf (aref word 3) #\p)
word
```

Output:

```lisp
"Lisp"
```

The function `length` returns the length of a string:

```lisp
(defvar s "supercalifragilisticexpialidocious")
(length s)
```

Output:

```lisp
34
```

## 1.3. Lists

Common Lisp uses *lists* as its primary compound data type to group together other values. Lists are written as a sequence of space-separated values (items) enclosed in parentheses. Lists can contain items of different types.

```lisp
(defvar squares (list 1 4 9 16 25))
```

Evaluating `squares` in the REPL:

```lisp
squares
```

Output:

```lisp
(1 4 9 16 25)
```

Like strings (and other sequence types), lists can be accessed by index using `elt`:

```lisp
(elt squares 0) ; indexing returns the item
```

Output:

```lisp
1
```

In addition `nth` can be used to access the nth item in a 0-indexed list.

```lisp
CL-USER> (nth 2 (list 1 2 3 4))
3 (2 bits, #x3, #o3, #b11)
```

Common Lisp does *not* support negative indexing with `nth` or `elt`. To access elements from the end, use a calculated index:

```lisp
(nth (- (length squares) 1) squares) ; last element (equivalent to squares[-1] in Python)
```

Output:

```lisp
25
```

or

```lisp
(car (last squares))
```

Output:

```lisp
25
```

To get a sublist (slicing), use `subseq`:

```lisp
(subseq squares 2 5) ; slicing returns a new list (equivalent to squares[2:5] in Python)
```

Output:

```lisp
(9 16 25)
```

Lists can be concatenated using `append`:

```lisp
(append squares (list 36 49 64 81 100))
```

Output:

```lisp
(1 4 9 16 25 36 49 64 81 100)
```

Lists in Common Lisp are mutable. You can change their content using `setf` with `elt`:

```lisp
(defvar cubes (list 1 8 27 65 125)) ; something's wrong here

(expt 4 3) ; the cube of 4 is 64, not 65!
```

Output:

```lisp
64
```

```lisp
(setf (elt cubes 3) 64) ; replace the wrong value

cubes
```

Output:

```lisp
(1 8 27 64 125)
```

You can add new items to a list using `push` or `append`. `push` adds to the beginning, while `append` creates a new list. For adding to the end, `nconc` is often more efficient than `append`:

```lisp
(setf cubes (nconc cubes (list 216))) ; add the cube of 6
(setf cubes (nconc cubes (list (expt 7 3)))) ; and the cube of 7

cubes
```

Output:

```lisp
(1 8 27 64 125 216 343)
```

Simple assignment in Common Lisp, like in Python, does *not* copy data. When you assign a list to a variable, the variable refers to the existing list:

```lisp
(defvar rgb (list "Red" "Green" "Blue"))
(setf rgba rgb)

(eq rgb rgba) ; they reference the same object (similar to id() in Python)
```

Output:

```lisp
T
```

```lisp
(nconc rgba (list "Alph"))

rgb
```

Output:

```lisp
("Red" "Green" "Blue" "Alph")
```

To create a copy of a list, use `copy-list`:

```lisp
(defvar correct-rgba (copy-list rgba))
(setf (elt correct-rgba (- (length correct-rgba) 1)) "Alpha")

correct-rgba
```

Output:

```lisp
("Red" "Green" "Blue" "Alpha")
```

```lisp
rgba
```

Output:

```lisp
("Red" "Green" "Blue" "Alph")
```

Assignment to slices (using `setf` with `subseq`) is also possible:

```lisp
(defvar letters (list 'a 'b 'c 'd 'e 'f 'g))

letters
```

Output:

```lisp
(A B C D E F G)
```

```lisp
(setf (subseq letters 2 5) (list 'C 'D 'E))

letters
```

Output:

```lisp
(A B C D E F G)
```

To remove elements, you can use `setf` with `subseq` and an empty list, or use `delete`:

```lisp
(setf (subseq letters 2 5) nil)
letters
```

Output:

```lisp
(A B F G)
```

To clear the list:

```lisp
(setf letters nil)
letters
```

Output:

```lisp
NIL
```

The function `length` also applies to lists:

```lisp
(setf letters (list 'a 'b 'c 'd))
(length letters)
```

Output:

```lisp
4
```

It is possible to nest lists:

```lisp
(setf a (list 'a 'b 'c))
(setf n (list 1 2 3))
(setf x (list a n))

x
```

Output:

```lisp
((A B C) (1 2 3))
```

```lisp
(elt x 0)
```

Output:

```lisp
(A B C)
```

```lisp
(elt (elt x 0) 1)
```

Output:

```lisp
B
```

## 1.4. First Steps Towards Programming

Of course, we can use Common Lisp for more complicated tasks than adding two and two together. For instance, we can write an initial sub-sequence of the Fibonacci series as follows:

```lisp
;; Fibonacci series:
;; the sum of two elements defines the next

(let ((a 0) (b 1)) ; Multiple assignment using LET
  (loop
    (when (> a 10) (return)) ; Exit the loop when a >= 10
    (print a)
    (rotatef a b (+ a b)))) ; Simultaneous assignment using ROTATEF
```

Output:

```lisp
0 
1 
1 
2 
3 
5 
8 
```

This example introduces several new features.

* The first line uses `let` with a binding list `((a 0) (b 1))` to introduce local variables `a` and `b` and simultaneously initialize them to 0 and 1. This is Common Lisp's equivalent of multiple assignment. The last line uses `rotatef` which is the idiomatic way to swap variable values and perform simultaneous assignment in Common Lisp. `rotatef` rotates the values of the given variables. `(rotatef a b (+ a b))` is equivalent to the Python `a, b = b, a+b`.

* The `loop` macro introduces an infinite loop. The `when` form provides a conditional exit from the loop. `(when (> a 10) (return))` is equivalent to `if a > 10: break` in Python. The loop continues as long as the condition `(> a 10)` is false (i.e., `a` is less than or equal to 10). The standard comparison operators are similar to C and Python: `<` (less than), `>` (greater than), `=` (equal to), `<=` (less than or equal to), `>=` (greater than or equal to), and `/=` (not equal to). Note that `=` is for numeric equality, `eql` is for general object equality (and is what is usually wanted) and `eq` is for object identity.

* Indentation is not syntactically significant in Common Lisp, unlike Python. However, it is *extremely* important for readability. The code within the `let` and `loop` forms is indented to show the structure. Common Lisp code is typically formatted using consistent indentation.

* The `print` function writes the value of its argument(s). It prints a newline after each argument by default. To print without a newline, you can use `princ` or `write-string` to a stream. To format output, `format` is the standard way. It is much more powerful than Python's `print`.

Example using `format`:

```lisp
(let ((i (* 256 256)))
  (format t "The value of i is ~d~%" i)) ; ~d is for decimal integer, ~% is for newline
```

Output:

```lisp
The value of i is 65536
```

To avoid the newline after the output, or end the output with a different string, you can use `format` with different directives. For example, to separate numbers with commas:

```lisp
(let ((a 0) (b 1))
  (loop
    (when (> a 1000) (return))
    (format t "~d," a) ; Print a followed by a comma, no newline
    (rotatef a b (+ a b))))
(terpri) ; Print a final newline
```

Output (the last comma will remain, a more sophisticated format string could avoid that):

```lisp
0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,
```

A more elegant way to avoid the trailing comma is to use `loop for` and `format` with conditional printing:

```lisp
(let ((fib_numbers (loop for a = 0 then b
                         and b = 1 then (+ a b)
                         while (< a 1000)
                         collect a)))
  (format t "~{~A~^, ~}" fib_numbers)) ; Print each element, and a comma unless it's the last one.
(terpri)
```

Output:

```lisp
0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987
NIL
```

The `format` form is very powerful and it deserves a separate tutorial on it's own; however, here's a short intro to get you started:

## 1.5. The `format` directive

Let's explore the basics of the Common Lisp `format` directive. `format` is a powerful tool for creating formatted output. It takes a destination (usually `t` for standard output, or `nil` to return a string), a format string, and any arguments to be formatted.

### 1. Basic String Output

The simplest use is just printing a string:

```lisp
(format t "Hello, world!")
```

Output:

```lisp
Hello, world!
```

### 2. Inserting Values

The `~a` directive inserts an argument using `princ` (human-readable representation):

```lisp
(let ((name "Alice"))
  (format t "Hello, ~a!" name))
```

Output:

```lisp
Hello, Alice!
```

### 3. Newlines

`~%` inserts a newline character:

```lisp
(format t "First line.~%Second line.")
```

Output:

```lisp
First line.
Second line.
```

### 4. Formatting Integers

`~d` formats an integer in decimal:

```lisp
(format t "The number is ~d." 42)
```

Output:

```lisp
The number is 42.
```

`~x` formats an integer in hexadecimal:

```lisp
(format t "The hexadecimal value is ~x." 255)
```

Output:

```lisp
The hexadecimal value is ff.
```

### 5. Formatting Floating-Point Numbers

`~f` formats a floating-point number:

```lisp
(format t "The value is ~f." 3.14159)
```

Output:

```lisp
The value is 3.14159.
```

You can control the number of decimal places:

```lisp
(format t "The value is ~,2f." 3.14159) ; 2 decimal places
```

Output:

```lisp
The value is 3.14
```

### 6. Conditional Text

`~[...~;...~]` allows for conditional output. The first clause is used if the argument is `nil`, the second if it is not:

```lisp
(format t "The value is ~[nil~;not nil~]." nil)
(format t "The value is ~[nil~;not nil~]." 1)
```

Output:

```lisp
The value is nil.
The value is not nil.
```

### 7. Iterating over Lists (and avoiding trailing separators)

`~{...~}` iterates over a list. Combined with `~^`, you can avoid trailing separators:

```lisp
(format t "The numbers are: ~{~a~^, ~}." '(1 2 3 4))
```

Output:

```lisp
The numbers are: 1, 2, 3, 4.
```

### 8. Returning a String

To get the formatted output as a string instead of printing it, use `nil` as the first argument:

```lisp
(let ((formatted-string (format nil "The answer is ~d." 42)))
  (print formatted-string))
```

Output:

```lisp
"The answer is 42."
```

These examples cover the most commonly used `format` directives. `format` is much more powerful than this short introduction shows, but these basics will get you started. For more details, consult the Common Lisp Technical Reference's [Section on Formatted Output (22.3)](https://lisp-docs.github.io/cl-language-reference/chap-22/cc-d-formatted-output).
