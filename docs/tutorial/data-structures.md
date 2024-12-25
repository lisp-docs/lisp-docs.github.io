---
sidebar_position: 5
---

# Data Structures

## Introduction to Data Structures in Common Lisp

Data structures are fundamental to programming, providing ways to organize and store data efficiently. Common Lisp offers a rich set of built-in data structures, each with its own strengths and weaknesses, suitable for different tasks. This tutorial will guide you through the most important data structures in Common Lisp and the forms used to manipulate them.

**Key Concepts:**

* **Data Types:** Understanding the basic data types in Common Lisp, such as numbers, characters, strings, symbols, and conses.
* **Data Structure Properties:** Considering factors like mutability (whether a structure can be modified), access time (how quickly elements can be retrieved), and memory usage when choosing a data structure.
* **Constructors and Accessors:** Functions for creating and accessing elements within data structures.

**Table of Contents:**

**1. Conses and Lists:**

* `cons`: Constructing cons cells (the building blocks of lists).
* `car`, `cdr`: Accessing the parts of a cons cell.
* `list`: Creating lists.
* List manipulation functions: `append`, `reverse`, `nth`, `nthcdr`, `member`, `assoc`, and others.
* Dotted lists.
* Association lists (alists).
* Property lists (plists).

**2. Arrays:**

* `make-array`: Creating arrays.
* `aref`: Accessing array elements.
* Multi-dimensional arrays.
* Adjustable arrays.
* Specialized arrays.

**3. Structures:**

* `defstruct`: Defining structures (user-defined record types).
* Structure accessors.

**4. Hash Tables:**

* `make-hash-table`: Creating hash tables.
* `gethash`, `setf gethash`: Accessing and modifying hash table entries.
* Hash table options: `:test`, `:size`, `:rehash-size`, `:rehash-threshold`.
* Iterating over hash tables.

**5. Strings:**

* String literals.
* String manipulation functions: `concatenate`, `subseq`, `string-upcase`, `string-downcase`, `string=`, and others.

**6. Characters:**

* Character literals.
* Character comparison functions.

**7. Symbols:**

* Symbol literals.
* Using symbols as keys and identifiers.

This tutorial will provide clear explanations and examples for each data structure, helping you understand their usage and choose the appropriate structure for your specific programming needs. We will also cover common operations and functions for manipulating these structures.

You've requested the same section again. I will provide it, but this time I'll include a few more examples and explanations of some of the "and others" list functions mentioned previously, to make it even more complete.

## 1. Conses and Lists in Common Lisp

Lists are a fundamental data structure in Common Lisp. They are built from *cons cells*, which are pairs of values. Understanding conses is crucial for understanding how lists work.

### 1.1 `cons`: Constructing Cons Cells

The `cons` function creates a new cons cell. It takes two arguments:

* The first argument becomes the *car* (Contents of Address part of Register) of the cons cell.
* The second argument becomes the *cdr* (Contents of Decrement part of Register) of the cons cell.

```lisp
(cons 1 2)  ; Creates a cons cell whose car is 1 and cdr is 2.
```

This is often represented visually as a box with two compartments:

```lisp
+---+---+
| 1 | 2 |
+---+---+
  car cdr
```

### 1.2 `car` and `cdr`: Accessing Parts of a Cons Cell

The `car` function returns the car of a cons cell, and the `cdr` function returns the cdr.

```lisp
(let ((my-cons (cons 1 2)))
  (print (car my-cons)) ; Prints 1
  (print (cdr my-cons))) ; Prints 2
```

You can chain `car` and `cdr` calls using abbreviations like `cadr`, `cddr`, `caddr`, etc.

```lisp
(let ((my-list (cons 1 (cons 2 (cons 3 nil)))))
  (print (cadr my-list))) ; Equivalent to (car (cdr my-list)), prints 2
```

### 1.3 `list`: Creating Lists

While you can create lists using nested `cons` calls, the `list` function provides a more convenient way.

```lisp
(list 1 2 3) ; Creates a list containing 1, 2, and 3.
```

This is equivalent to:

```lisp
(cons 1 (cons 2 (cons 3 nil)))
```

A list is simply a chain of cons cells where the cdr of the last cell is `nil`. `nil` represents the empty list.

Visual representation of `(list 1 2 3)`:

```lisp
+---+---+     +---+---+     +---+---+
| 1 | --+---->| 2 | --+---->| 3 |nil|
+---+---+     +---+---+     +---+---+
```

### 1.4 List Manipulation Functions

Common Lisp provides many functions for manipulating lists:

* **`append`**: Creates a *new* list by concatenating two or more lists.

    ```lisp
    (append (list 1 2) (list 3 4)) ; Returns (1 2 3 4)
    (append (list 1 2) '(3 4))      ; Also returns (1 2 3 4)
    (append '(1 2) '(3) '(4 5 6)) ; Returns (1 2 3 4 5 6)
    ```

    It's important to remember that `append` *copies* the lists (except possibly the last argument). If you want to modify a list in place, you should use `nconc`.

* **`nconc`**: Concatenates lists by modifying the *cdr* of the last cons cell of the first list. This is a destructive operation.

    ```lisp
    (let ((list1 (list 1 2)) (list2 (list 3 4)))
        (nconc list1 list2) ; Returns (1 2 3 4). list1 is now (1 2 3 4).
        (print list1))
    ```

* **`reverse`**: Creates a *new* list with the elements in reverse order.

    ```lisp
    (reverse (list 1 2 3)) ; Returns (3 2 1)
    ```

* **`nth`**: Returns the nth element of a list (zero-based indexing).

    ```lisp
    (nth 1 (list 'a 'b 'c)) ; Returns B
    ```

* **`nthcdr`**: Returns the cdr of a list n times.

    ```lisp
    (nthcdr 2 (list 'a 'b 'c 'd)) ; Returns (C D)
    ```

* **`member`**: Tests if an element is a member of a list. Returns the tail of the list starting with the element if found, otherwise `nil`. Uses `eql` for comparison by default.

    ```lisp
    (member 'b (list 'a 'b 'c)) ; Returns (B C)
    (member 'd (list 'a 'b 'c)) ; Returns NIL
    (member '(1 2) (list (list 1) (list 1 2))) ; Returns NIL because (list 1 2) and '(1 2) are different objects.
    (member '(1 2) (list (list 1) (list 1 2)) :test #'equal) ; Returns ((1 2)) because equal compares list contents.
    ```

* **`assoc`**: Searches an association list (alist) for a key. Returns the cons cell whose car is `eql` to the key, or `nil` if not found.

    ```lisp
    (assoc 'b '((a 1) (b 2) (c 3))) ; Returns (B 2)
    (assoc 'd '((a 1) (b 2) (c 3))) ; Returns NIL
    ```

* **`remove`**: Creates a *new* list with all occurrences of a specified element removed.

    ```lisp
    (remove 'b (list 'a 'b 'c 'b 'd)) ; Returns (A C D)
    ```

* **`remove-duplicates`**: Creates a *new* list with duplicate elements removed.

    ```lisp
    (remove-duplicates (list 1 2 1 3 2 4 1)) ; Returns (1 2 3 4) (order may vary)
    ```

* **`subst`**: Creates a *new* list with all occurrences of an old element substituted with a new element.

    ```lisp
    (subst 'x 'b (list 'a 'b 'c 'b 'd)) ; Returns (A X C X D)
    ```

### 1.5 Dotted Lists

A dotted list is a cons cell where the cdr is *not* `nil` or another cons cell.

```lisp
(cons 1 2) ; This is a dotted list: (1 . 2)
```

Dotted lists are less common than proper lists (lists that end in `nil`). They are mainly used for specific low level operations.

### 1.6 Association Lists (Alists)

An association list (alist) is a list of cons cells, where each cons cell represents a key-value pair. `assoc` is used to retrieve values from an alist by key.

```lisp
(let ((my-alist '((name "Alice") (age 30) (city "New York"))))
  (print (cdr (assoc 'name my-alist))) ; Prints "Alice"
  (print (cdr (assoc 'age my-alist))))    ; Prints 30
```

### 1.7 Property Lists (Plists)

A property list (plist) is a list of alternating keys and values. `getf` is used to retrieve values from a plist by key.

```lisp
(let ((my-plist '(name "Bob" age 25 city "London")))
  (print (getf my-plist 'name)) ; Prints "Bob"
  (print (getf my-plist 'age)))  ; Prints 25
```

Plists are often used for storing metadata associated with objects or other data structures. They are used in CLOS to store information about classes, for example.

This section covered the fundamental data structure of lists in Common Lisp, including the underlying cons cells and important list manipulation functions. Understanding these concepts is essential for working effectively with Lisp. The next section will cover vectors.

## 2. Arrays in Common Lisp

Arrays in Common Lisp are ordered collections of elements, similar to vectors, but they can have multiple dimensions. They provide efficient access to elements using integer indices.

### 2.1 `make-array`: Creating Arrays

The `make-array` function is used to create arrays. It takes several keyword arguments to control the array's properties.

* **`:dimensions`**: This is a required argument. It specifies the dimensions of the array. For a one-dimensional array (a vector), it's a single integer. For multi-dimensional arrays, it's a list of integers.
* **`:initial-element`**: Specifies the initial value for all elements in the array.
* **`:element-type`**: Specifies the data type of the elements that the array can hold. This can improve efficiency.
* **`:adjustable`**: Specifies whether the array's size can be changed after creation.
* **`:fill-pointer`**: For adjustable arrays, this tracks the number of currently active elements.

**Examples:**

* Creating a one-dimensional array (vector) of 5 elements, initialized to 0:

    ```lisp
    (make-array 5 :initial-element 0)
    ```

* Creating a two-dimensional array (a matrix) of 3 rows and 4 columns:

    ```lisp
    (make-array '(3 4))
    ```

* Creating a vector of 10 strings:

    ```lisp
    (make-array 10 :element-type 'string)
    ```

* Creating a vector of 10 characters:

    ```lisp
    (make-array 10 :element-type 'character)
    ```

* Creating an adjustable vector with a fill pointer:

    ```lisp
    (make-array 5 :adjustable t :fill-pointer 0)
    ```

### 2.2 `aref`: Accessing Array Elements

The `aref` function is used to access elements of an array. It takes the array and the indices as arguments.

**Examples:**

```lisp
(let ((my-vector (make-array 5 :initial-element 0)))
  (setf (aref my-vector 2) 10) ; Sets the element at index 2 to 10
  (print (aref my-vector 2))    ; Prints 10
  (print (aref my-vector 0)))    ; Prints 0
```

For multi-dimensional arrays, you provide multiple indices:

```lisp
(let ((my-matrix (make-array '(3 4))))
  (setf (aref my-matrix 1 2) 42) ; Sets the element at row 1, column 2 to 42
  (print (aref my-matrix 1 2)))    ; Prints 42
```

### 2.3 Multi-dimensional Arrays

As shown above, you create multi-dimensional arrays by providing a list of dimensions to `make-array`. You access elements using `aref` with multiple indices.

```lisp
(let ((cube (make-array '(2 3 4) :initial-element 0))) ; A 2x3x4 array
  (setf (aref cube 1 2 3) 123)
  (print (aref cube 1 2 3))) ; Prints 123
```

### 2.4 Adjustable Arrays

Adjustable arrays can be resized after they are created. You create them using the `:adjustable t` keyword argument to `make-array`.

```lisp
(let ((my-adjustable-vector (make-array 3 :adjustable t :fill-pointer 3 :initial-contents '(1 2 3))))
    (print my-adjustable-vector)
    (array-total-size my-adjustable-vector) ; the total size of the array
    (array-dimensions my-adjustable-vector) ; the dimensions of the array
    (array-has-fill-pointer-p my-adjustable-vector) ; t because it has a fill pointer
    (fill-pointer my-adjustable-vector) ; the fill pointer
    (vector-push-extend 4 my-adjustable-vector) ; adds element to the end and extends the array if needed
    (print my-adjustable-vector)
    (fill-pointer my-adjustable-vector) ; the fill pointer has been increased
    (vector-push-extend 5 my-adjustable-vector)
    (print my-adjustable-vector)
    (array-total-size my-adjustable-vector) ; the total size has been increased
    (fill-pointer my-adjustable-vector)
    (vector-pop my-adjustable-vector) ; removes the last element and decreases the fill pointer
    (print my-adjustable-vector)
    (fill-pointer my-adjustable-vector)
    (vector-push 6 my-adjustable-vector) ; adds the element at the index of the fill pointer and increases the fill pointer by one
    (print my-adjustable-vector)
    (fill-pointer my-adjustable-vector)
)
```

Key functions for working with adjustable arrays:

* **`array-total-size`**: Returns the total number of elements the array *can* hold.
* **`array-dimensions`**: Returns a list of the dimensions of the array.
* **`array-has-fill-pointer-p`**: Checks if the array has a fill pointer.
* **`fill-pointer`**: Returns or sets the fill pointer of an adjustable array.
* **`vector-push-extend`**: Adds an element to the end of an adjustable vector, extending the vector if necessary.
* **`vector-pop`**: Removes the last element of an adjustable vector and decreases the fill pointer.
* **`vector-push`**: Adds an element at the index of the fill pointer and increases the fill pointer by one.

### 2.5 Specialized Arrays

Specialized arrays can hold elements of a specific data type. This can improve performance and reduce memory usage. You specify the element type using the `:element-type` keyword argument to `make-array`.

```lisp
(make-array 10 :element-type 'bit)       ; An array of bits
(make-array 20 :element-type 'character) ; An array of characters
(make-array '(2 2) :element-type 'single-float) ; A 2x2 array of single-precision floating-point numbers
```

Commonly used element types include:

* `bit`
* `character`
* `base-char`
* `single-float`
* `double-float`
* `integer`
* `t` (the most general type, can hold any Lisp object)

Using specialized arrays can be more efficient than using general arrays (with `:element-type 't`) when you know the type of data you will be storing.

This section covered the basics of arrays in Common Lisp, including how to create them, access their elements, and work with multi-dimensional, adjustable, and specialized arrays. Arrays are a powerful and efficient data structure for many programming tasks. Remember that vectors are one-dimensional arrays and therefore all the functions that work on arrays also work on vectors. However there are some functions that only work on vectors, like the ones that use the fill pointer.

## 3. Structures in Common Lisp

Structures in Common Lisp provide a way to define user-defined record types, similar to structs in C or records in Pascal. They allow you to group related data into a single named entity with named fields (slots).

### 3.1 `defstruct`: Defining Structures

The `defstruct` macro is used to define structures. Its basic syntax is:

```lisp
(defstruct structure-name
  slot-name1
  slot-name2
  ...)
```

* **`structure-name`**: A symbol that names the structure.
* **`slot-name1`, `slot-name2`, ...**: Symbols that name the slots (fields) of the structure.

**Example:**

```lisp
(defstruct point
  x
  y)
```

This defines a structure named `point` with two slots: `x` and `y`.

`defstruct` automatically generates several functions:

* **Constructor:** `make-structure-name` (e.g., `make-point`). This function creates instances of the structure.
* **Accessors:** `structure-name-slot-name` (e.g., `point-x`, `point-y`). These functions access the values of the slots.

### 3.2 Structure Accessors

Let's use the `point` structure we defined earlier:

```lisp
(defstruct point
  x
  y)

(let ((p (make-point :x 10 :y 20))) ; Create a point instance
  (print (point-x p)) ; Access the x slot (prints 10)
  (print (point-y p)) ; Access the y slot (prints 20)

  (setf (point-x p) 30) ; Modify the x slot
  (print (point-x p))) ; Prints 30
```

`make-point` takes keyword arguments corresponding to the slot names to initialize the slots. The accessors `point-x` and `point-y` are used to read and write the slot values. `setf` is used to set the value of a slot using the accessor function.

**More `defstruct` Options:**

`defstruct` provides several options to customize the behavior of structures:

* **`:include`**: Allows a structure to inherit slots from another structure.

    ```lisp
    (defstruct (3d-point (:include point)) ; 3d-point inherits from point
      z)

    (let ((p3 (make-3d-point :x 10 :y 20 :z 30)))
      (print (point-x p3)) ; Access inherited slot (prints 10)
      (print (3d-point-z p3))) ; Access z slot (prints 30)
    ```

* **`:conc-name`**: Changes the prefix of the accessor functions.

    ```lisp
    (defstruct (my-point (:conc-name mp-)) ; Accessors will be mp-x, mp-y
      x
      y)

    (let ((p (make-my-point :x 5 :y 7)))
      (print (mp-x p))) ; Prints 5
    ```

* **`:type`**: Specifies the underlying representation of the structure. This can be used for efficiency or to interact with other systems. Common types include `:list`, `:vector`, and others. If no type is specified, the structure is implemented as a record.

    ```lisp
    (defstruct (vector-point (:type vector))
      x
      y)
    (let ((vp (make-vector-point :x 10 :y 20)))
        (print vp) ; prints #((10 20) VECTOR-POINT)
        (print (vector-point-x vp)) ; prints 10
    )
    ```

* **`:named`**: Creates a predicate function `structure-name-p` that tests if an object is an instance of the structure.

    ```lisp
    (defstruct (person (:named person-p))
      name
      age)

    (let ((alice (make-person :name "Alice" :age 30))
          (not-a-person '(a b c)))
      (print (person-p alice))      ; Prints T
      (print (person-p not-a-person))) ; Prints NIL
    ```

* **`:print-function`**: Specifies a function that is used to print the structure.

    ```lisp
    (defstruct (colored-point (:print-function print-colored-point))
        x
        y
        color)
    (defun print-colored-point (point stream depth)
        (declare (ignore depth))
        (format stream "#<Colored Point (~a,~a) ~a>" (colored-point-x point) (colored-point-y point) (colored-point-color point)))
    (let ((cp (make-colored-point :x 10 :y 20 :color "red")))
        (print cp)) ; prints #<Colored Point (10,20) red>
    ```

Structures are a convenient way to create simple data aggregates in Common Lisp. They are less flexible than CLOS classes (which we covered earlier), but they are often simpler to use for basic data structures. If you need more advanced object-oriented features like inheritance, method dispatch, or method combination, you should use CLOS classes.

## 4. Hash Tables in Common Lisp

Hash tables are a powerful data structure in Common Lisp for storing key-value pairs. They provide efficient access to values based on their associated keys.

### 4.1 `make-hash-table`: Creating Hash Tables

The `make-hash-table` function creates a new hash table. It takes several keyword arguments to customize the hash table's behavior:

* **`:test`**: Specifies the equality test used to compare keys. The default is `eql`. Common options include:
  * `eql`: Tests for object identity (the default).
  * `equal`: Tests for structural equality (compares the contents of objects).
  * `eq`: Tests for object identity (stricter than `eql` for numbers and characters).
  * `equalp`: Tests for structural equality, ignoring case for strings and some other minor differences.
* **`:size`**: An initial estimate of the number of entries the hash table will hold. This is a hint to the implementation and doesn't limit the hash table's capacity.
* **`:rehash-size`**: Controls how much the hash table's size is increased when it needs to be rehashed (resized). It can be a positive integer (the number of new buckets) or a floating-point number greater than 1 (a multiplicative factor). The default is implementation-dependent.
* **`:rehash-threshold`**: A floating-point number between 0 and 1 that determines when the hash table is rehashed. When the ratio of the number of entries to the size of the hash table exceeds this threshold, the hash table is rehashed. The default is implementation-dependent.

**Examples:**

* Creating a hash table using the default `eql` test:

    ```lisp
    (make-hash-table)
    ```

* Creating a hash table that uses `equal` for key comparison (useful for comparing strings):

    ```lisp
    (make-hash-table :test #'equal)
    ```

* Creating a hash table with an initial size of 100:

    ```lisp
    (make-hash-table :size 100)
    ```

* Creating a hash table with a rehash-size of 1.5 (increase size by 50% on rehash):

    ```lisp
    (make-hash-table :rehash-size 1.5)
    ```

### 4.2 `gethash` and `setf gethash`: Accessing and Modifying Hash Table Entries

The `gethash` function retrieves the value associated with a key in a hash table. It takes the key and the hash table as arguments. It returns two values: the value associated with the key, and a boolean value indicating whether the key was found in the hash table.

```lisp
(let ((my-hash (make-hash-table)))
  (setf (gethash 'a my-hash) 1)
  (setf (gethash 'b my-hash) 2)

  (multiple-value-bind (value present) (gethash 'a my-hash)
    (format t "Value of a: ~a, Present: ~a~%" value present)) ; Prints "Value of a: 1, Present: T"

  (multiple-value-bind (value present) (gethash 'c my-hash)
    (format t "Value of c: ~a, Present: ~a~%" value present)) ; Prints "Value of c: NIL, Present: NIL"
)
```

`setf gethash` is used to associate a value with a key in a hash table. It takes the value, the key, and the hash table as arguments.

If the key is not found `gethash` returns `nil` as the value and `nil` as the present indicator. You can provide a default value as a third argument to `gethash` that will be returned if the key is not found:

```lisp
(let ((my-hash (make-hash-table)))
  (gethash 'd my-hash 42)) ; returns 42, nil
```

### 4.3 Hash Table Options

We've already covered the main hash table options in the `make-hash-table` section. Here's a quick recap:

* `:test`: Equality test for keys (`eql`, `equal`, `eq`, `equalp`).
* `:size`: Initial size hint.
* `:rehash-size`: How much to increase the size when rehashing.
* `:rehash-threshold`: When to rehash (ratio of entries to size).

Choosing the appropriate `:test` is important. If you are using strings as keys, you should use `:test #'equal` or `:test #'equalp`. If you are using symbols or numbers, `eql` is usually sufficient.

### 4.4 Iterating over Hash Tables

You can iterate over the entries in a hash table using the `maphash` function. It takes a function and the hash table as arguments. The function is called for each key-value pair in the hash table.

```lisp
(let ((my-hash (make-hash-table :test #'equal)))
  (setf (gethash "apple" my-hash) 1)
  (setf (gethash "banana" my-hash) 2)
  (setf (gethash "cherry" my-hash) 3)

  (maphash #'(lambda (key value)
               (format t "Key: ~a, Value: ~a~%" key value))
           my-hash))
```

The output might be in a different order depending on the implementation but will contain:

```lisp
Key: cherry, Value: 3
Key: banana, Value: 2
Key: apple, Value: 1
```

`maphash` is primarily used for side effects (like printing), as it returns `nil`.

Hash tables are a very efficient data structure for lookups, insertions, and deletions when you need to associate values with keys. They are widely used in Common Lisp programming.

## 5. Strings in Common Lisp

Strings in Common Lisp are sequences of characters. They are represented as arrays of characters and are mutable (their contents can be changed).

### 5.1 String Literals

String literals are enclosed in double quotes:

```lisp
"This is a string."
"Another string with \"escaped quotes\"."
"" ; The empty string
```

### 5.2 String Manipulation Functions

Common Lisp provides a rich set of functions for manipulating strings:

* **`concatenate`**: Creates a new string by concatenating two or more strings (or sequences).

    ```lisp
    (concatenate 'string "Hello, " "world!") ; Returns "Hello, world!"
    (concatenate 'string "The number is " 42) ; Type error, 42 is not a sequence. Use format instead for mixed data.
    ```

    For mixed data types use `format`:

    ```lisp
    (format nil "The number is ~a" 42) ; Returns "The number is 42"
    ```

* **`subseq`**: Extracts a subsequence (substring) from a string.

    ```lisp
    (subseq "Hello, world!" 7)    ; Returns "world!" (from index 7 to the end)
    (subseq "Hello, world!" 0 5)  ; Returns "Hello" (from index 0 up to, but not including, index 5)
    ```

* **`string-upcase`**: Creates a new string with all characters converted to uppercase.

    ```lisp
    (string-upcase "hello") ; Returns "HELLO"
    ```

* **`string-downcase`**: Creates a new string with all characters converted to lowercase.

    ```lisp
    (string-downcase "HELLO") ; Returns "hello"
    ```

* **`string=`**: Compares two strings for equality (case-sensitive).

    ```lisp
    (string= "hello" "hello") ; Returns T
    (string= "hello" "Hello") ; Returns NIL
    ```

* **`string-equal`**: Compares two strings for equality (case-insensitive).

    ```lisp
    (string-equal "hello" "Hello") ; Returns T
    ```

* **`string<`, `string>`, `string<=`, `string>=`**: Perform lexicographical comparisons (case-sensitive). There are also case-insensitive versions: `string-lessp`, `string-greaterp`, `string-not-greaterp`, `string-not-lessp`.

    ```lisp
    (string< "apple" "banana") ; Returns T
    (string> "zebra" "apple") ; Returns T
    ```

* **`length`**: Returns the length of a string.

    ```lisp
    (length "Common Lisp") ; Returns 11
    ```

* **`char`**: Accesses a character at a specific index in a string (zero-based).

    ```lisp
    (char "Common Lisp" 7) ; Returns #\L
    ```

* **`string`**: Converts various objects (like symbols or characters) to strings.

    ```lisp
    (string 'symbol) ; Returns "SYMBOL"
    (string #\a)    ; Returns "a"
    ```

## 6. Characters in Common Lisp

Characters in Common Lisp represent individual textual characters.

### 6.1 Character Literals

Character literals are prefixed with `#\`:

```lisp
#\a       ; Lowercase 'a'
#\A       ; Uppercase 'A'
#\Space   ; The space character
#\Newline ; The newline character
#\\       ; The backslash character
```

### 6.2 Character Comparison Functions

Common Lisp provides functions for comparing characters:

* **`char=`**: Compares two characters for equality (case-sensitive).

    ```lisp
    (char= #\a #\a) ; Returns T
    (char= #\a #\A) ; Returns NIL
    ```

* **`char-equal`**: Compares two characters for equality (case-insensitive).

    ```lisp
    (char-equal #\a #\A) ; Returns T
    ```

* **`char<`, `char>`, `char<=`, `char>=`**: Perform lexicographical comparisons (case-sensitive). There are also case-insensitive versions: `char-lessp`, `char-greaterp`, `char-not-greaterp`, `char-not-lessp`.

    ```lisp
    (char< #\a #\b) ; Returns T
    (char> #\z #\a) ; Returns T
    ```

* **`char-code`**: Returns the numeric character code of a character (typically its ASCII or Unicode value).

    ```lisp
    (char-code #\a) ; Returns 97 (in most implementations using ASCII or UTF-8)
    ```

* **`code-char`**: Returns the character corresponding to a given numeric character code.

    ```lisp
    (code-char 97) ; Returns #\a (in most implementations using ASCII or UTF-8)
    ```

This section covered strings and characters, which are fundamental data types for working with text in Common Lisp. Understanding string manipulation functions and character comparison is essential for many programming tasks.

## 7. Symbols in Common Lisp

Symbols are a unique data type in Lisp, distinct from strings, characters, or numbers. They are atomic data objects with a name, a value (which can be anything), a function definition (which can be a function), and a property list (which can store arbitrary properties). This tutorial focuses on their basic use as literals, keys, and identifiers.

### 7.1 Symbol Literals

Symbol literals are typically written as a sequence of alphanumeric characters and certain other characters (like `-`, `+`, `*`, `/`, `<`, `>`, `=`, etc.), without double quotes. By default, symbols are read as uppercase.

```lisp
foo
BAR
my-variable
+addition
*multiplication
<less-than
```

If you need to include lowercase letters or other special characters in a symbol's name, you can escape them with a backslash `\` or enclose the entire symbol in vertical bars `|`.

```lisp
|MixedCaseSymbol|
\|Special Characters!\|
```

When the Lisp reader encounters an unescaped symbol, it *interns* it, meaning it checks if a symbol with that name already exists in the current *package*. If it does, it returns the existing symbol; otherwise, it creates a new one. This ensures that symbols with the same name are `eq` to each other (i.e., they are the same object in memory).

```lisp
(eq 'foo 'foo) ; Returns T
(eq 'foo 'FOO) ; Returns T (symbols are read as uppercase by default)
(eq |Foo| |foo|) ; Returns NIL (because case matters inside vertical bars)
```

The single quote `'` is a shorthand for `(quote ...)`. `(quote foo)` evaluates to the symbol `foo`.

### 7.2 Using Symbols as Keys and Identifiers

Symbols are commonly used in two main ways:

* **As keys in data structures:** Symbols are often used as keys in association lists (alists), property lists (plists), and hash tables. Because of the interning process, comparing symbols for equality using `eq` is very fast.

    ```lisp
    (let ((my-plist '(name "Alice" age 30 city "New York")))
      (getf my-plist 'name)) ; Returns "Alice"

    (let ((my-alist '((name . "Bob") (age . 25))))
        (cdr (assoc 'name my-alist))) ; returns "Bob"

        (let ((my-hash (make-hash-table)))
            (setf (gethash 'country my-hash) "Israel")
            (gethash 'country my-hash)) ; returns "Israel", T
    )
    ```

* **As identifiers (variable and function names):** Symbols are used to name variables, functions, classes, and other program entities.

    ```lisp
    (defvar *my-global-variable* 10) ; *...* is a naming convention for global variables
    (defun my-function (x) (+ x 5))
    ```

    In this context, the symbol represents the entity it names. When you refer to `*my-global-variable*` or `my-function` in your code, the Lisp system looks up the value or function definition associated with that symbol.

**Packages:**

Symbols are organized into *packages*, which are namespaces that help prevent name collisions. By default, when you type a symbol at the top level of the Lisp REPL, it is interned in the `COMMON-LISP-USER` package (often abbreviated as `USER`).

To refer to a symbol in a different package, you use the package prefix `package-name::symbol-name` or `package-name:symbol-name`. The double colon `::` means the symbol is *external* to the package and therefore accessible from other packages. The single colon `:` means the symbol is *internal* to the package and by default not accessible from other packages.

```lisp
CL:list ; Refers to the list function in the CL (COMMON-LISP) package
KEYWORD:test ; Refers to the test keyword in the KEYWORD package.
```

Understanding how symbols are interned and how packages work is essential for writing larger Lisp programs and avoiding name conflicts. For most simple programs you will use the `USER` package and don't need to worry about packages.

This section covered the basics of symbols in Common Lisp, their use as literals, keys, and identifiers, and the concept of packages. Symbols are a fundamental part of the Lisp language and play a crucial role in its flexibility and expressiveness.
