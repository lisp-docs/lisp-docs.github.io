---
sidebar_position: 7
---

# Types

## Introduction to the Common Lisp Type System

Common Lisp possesses a rich and powerful type system that goes far beyond simple static typing found in languages like C or Java. It's a *dynamic* type system, meaning type checking primarily occurs at runtime. However, it also includes features for type declarations that can be used for optimization and static analysis. It is also a *hierarchy* of types, where types can be subtypes and supertypes of each other. The Common Lisp Object System (CLOS) is integrated into the type system, making classes themselves types.

This tutorial will introduce you to the core concepts of the Common Lisp type system, including built-in types, type specifiers, type checking functions, and how types interact with CLOS. We will not reiterate CLOS concepts already covered in the previous tutorial, focusing instead on how types relate to CLOS.

**Key Concepts:**

* **Types and Type Specifiers:** Ways to denote types in Lisp code.
* **Type Hierarchy:** The relationships between different types (subtypes, supertypes).
* **Type Checking:** Functions for testing the type of an object.
* **Type Declarations:** Hints to the compiler for optimization.
* **Type Specifiers and CLOS:** How classes and objects relate to the type system.

**Table of Contents:**

**1. Basic Type Concepts:**

* What is a type?
* Type specifiers.
* The universal type `T` and the empty type `NIL`.

**2. Built-in Types:**

* Numeric types: `integer`, `rational`, `real`, `complex`, `float`.
* Character types: `character`.
* Symbol types: `symbol`.
* List types: `list`, `cons`.
* Array types: `array`, `vector`, `string`.
* Other important types: `null`, `function`, `pathname`.

**3. Type Specifiers:**

* Atomic type specifiers (e.g., `integer`, `string`).
* Compound type specifiers (e.g., `(integer 0 *)`, `(array string (10))`).
* `EQL` type specifiers (e.g., `(eql 5)`, `(eql 'foo)`).
* `MEMBER` type specifiers (e.g., `(member :red :green :blue)`).

**4. Type Checking Functions:**

* `typep`: Testing if an object is of a given type.
* `type-of`: Returning the most specific type of an object.

**5. Type Declarations:**

* `declare` special form.
* Using type declarations for optimization.
* Type inference.

**6. Types and CLOS:**

* Classes as types.
* `typep` with class names.
* `subtypep`: Determining subtype relationships between classes and other types.
* Using `deftype` to create type aliases for class names or other type specifiers.

**7. Examples and Advanced Techniques:**

* Defining custom types using `deftype`.
* Using types for error checking and validation.
* Interactions of types with generic functions and methods.

This tutorial will provide a comprehensive overview of the Common Lisp type system, equipping you with the knowledge to effectively use types in your Lisp programs.

## 1. Basic Type Concepts

This section introduces the fundamental concepts of types in Common Lisp.

### 1.1 What is a Type?

In Common Lisp, a *type* is a set of objects. An object is said to be *of* a particular type if it is a member of that set. Types are used to classify objects and reason about their behavior.

Unlike statically typed languages where type checking is performed at compile time, Common Lisp is dynamically typed. This means that type checking primarily occurs at runtime. When an operation is performed on an object, the Lisp system checks if the object is of the expected type for that operation. If not, a type error is signaled.

For example, the `+` operator expects numeric arguments. If you try to add a number and a string, a type error will occur.

```lisp
(+ 5 "hello") ; This will signal a type error.
```

### 1.2 Type Specifiers

A *type specifier* is a symbolic expression that denotes a type. Type specifiers are used in type declarations, type checking functions, and other contexts where you need to refer to a type.

Some examples of type specifiers include:

* `integer`: Represents the set of all integers.
* `string`: Represents the set of all strings.
* `symbol`: Represents the set of all symbols.
* `(integer 0 10)`: Represents the set of integers between 0 and 10 (inclusive).
* `(vector integer)`: Represents the set of all vectors whose elements are integers.

Type specifiers can be simple atomic symbols (like `integer` or `string`), or they can be more complex compound expressions (like the examples above). We will explore compound type specifiers in more detail later.

### 1.3 The Universal Type `T` and the Empty Type `NIL`

Two special type specifiers play important roles in the Common Lisp type system:

* **`T` (Top):** This is the *universal type*. Every object in Common Lisp is of type `T`. It represents the set of *all* objects. `T` is the supertype of all other types.

    ```lisp
    (typep 5 't)      ; Returns T
    (typep "hello" 't) ; Returns T
    (typep 'foo 't)   ; Returns T
    ```

* **`NIL` (Bottom):** This is the *empty type* (also called *bottom type*). No object is of type `NIL`. It represents the empty set. `NIL` is the subtype of all other types. It is important to distinguish the type `NIL` from the symbol `NIL` which represents logical false, and is the only object of type `NULL`.

    ```lisp
    (typep 5 'nil)      ; Returns NIL
    (typep "hello" 'nil) ; Returns NIL
    (typep 'foo 'nil)   ; Returns NIL

    (typep nil 'null) ; Returns T
    (typep nil 'symbol) ; Returns T
    (typep nil 'list) ; Returns T
    ```

The relationship between types can be visualized as a hierarchy. `T` sits at the top of the hierarchy, and `NIL` sits at the bottom. All other types fall somewhere in between.

For example, `integer` is a subtype of `real`, which is a subtype of `number`, which is a subtype of `T`. `NIL` is a subtype of `integer`, `real`, `number`, and every other type.

Understanding `T` and `NIL` is important for reasoning about type relationships and writing correct type checks. For example, if a function can accept any type of argument, you would use `T` as the argument's type specifier. If a function should never be called with any argument, you might use `NIL` in a type declaration to indicate this (although this is less common).

This section introduced basic type concepts, type specifiers, and the special types `T` and `NIL`. The next section will cover the built-in types in Common Lisp.

## 2. Built-in Types

Common Lisp provides a rich set of built-in types that cover a wide range of data structures and values. This section describes some of the most important built-in types.

### 2.1 Numeric Types

Common Lisp has a comprehensive numeric type hierarchy:

* **`integer`:** Represents whole numbers (e.g., -2, 0, 100). Integers can be arbitrarily large (bignums).

    ```lisp
    (typep 10 'integer)      ; Returns T
    (typep -5 'integer)     ; Returns T
    (typep 123456789012345678901234567890 'integer) ; Returns T (bignum)
    ```

* **`rational`:** Represents numbers that can be expressed as a ratio of two integers (e.g., 1/2, 3/4, -7/3). Integers are a subtype of rationals.

    ```lisp
    (typep 1/2 'rational)   ; Returns T
    (typep 6/3 'rational)   ; Returns T (equivalent to the integer 2)
    (typep 2 'rational) ; Returns T
    ```

* **`real`:** Represents all numbers that can be plotted on a number line. This includes rationals and floating-point numbers.

    ```lisp
    (typep 3.14 'real)      ; Returns T
    (typep 1/3 'real)    ; Returns T
    (typep 5 'real)       ; Returns T
    ```

* **`float`:** Represents floating-point numbers (e.g., 3.14, -0.001, 1.23e10). Common Lisp supports multiple floating-point formats: `short-float`, `single-float`, `double-float`, and `long-float`.

    ```lisp
    (typep 3.14f0 'single-float) ; Returns T
    (typep 3.14d0 'double-float) ; Returns T
    (typep 3.14 'float) ; Returns T
    ```

* **`complex`:** Represents complex numbers, which have a real and an imaginary part (e.g., #C(3 4), #C(0 1)).

    ```lisp
    (typep #C(3 4) 'complex)   ; Returns T
    ```

The numeric types form a subtype hierarchy: `integer` ⊂ `rational` ⊂ `real` ⊂ `complex`. Also, `float` ⊂ `real`.

### 2.2 Character Types

* **`character`:** Represents single characters (e.g., #\A, #\space, #\newline).

    ```lisp
    (typep #\a 'character)   ; Returns T
    (typep #\Space 'character) ; Returns T
    ```

### 2.3 Symbol Types

* **`symbol`:** Represents symbolic names (e.g., `foo`, `BAR`, `my-variable`). Symbols are used as identifiers in Lisp code.

    ```lisp
    (typep 'foo 'symbol)   ; Returns T
    (typep 'Bar123 'symbol) ; Returns T
    (typep nil 'symbol) ; Returns T. nil is a symbol
    ```

### 2.4 List Types

* **`list`:** Represents linked lists of objects. The empty list is represented by `nil`.

    ```lisp
    (typep '(1 2 3) 'list)   ; Returns T
    (typep nil 'list)     ; Returns T (nil is also the empty list)
    ```

* **`cons`:** Represents a cons cell, which is the basic building block of lists. A cons cell has two parts: the `car` (first element) and the `cdr` (rest of the list). A list is either `nil` or a `cons` whose `cdr` is a list.

    ```lisp
    (typep (cons 1 2) 'cons)   ; Returns T
    (typep '(1 . 2) 'cons)   ; Returns T
    (typep '(1 2) 'cons)   ; Returns T
    ```

`list` is equivalent to `(or null cons)`. `null` is a subtype of `symbol` and only has one element: `nil`.

### 2.5 Array Types

* **`array`:** Represents multi-dimensional arrays of objects.

    ```lisp
    (typep #(1 2 3) 'array)           ; Returns T
    (typep (make-array '(3 3)) 'array) ; Returns T
    ```

* **`vector`:** Represents one-dimensional arrays. It is a subtype of `array`.

    ```lisp
    (typep #(1 2 3) 'vector) ; Returns T
    ```

* **`string`:** Represents sequences of characters. It is a specialized type of vector.

    ```lisp
    (typep "hello" 'string) ; Returns T
    ```

`string` is equivalent to `(vector character)`. `vector` is equivalent to `(array t (*))`.

### 2.6 Other Important Types

* **`null`:** Represents the type containing only the symbol `nil`. It is a subtype of `symbol` and `list`.

    ```lisp
    (typep nil 'null) ; Returns T
    ```

* **`function`:** Represents functions.

    ```lisp
    (typep #'+ 'function) ; Returns T
    (typep #'(lambda (x) (* x x)) 'function) ; Returns T
    ```

* **`pathname`:** Represents file and directory names.

    ```lisp
    (typep #p"/tmp/foo.txt" 'pathname) ; Returns T (implementation-dependent)
    ```

This section covered some of the most important built-in types in Common Lisp. Understanding these types is crucial for writing correct and efficient Lisp code. The next section will cover type specifiers in more detail, including compound type specifiers.

## 3. Type Specifiers

Type specifiers are symbolic expressions that denote types in Common Lisp. They are used in various contexts, including type declarations, type checking, and function signatures. This section explores different kinds of type specifiers.

### 3.1 Atomic Type Specifiers

*Atomic type specifiers* are simple symbols that represent built-in types. We've already seen several examples in the previous section:

* `integer`
* `rational`
* `real`
* `complex`
* `float`
* `character`
* `symbol`
* `list`
* `cons`
* `array`
* `vector`
* `string`
* `null`
* `function`
* `pathname`

These symbols directly name the corresponding built-in types.

### 3.2 Compound Type Specifiers

*Compound type specifiers* are more complex expressions that allow you to specify more precise types. They are typically lists where the first element is a type specifier and subsequent elements provide additional constraints.

* **(INTEGER * *):** Represents all integers (equivalent to `integer`).

    ```lisp
    (typep 10 '(integer * *)) ; Returns T
    ```

* **(INTEGER 0 *):** Represents non-negative integers (integers greater than or equal to 0).

    ```lisp
    (typep 5 '(integer 0 *))  ; Returns T
    (typep 0 '(integer 0 *))  ; Returns T
    (typep -1 '(integer 0 *)) ; Returns NIL
    ```

* **(INTEGER 0 10):** Represents integers between 0 and 10 (inclusive).

    ```lisp
    (typep 5 '(integer 0 10)) ; Returns T
    (typep 11 '(integer 0 10)); Returns NIL
    ```

* **(FLOAT 0.0 1.0):** Represents floating-point numbers between 0.0 and 1.0 (exclusive).

    ```lisp
    (typep 0.5 '(float 0.0 1.0)) ; Returns T
    (typep 1.0 '(float 0.0 1.0)) ; Returns NIL
    ```

* **(ARRAY T (10)):** Represents a one-dimensional array (vector) of 10 elements of any type.

    ```lisp
    (typep #(1 2 3 4 5 6 7 8 9 10) '(array t (10))) ; Returns T
    (typep #(1 2 3) '(array t (10))) ; Returns NIL
    ```

* **(ARRAY STRING (3 4)):** Represents a two-dimensional array of strings with dimensions 3x4.

    ```lisp
    (typep (make-array '(3 4) :element-type 'string :initial-element "hello") '(array string (3 4))) ; Returns T
    ```

* **(VECTOR INTEGER):** Represents a one dimensional array of integers.

    ```lisp
    (typep #(1 2 3) '(vector integer)) ; Returns T
    (typep #(1 2 "3") '(vector integer)) ; Returns NIL
    ```

* **(STRING 10):** Represents a string of length 10.

    ```lisp
    (typep "0123456789" '(string 10)) ; Returns T
    (typep "0123" '(string 10)) ; Returns NIL
    ```

* **(SIMPLE-VECTOR 10):** Represents a simple vector of length 10. Simple vectors are optimized vectors that can only hold objects of a single type.

* **(SIMPLE-ARRAY BIT (10 20)):** Represents a simple two dimensional array of bits of dimensions 10x20.

### 3.3 `EQL` Type Specifiers

The `EQL` type specifier allows you to specify a type that contains only a single specific object.

* **(EQL 5):** Represents the type containing only the integer 5.

    ```lisp
    (typep 5 '(eql 5))   ; Returns T
    (typep 6 '(eql 5))   ; Returns NIL
    ```

* **(EQL 'foo):** Represents the type containing only the symbol `foo`.

    ```lisp
    (typep 'foo '(eql 'foo)) ; Returns T
    (typep 'bar '(eql 'foo)) ; Returns NIL
    ```

`EQL` type specifiers are often used in `CASE` statements and method specializers.

### 3.4 `MEMBER` Type Specifiers

The `MEMBER` type specifier allows you to define a type as a set of specific objects.

* **(MEMBER :red :green :blue):** Represents the type containing only the keywords `:red`, `:green`, and `:blue`.

    ```lisp
    (typep :red '(member :red :green :blue))   ; Returns T
    (typep :yellow '(member :red :green :blue)) ; Returns NIL
    ```

`MEMBER` type specifiers are useful for defining enumerated types or when a variable can only hold a limited set of values.

This section covered different types of type specifiers, including atomic, compound, `EQL`, and `MEMBER` specifiers. These specifiers provide a powerful and flexible way to express types in Common Lisp. The next section will cover type checking functions.

## 4. Type Checking Functions

This section describes the primary functions used for type checking in Common Lisp: `typep` and `type-of`.

### 4.1 `typep`: Testing if an Object is of a Given Type

The `typep` function is used to test if an object is of a given type. Its syntax is:

```lisp
(typep object type-specifier)
```

* **`object`**: The object to test.
* **`type-specifier`**: The type specifier to test against.

`typep` returns `T` if the object is of the specified type, and `NIL` otherwise.

```lisp
(typep 10 'integer)             ; Returns T
(typep 3.14 'float)             ; Returns T
(typep "hello" 'string)           ; Returns T
(typep '(1 2 3) 'list)            ; Returns T
(typep 'foo 'symbol)             ; Returns T
(typep 10 '(integer 0 100))     ; Returns T
(typep 200 '(integer 0 100))    ; Returns NIL
(typep :red '(member :red :green :blue)) ; Returns T
(typep 5 '(eql 5))               ; Returns T
(typep 5 '(eql 6))               ; Returns NIL
(typep #(1 2 3) '(vector integer)) ; Returns T
(typep #(1 2 "3") '(vector integer)) ; Returns NIL
(typep (make-array '(3 4) :element-type 'string) '(array string (3 4))) ; Returns T
(typep (make-array '(3 4) :element-type 'string) 'array) ; Returns T
(typep (make-array '(3 4)) 'array) ; Returns T
```

`typep` handles the type hierarchy correctly. If a type `A` is a subtype of type `B`, and an object is of type `A`, then `typep` will also return `T` when testing if the object is of type `B`.

```lisp
(typep 5 'real)     ; Returns T (integer is a subtype of real)
(typep "hello" 'sequence) ; Returns T (string is a subtype of sequence)
```

### 4.2 `type-of`: Returning the Most Specific Type of an Object

The `type-of` function returns the *most specific* type of an object. This is often a more precise type than what `typep` would indicate.

```lisp
(type-of 10)       ; Returns INTEGER
(type-of 3.14)     ; Returns SINGLE-FLOAT (or DOUBLE-FLOAT on some implementations)
(type-of "hello")   ; Returns (SIMPLE-ARRAY CHARACTER (5)) or STRING
(type-of '(1 2 3))  ; Returns CONS
(type-of 'foo)     ; Returns SYMBOL
(type-of nil)      ; Returns NULL
(type-of #())      ; Returns (SIMPLE-ARRAY T (0))
(type-of #(1 2 3))  ; Returns (SIMPLE-ARRAY FIXNUM (3)) or (SIMPLE-ARRAY INTEGER (3))
(type-of (make-array '(2 3))) ; Returns (SIMPLE-ARRAY T (2 3))
```

The exact return value of `type-of` can be implementation-dependent, especially for array types. For example, some implementations might return `STRING` for string objects, while others might return a more specific array type like `(SIMPLE-ARRAY CHARACTER (*))`.

`type-of` is useful for introspection and debugging when you need to know the precise type of an object.

**Key Differences between `typep` and `type-of`:**

* `typep` tests if an object is *of* a given type (including supertypes). It returns a boolean (`T` or `NIL`).
* `type-of` returns the *most specific* type of an object. It returns a type specifier.

In most cases, `typep` is the function you'll use for type checking in your code. `type-of` is more useful for introspection and debugging.

## 5. Type Declarations

Type declarations in Common Lisp are used to provide hints to the compiler about the types of variables, function arguments, and return values. They are primarily used for optimization and can also be used for static analysis and documentation.

### 5.1 `declare` Special Form

The `declare` special form is used to introduce type declarations. It can be used in various contexts, including:

* Inside `let` forms: To declare the types of local variables.
* At the top level of a function definition: To declare the types of function arguments and the return value.
* Globally at top level.

The basic syntax of `declare` is:

```lisp
(declare (type type-specifier variable1 variable2 ...))
```

**Examples:**

```lisp
(let ((x 10))
  (declare (type integer x)) ; Declare x as an integer
  (+ x 5))

(defun my-function (x y)
  (declare (type integer x y)
           (values integer)) ; Declare x and y as integers, and the return value as an integer
  (+ x y))

(defun my-string-function (x)
    (declare (type string x)
             (values string))
    (concatenate 'string x " suffix"))

(defconstant +my-global-constant+ 10
    "A global constant"
    (declare (type integer +my-global-constant+)))
```

Multiple declarations can be combined within a single `declare` form:

```lisp
(declare (type integer x y) (type string z))
```

### 5.2 Using Type Declarations for Optimization

The primary purpose of type declarations is to provide information to the compiler that can be used for optimization. By knowing the types of variables, the compiler can generate more efficient code.

For example, if the compiler knows that a variable is always an integer, it can use specialized integer arithmetic instructions instead of more general (and slower) arithmetic operations.

However, it's important to understand that type declarations in Common Lisp are *not* mandatory. The Lisp system will still function correctly even without type declarations. If a type declaration is incorrect, a type error will be signaled at runtime.

### 5.3 Type Inference

Common Lisp implementations often perform some level of *type inference*. This means that the compiler can sometimes deduce the types of variables even without explicit type declarations.

For example, if you have the expression `(+ 5 10)`, the compiler can infer that the result will be an integer, even if no type declarations are present.

Type inference can reduce the need for explicit type declarations in many cases. However, providing type declarations can still be beneficial for optimization and for making your code more readable and maintainable.

This section covered type checking functions (`typep` and `type-of`) and type declarations (`declare`). These tools are essential for working with types in Common Lisp. The next section will cover how types interact with CLOS.

## 6. Types and CLOS

This section explains how the Common Lisp Object System (CLOS) integrates with the type system.

### 6.1 Classes as Types

In CLOS, classes are themselves types. This means that you can use class names as type specifiers. If an object is an instance of a class, it is also considered to be of that class's type.

```lisp
(defclass person ()
  ((name :initarg :name :accessor person-name)))

(let ((p (make-instance 'person :name "Alice")))
  (typep p 'person)) ; Returns T
```

In this example, `person` is both a class name and a type specifier. The `typep` function correctly determines that the object `p` is of type `person`.

### 6.2 `typep` with Class Names

You can use `typep` with class names just like with any other type specifier. `typep` handles the class hierarchy correctly, meaning if `B` is a subclass of `A`, then an instance of `B` is also considered to be of type `A`.

```lisp
(defclass employee (person)
  ((employee-id :initarg :id :accessor employee-id)))

(let ((e (make-instance 'employee :name "Bob" :id 123)))
  (typep e 'employee)) ; Returns T
(typep e 'person)   ; Returns T (employee is a subclass of person)
```

### 6.3 `subtypep`: Determining Subtype Relationships Between Classes and Other Types

The `subtypep` function is used to determine if one type is a subtype of another. Its syntax is:

```lisp
(subtypep type-specifier1 type-specifier2 &optional environment)
```

It returns two values:

1. A boolean indicating whether `type-specifier1` is a subtype of `type-specifier2`.
2. A boolean indicating whether the subtype relationship could be determined definitely. This second value is important because some subtype relationships can only be determined at runtime.

```lisp
(subtypep 'integer 'real) ; Returns T, T
(subtypep 'real 'integer) ; Returns NIL, T
(subtypep 'employee 'person) ; Returns T, T
(subtypep 'person 'employee) ; Returns NIL, T
(subtypep '(integer 0 10) 'integer) ; Returns T, T
(subtypep 'null 'list) ; returns T, T
(subtypep 'null 'symbol) ; returns T, T
(subtypep 'list 'null) ; returns NIL, T
```

`subtypep` is particularly useful when working with class hierarchies and complex type specifiers.

### 6.4 Using `deftype` to Create Type Aliases for Class Names or Other Type Specifiers

The `deftype` macro allows you to create new type specifiers as aliases for existing ones. This can make your code more readable and maintainable, especially when dealing with complex type specifiers or class names.

```lisp
(deftype positive-integer () '(integer 1 *)) ; Define positive-integer
(deftype my-string () 'string) ; Define my-string as an alias for string

(typep 5 'positive-integer) ; Returns T
(typep 0 'positive-integer) ; Returns NIL
(typep "hello" 'my-string)  ; Returns T

(defclass my-class () ())
(deftype my-class-type () 'my-class)

(typep (make-instance 'my-class) 'my-class-type) ; Returns T
```

`deftype` can also be used to create more complex type specifiers by using a lambda list:

```lisp
(deftype range (min max) `(integer ,min ,max))

(typep 5 '(range 0 10))   ; Returns T
(typep 15 '(range 0 10))  ; Returns NIL
```

In this example, `range` is a type specifier that takes two arguments, `min` and `max`, and creates an `INTEGER` type specifier with the specified range.

Using `deftype` with classes can be helpful for creating more descriptive type names or for abstracting away implementation details. For example, if you later decide to change the implementation of `my-class`, you can simply change the definition of `my-class-type` without having to modify all the code that uses it.

This section explained how types and CLOS are integrated, how to use `typep` with class names, how to determine subtype relationships with `subtypep`, and how to create type aliases with `deftype`. The next section will cover some examples and advanced techniques using types.

## 7. Examples and Advanced Techniques

This section demonstrates some practical applications and advanced techniques related to the Common Lisp type system.

### 7.1 Defining Custom Types Using `deftype`

We've already seen a basic example of `deftype` in the previous section. Here are some more advanced examples:

**1. Defining a type for even integers:**

```lisp
(deftype even-integer ()
  `(integer * ,(1- most-positive-fixnum) 2))
; or
(deftype even-integer ()
 `(satisfies evenp))
```

The first version uses a compound type specifier with a `mod` test. The second version uses the `satisfies` type specifier, which takes a function and defines a type of all objects for which the function returns true.

```lisp
(typep 4 'even-integer)  ; Returns T
(typep 5 'even-integer)  ; Returns NIL
(typep -2 'even-integer) ; Returns T
```

**2. Defining a type for RGB colors:**

```lisp
(deftype rgb-color ()
  `(vector (integer 0 255) 3))

(typep #(255 0 0) 'rgb-color)   ; Returns T (Red)
(typep #(0 255 0) 'rgb-color)   ; Returns T (Green)
(typep #(0 0 255) 'rgb-color)   ; Returns T (Blue)
(typep #(255 0 0 1) 'rgb-color) ; Returns NIL (wrong number of elements)
(typep #(255 0 0.5) 'rgb-color) ; Returns NIL (wrong element type)
```

This defines an `rgb-color` type as a vector of three integers between 0 and 255.

### 7.2 Using Types for Error Checking and Validation

Types can be used for error checking and validation, both statically (with type declarations) and dynamically (with `typep`).

**1. Type declarations for static checking:**

While type declarations are primarily for optimization, some Lisp implementations or external tools can use them for static analysis to catch potential type errors at compile time.

```lisp
(defun my-add (x y)
  (declare (type integer x y)
           (values integer))
  (+ x y))

(my-add 5 10)    ; Works fine
;(my-add 5 "hello") ; Might produce a warning or error during compilation or static analysis
```

**2. `typep` for dynamic checking:**

`typep` can be used to perform explicit type checks at runtime and handle potential type errors gracefully.

```lisp
(defun safe-add (x y)
  (if (and (typep x 'integer) (typep y 'integer))
      (+ x y)
      (error "Arguments must be integers.")))

(safe-add 5 10)    ; Returns 15
(safe-add 5 "hello") ; Signals an error: "Arguments must be integers."
```

**3. Using `check-type` for concise type checking and error signaling:**

The `check-type` macro simplifies type checking and error signaling. Its syntax is:

```lisp
(check-type place type-specifier &optional string)
```

* `place`: A place (a variable or a generalized variable like `(aref array index)`).
* `type-specifier`: The expected type.
* `string`: An optional error message.

If the value of `place` is not of the specified type, `check-type` signals a correctable `type-error` condition.

```lisp
(defun safe-add (x y)
  (check-type x integer "X must be an integer.")
  (check-type y integer "Y must be an integer.")
  (+ x y))

(safe-add 5 10)    ; Returns 15
(safe-add 5 "hello") ; Signals a correctable TYPE-ERROR.
```

### 7.3 Interactions of Types with Generic Functions and Methods

Types play an important role in method dispatch in CLOS. When a generic function is called, CLOS uses the types of the arguments to determine which method to apply.

**1. Specializers:**

Methods are specialized on types. The *specializers* in a `defmethod` form specify the types of the arguments for which the method is applicable.

```lisp
(defgeneric greet (x))

(defmethod greet ((x string))
  (format t "Hello, ~a!~%" x))

(defmethod greet ((x integer))
  (format t "Number ~a!~%" x))

(greet "Alice") ; Prints "Hello, Alice!"
(greet 10)      ; Prints "Number 10!"
```

In this example, the `greet` generic function has two methods: one specialized on `string` and one specialized on `integer`. CLOS dispatches to the appropriate method based on the type of the argument.

**2. Method precedence:**

When multiple methods are applicable, CLOS uses the class precedence list (CPL) to determine which method is most specific. The CPL is based on the subtype relationships between the specializers.

```lisp
(defclass animal () ())
(defclass dog (animal) ())

(defgeneric speak (x))

(defmethod speak ((x animal))
  (format t "Generic animal sound~%"))

(defmethod speak ((x dog))
  (format t "Woof!~%"))

(speak (make-instance 'animal)) ; Prints "Generic animal sound"
(speak (make-instance 'dog))    ; Prints "Woof!" (dog is more specific than animal)
```

This section provided examples of defining custom types, using types for error checking and validation, and how types interact with generic functions and methods. Understanding these concepts is essential for writing robust and efficient Common Lisp code.
