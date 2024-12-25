---
sidebar_position: 2
---

# MetaObject Protocol

:::warning
This article **needs to be checked!**.
Please help contribute by checking it and making changes in [our repository](https://github.com/lisp-docs/lisp-docs.github.io) or by clicking on the "Edit this page" link below.
:::

## Introduction to the Metaobject Protocol (MOP) in Common Lisp

The Metaobject Protocol (MOP) is a powerful and advanced feature of the Common Lisp Object System (CLOS) that allows you to introspect and customize the behavior of the object system itself. It provides a standardized interface for accessing and modifying the internal workings of classes, methods, generic functions, and other core components of CLOS.

Think of it this way: CLOS defines how objects behave, while the MOP defines how CLOS itself behaves. This meta-level control allows for incredibly flexible and powerful metaprogramming techniques, enabling you to tailor CLOS to specific needs, implement advanced object-oriented features, or even create entirely new object models on top of CLOS.

This tutorial will introduce you to the fundamental concepts of the MOP, including metaobjects, standard metaobject classes, and the key protocols for customizing CLOS behavior.

**Key Concepts:**

* **Metaobjects:** Objects that represent the components of CLOS (e.g., classes, methods, generic functions).
* **Standard Metaobject Classes:** Predefined classes for metaobjects (e.g., `standard-class`, `standard-method`, `standard-generic-function`).
* **Metaobject Protocols:** Standardized interfaces for accessing and manipulating metaobjects.
* **Introspection:** Examining the structure and behavior of CLOS entities.
* **Customization:** Modifying the default behavior of CLOS.

**Table of Contents:**

**1. Introduction to Metaobjects:**

* What are metaobjects?
* The relationship between objects and metaobjects.
* The concept of reification.

**2. Standard Metaobject Classes:**

* `standard-class`: The default metaclass for user-defined classes.
* `standard-method`: The default metaclass for methods.
* `standard-generic-function`: The default metaclass for generic functions.
* Other important metaobject classes (e.g., `slot-definition`, `method-combination`).

**3. Basic MOP Operations:**

* `class-of`: Getting the class of an object.
* `class-name`: Getting the name of a class.
* `find-class`: Finding a class object by name.
* `slot-value`: Accessing slot values (even for metaobjects).

**4. Customizing Class Creation:**

* Defining custom metaclasses using `defclass`.
* Customizing `initialize-instance` for metaclasses.
* Adding or modifying slots during class creation.

**5. Customizing Method Dispatch:**

* Method combinations and their metaobject representation.
* Defining custom method combinations.
* Customizing method selection.

**6. Introspection and Reflection:**

* Inspecting class structure (slots, superclasses).
* Examining method definitions and specializers.
* Working with generic function metaobjects.

**7. Examples and Advanced Techniques:**

* Implementing a simple object database.
* Implementing aspects using the MOP.
* Creating a new object model on top of CLOS.

This tutorial will provide clear explanations and practical examples to help you understand the MOP and its capabilities. It will empower you to leverage the full power of CLOS and perform advanced metaprogramming in Common Lisp. It’s important to note that the MOP is an advanced topic and requires a solid understanding of CLOS itself.

## 1. Introduction to Metaobjects

The Metaobject Protocol (MOP) provides a way to programmatically access and manipulate the structure and behavior of the Common Lisp Object System (CLOS) itself. This is achieved through *metaobjects*.

### 1.1 What are Metaobjects?

In standard object-oriented programming, *objects* are instances of *classes*. A class defines the structure (slots) and behavior (methods) of its instances. In CLOS, even classes, methods, and generic functions are themselves objects. These objects that represent the components of the object system are called *metaobjects*.

Think of it this way:

* **Objects:** Represent data and have behavior.
* **Metaobjects:** Represent the *definitions* and *behavior* of objects, classes, methods and generic functions. They describe how the object system works.

For example:

* A *class* is a metaobject that describes the structure and behavior of its instances (regular objects).
* A *method* is a metaobject that describes a specific behavior of a generic function for a particular set of argument specializers.
* A *generic function* is a metaobject that dispatches method calls based on the classes of the arguments.

### 1.2 The Relationship Between Objects and Metaobjects

The relationship between objects and metaobjects is a *meta-level* relationship. An object is an *instance* of a class. A class is an *instance* of a *metaclass*.

Here’s an analogy:

Imagine you have a house (an object). The blueprint for the house is like a class. But who designed the blueprint? An architect. The architect’s design principles and tools are like a metaclass.

In CLOS:

* `(make-instance 'my-class)` creates an *object*.
* `defclass` creates a *class* (which is a metaobject).
* The default metaclass (`standard-class`) defines how classes are created and behave.

A class is an instance of a meta-class, therefore we can say that a meta-class is a class whose instances are classes.

### 1.3 The Concept of Reification

*Reification* is the process of making something that was previously implicit or abstract explicit and accessible as a data structure. In the context of the MOP, reification means that the components of CLOS (classes, methods, generic functions, etc.) are made available as first-class objects (metaobjects) that can be manipulated programmatically.

Before the MOP, these components were part of the implementation of CLOS and were not directly accessible to the programmer. The MOP reified these components, making them available as objects that can be inspected, modified, and even replaced.

This reification allows for powerful metaprogramming techniques, such as:

* **Customizing class creation:** You can define custom metaclasses to control how classes are created, adding or modifying slots, changing inheritance behavior, and more.
* **Customizing method dispatch:** You can define custom method combinations or alter the method selection process.
* **Implementing new object models:** You can build entirely new object systems on top of CLOS, with different inheritance mechanisms, dispatch strategies, and other features.

**Example:**

When you define a class using `defclass`:

```lisp
(defclass person ()
  ((name :initarg :name :accessor person-name)
   (age :initarg :age :accessor person-age)))
```

Behind the scenes, CLOS uses the `standard-class` metaclass (by default) to create a class object representing `person`. This class object is a metaobject that contains information about the class, such as its name, its superclasses (in this case, `standard-object`), its slots, and its methods. The MOP allows you to access and manipulate this class object directly.

In summary, metaobjects are objects that represent the components of CLOS. Reification makes these components accessible to the programmer, enabling powerful metaprogramming. This introduction lays the groundwork for understanding how to use the MOP to customize and extend CLOS. In the next section, we will explore the standard metaobject classes.

## 2. Standard Metaobject Classes

This section introduces the most important standard metaobject classes in CLOS. These classes represent the core components of the object system and provide the foundation for customization through the MOP.

### 2.1 `standard-class`: The Default Metaclass for User-Defined Classes

`standard-class` is the default metaclass for classes defined using `defclass` when no explicit metaclass is specified. It provides the standard behavior for class creation, inheritance, and instance creation.

When you define a class like this:

```lisp
(defclass person ()
  ((name :initarg :name :accessor person-name)
   (age :initarg :age :accessor person-age)))
```

CLOS creates an instance of `standard-class` to represent the `person` class. This instance contains information about:

* The class name (`person`).
* The superclasses (in this case, `standard-object` by default).
* The slots (`name` and `age`).
* The class precedence list (CPL), which determines the order in which methods are applied.

You can access the class object itself using `find-class`:

```lisp
(find-class 'person) ; Returns the class object for PERSON.
```

You can check if an object is an instance of `standard-class` using `typep`:

```lisp
(typep (find-class 'person) 'standard-class) ; Returns T
```

### 2.2 `standard-method`: The Default Metaclass for Methods

`standard-method` is the default metaclass for methods defined using `defmethod`. It represents a specific implementation of a generic function for a particular set of argument specializers.

When you define a method like this:

```lisp
(defmethod greet ((p person))
  (format t "Hello, ~a!~%" (person-name p)))
```

CLOS creates an instance of `standard-method` to represent this method. This instance contains information about:

* The generic function it belongs to (`greet`).
* The specializers (in this case, `(person)`).
* The method's body (the `format` expression).

You can access the method object through the generic function’s method list using `method-combination` and `compute-applicable-methods`.

```lisp
(defgeneric greet (object))
(defmethod greet ((p person))
  (format t "Hello, ~a!~%" (person-name p)))

(let ((gf (fdefinition 'greet))) ; get the generic function object
    (print (method-combination gf)) ; prints STANDARD
    (print (compute-applicable-methods gf (list (make-instance 'person :name "Bob")))) ; prints a list containing the method
)
```

### 2.3 `standard-generic-function`: The Default Metaclass for Generic Functions

`standard-generic-function` is the default metaclass for generic functions defined using `defgeneric`. It manages the dispatch of method calls based on the classes of the arguments.

When you define a generic function like this:

```lisp
(defgeneric greet (object))
```

CLOS creates an instance of `standard-generic-function`. This instance contains information about:

* The generic function's name (`greet`).
* The lambda list (the parameter list).
* The method combination (by default, `standard`).
* The methods associated with the generic function.

You can access the generic function object using `fdefinition`:

```lisp
(fdefinition 'greet) ; Returns the generic function object for GREET.
```

You can check if an object is a generic function using `fboundp` and `functionp`:

```lisp
(fboundp 'greet) ; Returns T
(functionp (fdefinition 'greet)) ; Returns T
```

### 2.4 Other Important Metaobject Classes

Besides the core metaobject classes mentioned above, there are other important metaobject classes that play a role in CLOS:

* **`slot-definition`**: Represents a slot in a class. Instances of `standard-class` have a list of `slot-definition` objects that describe the slots of the class.
* **`method-combination`**: Represents a method combination type (e.g., `standard`, `+`, `append`). Method combinations define how methods are combined when multiple methods are applicable to a generic function call.

These metaobject classes are essential for customizing the behavior of CLOS at a deeper level. By understanding their structure and behavior, you can implement advanced metaprogramming techniques.

This section introduced the standard metaobject classes in CLOS. Understanding these classes is crucial for working with the MOP. The next sections will cover how to use the MOP to customize class creation, method dispatch, and other aspects of CLOS.

## 3. Basic MOP Operations

This section covers some fundamental operations for working with metaobjects in Common Lisp. These operations allow you to introspect and access information about classes, objects, and their components.

### 3.1 `class-of`: Getting the Class of an Object

The `class-of` function returns the class of an object. This is a basic but essential MOP operation.

```lisp
(defclass person ()
  ((name :initarg :name :accessor person-name)))

(let ((p (make-instance 'person :name "Alice")))
  (class-of p)) ; Returns the class object for PERSON.
```

The result of `(class-of p)` is the *class object* itself, not the symbol `'person`. You can compare it using `eq`:

```lisp
(eq (class-of p) (find-class 'person)) ; Returns T
```

### 3.2 `class-name`: Getting the Name of a Class

The `class-name` function returns the name of a class (as a symbol).

```lisp
(defclass person () ())

(class-name (find-class 'person)) ; Returns PERSON
```

If the class is anonymous, the function might return `nil`.

### 3.3 `find-class`: Finding a Class Object by Name

The `find-class` function takes a symbol representing a class name and returns the corresponding class object.

```lisp
(defclass person () ())

(find-class 'person) ; Returns the class object for PERSON.
```

If the class is not found, `find-class` returns `nil`. You can specify a second optional argument which is a boolean value indicating whether an error should be signaled if the class is not found. The default is `t`.

```lisp
(find-class 'non-existent-class) ; Signals a simple error
(find-class 'non-existent-class nil) ; Returns nil
```

`find-class` searches the current *class environment*, which is influenced by packages. By default, it searches the current package.

### 3.4 `slot-value`: Accessing Slot Values (Even for Metaobjects)

The `slot-value` function is used to access the value of a slot in an object. Importantly, it can also be used to access slots of *metaobjects*. This is a key aspect of the MOP, as it allows you to inspect and manipulate the internal state of classes, methods, and generic functions.

```lisp
(defclass person ()
  ((name :initarg :name :accessor person-name)))

(let ((p (make-instance 'person :name "Bob")))
  (slot-value p 'name)) ; Returns "Bob"

; Accessing slots of a metaobject (class):
(let ((person-class (find-class 'person)))
  (slot-value person-class 'name)) ; This would signal an error, because the class object does not have a slot named name

(defclass my-class ()
    ((my-slot :initform 42)))

(let ((my-class-object (find-class 'my-class)))
    (print (slot-value my-class-object 'slots)) ; prints a list of slot definitions
    (let ((my-slot-definition (first (slot-value my-class-object 'slots))))
        (print (slot-value my-slot-definition 'name)) ; prints MY-SLOT
        (print (slot-value my-slot-definition 'initform)) ; prints 42
    )
)
```

In the last example, we accessed the `slots` slot of the `my-class` class object. This slot contains a list of `slot-definition` metaobjects, which themselves have slots like `name` and `initform`. This demonstrates how `slot-value` can be used to navigate the metaobject hierarchy.

It’s important to note that accessing slots of metaobjects directly using `slot-value` can be implementation-dependent. The MOP provides a more portable and standardized way to access metaobject information through *metaobject protocols* (generic functions defined on metaobject classes), which we will cover in later sections. However, `slot-value` remains a useful tool for basic introspection and is often used in MOP implementations.

These basic MOP operations (`class-of`, `class-name`, `find-class`, `slot-value`) provide the foundation for more advanced metaprogramming techniques. They allow you to inspect and understand the structure of your CLOS objects and classes. The following sections will build upon these basics to show you how to customize class creation and method dispatch.

## 4. Customizing Class Creation

This section explains how to customize the process of class creation in CLOS using the Metaobject Protocol. This involves defining custom metaclasses and customizing the `initialize-instance` method for these metaclasses.

### 4.1 Defining Custom Metaclasses using `defclass`

A *metaclass* is a class whose instances are classes. You define a custom metaclass using `defclass`, just like defining any other class, but you typically inherit from `standard-class` (or another appropriate metaclass).

```lisp
(defclass my-metaclass (standard-class)
  ((extra-info :initarg :extra-info :accessor extra-info)))
```

This defines a metaclass called `my-metaclass` with an additional slot called `extra-info`.

To use a custom metaclass for a class, you specify the `:metaclass` option in the `defclass` form:

```lisp
(defclass my-class ()
  ((a :initarg :a :accessor my-class-a))
  (:metaclass my-metaclass)
  (:extra-info "Some metadata"))
```

Now, the `my-class` class is an instance of `my-metaclass`, not `standard-class`. You can access the `extra-info` slot of the `my-class` class object:

```lisp
(slot-value (find-class 'my-class) 'extra-info) ; Returns "Some metadata"
```

### 4.2 Customizing `initialize-instance` for Metaclasses

The `initialize-instance` method is called when a new instance of a class is created (including when a new class is created, since classes are instances of metaclasses). By specializing `initialize-instance` on your custom metaclass, you can customize the class creation process.

```lisp
(defclass my-metaclass (standard-class)
  ((creation-time :accessor creation-time)))

(defmethod initialize-instance ((class my-metaclass) &rest initargs)
  (call-next-method) ; Call the standard initialize-instance method
  (setf (creation-time class) (get-universal-time))
  class)

(defclass my-class ()
  ()
  (:metaclass my-metaclass))

(creation-time (find-class 'my-class)) ; Returns the time when my-class was created.
```

In this example, we specialize `initialize-instance` on `my-metaclass`. We first call `call-next-method` to ensure that the standard initialization process is performed. Then, we set the `creation-time` slot of the class object.

You can use `&key` parameters in the `initialize-instance` method to handle custom options passed to `defclass`:

```lisp
(defclass my-metaclass (standard-class)
  ((custom-option :initarg :custom-option :accessor custom-option)))

(defmethod initialize-instance ((class my-metaclass) &rest initargs &key custom-option &allow-other-keys)
  (call-next-method)
  (when custom-option
    (setf (custom-option class) custom-option))
  class)

(defclass my-class ()
  ()
  (:metaclass my-metaclass)
  (:custom-option "A custom value"))

(custom-option (find-class 'my-class)) ; Returns "A custom value"
```

### 4.3 Adding or Modifying Slots During Class Creation

You can add or modify slots during class creation by manipulating the `slots` slot of the class metaobject within `initialize-instance`.

```lisp
(defclass my-metaclass (standard-class) ())

(defmethod initialize-instance ((class my-metaclass) &rest initargs)
    (call-next-method)
    ; Add a new slot to the class
    (setf (slot-value class 'slots)
        (append (slot-value class 'slots)
                (list (make-instance 'standard-effective-slot-definition
                                     :name 'added-slot
                                     :initform 0))))
    class)

(defclass my-class ()
    ((original-slot :initform "foo"))
    (:metaclass my-metaclass))

(mapcar #'slot-definition-name (slot-value (find-class 'my-class) 'slots)) ; prints (ORIGINAL-SLOT ADDED-SLOT)
```

In this example, we add a new slot named `added-slot` with an `initform` of 0 to the class being created. `standard-effective-slot-definition` is used to create the new slot definition.

**Important Considerations:**

* Modifying the structure of a class after instances have been created can have complex consequences. CLOS provides mechanisms for updating instances, but it's often best to design your classes carefully from the beginning.
* The MOP is a powerful tool, but it should be used judiciously. Overuse of metaprogramming can make code harder to understand and maintain.

This section covered how to customize class creation using custom metaclasses and the `initialize-instance` method. This allows you to exert fine-grained control over the structure and behavior of your classes. The next sections will cover customizing method dispatch and other advanced MOP topics.

## 5. Customizing Method Dispatch

This section delves into customizing method dispatch in CLOS using the Metaobject Protocol. This primarily involves understanding and manipulating *method combinations*.

### 5.1 Method Combinations and Their Metaobject Representation

When a generic function is called, CLOS determines which methods are *applicable* (their specializers match the arguments). If multiple methods are applicable, CLOS uses a *method combination* to determine how the results of these methods are combined.

The standard method combination is called `standard`. It defines the order in which methods are called (most specific to least specific) and how their results are combined (primary methods are called first, then `before` methods, then `after` methods, and finally `around` methods).

Method combinations are themselves represented by metaobjects of class `method-combination`. You can access the method combination of a generic function using `method-combination`:

```lisp
(defgeneric my-generic-function (x))
(method-combination (fdefinition 'my-generic-function)) ; Returns STANDARD
```

### 5.2 Defining Custom Method Combinations

You can define custom method combinations using `define-method-combination`. The syntax is complex and allows for a high degree of customization. Here, we'll cover a simplified example.

```lisp
(define-method-combination my-combination (&optional (order :most-specific-first))
  ((methods (method-combination-methods)))
  (case order
    (:most-specific-first
     `(progn ,@(mapcar #'(lambda (method) `(call-method ,method)) methods)))
    (:most-specific-last
     `(progn ,@(reverse (mapcar #'(lambda (method) `(call-method ,method)) methods))))))
```

This defines a method combination called `my-combination` that takes an optional argument `order`. If `order` is `:most-specific-first` (the default), it calls the applicable methods from most specific to least specific. If `order` is `:most-specific-last`, it calls them in reverse order.

To use a custom method combination, you specify the `:method-combination` option in `defgeneric`:

```lisp
(defgeneric my-generic-function (x)
  (:method-combination my-combination))

(defmethod my-generic-function ((x number))
  (format t "Number method~%")
  x)

(defmethod my-generic-function ((x integer))
  (format t "Integer method~%")
  (* x 2))

(my-generic-function 5)
; Output:
; Integer method
; Number method
; 10

(defgeneric my-other-generic-function (x)
  (:method-combination my-combination (:most-specific-last)))

(defmethod my-other-generic-function ((x number))
  (format t "Number method~%")
  x)

(defmethod my-other-generic-function ((x integer))
  (format t "Integer method~%")
  (* x 2))

(my-other-generic-function 5)
; Output:
; Number method
; Integer method
; 10
```

### 5.3 Customizing Method Selection

You can customize method selection by specializing the generic functions that are responsible for determining applicable methods and ordering them. The most important generic function for this is `compute-applicable-methods`.

`compute-applicable-methods` takes a generic function and a list of arguments and returns a list of applicable methods, ordered according to the method combination. By specializing this generic function, you can change the method selection process entirely.

Here's a simplified example that demonstrates how to filter applicable methods based on a condition:

```lisp
(defgeneric my-generic-function (x))

(defmethod compute-applicable-methods ((gf standard-generic-function) args)
  (remove-if-not #'(lambda (method)
                     (let ((specializer (first (method-specializers method))))
                       (or (eq specializer 't) ; Always include methods specialized on T
                           (evenp (first args))))) ; Only include methods if the first argument is even
                 (call-next-method))) ; Call the standard compute-applicable-methods

(defmethod my-generic-function ((x number))
  (format t "Number method~%")
  x)

(defmethod my-generic-function ((x integer))
  (format t "Integer method~%")
  (* x 2))

(my-generic-function 4)
; Output:
; Integer method
; 8

(my-generic-function 5)
; Output:
; Number method
; 5
```

In this example, we specialize `compute-applicable-methods` to filter the applicable methods, only keeping those where the first argument is even (or if the method is specialized on `T`).

Customizing method dispatch is an advanced technique that allows for very fine-grained control over the behavior of CLOS. However, it's often complex and should be used only when necessary. Most of the time, the standard method combination and the standard method selection process are sufficient.

This section covered customizing method dispatch using custom method combinations and specializing `compute-applicable-methods`. This enables very powerful and flexible control over how generic functions behave. This concludes the tutorial on the Metaobject Protocol.

## 6. Introspection and Reflection in CLOS

Introspection and reflection are powerful features of CLOS that allow you to examine and manipulate the structure and behavior of objects, classes, methods, and generic functions at runtime. This section explores how to perform these operations using the Metaobject Protocol.

### 6.1 Inspecting Class Structure (Slots, Superclasses)

You can inspect the structure of a class using several MOP functions:

* **`class-slots`**: Returns a list of `slot-definition` metaobjects for a given class. Each `slot-definition` contains information about a slot, such as its name, initargs, initform, and accessors.

    ```lisp
    (defclass person ()
      ((name :initarg :name :accessor person-name)
       (age :initarg :age :accessor person-age)))

    (mapcar #'slot-definition-name (class-slots (find-class 'person))) ; Returns (NAME AGE)
    (mapcar #'slot-definition-initargs (class-slots (find-class 'person))) ; Returns ((:NAME) (:AGE))
    ```

* **`class-direct-slots`**: Returns a list of `slot-definition` metaobjects for the slots directly defined in the class (excluding inherited slots).

    ```lisp
    (defclass employee (person)
      ((employee-id :initarg :id :accessor employee-id)))

    (mapcar #'slot-definition-name (class-slots (find-class 'employee))) ; Returns (NAME AGE EMPLOYEE-ID)
    (mapcar #'slot-definition-name (class-direct-slots (find-class 'employee))) ; Returns (EMPLOYEE-ID)
    ```

* **`class-precedence-list`**: Returns the class precedence list (CPL) for a class. The CPL determines the order in which methods are applied when a generic function is called.

    ```lisp
    (class-precedence-list (find-class 'employee)) ; Returns a list of classes: (EMPLOYEE PERSON STANDARD-OBJECT T)
    ```

* **`class-direct-superclasses`**: Returns the direct superclasses of a class.

    ```lisp
    (class-direct-superclasses (find-class 'employee)) ; Returns (PERSON)
    ```

### 6.2 Examining Method Definitions and Specializers

You can examine method definitions and specializers using the following functions:

* **`method-specializers`**: Returns a list of the specializers of a method.

    ```lisp
    (defgeneric greet (x))
    (defmethod greet ((x person))
      (format t "Hello, ~a!~%" (person-name x)))

    (let ((method (find-method #'greet '() (list (find-class 'person)))))
        (method-specializers method)) ; Returns (PERSON)
    ```

* **`method-lambda-list`**: Returns the lambda list of a method.

    ```lisp
    (let ((method (find-method #'greet '() (list (find-class 'person)))))
        (method-lambda-list method)) ; Returns (X)
    ```

* **`method-function`**: Returns the compiled function that implements the method.

    ```lisp
    (let ((method (find-method #'greet '() (list (find-class 'person)))))
        (function-lambda-expression (method-function method))) ; returns the lambda expression of the method
    ```

* **`find-method`**: Finds a method of a generic function that matches a given set of specializers.

    ```lisp
    (find-method #'greet '() (list (find-class 'person))) ; Returns the method object.
    (find-method #'greet '() (list (find-class 'integer))) ; Returns nil because there is no method specialized on integer
    ```

### 6.3 Working with Generic Function Metaobjects

You can access and manipulate generic function metaobjects using these functions:

* **`generic-function-name`**: Returns the name of a generic function.

    ```lisp
    (defgeneric greet (x))
    (generic-function-name (fdefinition 'greet)) ; Returns GREET
    ```

* **`generic-function-methods`**: Returns a list of all methods associated with a generic function.

    ```lisp
    (defmethod greet ((x number)) nil)
    (defmethod greet ((x integer)) nil)
    (length (generic-function-methods (fdefinition 'greet))) ; Returns 2
    ```

* **`generic-function-method-class`**: Returns the class of the methods associated with a generic function (usually `standard-method`).

    ```lisp
    (generic-function-method-class (fdefinition 'greet)) ; Returns STANDARD-METHOD
    ```

* **`method-combination`**: Returns the method combination object of the generic function.

    ```lisp
    (method-combination (fdefinition 'greet)) ; Returns STANDARD
    ```

**Example: Printing Information about a Class:**

This example demonstrates how to use introspection to print information about a class:

```lisp
(defun print-class-info (class-name)
  (let ((class (find-class class-name)))
    (when class
      (format t "Class: ~a~%" (class-name class))
      (format t "Superclasses: ~a~%" (class-direct-superclasses class))
      (format t "Slots:~%")
      (dolist (slot (class-slots class))
        (format t "  ~a (initargs: ~a)~%"
                (slot-definition-name slot)
                (slot-definition-initargs slot))))))

(print-class-info 'employee)
```

This function prints the name, superclasses, and slots of a given class.

Introspection and reflection are powerful tools for understanding and manipulating the structure and behavior of your CLOS code. They are particularly useful for debugging, metaprogramming, and building tools that analyze or manipulate code. This concludes the tutorial on the Metaobject Protocol. While many more advanced features exist, this introduction should provide a solid foundation for further exploration.

## 7. Examples and Advanced Techniques using the MOP

This section explores some more advanced techniques and provides examples of how the Metaobject Protocol (MOP) can be used to solve real-world problems.

### 7.1 Implementing a Simple Object Database

The MOP can be used to implement a simple in-memory object database. This example demonstrates how to automatically assign unique IDs to objects upon creation.

```lisp
(defclass db-class (standard-class)
  ((next-id :initform 0 :accessor next-id)))

(defmethod initialize-instance ((class db-class) &rest initargs)
  (call-next-method)
  class)

(defmethod make-instance ((class db-class) &rest initargs)
  (let ((instance (call-next-method)))
    (setf (getf initargs :id) (incf (next-id class))) ; Auto-assign ID
    (apply #'reinitialize-instance instance initargs)
    instance))

(defclass db-object ()
  ((id :initarg :id :accessor object-id))
  (:metaclass db-class))

(defclass person (db-object)
  ((name :initarg :name :accessor person-name)))

(let ((p1 (make-instance 'person :name "Alice"))
      (p2 (make-instance 'person :name "Bob")))
  (format t "P1 ID: ~a, Name: ~a~%" (object-id p1) (person-name p1))
  (format t "P2 ID: ~a, Name: ~a~%" (object-id p2) (person-name p2)))
; Output:
; P1 ID: 1, Name: Alice
; P2 ID: 2, Name: Bob
```

Here's how it works:

* `db-class` is a custom metaclass that inherits from `standard-class` and has a slot `next-id` to keep track of the next available ID.
* The `initialize-instance` method for `db-class` does nothing special but is needed to make the `next-id` slot work.
* The `make-instance` method specialized on `db-class` now automatically assigns a unique ID to each instance by incrementing `next-id`.
* `db-object` is a base class for all database objects, which has an `id` slot.
* `person` inherits from `db-object` and thus automatically gains the ID functionality.

This is a simple example, but it illustrates how the MOP can be used to add common behavior to classes automatically.

### 7.2 Implementing Aspects Using the MOP

*Aspect-Oriented Programming (AOP)* is a programming paradigm that allows you to modularize cross-cutting concerns (aspects) such as logging, tracing, or security. The MOP can be used to implement aspects in CLOS.

This example demonstrates how to implement a simple tracing aspect:

```lisp
(defclass traced-class (standard-class) ())

(defmethod initialize-instance ((class traced-class) &rest initargs)
  (call-next-method)
  (dolist (method (generic-function-methods (fdefinition (intern (format nil "~a-~a" (package-name (symbol-package (class-name class))) (class-name class))))))
      (let ((original-function (method-function method)))
          (setf (method-function method)
              #'(lambda (&rest args)
                  (format t "Entering ~a with args: ~a~%" method args)
                  (multiple-value-prog1 (apply original-function args)
                      (format t "Exiting ~a~%" method))))))
  class)

(defgeneric person-greet (p))

(defclass person ()
    ((name :initarg :name :accessor person-name))
    (:metaclass traced-class))

(defmethod person-greet ((p person))
    (format t "Hello ~a~%" (person-name p)))

(person-greet (make-instance 'person :name "Alice"))
; Output:
; Entering #<STANDARD-METHOD PERSON-GREET (PERSON)> with args: (#<PERSON {1004838383}>)
; Hello Alice
; Exiting #<STANDARD-METHOD PERSON-GREET (PERSON)>
```

Here's how it works:

* `traced-class` is a custom metaclass.
* The `initialize-instance` method for `traced-class` iterates through all methods associated with the class, and wraps the original method function with a new function that prints tracing information before and after calling the original method.
* By making a class use the `traced-class` metaclass, all methods of that class are automatically traced.

This is a simplified example of aspect implementation. More complex aspects can be implemented using more advanced MOP techniques.

### 7.3 Creating a New Object Model on Top of CLOS

The MOP provides enough power to implement entirely new object models on top of CLOS. This is a very advanced technique and is rarely necessary for most applications. However, it demonstrates the extreme flexibility of the MOP.

Implementing a new object model typically involves:

* Defining custom metaclasses for classes, methods, and generic functions.
* Customizing method dispatch.
* Defining new method combinations.

This is a very complex topic and is beyond the scope of this introductory tutorial. However, it's important to understand that the MOP provides this level of customization.

These examples illustrate some of the advanced capabilities of the MOP. It's a powerful tool that allows you to customize and extend CLOS in significant ways. However, it's important to use it judiciously, as overuse can make code more complex. For most applications, the standard behavior of CLOS is sufficient. This concludes the tutorial.
