---
sidebar_position: 6
---

# Object System

:::warning
This article **needs to be checked!**.
Please help contribute by checking it and making changes in [our repository](https://github.com/lisp-docs/lisp-docs.github.io) or by clicking on the "Edit this page" link below.
:::

## Introduction to CLOS (Common Lisp Object System)

The Common Lisp Object System (CLOS) is a powerful and flexible object-oriented programming system built into Common Lisp. Unlike some object systems that were added as an afterthought, CLOS is an integral part of the language, deeply integrated with its other features. CLOS is based on the *generic function* paradigm, which is different from the more common message-passing paradigm found in languages like Java or C++. This difference leads to a more flexible and expressive way of defining object behavior.

This tutorial will introduce you to the core concepts of CLOS, including classes, instances, generic functions, methods, and method combinations. We'll explore how these concepts work together to create object-oriented programs in Common Lisp.

**Key Concepts of CLOS:**

* **Classes:** Blueprints for creating objects, defining their structure (slots).
* **Instances:** Concrete objects created from classes.
* **Slots:** The data members or attributes of an object.
* **Generic Functions:** Functions that define a general operation, but their specific behavior depends on the classes of their arguments.
* **Methods:** Implementations of a generic function for specific classes of arguments.
* **Method Combination:** Mechanisms for combining the results of multiple methods that apply to a given generic function call.

**Table of Contents:**

**1. Defining Classes:**

* `defclass`: Defining classes and their slots.
  * Slot options: `:initarg`, `:initform`, `:accessor`, `:reader`, `:writer`.
  * Class precedence list.

**2. Creating Instances:**

* `make-instance`: Creating objects from classes.
* Initializing slots using `:initarg` and `:initform`.

**3. Accessing Slots:**

* Using accessors, readers, and writers.
* `slot-value`: Directly accessing slot values (less common).

**4. Generic Functions and Methods:**

* `defgeneric`: Defining generic functions.
  * Method qualifiers.
* `defmethod`: Defining methods for generic functions.
  * Specializers.
* Method dispatch.

**5. Method Combination:**

* Standard method combination: `:before`, `:after`, `:around`, `:primary`.
* Other method combination types.

**6. Inheritance:**

* Single inheritance.
* Multiple inheritance.
* Method inheritance.

**7. Metaclasses (Brief Introduction):**

* What are metaclasses?
* A simple example.

**8. Example CLOS Program:**

* A more complete example showing how the different parts of CLOS work together.

By the end of this tutorial, you will have a solid understanding of the fundamental principles of CLOS and be able to use it to create object-oriented programs in Common Lisp. You will understand how generic functions and methods provide a flexible and powerful way to define object behavior, and how method combination allows you to customize the execution of methods in complex inheritance hierarchies. We will also touch on the advanced topic of metaclasses to give you a glimpse into the metaobject protocol of CLOS.

## 1. Defining Classes in CLOS

In CLOS, classes are blueprints for creating objects. They define the structure and behavior that objects of that class will have. The primary way to define classes is using the `defclass` macro.

### 1.1 `defclass`: Defining Classes and Their Slots

The general syntax of `defclass` is:

```lisp
(defclass class-name (superclasses)
  ((slot-name1 slot-options1)
   (slot-name2 slot-options2)
   ...)
  class-options)
```

* **`class-name`**: A symbol that names the class.
* **`(superclasses)`**: A list of superclasses from which this class inherits. If the class has no superclasses, this can be `()` or just omitted. `standard-object` is the default superclass if none are specified.
* **`((slot-name1 slot-options1) ...)`**: A list of slot specifications. Each slot specification is a list containing the slot's name (a symbol) and its options.
* **`class-options`**: Options that apply to the class as a whole (less common).

#### 1.1.1 Slot Options

Here are the most commonly used slot options:

* **`:initarg`**: Specifies one or more keywords that can be used to initialize the slot when creating an instance.

    ```lisp
    (defclass person ()
      ((name :initarg :name)
       (age :initarg :age)))
    ```

    Now, when creating a `person` instance, you can use `:name` and `:age` to set the initial values:

    ```lisp
    (make-instance 'person :name "Alice" :age 30)
    ```

* **`:initform`**: Specifies a default value for the slot if no `:initarg` is provided.

    ```lisp
    (defclass person ()
      ((name :initarg :name)
       (age :initarg :age :initform 0)))
    ```

    If you create a `person` without specifying an age, it will default to 0:

    ```lisp
    (make-instance 'person :name "Bob") ; Age will be 0
    ```

* **`:accessor`**: Creates both a reader and a writer function for the slot.

    ```lisp
    (defclass person ()
      ((name :accessor person-name)
       (age :accessor person-age)))
    ```

    This creates the functions `person-name` (to read the name) and `(setf person-name)` (to set the name):

    ```lisp
    (let ((p (make-instance 'person :name "Carol" :age 25)))
      (print (person-name p)) ; Prints "Carol"
      (setf (person-name p) "Carrie") ; Sets the name to "Carrie"
      (print (person-name p))) ; Prints "Carrie"
    ```

* **`:reader`**: Creates only a reader function for the slot.

    ```lisp
    (defclass person ()
      ((name :reader person-name)))
    ```

* **`:writer`**: Creates only a writer function for the slot. It's less common to use `:writer` without a corresponding `:reader` or `:accessor`.

    ```lisp
    (defclass person ()
      ((name :writer (setf person-name))))
    ```

    This creates the `(setf person-name)` function.

Example combining several options:

```lisp
(defclass dog ()
  ((name :initarg :name :accessor dog-name)
   (breed :initarg :breed :initform "Unknown" :accessor dog-breed)
   (age :initarg :age :initform 0 :accessor dog-age)))

(let ((my-dog (make-instance 'dog :name "Fido" :age 3)))
  (format t "Name: ~a, Breed: ~a, Age: ~a~%"
          (dog-name my-dog) (dog-breed my-dog) (dog-age my-dog)))

(let ((another-dog (make-instance 'dog :name "Rover")))
    (format t "Name: ~a, Breed: ~a, Age: ~a~%"
        (dog-name another-dog) (dog-breed another-dog) (dog-age another-dog)))
```

Output:

```lisp
Name: Fido, Breed: Unknown, Age: 3
Name: Rover, Breed: Unknown, Age: 0
```

#### 1.1.2 Class Precedence List

When a class inherits from multiple superclasses (multiple inheritance), CLOS uses a *class precedence list* (CPL) to determine the order in which methods are inherited and combined. The CPL is a total ordering of the class and its superclasses.

For single inheritance, the CPL is simple: it's the class itself followed by its superclasses, up to `t` (the root of the class hierarchy).

For multiple inheritance, the CPL is more complex and is calculated using a specific algorithm. The basic idea is that a class always precedes its superclasses in the CPL, and if a class inherits from multiple superclasses, their order in the superclass list influences their relative order in the CPL.

Example of simple inheritance:

```lisp
(defclass animal ()
  ((name :initarg :name :accessor animal-name)))

(defclass cat (animal)
  ((breed :initarg :breed :accessor cat-breed)))

(print (class-precedence-list 'cat))
```

Output (may vary slightly depending on the Lisp implementation):

```lisp
(CAT ANIMAL STANDARD-OBJECT T)
```

This shows that `cat` precedes `animal`, which precedes `standard-object`, which precedes `t`.

Understanding the class precedence list is crucial for understanding method inheritance and method combination, which we will cover in the next sections. This introduction to `defclass` provides the foundation for working with objects in CLOS.

## 2. Creating Instances in CLOS

Once you have defined a class using `defclass`, you can create instances (objects) of that class using the `make-instance` function.

### 2.1 `make-instance`: Creating Objects

The `make-instance` function takes the class name (a symbol) as its first argument, followed by keyword arguments to initialize the slots of the new instance.

```lisp
(defclass person ()
  ((name :initarg :name :accessor person-name)
   (age :initarg :age :initform 0 :accessor person-age)))

(let ((alice (make-instance 'person :name "Alice" :age 30)))
  (print alice))
```

This creates an instance of the `person` class, initializes the `name` slot to "Alice" and the `age` slot to 30, and binds the instance to the variable `alice`. The output will be a representation of the object, which depends on the Lisp implementation. It might look something like this:

```lisp
#<PERSON {1004944833}>
```

The important thing is that `alice` now holds an object of type `person`.

### 2.2 Initializing Slots using `:initarg` and `:initform`

As shown in the previous example, you initialize slots using keyword arguments corresponding to the `:initarg` options specified in the `defclass` definition.

If a slot has an `:initform` and no corresponding `:initarg` is provided to `make-instance`, the `:initform` is used to initialize the slot.

```lisp
(let ((bob (make-instance 'person :name "Bob")))
  (print (person-age bob))) ; Prints 0 because of the :initform
```

If a slot has both an `:initarg` and an `:initform`, and the `:initarg` is provided to `make-instance`, the `:initarg`'s value takes precedence:

```lisp
(let ((charlie (make-instance 'person :name "Charlie" :age 25)))
  (print (person-age charlie))) ; Prints 25, not the default 0
```

It is important to note that the `:initform` is evaluated *each time* an instance is created and the initarg is not supplied. If you want a slot to be initialized to a value that is calculated only *once* when the class is defined, use a function call in the `:initform`:

```lisp
(defclass counter ()
  ((count :initform (gensym) :accessor counter-value)))

(let ((c1 (make-instance 'counter))
      (c2 (make-instance 'counter)))
  (print (counter-value c1))
  (print (counter-value c2)))
```

This will print two different gensyms. If you would have used `(count :initform 0 :accessor counter-value)` the value would be shared between all instances.

## 3. Accessing Slots in CLOS

Once you have created an instance, you need a way to access and modify its slots. CLOS provides two primary ways to do this: using accessors (readers and writers) and directly using `slot-value`.

### 3.1 Using Accessors, Readers, and Writers

The preferred way to access slots is through accessor functions that are automatically generated by the `:accessor`, `:reader`, and `:writer` slot options in `defclass`.

* **`:accessor`**: Creates both a reader and a writer function.

    ```lisp
    (defclass person ()
      ((name :accessor person-name)
       (age :accessor person-age)))

    (let ((david (make-instance 'person :name "David" :age 40)))
      (print (person-name david))      ; Reader: Prints "David"
      (setf (person-name david) "Dave") ; Writer: Changes the name
      (print (person-name david))      ; Prints "Dave"
      (print (person-age david))      ; Reader: Prints 40
      (setf (person-age david) 41)
      (print (person-age david)))      ; Prints 41
    ```

* **`:reader`**: Creates only a reader function. The slot becomes read-only using this method.

    ```lisp
    (defclass read-only-person ()
      ((name :reader read-only-person-name)))

    (let ((eve (make-instance 'read-only-person :name "Eve")))
      (print (read-only-person-name eve)) ; Works fine
      ;(setf (read-only-person-name eve) "Eva") ; This would cause an error
    )
    ```

* **`:writer`**: Creates only a writer function. This is less common, as you usually want to read the slot as well.

### 3.2 `slot-value`: Directly Accessing Slot Values

You can also directly access a slot's value using the `slot-value` function. This takes the instance and the slot name as arguments.

```lisp
(let ((frank (make-instance 'person :name "Frank" :age 35)))
  (print (slot-value frank 'name))) ; Prints "Frank"
```

However, using accessors is generally preferred over `slot-value` for several reasons:

* **Encapsulation:** Accessors provide a level of abstraction, allowing you to change the internal representation of a class without breaking code that uses the accessors.
* **Method Dispatch:** Accessors are generic functions, which allows methods to be specialized on them, providing more flexibility in object behavior.
* **Style:** Using accessors is considered more idiomatic CLOS style.

Therefore, you should almost always use accessors unless you have a very specific reason to use `slot-value`.

These sections explain how to create instances of classes and how to access and modify their slots. Accessors are the preferred way to interact with objects in CLOS, providing encapsulation and supporting the powerful features of generic functions and methods, which we will cover in the next parts of this tutorial.

## 4. Generic Functions and Methods in CLOS

A key distinction of CLOS compared to other object-oriented systems is its use of *generic functions* instead of the traditional message-passing paradigm. A generic function defines a general operation, and its specific behavior is determined by the *methods* that are defined for it, based on the classes (or other specializers) of the arguments passed to it.

### 4.1 `defgeneric`: Defining Generic Functions

The `defgeneric` macro defines a generic function. It specifies the function's name, its parameter list, and optionally a docstring and other options.

```lisp
(defgeneric describe (object)
  (:documentation "Describes an object."))
```

* **`describe`**: The name of the generic function.
* **`(object)`**: The parameter list. In this case, there's one parameter named `object`.
* **`(:documentation "Describes an object.")`**: An optional docstring.

`defgeneric` does *not* provide an implementation for the function. It simply declares that a generic function exists and specifies its interface.

#### 4.1.1 Method Qualifiers

`defgeneric` can also take method qualifiers. These are used to control how method combination works (which we'll discuss later). The most common qualifier is `:method`, which indicates a standard method. If no qualifier is specified, `:method` is assumed.

```lisp
(defgeneric area (shape)
  (:documentation "Calculates the area of a shape."))
```

### 4.2 `defmethod`: Defining Methods for Generic Functions

The `defmethod` macro defines a method for a generic function. It specifies the method's name (which must be the name of an existing generic function), a *specialized parameter list*, and the method's body.

```lisp
(defclass circle ()
  ((radius :initarg :radius :accessor circle-radius)))

(defmethod area ((c circle)) ; Specialized on the class CIRCLE
  (let ((r (circle-radius c)))
    (* pi r r)))

(defclass square ()
  ((side :initarg :side :accessor square-side)))

(defmethod area ((s square)) ; Specialized on the class SQUARE
  (let ((side (square-side s)))
    (* side side)))

(let ((my-circle (make-instance 'circle :radius 5))
      (my-square (make-instance 'square :side 4)))
  (format t "Circle area: ~f~%" (area my-circle))
  (format t "Square area: ~f~%" (area my-square)))
```

Output:

```lisp
Circle area: 78.53982
Square area: 16.0
```

* **`area`**: The name of the generic function.
* **`((c circle))`**: The specialized parameter list. `(c circle)` means that this method is specialized on the class `circle`. When `area` is called with an argument of class `circle`, this method will be chosen.
* The body of the method calculates the area of a circle.

Similarly, the second `defmethod` defines a method for the `area` generic function specialized on the class `square`.

#### 4.2.1 Specializers

The specialized parameter list in `defmethod` determines when a method is applicable. The most common type of specializer is a *class name*, as shown in the examples above. When a class name is used as a specializer, the method is applicable if the corresponding argument is an instance of that class or any of its subclasses.

Other types of specializers include:

* **`eql` specializers:** `(eql value)` specifies that the argument must be `eql` to `value`.

    ```lisp
    (defmethod describe ((object (eql :hello)))
      (print "You said hello!"))

    (describe :hello) ; Prints "You said hello!"
    ```

* **`(satisfies predicate)` specializers:** `(satisfies predicate)` specifies that the argument must satisfy the given predicate function.

    ```lisp
    (defmethod describe ((n (satisfies evenp)))
      (format t "~d is an even number.~%" n))

    (describe 4) ; Prints "4 is an even number."
    (describe 3) ; No applicable method
    ```

### 4.3 Method Dispatch

*Method dispatch* is the process of determining which method to execute when a generic function is called. CLOS uses a sophisticated dispatch algorithm that considers the classes (or other specializers) of *all* arguments to the generic function.

In the previous `area` example, when `(area my-circle)` is called, CLOS checks the class of `my-circle` (which is `circle`) and finds the method specialized on `circle`. It then executes that method. Similarly, when `(area my-square)` is called, the method specialized on `square` is executed.

This is a fundamental difference from message-passing systems, where the method is determined solely by the class of the *first* argument (the receiver). CLOS's multi-method dispatch allows for more flexible and expressive object-oriented programming.

This section introduced the core concepts of generic functions and methods in CLOS. Understanding how these concepts work together is crucial for using CLOS effectively. The next section will cover method combination, which provides even more control over method execution.

## 5. Method Combination in CLOS

Method combination is a powerful feature of CLOS that determines how multiple methods applicable to a generic function call are combined and executed. This allows you to add behavior before, after, or around the primary method, providing great flexibility in customizing object behavior.

### 5.1 Standard Method Combination

The standard method combination type is the most commonly used. It uses four method qualifiers: `:before`, `:after`, `:around`, and `:primary`.

Let's illustrate with an example:

```lisp
(defgeneric operate (x y)
  (:documentation "Performs an operation on x and y."))

(defmethod operate :before ((x number) (y number))
  (format t "Before operation: x = ~a, y = ~a~%" x y))

(defmethod operate :after ((x number) (y number))
  (format t "After operation.~%"))

(defmethod operate :around ((x number) (y number))
  (format t "Around operation (before).~%")
  (let ((result (call-next-method))) ; Call the next most specific method
    (format t "Around operation (after). Result was ~a~%" result)
    result))

(defmethod operate ((x integer) (y integer))
  (format t "Primary method (integers): ~%")
  (+ x y))

(defmethod operate ((x float) (y float))
  (format t "Primary method (floats): ~%")
  (* x y))

(operate 5 3)
(operate 2.5 4.0)
```

Output:

```lisp
Around operation (before).
Before operation: x = 5, y = 3
Primary method (integers): 
After operation.
Around operation (after). Result was 8
8
Around operation (before).
Before operation: x = 2.5, y = 4.0
Primary method (floats): 
After operation.
Around operation (after). Result was 10.0
10.0
```

Here's how the method combination works:

1. **`:around` methods:** The most specific `:around` method is executed first. It has the opportunity to completely control the execution of the generic function. It can choose to call the next most specific method using `call-next-method` or not. If it doesn't call `call-next-method`, the other methods (including `:before`, `:primary`, and `:after`) will not be executed.

2. **`:before` methods:** If the `:around` method calls `call-next-method`, then all applicable `:before` methods are executed in most-specific-first order.

3. **`:primary` method:** The most specific `:primary` method is executed.

4. **`:after` methods:** After the `:primary` method returns, all applicable `:after` methods are executed in *least*-specific-first order (the reverse of `:before` methods).

In our example:

* When `(operate 5 3)` is called:
    1. The `:around` method for numbers is executed.
    2. Inside the `:around` method, `call-next-method` is called.
    3. The `:before` method for numbers is executed.
    4. The `:primary` method for integers is executed.
    5. The `:after` method for numbers is executed.
    6. The `:around` method continues and returns the result from the primary method.
* When `(operate 2.5 4.0)` is called, the float methods are used.

If there are multiple `:before` or `:after` methods applicable, they are executed in the order determined by the class precedence list of the specialized parameters.

If there are multiple `:around` methods, only the *most specific* one is executed.

If there are no `:around` methods, the `:before`, `:primary` and `:after` methods are executed. If there is no `:primary` method an error will be signaled.

### 5.2 Other Method Combination Types

Besides the standard method combination, CLOS provides other types that offer different ways to combine methods:

* **`+` method combination:** Used for accumulating results (e.g., summing values).
* `list` method combination: Used for collecting results into a list.
* `append` method combination: Used for appending lists.
* `nconc` method combination: same as append but using `nconc`
* `progn` method combination: Executes methods sequentially.

These method combination types are specified in the `defgeneric` form using the `:method-combination` option:

```lisp
(defgeneric combine-values (x y)
  (:method-combination +))

(defmethod combine-values ((x number) (y number))
  x)

(defmethod combine-values ((x string) (y string))
  (parse-integer y))

(combine-values 5 10) ; returns 15
(combine-values "hello" "10") ; returns 10
```

In this example, the `+` method combination type is used. If there are multiple applicable methods, the results of all the primary methods are summed.

Method combination is a powerful feature that allows for highly customizable object behavior. The standard method combination is sufficient for most cases, but the other types provide specialized behavior for specific needs. Understanding method combination is essential for mastering CLOS and writing advanced object-oriented programs in Common Lisp.

## 6. Inheritance in CLOS

Inheritance is a fundamental concept in object-oriented programming that allows you to create new classes based on existing ones, inheriting their properties and behavior. CLOS supports both single and multiple inheritance, providing a powerful mechanism for code reuse and organization.

### 6.1 Single Inheritance

Single inheritance means a class can inherit from only one direct superclass.

```lisp
(defclass animal ()
  ((name :initarg :name :accessor animal-name)
   (sound :initform "Generic animal sound" :accessor animal-sound)))

(defclass dog (animal) ; Dog inherits from animal
  ((breed :initarg :breed :accessor dog-breed)))

(let ((fido (make-instance 'dog :name "Fido" :breed "Labrador")))
  (format t "Name: ~a~%" (animal-name fido))
  (format t "Sound: ~a~%" (animal-sound fido))
  (format t "Breed: ~a~%" (dog-breed fido)))
```

Output:

```lisp
Name: Fido
Sound: Generic animal sound
Breed: Labrador
```

In this example, `dog` inherits from `animal`. A `dog` instance has all the slots of `animal` (name and sound) in addition to its own slots (breed). The `animal-name` and `animal-sound` accessors work on `dog` instances as well.

### 6.2 Multiple Inheritance

Multiple inheritance means a class can inherit from multiple superclasses. This allows you to combine features from different classes into a single class.

```lisp
(defclass wheeled-vehicle ()
  ((number-of-wheels :initarg :wheels :accessor vehicle-wheels)))

(defclass motorized-vehicle ()
  ((engine-type :initarg :engine :accessor vehicle-engine)))

(defclass car (wheeled-vehicle motorized-vehicle) ; Car inherits from both
  ((model :initarg :model :accessor car-model)))

(let ((my-car (make-instance 'car :wheels 4 :engine "Gasoline" :model "Sedan")))
  (format t "Wheels: ~a~%" (vehicle-wheels my-car))
  (format t "Engine: ~a~%" (vehicle-engine my-car))
  (format t "Model: ~a~%" (car-model my-car)))
```

Output:

```lisp
Wheels: 4
Engine: Gasoline
Model: Sedan
```

`car` inherits slots from both `wheeled-vehicle` and `motorized-vehicle`.

**Class Precedence List and Multiple Inheritance:**

When a class inherits from multiple superclasses, the order of inheritance matters because it determines the *class precedence list* (CPL). The CPL defines the order in which methods are inherited and combined.

The CPL is calculated using a complex algorithm described in the CLOS specification. The key principles are:

1. A class always precedes its superclasses.
2. The order of superclasses in the `defclass` form influences their order in the CPL.

You can view the CPL of a class using `class-precedence-list`:

```lisp
(print (class-precedence-list 'car))
```

Output (may vary slightly depending on the Lisp implementation):

```lisp
(CAR WHEELED-VEHICLE MOTORIZED-VEHICLE STANDARD-OBJECT T)
```

This tells us that `car` is most specific, followed by `wheeled-vehicle`, then `motorized-vehicle`, then `standard-object`, and finally `t`.

### 6.3 Method Inheritance

Methods are inherited according to the CPL. If a method is defined for a superclass, it is also applicable to instances of its subclasses, unless a more specific method is defined for the subclass.

```lisp
(defgeneric move (vehicle distance)
  (:documentation "Moves a vehicle a certain distance."))

(defmethod move ((v wheeled-vehicle) distance)
  (format t "Moving a wheeled vehicle ~a units.~%" distance))

(defmethod move ((c car) distance)
  (format t "Driving a car ~a units.~%" distance))

(let ((my-bike (make-instance 'wheeled-vehicle :wheels 2))
      (my-car (make-instance 'car :wheels 4 :engine "Electric" :model "Roadster")))
  (move my-bike 10)
  (move my-car 20))
```

Output:

```lisp
Moving a wheeled vehicle 10 units.
Driving a car 20 units.
```

When `(move my-bike 10)` is called, the method specialized on `wheeled-vehicle` is used. When `(move my-car 20)` is called, the method specialized on `car` is used because it's more specific in the CPL of `car`. This is method overriding in action.

If there was no method specialized on `car`, the `wheeled-vehicle` method would be inherited by `car` and used.

Inheritance, along with generic functions and method combination, provides a powerful and flexible way to structure object-oriented programs in Common Lisp. It promotes code reuse, extensibility, and maintainability.

## 7. Metaclasses (Brief Introduction) in CLOS

Metaclasses are a powerful, but often advanced, feature of CLOS. They are classes whose instances are themselves classes. In simpler terms, a metaclass controls the creation and behavior of classes. They allow you to customize how classes are defined, how instances are created, and even how method dispatch works.

### 7.1 What are Metaclasses?

Think of a class as a blueprint for creating objects. A metaclass, then, is a blueprint for creating those blueprints (classes). Just as classes define the structure and behavior of their instances, metaclasses define the structure and behavior of their instances (which are classes).

Key aspects of metaclasses:

* They control class creation: When you use `defclass`, CLOS uses a metaclass to create the new class.
* They can customize class behavior: Metaclasses can add or modify slots, change how instances are initialized, and even alter method dispatch.
* The default metaclass is `standard-class`: Unless you specify otherwise, all classes are instances of `standard-class`.

### 7.2 A Simple Example

Let's create a simple example to illustrate the basic idea of metaclasses. We'll create a metaclass that automatically adds a creation timestamp to every class created with it.

```lisp
(defclass timestamped-class (standard-class) ()
  (:documentation "A metaclass that adds a creation timestamp to classes."))

(defmethod initialize-instance :after ((class timestamped-class) &key)
  (setf (getf (class-plist class) 'creation-timestamp) (get-universal-time)))

(defclass my-timestamped-class ()
  ()
  (:metaclass timestamped-class))

(print (getf (class-plist 'my-timestamped-class) 'creation-timestamp))

(defclass another-timestamped-class ()
  ()
  (:metaclass timestamped-class))

(print (getf (class-plist 'another-timestamped-class) 'creation-timestamp))

(defclass regular-class () ())

(print (getf (class-plist 'regular-class) 'creation-timestamp)) ; this will be NIL
```

Explanation:

1. **`defclass timestamped-class (standard-class) () ...`**: This defines `timestamped-class` as a *metaclass* by inheriting from `standard-class`.
2. **`defmethod initialize-instance :after ((class timestamped-class) &key)`**: This defines an `:after` method for `initialize-instance` specialized on `timestamped-class`. `initialize-instance` is a generic function that is called when a class is created. The `:after` method ensures that our code runs *after* the standard initialization.
3. **`(setf (getf (class-plist class) 'creation-timestamp) (get-universal-time))`**: This is the core of the metaclass's behavior. `class-plist` returns the property list of the class object. We use `getf` and `setf` to store the creation time (obtained with `get-universal-time`) in the property list under the key `'creation-timestamp`.
4. **`defclass my-timestamped-class () () (:metaclass timestamped-class)`**: This defines a regular class, `my-timestamped-class`, but with the crucial `(:metaclass timestamped-class)` option. This tells CLOS to use our custom `timestamped-class` metaclass when creating `my-timestamped-class`.
5. When `my-timestamped-class` is created, the `initialize-instance` method of `timestamped-class` is called, adding the creation timestamp to the class's property list.

Now every class defined with `(:metaclass timestamped-class)` will automatically have a creation timestamp stored in its property list.

This is a simplified example, but it demonstrates the fundamental idea of metaclasses: they control how classes are created and can be used to add or modify class behavior in powerful ways. They are used to implement advanced features like object databases, aspect-oriented programming, and other meta-programming techniques.

Metaclasses are a complex topic, and this is just a brief introduction. There are many more details to explore, such as customizing method dispatch, defining custom class initialization protocols, and more. However, this example should provide a basic understanding of what metaclasses are and how they can be used.

## 8. Example CLOS Program: A Simple Inventory System

This example demonstrates how the different parts of CLOS (classes, instances, generic functions, methods, inheritance) work together to create a simple inventory management system for a store.

```lisp
;; Define the base class for items
(defclass item ()
  ((name :initarg :name :accessor item-name)
   (price :initarg :price :accessor item-price)
   (quantity :initarg :quantity :accessor item-quantity))
  (:documentation "Represents an item in the inventory."))

;; Define a subclass for books
(defclass book (item)
  ((author :initarg :author :accessor book-author)
   (isbn :initarg :isbn :accessor book-isbn))
  (:documentation "Represents a book in the inventory."))

;; Define a subclass for electronics
(defclass electronics (item)
  ((manufacturer :initarg :manufacturer :accessor electronics-manufacturer)
   (warranty-period :initarg :warranty :accessor electronics-warranty))
  (:documentation "Represents an electronic item."))

;; Define a generic function to display item information
(defgeneric display-item (item)
  (:documentation "Displays information about an item."))

;; Define methods for displaying different item types
(defmethod display-item ((item item))
  (format t "Name: ~a~%" (item-name item))
  (format t "Price: $~,2f~%" (item-price item))
  (format t "Quantity: ~d~%" (item-quantity item)))

(defmethod display-item ((book book))
  (call-next-method) ; Call the generic method for item
  (format t "Author: ~a~%" (book-author book))
  (format t "ISBN: ~a~%" (book-isbn book)))

(defmethod display-item ((electronics electronics))
  (call-next-method) ; Call the generic method for item
  (format t "Manufacturer: ~a~%" (electronics-manufacturer electronics))
  (format t "Warranty: ~a months~%" (electronics-warranty electronics)))

;; Define a generic function to restock items
(defgeneric restock (item quantity)
  (:documentation "Restocks an item by a given quantity."))

(defmethod restock ((item item) (quantity integer))
  (incf (item-quantity item) quantity)
  (format t "Restocked ~a by ~d. New quantity: ~d~%"
          (item-name item) quantity (item-quantity item)))

;; Create some inventory items
(let ((my-book (make-instance 'book :name "The Lisp Cookbook" :price 29.99 :quantity 10 :author "Peter Seibel" :isbn "978-1484206773"))
      (my-laptop (make-instance 'electronics :name "Laptop X1" :price 1200.00 :quantity 5 :manufacturer "XYZ Corp" :warranty 12)))

  ;; Display the items
  (format t "--- Book Information ---~%")
  (display-item my-book)
  (format t "--- Laptop Information ---~%")
  (display-item my-laptop)

    ;; Restock the book
    (restock my-book 5)

    ;; Display again to confirm restock.
    (format t "--- Book Information after restock ---~%")
    (display-item my-book))
)
```

Output:

```lisp
--- Book Information ---
Name: The Lisp Cookbook
Price: $29.99
Quantity: 10
Author: Peter Seibel
ISBN: 978-1484206773
--- Laptop Information ---
Name: Laptop X1
Price: $1200.00
Quantity: 5
Manufacturer: XYZ Corp
Warranty: 12 months
Restocked The Lisp Cookbook by 5. New quantity: 15
--- Book Information after restock ---
Name: The Lisp Cookbook
Price: $29.99
Quantity: 15
Author: Peter Seibel
ISBN: 978-1484206773
```

Key aspects of this example:

* **Classes and Inheritance:** `book` and `electronics` inherit from `item`, demonstrating single inheritance.
* **Slots:** Each class defines its own slots with `:initarg` and `:accessor` options.
* **Generic Functions and Methods:** `display-item` and `restock` are generic functions with methods specialized on different classes.
* **Method Combination:** The `display-item` methods use `call-next-method` to call the more general `item` method, demonstrating method combination.
* **Polymorphism:** The `display-item` function behaves differently depending on the class of the item passed to it.

This example illustrates how CLOS can be used to model real-world objects and their interactions in a clear and organized way. The use of generic functions and methods allows for extensibility and maintainability. For example, if you wanted to add a new type of item (e.g., "DVD"), you would simply define a new class and specialize the `display-item` generic function for that class, without having to modify existing code.
