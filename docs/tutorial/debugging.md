---
sidebar_position: 9.6
---

# Debugging Common Lisp

## **Tutorial: Debugging in Common Lisp with SBCL, SLIME, and SLY**

### **Introduction**

This tutorial will guide you through the process of debugging in Common Lisp with an emphasis on practical, real-world workflows. We’ll cover:

1. **Setting up SBCL for efficient debugging.**
2. **Using SLIME/SLY for interactive development.**
3. **Common debugging workflows and best practices.**
4. **Debugging with the SBCL debugger, modifying live code, and testing in-place.**

Let’s get started!

---

### **1. Setting Up SBCL for Debugging**

**SBCL** (Steel Bank Common Lisp) is a highly efficient implementation of Common Lisp, and it’s widely used for production systems. To ensure a smooth debugging experience, you need to configure your compiler and runtime settings properly.

#### **1.1 Installing SBCL**

Ensure SBCL is installed and available on your system. You can download it from [SBCL's official website](http://www.sbcl.org/).

You can check if SBCL is installed via:

```sh
sbcl --version
```

#### **1.2 Compiler Settings for Debugging**

To set the SBCL compiler settings for debugging, you should modify the `sbcl` environment settings before you start a session. Here are some useful options to enable more detailed debug information:

* **Disable optimization**: Disable optimizations when you need more detailed debugging information. This will slow down execution but make debugging much easier.

  ```lisp
  (setq *sbcl-optimize* :debug)
  ```

* **Enable full debugging info**: To keep debugging symbols, use the `*debugger-hook*` and `*compile-file-hook*` to help in debugging sessions.

* **Control Debugging Level**: You can set the `*compile-verbose*` flag to control the level of debugging information:

  ```lisp
  (setq *compile-verbose* t)
  ```

#### **1.3 Optimizing for Performance**

When you're not debugging and want to focus on performance, consider using the following compiler settings for SBCL:

* **Use aggressive optimizations**: SBCL offers several optimization levels. For example, you can set `:speed` to maximize performance and `:safety` to ensure no runtime errors.

  ```lisp
  (setq *sbcl-optimize* :speed)
  ```

You can also manually adjust SBCL's optimization during function compilation:

```lisp
(defmethod some-function ((x some-type))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  ; your code here
)
```

This will maximize performance while avoiding unnecessary debugging info.

---

### **2. Using SLIME/SLY for Interactive Development**

#### **2.1 Setting Up SLIME/SLY**

SLIME and SLY are powerful tools that provide interactive programming, debugging, and REPL support for Common Lisp.

* **SLIME** is an older, but still widely used tool, whereas **SLY** is a more recent alternative that provides better performance and new features. Both integrate well with Emacs.

1. **Install SLIME/SLY via Quicklisp** (if you haven't already):

   * For SLIME:

     ```lisp
     (ql:quickload "slime")
     ```

   * For SLY (the modern alternative to SLIME):

     ```lisp
     (ql:quickload "sly")
     ```

2. **Configure Emacs for SLIME/SLY**:

   * Add the following to your Emacs configuration file (`~/.emacs.d/init.el` or `.emacs`):

     For SLIME:

     ```lisp
     (setq inferior-lisp-program "/path/to/sbcl")  ;; Path to SBCL
     (require 'slime)
     (slime-setup)
     ```

     For SLY:

     ```lisp
     (setq inferior-lisp-program "/path/to/sbcl")  ;; Path to SBCL
     (require 'sly)
     (sly-setup)
     ```

---

### **3. Using the SBCL Debugger**

The SBCL debugger allows you to inspect your program’s state at runtime, and even modify code without restarting the program.

#### **3.1 Triggering the Debugger**

The SBCL debugger can be triggered automatically when an error occurs, or you can manually invoke it.

* **Automatic Trigger**: If your program hits an error, the debugger will activate automatically. It will show you the backtrace, the current stack frame, and allow you to inspect variables.

* **Manually Triggering**: You can invoke the debugger manually in a running program by using the `sb-debug` package:

  ```lisp
  (sb-debug:debug)
  ```

#### **3.2 Debugging in the SBCL REPL**

The REPL provides interactive debugging features. You can step through code, inspect the stack, and even modify variables.

* **Viewing the Stack**:

  In the SBCL debugger, use `:stack` to inspect the call stack. This will show the functions in the current call hierarchy.

* **Inspecting Variables**:

  You can inspect variables in the current frame using `:inspect`:

  ```lisp
  :inspect some-variable
  ```

* **Stepping Through Code**:

  You can step through the code using `:step`, `:next`, `:finish`, and `:cont` commands.

---

### **4. Debugging Workflow: Interactive Debugging with SLIME/SLY**

Now let’s go through a common debugging workflow where we interact with the code during runtime.

#### **4.1 Example Workflow: Handling Errors and Modifying Code in the Debugger**

Imagine you have the following function that throws an error:

```lisp
(defun divide (x y)
  (if (zerop y)
      (error "Cannot divide by zero")
      (/ x y)))
```

When you call `(divide 5 0)`, the program will trigger an error.

* **Step 1: Start the REPL in Emacs with SLIME/SLY**. You’ll see the error message in the SLIME/SLY buffer and automatically be dropped into the SBCL debugger.

* **Step 2: Inspect the error.** Look at the stack, and check the values of `x` and `y` in the debugger.

* **Step 3: Modify the code in-place**. Let’s say you want to add a check for negative numbers in the divisor. You can edit the code directly in the REPL and reload the definition.

  ```lisp
  (defun divide (x y)
    (if (or (zerop y) (< y 0))
        (error "Invalid divisor")
        (/ x y)))
  ```

  After modifying the code, simply continue the execution using `:cont` to let the code run with the updated behavior.

#### **4.2 Example Workflow: Live Modifications and Testing Stack Data**

Let’s say you are debugging a function and encounter an error. The error stops execution, but you want to continue working with the data at that point.

* **Step 1: Inspect the current stack frame** to view local variables.

* **Step 2: Test changes live in the debugger**. You can change the value of a variable in the current frame:

  ```lisp
  (setf some-variable new-value)
  ```

* **Step 3: Continue execution with the new value** by using `:cont`.

---

### **5. Best Practices for Debugging in Common Lisp**

* **Use SLIME/SLY for fast iterative development**: You can quickly test code, modify it, and inspect errors without restarting the whole process.
* **Keep debugging symbols enabled when debugging**: Turn off optimizations and debug info only when performance is not an issue.
* **Interact with live data**: When an error occurs, don’t just fix the code and continue—interact with the stack and modify state to test your fix without restarting.
* **Understand the debugging tools**: Learn how to use the SBCL debugger effectively, and use SLIME/SLY to interact with your program in real-time.

---

### **Conclusion**

With SBCL, SLIME/SLY, and the SBCL debugger, debugging in Common Lisp is both powerful and flexible. By following the steps in this tutorial, you should be able to interact with your code, inspect errors, modify live code, and continue execution with minimal interruptions.

Remember to always test your changes interactively in the REPL and use the SBCL debugger to manage your program’s state in real-time!

Happy debugging!
