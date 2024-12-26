---
sidebar_position: 2.05
---

# Documenting your Code

## Introduction to Documenting Code in Common Lisp

Documenting code is a crucial aspect of software development, ensuring maintainability, collaboration, and understanding. Common Lisp provides excellent built-in facilities for documenting your code directly within the source files. These facilities allow you to embed documentation strings, which can then be extracted by various tools to generate documentation in different formats.

This tutorial will introduce you to the core concepts and forms for documenting Common Lisp code, covering how to attach documentation to various program entities and how to access this documentation programmatically.

**Key Concepts:**

* **Documentation Strings:** Strings attached to program entities (functions, variables, classes, etc.) that describe their purpose and usage.
* **`documentation` Function:** Accessing documentation strings programmatically.
* **Documenting Different Entities:** How to document functions, macros, variables, classes, packages, and more.
* **Style and Best Practices:** Guidelines for writing effective documentation.

**Table of Contents:**

**1. Documentation Strings:**

* What are documentation strings?
* Syntax for attaching documentation strings.
* Placement of documentation strings.

**2. The `documentation` Function:**

* Basic usage of `documentation`.
* Specifying the documentation type.
* Handling missing documentation.

**3. Documenting Different Entities:**

* Documenting functions (`defun`).
* Documenting macros (`defmacro`).
* Documenting variables (`defvar`, `defparameter`, `defconstant`).
* Documenting classes (`defclass`).
* Documenting packages (`defpackage`).
* Documenting generic functions (`defgeneric`).
* Documenting methods (`defmethod`).
* Documenting structures (`defstruct`).

**4. Style and Best Practices:**

* Writing clear and concise documentation.
* Using consistent formatting.
* Documenting parameters, return values, and side effects.
* Examples of good documentation.

**5. Tools and Utilities:**

* Brief overview of documentation generators (e.g., Sphinx with the `cl-sphinx` extension).

This tutorial will provide a comprehensive guide to documenting your Common Lisp code effectively, enabling you to create well-documented and maintainable projects.
