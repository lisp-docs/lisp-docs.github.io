# Package Inferred Systems

:::warning
This article is incomplete, we need to finish writing it. Please help by contributing to the project.
:::

## Introduction

Using [ASDF](https://asdf.common-lisp.dev/) there is an option for `package-inferred-system`. This is a way to make it easier to define a system AKA it's `.asd` file by having ASDF figure out what files to load based on how the system is programmed.

> Starting with release 3.1.2, ASDF supports a one-package-per-file style of programming, in which each file is its own system, and dependencies are deduced from the defpackage form or its variant, uiop:define-package.
>
>In this style of system definition, package names map to systems with the same name (in lower case letters), and if a system is defined with :class package-inferred-system, then system names that start with that name (using the slash / separator) refer to files under the filesystem hierarchy where the system is defined. For instance, if system my-lib is defined in /foo/bar/my-lib/my-lib.asd, then system my-lib/src/utility will be found in file /foo/bar/my-lib/src/utility.lisp.
>
>One package per file style was made popular by faslpath and quick-build, and at the cost of stricter package discipline, may yield more maintainable code. This style is used in ASDF itself (starting with ASDF 3), by lisp-interface-library, and a few other libraries.
>
> [Source](https://asdf.common-lisp.dev/asdf/The-package_002dinferred_002dsystem-extension.html)

See the [ASDF Docs on Package Inferred Systems](https://asdf.common-lisp.dev/asdf/The-package_002dinferred_002dsystem-extension.html) for a short reference on it. Here we will explain it with examples in a more lengthly way.

## How to Define a Package Inferred System?

The option `package-inferred-system` has to be declared in the `.asd` file.

https://github.com/fare/asdf/blob/master/doc/best_practices.md

## Examples of Package Inferred Systems

- https://github.com/fare/lisp-interface-library?tab=readme-ov-file
- https://github.com/ruricolist/overlord
- https://github.com/ruricolist/vernacular
