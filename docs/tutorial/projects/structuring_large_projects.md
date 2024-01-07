# Structuring Large Projects

:::warning
This article is incomplete, we need to finish writing it. Please help by contributing to the project.
:::

## Introduction

- What is a protocol? bring link from Eclector and CLIM docs and maybe even quote it
- What is a module?
- What is a package. Sample of CL-USER package which is so large and how it's different from say java and python where a file is a semantic unit in the code, whereas in lisp it is not really relevant, files are more for us to read and organize code, but not semantically meaningful
- Note about CLOS how it's data representation and not encapsulation (contrast in a collapsible with java)

## How To Do It



## Examples

- [Cluffer](https://github.com/robert-strandh/Cluffer/blob/master/cluffer.asd)
  - add discussion here with other sample `asd` files, and how they declare all packages and classes etc each in their own files
  - [cluffer-base.asd](https://github.com/robert-strandh/Cluffer/blob/master/Base/cluffer-base.asd)
    - [packages.lisp](https://github.com/robert-strandh/Cluffer/blob/master/Base/packages.lisp)
    - [internal-protocol.lisp](https://github.com/robert-strandh/Cluffer/blob/master/Base/internal-protocol.lisp)
  - [cluffer-standard-line.asd](https://github.com/robert-strandh/Cluffer/blob/master/Standard-line/cluffer-standard-line.asd)
- What other projects? see irc logs...