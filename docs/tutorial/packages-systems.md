---
sidebar_position: 2
---

# Packages and Systems

> Original Author: [Steve Losh](https://stevelosh.com)
>
> [Source](https://stevelosh.com/blog/2018/08/a-road-to-common-lisp/#s30-modern-common-lisp)
>
> Slightly modified to fit this tutorial.

## Structure

Common Lisp's terminology for various parts of projects is often confusing to new people because it's old and uses a lot of words that we use now (like <GlossaryTerm term={"package"}><i>[“package”](https://lisp-docs.github.io/cl-language-reference/chap-11/bb-b-package-concepts)</i></GlossaryTerm>) to mean subtly different things than people mean today. Things get easier once you internalize what Common Lisp means by the terms.

## Packages

We often see questions in IRC and Discord that look something like: "How do I export a class from a package"? Questions worded like this are a sign of a very common misunderstanding about what packages in Common Lisp *actually are*.

**A package in Common Lisp is a container for symbols.** That's it. They're a way to group related names (<GlossaryTerm term={"symbol"}><DictionaryLink term={"symbol"}><b>symbols</b></DictionaryLink></GlossaryTerm>) together so you don't have to do the miserable prefixing of every name with `mylibrary-...` like you need to do in Emacs Lisp or C to avoid name clashes.

You don't export a class from a package, you export a <GlossaryTerm term={"symbol"}><DictionaryLink term={"symbol"}><b>symbol</b></DictionaryLink></GlossaryTerm>. You don't import a function, you import the symbol it's attached to. This sounds pedantic, but is important to keep clear in your head as you start using the package system. If you're not clear on what exactly a symbol is, I wrote a [separate post](https://stevelosh.com/blog/2016/06/symbolic-computation/) just about symbols which you might find helpful.

Another major tripping point for new people is the relationship between packages and files. Or, rather: the completely lack of any relationship in Common Lisp.

In many languages like Python, Java, or Clojure, a file's package and its location on the hard drive are tied together. For example: when you say import `foo.bar.baz` in Python, Python will look for a `baz.py` file inside the `foo/bar/` directory (it's a little more complicated than this, but that doesn't matter for this example).

In Common Lisp, this is not the case. **Files and packages are completely unrelated in Common Lisp.** You can have many files that all work in the same package, or one file that switches between many packages, or even create or modify packages at runtime.

This gives you the flexibility to work however you want. For example: in my procedural art library [Flax](https://github.com/sjl/flax) most of the packages are each used in one specific file, much like you would do in modern languages. But the flax.drawing package contains not only a drawing protocol but also several implementations of that protocol (PNG, SVG, etc), and so I split the code into a [series of separate files](https://github.com/sjl/flax/tree/master/src/drawing), each one dealing with how to draw a single format (plus one for the protocol itself).

I could have created separate packages for each implementation and set up the imports/exports between them, but I didn't feel like the extra boilerplate was worth it. Common Lisp is flexible enough to let you make such choices.

So if files and packages aren't related, the next question is: how does Common Lisp know where to *find* anything on disk when it comes time to load the code?

## Systems

A system in Common Lisp is a collection of serveral things:

- Some code.
- A description of how to load that code.
- A list of other systems this system depends on, which need to be loaded prior to loading this one.
- Some metadata like author, license, version, homepage, etc.

The Common Lisp language itself has no knowledge of systems. If you look at [section 11.9](https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node120.html) of CLtL2 you'll see that it was imagined that each author would write their own custom file to load their code. But since Common Lisp gives you the power to abstract almost anything, people eventually abstracted the process of loading Common Lisp code.

[ASDF](https://asdf.common-lisp.dev/) is a Common Lisp library bundled with most modern implementations which handles defining and loading systems. The name ASDF stands for "Another System Definition Facility", so as you might guess there have been several other such libraries. ASDF is the one everyone uses today.

ASDF standardizes the process of defining a system into something like this:

The system definition(s) for a project called `foo` would be in a file named `foo.asd`.
Each system is defined with a `(defsystem ...)` form inside this file.
We'll talk more about what a "project" is shortly. Note the extension of the file is `asd`, not `asdf`, which is a little confusing, but was probably chosen to work in environments with three-letter-extension limits.

The [ASDF manual](https://asdf.common-lisp.dev/#documentation) is the definitive resource for the syntax and semantics of `defsystem`, but can be a little heavy to read if you're just getting started. Another way to get started is to read some .asd files of some small-to-medium sized open source projects and see how they handle things.

Systems and packages are orthogonal in Common Lisp. Some systems (like small libraries) will define exactly one package. Some systems will define multiple packages. Rarely a system might not define any new packages, but will use or add to an existing one.

For example:

- My directed graph library [cl-digraph](https://github.com/sjl/cl-digraph) contains a system called `cl-digraph`.
- That system has a description of how to load the code, which lives in the [`cl-digraph.asd`](https://github.com/sjl/cl-digraph/blob/master/cl-digraph.asd) file.
- One of the files specified for loading is [`package.lisp`](https://github.com/sjl/cl-digraph/blob/master/package.lisp), which creates a package called `digraph`.

Even though ASDF standardizes some aspects of system definition, it still gives you plenty of flexibility. As you read projects by different authors you'll encounter different ways of organizing systems — this can be a little overwhelming at first, but it means you can organize a system in the way that works *best for that system*, which is really nice once you've got some experience under your belt.

One example of this is how people define packages for their systems. There are a couple of common ways to do this you'll see in the wild:

- A single `package.lisp` file which contains all the definitions for all the packages in the project, and gets loaded before all other files. This is the strategy I usually prefer.
- Each file defines its package at the top of the file, much like you would in Clojure or other modern languages. Care is taken in the system definition to load the files in the correct order so that each package is defined before it is ever used.
To review: a system is a collection of code and a description of how to load it, a list of its dependencies, and some metadata. Now let's move up one level higher to the final layer of structure you need to know about.

## Projects

A project in Common Lisp is not an official term defined anywhere that I know of, but is a word that's generally used to mean something like a library, a framework, an application, etc.

A project will usually define at least one system, because systems are where you describe how to load the code, and if a project didn't define a system how would you know how to load its code? My string-wrapping library [Bobbin](https://github.com/sjl/bobbin) is a project that defines *two* systems:

- The `bobbin` system contains the actual data structure and API. It has no dependencies.
- The `bobbin/test` system contains the unit tests. It depends on the `bobbin` system (because that's the code it's going to test) and the `1am` system (a unit test framework). I made this a separate system because it allows users to load the main code without also having to load the unit testing framework if they're not going to be running the tests.

Both of these systems are defined in the [`bobbin.asd`](https://github.com/sjl/bobbin/blob/master/bobbin.asd) file. ASDF [treats systems with a forward slash in their name specially](https://asdf.common-lisp.dev/asdf.html#index-find_002dsystem) and knows to look for them in the asd file named with the text before the slash.

We saw how Common Lisp has no concept of a system — that concept comes from ASDF. Similarly, ASDF has no concept of the internet or of reaching out to somewhere to download things. ASDF assumes you have somehow acquired the systems you want to load and stored them on your hard drive, perhaps by sending a check to an address and receiving a copy of the code on floppy disk, as many of my old Lisp books offer in their final pages.

[Quicklisp](https://www.quicklisp.org/beta/) is another library that works on top of ASDF to provide the "download projects from the internet automatically if necessary" functionality that people expect in the modern world. So when you say `(ql:quickload :bobbin)` you’re asking Quicklisp to download Bobbin (and any dependencies) if necessary, and then hand it off to ASDF to actually load the code of the bobbin system.

Unlike ASDF, Quicklisp is relatively new in the Common Lisp world (it's only about eight years old) and so is not bundled with any modern Lisp implementations that I know of, which is why you need to install it separately.

## Recap

Here's a quick recap of the different layers of project structure you'll encounter in Common Lisp. Jot these down on a post it note you can refer to as you're learning.

- **Files** are files on your hard drive.
- **Packages** are containers of symbols. They are orthogonal to files.
- **Systems** are collections of code, instructions on how to load that code, dependency lists, and metadata. They are orthogonal to packages.
- **Projects** are high-level collections of... "stuff" such as code, documentation, maybe some image assets, etc. They are (mostly) orthogonal to systems (are you seeing a trend here?).
- Common Lisp itself knows about files and packages.
- ASDF adds systems.
- Quicklisp adds the internet.
