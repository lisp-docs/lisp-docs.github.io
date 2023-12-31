---
title: About the Technical Reference
sidebar_position: 1
---

The point of the technical reference is to provide documentation to both learn and reference Common Lisp. The goal is to have **additional** **explanations** and **examples** on everything to make the language much easier **to learn** and understand.

Features of the Reference:

- Additional explanations to the standard specification
- Additional examples to the standard specification
- Fixes to mistakes in the standard specification
- It has a modern rendering of the ANSI dpANS3R+ standard draft specification
  - dpANS3R+ being the dpANS3 with the dpANS3R changes applied to it.
  - See the [source repo here](https://github.com/lisp-docs/cl-standard/tree/master/dpANS3R%2B)
- Glossary definition tooltips to make it easier to read.
- Code Blocks with Syntax Highlighting
- Github repo where anyone can contribute
- Easy Search of the Reference with `Ctrl+k` or `Cmd+k`
- Side Panel Table of Contents of the whole Reference
- Side Panel Table of Contents of the specific section being read

## But why? Aren't there a bunch of spec projects already?

### What, Why and for Whom is a Technical Reference?

A Technical Reference is a document **explaining** the aspects of the language, the **consequences** of those aspects, sometimes the **reasoning** behind them, and **examples** of usage.

The target audience for a Technical Reference are the **users** of the language i.e. people who are programming in Common Lisp.

The purpose of the reference is both to **learn** and **review** in detail **all aspects** of the language.

### What, Why and for Whom is a Language Specification?

A Language Specification is a **concise**, **precise**, and **accurate** **description** of **what** the aspects of a language are.

The target audence is mainly for **language implementers** to have a clear writing of what the aspects of the language are.

The purpose of the specification is so that different implementations will follow the same definitions of what the language is thereby providing the ability for any conforming Common Lisp code written to be able to work in any Common Lisp implementation. In other words any conforming Common Lisp code should be able to be executed by any Common Lisp Compiler and provide the same result.

The following are **not** part of what a Language Specification is:

- **How** the implementation is done.
- **Why** the aspects of the language are the way they are.
- **Examples** of the functionality in breath and depth.
- **Consequences** of the different aspects of the language working together.
- **Explanation** of concepts of the aspects of the language.

### An example of the difference between the Specification and the Reference

A **C programmer** who **does not know** how to program in Common Lisp, who **has never** programmed in Common Lisp, and **will never** program in Common Lisp could be a primary member of the **target audicence of the Specification**. If he is focused on writing a Common Lisp compiler.

However, such a **C Programmer** is **not the target audience of the Technical Reference** at all. Not only that, but for him to read the reference could be a waste of time.

Someone who is **programming in Common Lisp** actively is the **target audience of the Technical Reference**. From reading the standard it is not always clear how things work, nor why they do, nor what the normal way of applying certain aspect of the language is. How to use aspects of the language and how they combine to provide functionality is not explained in the Standard Specification. However those are the primary purposes of the Reference.

### Technical Reference Projects

There is only **one** [Technical Reference](https://lisp-docs.github.io/cl-language-reference/) project. The one in this website. If you hear of another project, let me know and I could add it to this list.

### Language Specification Projects

First a note on copyright: The official standard specification is **copyrighted** by **ANSI** and therefore it is illegal to reproduce without permission. The [CL Hyperspec (CLHS)](https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm) and [Franz Inc's Allegro CL Docs](https://franz.com/support/documentation/11.0/ansicl/ansicl.htm) are one of the few renderings based on this document besides ordering a paper copy from ANSI. Take a look at the CLHS [authorship note](https://www.lispworks.com/documentation/HyperSpec/Front/Help.htm#Authorship).

However, before the final version of the standard specification was agreed upon, a **draft** was published which became part of the **Public Domain**. Therefore anyone is free to use that version of the draft for whatever they want. All projects besides those with explicit copyright permission (the CL HyperSpec and Franz Inc. Allegro CL among them) are based on previous draft versions. There are drafts dpANS1, dpANS2, dpANS3, dpANS3R, dpANS3R+.

There have been quite a few Language Specification projects. Their goals usually being to provide an accessible rendering of the Specification. Here's a list of some them with notes:

- [ANSI Standard Specification](https://www.ansi.org/)
- [dpANS3, dpANS3R, and dpANS3R+ Original Sources](https://github.com/lisp-docs/cl-standard)
- [Project Building a PDF from the original TeX files](https://gitlab.com/vancan1ty/clstandard_build)
  - [PDF Output](https://github.com/lisp-docs/cl-standard/blob/master/new-spec/spec-source-pdf/cl-ansi-standard-draft-w-sidebar.pdf) of the project in the pevious repository. The previous repository contains a copy of this project. The latest public domain draft dpANS3R+ is the source used in the Technical Reference.
- [Common Lisp HyperSpec](https://www.lispworks.com/documentation/HyperSpec/Front/Contents.htm) is a HTML version with links of the original TeX files. It also contains errata and other details from the Specification. Note that this is **on of the only** projects based on the **final specification**. However even this project is **not the same** as the ANSI Standard.
- Franz, Inc's Allegro CL Docs rendering of the [ANSI Standard Specification](https://franz.com/support/documentation/11.0/ansicl/ansicl.htm).
  - I couldn't find where they state they got permission to publish, but nevertheless they [claim](https://franz.com/support/documentation/11.0/introduction.html#12-ansi-common-lisp-and-metaobject-protocol-documentation) it is the standard. 
  - Please note that their implementation has [non-comformance on some cases](https://franz.com/support/documentation/11.0/implementation.html#compliance-1) with the ANSI Standard. It's a good idea to check any implementation you work with if it has any non-comformance so that it doesn't surprise you if your code does not execute as expected.
- [CL Community Spec](https://cl-community-spec.github.io/pages/index.html). Here are some quotes of the issues:
  - > I started out using not the latest version of the draft, so that X3J13 changes were not included.
  - > After I started this project, someone pointed me to the NovaSpec, which is a nice rendering, much more complete than this, and improving a lot on the CLHS in my opinion.
- [NovaSpec](https://novaspec.org/cl/). See below for notes.

### A Note on **NovaSpec**

The purpose of NovaSpec as a specification is again for language implementers. It is **not** for people learning Common Lisp. Before working on most of the Technical Reference I spoke with Gilbert Baumann, the author of NovaSpec, to see if our goals aligned and we should just work together. However that is not the case and in fact **we need both projects**. NovaSpec is for compiler/implementation writers to have a better specification. There are **errors** and **problems** with the final official ANSI Standard. Yes, you read correctly. There are sometimes errors in examples, there are typos, changes to some sections that were agreeed upon but were not propagated throughout the entire specification cause discrepancies, and there  are other issues as well.

The NovaSpec project is mainly to fix many of those issues by providing **annotations** to the specification, much in the way as they are in this [sample page](http://bauhh.dyndns.org:8000/clim-spec/G.html#_1992) for CLIM.

The goal of NovaSpec is to provide those annotations and fixes to have a better specification than the official ANSI Standard Specification and (I imagine) to **replace** the ANSI Standard Specification with this new improved specification. Both specifications should be defining the exact same Common Lisp, with the NovaSpec one being more correct.

Therefore **both** projects are needed and should work in parallel. In fact, according to the corrections done to the NovaSpec, the Technical Reference should change as well. That is why people should contribute to **both** projects.

Whereas the specification is the **final** goal of NovaSpec (as the name implies, new specification...), the specification is merely the **beginning** point for the Technical Reference. It should evolve from there to have many more examples and explanations. The reason for starting with the same structure is to make it easier to read the specification from the reference. However, the Technical Reference will most certainly have new sections not contained in the specification to detail and explain usage of aspects of the language. It is not guaranteed that the section numbers will stay the same. I personally think it's a good idea if they do, but that will depend on future contributors and the community. The changes will be based on the needs of having a clearer reference giving preference to making it clearer and easier to learn and review over keeping section numbers aligned with the specification.

As of today, Thursday 2023/12/28, here are some sample additions to the Technical Reference that would **not** be added to the specification because of the differing goals:

- [function](https://lisp-docs.github.io/cl-language-reference/chap-5/f-d-dictionary/function_special-operator#expanded-reference-function)
- [or](https://lisp-docs.github.io/cl-language-reference/chap-5/f-d-dictionary/or_macro#expanded-reference-or)
- [and](https://lisp-docs.github.io/cl-language-reference/chap-5/f-d-dictionary/and_macro#expanded-reference-and)

Note that those changes are in the "Expanded Reference" but in the future changes may be done to the original specification text as long as it remains correct.

Here are sample changes that would apply to both the Technical Reference and the Specification because they are errors in the original:

- [progn examples](https://lisp-docs.github.io/cl-language-reference/chap-5/f-d-dictionary/progn_special-operator)
- [and examples](https://lisp-docs.github.io/cl-language-reference/chap-5/f-d-dictionary/and_macro)
- [or examples](https://lisp-docs.github.io/cl-language-reference/chap-5/f-d-dictionary/or_macro#expanded-reference-or)
- [function examples](https://lisp-docs.github.io/cl-language-reference/chap-5/f-d-dictionary/function_special-operator)
- [cond examples](https://lisp-docs.github.io/cl-language-reference/chap-5/f-d-dictionary/cond_macro)
- [funcall examples](https://lisp-docs.github.io/cl-language-reference/chap-5/f-d-dictionary/funcall_function)

However in the NovaSpec they may be marked as annotations to the original, whereas in the Technical Reference they are parts of the text itself without a real need to annotate.
