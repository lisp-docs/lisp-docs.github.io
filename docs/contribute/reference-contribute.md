---
sidebar_position: 2
---

# Technical Reference

## How To Contribute

Please just do a regular contribution on our [GitHub Repository](https://github.com/lisp-docs/cl-language-reference).

Please note that this is a **different repository** than the tutorial repository.

## Contribution Workflow

If there's an error in the dpANSr text that you are fixing, then change it right away in the relevant file. They are usually the files with the `_` prefix.

For adding examples and explanations, please edit the corresponding file (usually the one without the `_` prefix), or you can create a separate file marked section-number-examples.md or section-number-explanations.md and add an import in the appropiate section. The purpose of this is to keep separate the community added explanations and examples from the original specification. That way it will be easier to modify and add things in the future without having to worry about modifying the original spec. It will also be clear what is the original vs. the new content.

For all the dicionary item pages there already are "Expanded Reference" sections. For example the [and macro page](https://lisp-docs.github.io/cl-language-reference/chap-5/f-d-dictionary/and_macro#expanded-reference-and) already has a contribution.

Please note the difference between the [expanded reference file](https://github.com/lisp-docs/cl-language-reference/blob/develop/docs/chap-5/f-d-dictionary/and_macro.md?plain=1)  and the [original specification file](https://github.com/lisp-docs/cl-language-reference/blob/develop/docs/chap-5/f-d-dictionary/_and_macro.md?plain=1)

## What To Contribute

1. Examples of any aspect mentioned in the reference
2. Explanations of any aspect of the reference.
3. Fixes to the original specification when there were error. Please note the change with an "info" admonition if that is the case. [Here's a sample admonition](https://lisp-docs.github.io/cl-language-reference/chap-5/f-d-dictionary/and_macro) (The "INFO" box near the bottom). Here are the [admonition docs](https://docusaurus.io/docs/markdown-features/admonitions).
4. Whatever you think will make this reference better!

Please also see our [TODO page](/docs/contribute/todo) for more information on what to contribute and project TODO's.

## Sample Contributions

Here is a [sample commit](https://github.com/lisp-docs/cl-language-reference/commit/035001b98948524c1b03bc11b504709b47693be9) which shows a fix to the original specification. Please note the admonition by surounding the text with `\n:::info\n` and `\n:::\n`.
