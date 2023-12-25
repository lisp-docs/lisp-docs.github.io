# Contribute to the CL Technical Reference

## How To Contribute

Please just do a regular contribution on our [GitHub Repository](https://github.com/lisp-docs/cl-language-reference). 

## Contribution Requirements

If there's an error in the dpANSr text that you are fixing, then change it right away in the relevant file.

For adding examples and explanations, please create a separate file marked section-number-examples.md or section-number-explanations.md and add an import in the appropiate section. The purpose of this is to keep separate the community added explanations and examples from the original specification. That way it will be easier to modify and add things in the future without having to worry about modifying the original spec. It will also be clear what is the original vs. the new content.

## What To Contribute

1. Examples of any aspect mentioned in the reference
2. Explanations of any aspect of the reference.
3. Fixes to the original specification when there were error. Please note the change with an admonition if that is the case.
4. Whatever you think will make this reference better!

Please also see the [Status Blog Post](/blog/2023-11-26-status) for more information on what to contribute and project TODO's.

Please checkout our [TODO page](/docs/contribute/todo) for more.

## Sample Contributions

Here is a [sample commit](https://github.com/lisp-docs/cl-language-reference/commit/035001b98948524c1b03bc11b504709b47693be9) which shows a fix to the original specification. Please note the admonition by surounding the text with `\n:::info\n` and `\n:::\n`.
