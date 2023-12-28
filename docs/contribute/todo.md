
# Todo

Here we will list all the To Do items we have for this project, and you can help by taking care of any of them :D.

## Technical Reference

- We need to format al the Tables to markdown.
- **Examples**: We need expanded examples and **Explanations** for most of the content in the reference.
- Please take a look at the [Dictionary Items To Do's](/docs/contribute) in particular
- Update lisp code to be wrapped in code blocks to be formated with markdown. Markdown supports math formulas and special text, checkout the [Markdown Syntax](https://commonmark.org/help/) tutorial for how to do it. Note that we use [MDX](https://docusaurus.io/docs/markdown-features) instead of vanilla markdown.
  - **2023/12/06 Update:** All the dictionary pages example sections were surrounded with code blocks for syntax highlighting, so therefore most of what's left are the code blocks in the write up sections of the reference.
- Links to Sections like in See Also "Section 6.1.1.7 (Destructuring) "
- https://www.cliki.net/ANSI%20Clarifications%20and%20Errata
- https://www.lispworks.com/documentation/HyperSpec/Issues/I_Alpha.htm

## Tutorial

We need help writing any of these sections:

- Make beginner and experienced programmer tutorials
- Other items in the [Tutorial](https://github.com/lisp-docs/lisp-docs.github.io/tree/main/docs)
- [Getting Started](/docs/getting-started) pages
- [FAQs](/docs/faq)
- [Why Lisp](/docs/whylisp) pages for beginners and CTOs.


## Done

- ~~All the *Dictionary* Sections have to be separated in the docs. For example, all of section 9.2 in the original is included in our [9.1](https://lisp-docs.github.io/cl-language-reference/docs/chap-9/j-b-condition-system-concepts). We need to split that file into two. Similarly for chapters~~
  - ~~3.8~~
  - ~~4.4~~
  - ~~5.3~~
  - ~~6.2~~
  - ~~7.7~~
  - ~~8.1?~~
  - ~~9.2~~
  - ~~etc. It seems the last section of every chapter which is the *Dictionary* was bundled into the next to last section.~~
- ~~Fix the Dictionary Entry pages that were not parsed correctly.~~
- ~~Indent Common Lisp code blocks.~~
- ~~Break the Glossary into 27 pages, one per letter, plus the introduction and non alphabetical symbols page.~~
- ~~Add `<ClLinks>` wrapping to all italic and bolded text in the specification except for that inside code blocks and in titles, to provide links and tooltips...~~
- ~~Add Tooltips with definitions from the glossary everywhere that is relevant~~
  - The best way to do it is by creating a hash table of the dictionary, and everywhere an italizaed word is present, suroound it with a React Component which will check if there's a definition for that text in the glossary and it will add a tooltip if it's relevant
  - This should probably be a react component so that it can also be used in oher Lisp Docs pages and tutorials and shared easily across projects
  - Update the glossary to be HTML and not markdown
  - Finish all the dicionary item's parsing, then update the json files for the glossary and the dicionary terms
- ~~We should add links to all the bolded items in the reference to go to the appropiate pages. This should be actually fairly straight forward to do with code. We can just build a hash table of the dictionary pages available, with the name of each page, then simply find all the bolded text in the reference, and if its name matches an entry in the hash table, turn in into a link for that page.~~
- ~~Make the Tooltip Definition and links to dicionary pages~~
- ~~[Multiple Sidebars](https://docusaurus.io/docs/sidebar/multiple-sidebars) for: ~~
  - ~~Contribute~~
  - Why Lisp Section: For beginners, professionals, CTOs or Project Managers
- ~~In all code blocks: ~~
  - replace \&gt; and \&lt; with \>\<,
  - remove extra white lines script run...
  - run indent code blocks again
  - any place that is not inside a code block, or in a title, which is italics and or bold, add the react lisp-docs util...
  - get react router, test if it works, check base name, then do local route... Links...
  - go through all dictionary items, check that they are all there
- ~~make code blocks from sections named example which have figures...~~
  - title of section is examples
    - has a figure
  - no figure, then in that section, find first line which is either \n\s*\( or \n\s;, and start code block from there till the end of the section
