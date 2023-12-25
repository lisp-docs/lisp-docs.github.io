---
slug: 2023-11-26-status
title: Current Status
authors: daniel
#   name: Daniel N
#   title: Lisp Docs Core Team
#   url: https://github.com/lisp-docs
  # image_url: https://github.com/wgao19.png
tags: [status, todo, beginning]
---

So finally, first version is out!

However, this is a work in progress.

These are the changes that are needed for it to reach a version 1.0:

1. ~~Fix all the markdown titles for the right side panel navigation bar. For some reason during the parsing, some of the titles got messed up in the navigation bar. The titles in the actual content are repeated. The second one must also be removed.~~ **Edit: 2023/11/26**
2. Need to link up all italics to the actual page in the reference.
3. Need to fix the tables in all the files. Idea: Could find each table in the original TeX files and parse those and replace the text in the current markdown since the original is hopefully structured.
4. Need to parse and find every bit of lisp code and wrap it in the proprer ``` backquote wrappers with the lisp syntax for pretty print. See #3, could try to see if the original TeX marked code blocks and use those with text replace.
5. Make sure everything else is good!
6. ~~Change the `_category_.json` files to display position of chapters in sidebar correctly...~~ **Edit: 2023/11/27**
7. ~~Change the `_category_.json` contents to use double quotes instead of single quotes...~~ **Edit: 2023/11/27**
8. Add search plugin
9. "beach": "The description of each operator should probably be on a separate page, because for the language reference, more material will be added there"

However, with that said, the project is actually ready for contributions!

## How The Project is Structured

The idea here was to separate the files with the original dpANS3r text from the community additions. Therefore, to add examples or any content, just create a new file. Please name it relevant to the section you will add it to, and then just import it in that section like all the other files are imported.

So instead of writing the examples in the dpANS3r itself, it will be a new file `examples.md` where you can write anything you want. Then you would import it wherever relevant.

Please note that because react doesn't seem to allow using numbers in component names, I had to convert all numbers to letters. `A=0`, `B=1`, etc...
