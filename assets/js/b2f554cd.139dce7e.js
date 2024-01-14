"use strict";(self.webpackChunklisp_docs_github_io=self.webpackChunklisp_docs_github_io||[]).push([[1477],{10:e=>{e.exports=JSON.parse('{"blogPosts":[{"id":"/2023/12/24/","metadata":{"permalink":"/blog/2023/12/24/","editUrl":"https://github.com/lisp-docs/lisp-docs.github.io/blog/2023-12-24.md","source":"@site/blog/2023-12-24.md","title":"Beta Version Released!","description":"Finally, I can announe that the beta version is out!","date":"2023-12-24T00:00:00.000Z","formattedDate":"December 24, 2023","tags":[{"label":"status","permalink":"/blog/tags/status"},{"label":"beta","permalink":"/blog/tags/beta"},{"label":"beginning","permalink":"/blog/tags/beginning"},{"label":"tooltips","permalink":"/blog/tags/tooltips"},{"label":"parsing","permalink":"/blog/tags/parsing"}],"readingTime":0.62,"hasTruncateMarker":false,"authors":[{"name":"Daniel N","title":"Lisp Docs Contributor","url":"https://github.com/daninus14","key":"daniel"}],"frontMatter":{"title":"Beta Version Released!","authors":"daniel","tags":["status","beta","beginning","tooltips","parsing"]},"unlisted":false,"nextItem":{"title":"Update: Last Parsing of the Spec Done!","permalink":"/blog/2023/12/21/"}},"content":"Finally, I can announe that the **beta version** is out!\\n\\nThis is what I updated since last time:\\n\\n- Parsed again all the specification to account for missing/overwritten pages because of similarly named items.\\n- Added glossary definition tooltips everywhere I could (programatically, we can add more by hand by identifying any place missing a definition).\\n- Added all the links of \\"See Also\\" and any time something is referenced in the text of the specification, the link for that item should now be working.\\n- Added a lot of code blocks for example sections to provide syntax highlighting. Not all of the code in the reference is inside a code block, but most of them are. The rest most be parsed manually.\\n\\nEnjoy!"},{"id":"/2023/12/21/","metadata":{"permalink":"/blog/2023/12/21/","editUrl":"https://github.com/lisp-docs/lisp-docs.github.io/blog/2023-12-21.md","source":"@site/blog/2023-12-21.md","title":"Update: Last Parsing of the Spec Done!","description":"This is a major update. This is basically (hopefully) the last time we parsed the specification. There were a few bugs in the previous parsing which led to some missing pages because they were overwritten when multiple dictionary items shared the same name.","date":"2023-12-21T00:00:00.000Z","formattedDate":"December 21, 2023","tags":[{"label":"status","permalink":"/blog/tags/status"},{"label":"dictionary-items","permalink":"/blog/tags/dictionary-items"}],"readingTime":0.535,"hasTruncateMarker":false,"authors":[{"name":"Daniel N","title":"Lisp Docs Contributor","url":"https://github.com/daninus14","key":"daniel"}],"frontMatter":{"title":"Update: Last Parsing of the Spec Done!","authors":"daniel","tags":["status","dictionary-items"]},"unlisted":false,"prevItem":{"title":"Beta Version Released!","permalink":"/blog/2023/12/24/"},"nextItem":{"title":"Update: Dictionary Items Status","permalink":"/blog/2023/12/17/"}},"content":"This is a major update. This is basically (hopefully) the last time we parsed the specification. There were a few bugs in the previous parsing which led to some missing pages because they were overwritten when multiple dictionary items shared the same name.\\n\\nWith this parsing, I fixed a bunch of other issues, so hopefully this will be the last parsing update, and from here on out it will just be manual updates to this project.\\n\\nSo this makes it version 2.0 beta is out!\\n\\nAdditionally, the glossary has been parsed again, and improved dramatically, so that the tooltips should now show, and have proper html rendering."},{"id":"/2023/12/17/","metadata":{"permalink":"/blog/2023/12/17/","editUrl":"https://github.com/lisp-docs/lisp-docs.github.io/blog/2023-12-17.md","source":"@site/blog/2023-12-17.md","title":"Update: Dictionary Items Status","description":"Many dictionary item pages were updated. However, we still need to check quite a few chapters. In particular from chapter 12 to chapter 21 inclusive, we need to check the table of contents vs the standard specification dictionary items for that chapter.","date":"2023-12-17T00:00:00.000Z","formattedDate":"December 17, 2023","tags":[{"label":"status","permalink":"/blog/tags/status"},{"label":"dictionary-items","permalink":"/blog/tags/dictionary-items"}],"readingTime":0.21,"hasTruncateMarker":false,"authors":[{"name":"Daniel N","title":"Lisp Docs Contributor","url":"https://github.com/daninus14","key":"daniel"}],"frontMatter":{"title":"Update: Dictionary Items Status","authors":"daniel","tags":["status","dictionary-items"]},"unlisted":false,"prevItem":{"title":"Update: Last Parsing of the Spec Done!","permalink":"/blog/2023/12/21/"},"nextItem":{"title":"Latest Updates: Definitions Tooltip and others","permalink":"/blog/2023/12/12/"}},"content":"Many dictionary item pages were updated. However, we still need to check quite a few chapters. In particular from chapter 12 to chapter 21 inclusive, we need to check the table of contents vs the [standard specification](https://github.com/lisp-docs/cl-standard/blob/master/new-spec/spec-source-pdf/cl-ansi-standard-draft-w-sidebar.pdf) dictionary items for that chapter."},{"id":"/2023/12/12/","metadata":{"permalink":"/blog/2023/12/12/","editUrl":"https://github.com/lisp-docs/lisp-docs.github.io/blog/2023-12-12.md","source":"@site/blog/2023-12-12.md","title":"Latest Updates: Definitions Tooltip and others","description":"Definitions Tooltip","date":"2023-12-12T00:00:00.000Z","formattedDate":"December 12, 2023","tags":[{"label":"status","permalink":"/blog/tags/status"},{"label":"beginning","permalink":"/blog/tags/beginning"}],"readingTime":0.855,"hasTruncateMarker":false,"authors":[{"name":"Daniel N","title":"Lisp Docs Contributor","url":"https://github.com/daninus14","key":"daniel"}],"frontMatter":{"title":"Latest Updates: Definitions Tooltip and others","authors":"daniel","tags":["status","beginning"]},"unlisted":false,"prevItem":{"title":"Update: Dictionary Items Status","permalink":"/blog/2023/12/17/"},"nextItem":{"title":"Code Blocks Indented","permalink":"/blog/2023/12/07/code-indented"}},"content":"## Definitions Tooltip\\n\\nI made a react component tooltip for definitions to show you the definition of a term in place instead of a having to navigate to the glossary page. In addition, the same tooltip will link to the proper dictionary item if it exists, and if not, to the glossary page of the term. We still need to add the component all around, but this should make the functionality of the page much better. Here\'s an example: [See the \\"forms\\" word in section 6.1.1.1 of Loops](https://lisp-docs.github.io/cl-language-reference/docs/chap-6/g-b-the-loop-facility)\\n\\n## Dictionary Pages\\n\\nThere are a bunch of dictionary pages that did not get split correctly, so instead of showing up as a different page, they are part of another page. I\'ve been checking them manually (a lot of work) and have made [a list](/docs/contribute) of about half the chapters. The rest need to be checked. I hope to make a script to help splitting a lot of these pages. Please help identifying the pages in the missing chapters as they are marked."},{"id":"/2023/12/07/code-indented","metadata":{"permalink":"/blog/2023/12/07/code-indented","editUrl":"https://github.com/lisp-docs/lisp-docs.github.io/blog/2023-12-07-code-indented.md","source":"@site/blog/2023-12-07-code-indented.md","title":"Code Blocks Indented","description":"Code Blocks Indented","date":"2023-12-07T00:00:00.000Z","formattedDate":"December 7, 2023","tags":[{"label":"status","permalink":"/blog/tags/status"},{"label":"beginning","permalink":"/blog/tags/beginning"},{"label":"glossary","permalink":"/blog/tags/glossary"}],"readingTime":0.165,"hasTruncateMarker":false,"authors":[{"name":"Daniel N","title":"Lisp Docs Contributor","url":"https://github.com/daninus14","key":"daniel"}],"frontMatter":{"title":"Code Blocks Indented","authors":"daniel","tags":["status","beginning","glossary"]},"unlisted":false,"prevItem":{"title":"Latest Updates: Definitions Tooltip and others","permalink":"/blog/2023/12/12/"},"nextItem":{"title":"Glossary Improved!","permalink":"/blog/2023/12/07/"}},"content":"## Code Blocks Indented\\n\\nGood news, now the code blocks look even better! I indented them using the emacs indentation so it follows the conventional indentation. Changes made to 414 files :D\\n\\nEnjoy!"},{"id":"/2023/12/07/","metadata":{"permalink":"/blog/2023/12/07/","editUrl":"https://github.com/lisp-docs/lisp-docs.github.io/blog/2023-12-07.md","source":"@site/blog/2023-12-07.md","title":"Glossary Improved!","description":"Glossary Improved","date":"2023-12-07T00:00:00.000Z","formattedDate":"December 7, 2023","tags":[{"label":"status","permalink":"/blog/tags/status"},{"label":"beginning","permalink":"/blog/tags/beginning"},{"label":"glossary","permalink":"/blog/tags/glossary"}],"readingTime":0.21,"hasTruncateMarker":false,"authors":[{"name":"Daniel N","title":"Lisp Docs Contributor","url":"https://github.com/daninus14","key":"daniel"}],"frontMatter":{"title":"Glossary Improved!","authors":"daniel","tags":["status","beginning","glossary"]},"unlisted":false,"prevItem":{"title":"Code Blocks Indented","permalink":"/blog/2023/12/07/code-indented"},"nextItem":{"title":"Alpha Version 1.0 is Out!","permalink":"/blog/2023/12/06/"}},"content":"## Glossary Improved\\n\\nThe glossary has now:\\n\\n- Been split into multiple pages, one per letter\\n- The legend and introduction have their page\\n- A lot of the text was fixed for formatting issues, this was the bulk of the work"},{"id":"/2023/12/06/","metadata":{"permalink":"/blog/2023/12/06/","editUrl":"https://github.com/lisp-docs/lisp-docs.github.io/blog/2023-12-06.md","source":"@site/blog/2023-12-06.md","title":"Alpha Version 1.0 is Out!","description":"Finally, we are at Alpha, version 1.0 is out!","date":"2023-12-06T00:00:00.000Z","formattedDate":"December 6, 2023","tags":[{"label":"status","permalink":"/blog/tags/status"},{"label":"beginning","permalink":"/blog/tags/beginning"}],"readingTime":1.29,"hasTruncateMarker":false,"authors":[{"name":"Daniel N","title":"Lisp Docs Contributor","url":"https://github.com/daninus14","key":"daniel"}],"frontMatter":{"title":"Alpha Version 1.0 is Out!","authors":"daniel","tags":["status","beginning"]},"unlisted":false,"prevItem":{"title":"Glossary Improved!","permalink":"/blog/2023/12/07/"},"nextItem":{"title":"Dicionary Pages Broken Up","permalink":"/blog/2023/12/03/"}},"content":"## Finally, we are at Alpha, version 1.0 is out!\\n\\nAs of today, 2023/12/06 the Alpha version of the technical reference is out.\\n\\nThese are a few of the improvements since last time:\\n\\n- Complete Specification Update: There were parts of the spec missing because of an issue of files getting truncated before they were converted into markdown. I have now recovered the truncated parts, parsed them, and added them to the specification\\n- A large amount of code now shows up in code blocks with syntax highlighting.\\n- A lot of symbols like `\u2192`, `\u2261`, and a few others are now properly displayed\\n- The Index, Figures, and Credits were parsed to be displayed properly per the original\\n- The search index was updated to account for the new content\\n- A bunch of formatting issues for the markdown to be displayed properly\\n\\n## What Alpha version means\\n\\n- Now we are finally open to contributions of examples and added explanation\\n  - Contribution stubs were added at the end of Dictionary Items type pages see for example [progn](https://lisp-docs.github.io/cl-language-reference/docs/chap-5/f-d-dictionary/progn#expanded-reference-progn)\\n- The main text is unlikely to change much except for:\\n  - Minor formatting issues\\n  - Maybe there are still some symbols that are not rendering properly\\n  - Code Blocks for syntax highlighting in the non dictionary type pages have not yet been added\\n  - Tables have to be formatted\\n- Therefore we are open to other contributions that are more meaningful in terms of content and development, please see our [To Do Page](/docs/contribute/todo) for how you can contribute!"},{"id":"/2023/12/03/","metadata":{"permalink":"/blog/2023/12/03/","editUrl":"https://github.com/lisp-docs/lisp-docs.github.io/blog/2023-12-03.md","source":"@site/blog/2023-12-03.md","title":"Dicionary Pages Broken Up","description":"Dicionary Sections","date":"2023-12-03T00:00:00.000Z","formattedDate":"December 3, 2023","tags":[{"label":"status","permalink":"/blog/tags/status"},{"label":"beginning","permalink":"/blog/tags/beginning"}],"readingTime":0.39,"hasTruncateMarker":false,"authors":[{"name":"Daniel N","title":"Lisp Docs Contributor","url":"https://github.com/daninus14","key":"daniel"}],"frontMatter":{"title":"Dicionary Pages Broken Up","authors":"daniel","tags":["status","beginning"]},"unlisted":false,"prevItem":{"title":"Alpha Version 1.0 is Out!","permalink":"/blog/2023/12/06/"},"nextItem":{"title":"Search Implemented!","permalink":"/blog/2023/11/30/update"}},"content":"## Dicionary Sections\\n\\nAs of now, 2023/12/03, all the dicionary pages have been broken up into their particular directories with a page per entry.\\n\\nHowever there are still a few that need to be manually fixed because of problems with parsing the text with regex for certain exceptions.\\n\\nNevertheless, this improves the search results dramatically! The reference should now be much easier to use!\\n\\nI also added a [Dicionary Item\'s To Do Page](/docs/contribute) for adding specific examples.\\n\\nCheers!"},{"id":"/2023/11/30/update","metadata":{"permalink":"/blog/2023/11/30/update","editUrl":"https://github.com/lisp-docs/lisp-docs.github.io/blog/2023-11-30-update.md","source":"@site/blog/2023-11-30-update.md","title":"Search Implemented!","description":"Search for the Technical Reference","date":"2023-11-30T00:00:00.000Z","formattedDate":"November 30, 2023","tags":[{"label":"status","permalink":"/blog/tags/status"},{"label":"search","permalink":"/blog/tags/search"},{"label":"features","permalink":"/blog/tags/features"},{"label":"beginning","permalink":"/blog/tags/beginning"}],"readingTime":0.18,"hasTruncateMarker":false,"authors":[{"name":"Daniel N","title":"Lisp Docs Contributor","url":"https://github.com/daninus14","key":"daniel"}],"frontMatter":{"title":"Search Implemented!","authors":"daniel","tags":["status","search","features","beginning"]},"unlisted":false,"prevItem":{"title":"Dicionary Pages Broken Up","permalink":"/blog/2023/12/03/"},"nextItem":{"title":"Code Blocks and Tables","permalink":"/blog/2023/11/29/"}},"content":"## Search for the Technical Reference\\n\\nSearch for the Technical Reference has now been implemented! Please try it with `Ctrl+K` or `CMD-K` and enjoy! :smile:\\n\\n## Home Page Updates\\n\\nWe also updated the [Home Page](/) content."},{"id":"/2023/11/29/","metadata":{"permalink":"/blog/2023/11/29/","editUrl":"https://github.com/lisp-docs/lisp-docs.github.io/blog/2023-11-29.md","source":"@site/blog/2023-11-29.md","title":"Code Blocks and Tables","description":"Code Blocks","date":"2023-11-29T00:00:00.000Z","formattedDate":"November 29, 2023","tags":[{"label":"status","permalink":"/blog/tags/status"},{"label":"todo","permalink":"/blog/tags/todo"},{"label":"beginning","permalink":"/blog/tags/beginning"},{"label":"code-blocks","permalink":"/blog/tags/code-blocks"},{"label":"tables","permalink":"/blog/tags/tables"}],"readingTime":0.28,"hasTruncateMarker":false,"authors":[{"name":"Daniel N","title":"Lisp Docs Contributor","url":"https://github.com/daninus14","key":"daniel"}],"frontMatter":{"title":"Code Blocks and Tables","authors":"daniel","tags":["status","todo","beginning","code-blocks","tables"]},"unlisted":false,"prevItem":{"title":"Search Implemented!","permalink":"/blog/2023/11/30/update"},"nextItem":{"title":"Current Status","permalink":"/blog/2023-11-26-status"}},"content":"## Code Blocks\\n\\nThe current status is that for some reason replacing the code with lisp formatted code blocks was failing due to regex issues. \\n\\nHere are the lisp highlighted code blocks. **Edit:** removed link.\\n\\nIf you find any code that is not highlighted, please find the appropiate code block and replace it in the code!"},{"id":"2023-11-26-status","metadata":{"permalink":"/blog/2023-11-26-status","editUrl":"https://github.com/lisp-docs/lisp-docs.github.io/blog/2023-11-26-current-status.md","source":"@site/blog/2023-11-26-current-status.md","title":"Current Status","description":"So finally, first version is out!","date":"2023-11-26T00:00:00.000Z","formattedDate":"November 26, 2023","tags":[{"label":"status","permalink":"/blog/tags/status"},{"label":"todo","permalink":"/blog/tags/todo"},{"label":"beginning","permalink":"/blog/tags/beginning"}],"readingTime":1.83,"hasTruncateMarker":false,"authors":[{"name":"Daniel N","title":"Lisp Docs Contributor","url":"https://github.com/daninus14","key":"daniel"}],"frontMatter":{"slug":"2023-11-26-status","title":"Current Status","authors":"daniel","tags":["status","todo","beginning"]},"unlisted":false,"prevItem":{"title":"Code Blocks and Tables","permalink":"/blog/2023/11/29/"},"nextItem":{"title":"Mission","permalink":"/blog/mission"}},"content":"So finally, first version is out!\\n\\nHowever, this is a work in progress.\\n\\nThese are the changes that are needed for it to reach a version 1.0:\\n\\n1. ~~Fix all the markdown titles for the right side panel navigation bar. For some reason during the parsing, some of the titles got messed up in the navigation bar. The titles in the actual content are repeated. The second one must also be removed.~~ **Edit: 2023/11/26**\\n2. Need to link up all italics to the actual page in the reference.\\n3. Need to fix the tables in all the files. Idea: Could find each table in the original TeX files and parse those and replace the text in the current markdown since the original is hopefully structured.\\n4. Need to parse and find every bit of lisp code and wrap it in the proprer ``` backquote wrappers with the lisp syntax for pretty print. See #3, could try to see if the original TeX marked code blocks and use those with text replace.\\n5. Make sure everything else is good!\\n6. ~~Change the `_category_.json` files to display position of chapters in sidebar correctly...~~ **Edit: 2023/11/27**\\n7. ~~Change the `_category_.json` contents to use double quotes instead of single quotes...~~ **Edit: 2023/11/27**\\n8. Add search plugin\\n9. \\"beach\\": \\"The description of each operator should probably be on a separate page, because for the language reference, more material will be added there\\"\\n\\nHowever, with that said, the project is actually ready for contributions!\\n\\n## How The Project is Structured\\n\\nThe idea here was to separate the files with the original dpANS3r text from the community additions. Therefore, to add examples or any content, just create a new file. Please name it relevant to the section you will add it to, and then just import it in that section like all the other files are imported.\\n\\nSo instead of writing the examples in the dpANS3r itself, it will be a new file `examples.md` where you can write anything you want. Then you would import it wherever relevant.\\n\\nPlease note that because react doesn\'t seem to allow using numbers in component names, I had to convert all numbers to letters. `A=0`, `B=1`, etc..."},{"id":"mission","metadata":{"permalink":"/blog/mission","editUrl":"https://github.com/lisp-docs/lisp-docs.github.io/blog/2023-11-12-mission-again.md","source":"@site/blog/2023-11-12-mission-again.md","title":"Mission","description":"The mission of Common Lisp Docs and this website in particular is to provide Common Lisp Documentation.","date":"2023-11-12T00:00:00.000Z","formattedDate":"November 12, 2023","tags":[{"label":"mission","permalink":"/blog/tags/mission"},{"label":"beginning","permalink":"/blog/tags/beginning"}],"readingTime":0.42,"hasTruncateMarker":false,"authors":[{"name":"Daniel N","title":"Lisp Docs Core Team","url":"https://github.com/lisp-docs"}],"frontMatter":{"slug":"mission","title":"Mission","authors":{"name":"Daniel N","title":"Lisp Docs Core Team","url":"https://github.com/lisp-docs"},"tags":["mission","beginning"]},"unlisted":false,"prevItem":{"title":"Current Status","permalink":"/blog/2023-11-26-status"},"nextItem":{"title":"Mission","permalink":"/blog/2023/11/12/mission"}},"content":"The mission of Common Lisp Docs and this website in particular is to provide Common Lisp Documentation.\\n\\nThe Docs in this Website are aimed to be a User Guide.\\n\\nThis website [Common Lisp Reference](https://lisp-docs.github.io/cl-language-reference/) is meant to be the reference.\\n\\nWe will hopefully soon come up with a tutorials website and more!\\n\\nWe aim to make this website as comprehensive as websites for other languages.\\n\\nAdding some sample code below...\\n\\n```lisp\\n(format T \\"Hello World!\\")\\n```\\n\\n```lisp\\n    (defun my-sqr (x)\\n        (* x x))\\n```"},{"id":"/2023/11/12/mission","metadata":{"permalink":"/blog/2023/11/12/mission","editUrl":"https://github.com/lisp-docs/lisp-docs.github.io/blog/2023-11-12-mission.md","source":"@site/blog/2023-11-12-mission.md","title":"Mission","description":"The mission of Lisp Docs and this website in particular is to provide Common Lisp Documentation.","date":"2023-11-12T00:00:00.000Z","formattedDate":"November 12, 2023","tags":[{"label":"mission","permalink":"/blog/tags/mission"},{"label":"beginning","permalink":"/blog/tags/beginning"}],"readingTime":0.14,"hasTruncateMarker":false,"authors":[{"name":"Admin","title":"Common Lisp Docs Core Team","url":"https://github.com/lisp-docs"}],"frontMatter":{"title":"Mission","authors":{"name":"Admin","title":"Common Lisp Docs Core Team","url":"https://github.com/lisp-docs"},"tags":["mission","beginning"]},"unlisted":false,"prevItem":{"title":"Mission","permalink":"/blog/mission"},"nextItem":{"title":"TODO","permalink":"/blog/TODO"}},"content":"The mission of Lisp Docs and this website in particular is to provide Common Lisp Documentation.\\n\\nThe [Docs in this Website](/docs/tutorial) are aimed to be a User Guide."},{"id":"TODO","metadata":{"permalink":"/blog/TODO","editUrl":"https://github.com/lisp-docs/lisp-docs.github.io/blog/2023-11-12-todo.md","source":"@site/blog/2023-11-12-todo.md","title":"TODO","description":"TODO","date":"2023-11-12T00:00:00.000Z","formattedDate":"November 12, 2023","tags":[{"label":"todo","permalink":"/blog/tags/todo"},{"label":"beginning","permalink":"/blog/tags/beginning"}],"readingTime":0.415,"hasTruncateMarker":false,"authors":[{"name":"Daniel N","title":"Lisp Docs Core Team","url":"https://github.com/lisp-docs"}],"frontMatter":{"slug":"TODO","title":"TODO","authors":{"name":"Daniel N","title":"Lisp Docs Core Team","url":"https://github.com/lisp-docs"},"tags":["todo","beginning"]},"unlisted":false,"prevItem":{"title":"Mission","permalink":"/blog/2023/11/12/mission"}},"content":"## TODO\\n\\n* Find People to Help Us write the content of this website!\\n* Add a Getting Started in Common Lisp Page\\n* Add Links to the cookbook, HyperSpec, and Common Lisp, The Language 2\\n  * However, we eventually hope to completely replace the HyperSpec and CLTL2\\n* Add User Guide section and article stubs\\n* Think about what to add\\n* Add Beginner\'s User Guide\\n* Add Normal User Guide which should be fully comprehensive\\n* add 404\'s etc [GH Pages](https://docs.github.com/en/pages)"}]}')}}]);