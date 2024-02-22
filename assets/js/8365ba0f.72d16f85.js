"use strict";(self.webpackChunklisp_docs_github_io=self.webpackChunklisp_docs_github_io||[]).push([[7852],{7941:(e,n,i)=>{i.r(n),i.d(n,{assets:()=>a,contentTitle:()=>r,default:()=>c,frontMatter:()=>l,metadata:()=>o,toc:()=>h});var t=i(5893),s=i(1151);const l={},r="Todo",o={id:"contribute/todo",title:"Todo",description:"Here we will list all the To Do items we have for this project, and you can help by taking care of any of them :D.",source:"@site/docs/contribute/todo.md",sourceDirName:"contribute",slug:"/contribute/todo",permalink:"/docs/contribute/todo",draft:!1,unlisted:!1,editUrl:"https://github.com/lisp-docs/lisp-docs.github.io/tree/main/docs/contribute/todo.md",tags:[],version:"current",lastUpdatedBy:"Daniel Nussenbaum",frontMatter:{},sidebar:"contributeSidebar",previous:{title:"Technical Reference",permalink:"/docs/contribute/reference-contribute"}},a={},h=[{value:"Technical Reference",id:"technical-reference",level:2},{value:"Formatting To Dos",id:"formatting-to-dos",level:3},{value:"Tables",id:"tables",level:4},{value:"Code Blocks",id:"code-blocks",level:4},{value:"Links",id:"links",level:4},{value:"Extra New Lines",id:"extra-new-lines",level:4},{value:"File Names",id:"file-names",level:4},{value:"Extra Page Headers in Text",id:"extra-page-headers-in-text",level:4},{value:"Content To Dos",id:"content-to-dos",level:3},{value:"New Sections",id:"new-sections",level:3},{value:"Tutorial",id:"tutorial",level:2},{value:"Done",id:"done",level:2}];function d(e){const n={a:"a",code:"code",del:"del",em:"em",h1:"h1",h2:"h2",h3:"h3",h4:"h4",li:"li",p:"p",strong:"strong",ul:"ul",...(0,s.a)(),...e.components};return(0,t.jsxs)(t.Fragment,{children:[(0,t.jsx)(n.h1,{id:"todo",children:"Todo"}),"\n",(0,t.jsxs)(n.p,{children:["Here we will list all the To Do items we have for this project, and you can help by taking care of any of them ",":D","."]}),"\n",(0,t.jsx)(n.h2,{id:"technical-reference",children:"Technical Reference"}),"\n",(0,t.jsx)(n.h3,{id:"formatting-to-dos",children:"Formatting To Dos"}),"\n",(0,t.jsx)(n.h4,{id:"tables",children:"Tables"}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsxs)(n.li,{children:["We need to format al the Tables to markdown.","\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsx)(n.li,{children:"This can probably be done programmatically for the most part"}),"\n",(0,t.jsx)(n.li,{children:"Most tables are preformatted an follow a similar pattern"}),"\n",(0,t.jsxs)(n.li,{children:["Can check if it's a listing of dictionary items with a script similar to ",(0,t.jsx)(n.a,{href:"https://github.com/lisp-docs/process-dpans3r/blob/master/add-cl-links.py",children:"this one"})," and if every item is a dictionary item, add the ",(0,t.jsx)(n.code,{children:"<DictionaryLink>item</DictionaryLink>"})," wrapping to it and the link should just work"]}),"\n",(0,t.jsxs)(n.li,{children:["Need to figure out the styling to keep them to new lines, or maybe remove the ",(0,t.jsx)(n.code,{children:"<p></p>"})," tags all together."]}),"\n",(0,t.jsx)(n.li,{children:"Most tables are either listings of dictionary items, or code blocks. Can then check what the rest of the tables are and how many are left to format to see if it makes sense to do a programmatic or manual approach to fixing them"}),"\n"]}),"\n"]}),"\n"]}),"\n",(0,t.jsx)(n.h4,{id:"code-blocks",children:"Code Blocks"}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsxs)(n.li,{children:["Update lisp code to be wrapped in code blocks to be formated with markdown. Markdown supports math formulas and special text, checkout the ",(0,t.jsx)(n.a,{href:"https://commonmark.org/help/",children:"Markdown Syntax"})," tutorial for how to do it. Note that we use ",(0,t.jsx)(n.a,{href:"https://docusaurus.io/docs/markdown-features",children:"MDX"})," instead of vanilla markdown.","\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsxs)(n.li,{children:[(0,t.jsx)(n.strong,{children:"2023/12/06 Update:"})," All the dictionary pages example sections were surrounded with code blocks for syntax highlighting, so therefore most of what's left are the code blocks in the write up sections of the reference."]}),"\n"]}),"\n"]}),"\n"]}),"\n",(0,t.jsx)(n.h4,{id:"links",children:"Links"}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsxs)(n.li,{children:['Links to Sections like in See Also "Section 6.1.1.7 (Destructuring) "',"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsx)(n.li,{children:"These can be done programmatically as well, just have to map to the right page and to the right id"}),"\n",(0,t.jsxs)(n.li,{children:["there's a script that does the mapping here: ",(0,t.jsx)(n.a,{href:"https://github.com/lisp-docs/process-dpans3r/tree/master",children:"process-dpans3r"})," I just forgot the name, it's the one that calls the python function ",(0,t.jsx)(n.code,{children:"ord"})]}),"\n",(0,t.jsx)(n.li,{children:"See how the id's are mapped by just navigating to any subsection in a reference page..."}),"\n"]}),"\n"]}),"\n"]}),"\n",(0,t.jsx)(n.h4,{id:"extra-new-lines",children:"Extra New Lines"}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsx)(n.li,{children:"Some Pages have new lines in the middle of parragraphs that need to be removed"}),"\n"]}),"\n",(0,t.jsx)(n.h4,{id:"file-names",children:"File Names"}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsx)(n.li,{children:"Rename files to have numbers in them? This can make it easier to find the right file for edditing. Careful not to chage the react component names however, that was the original reason everything was changed..."}),"\n"]}),"\n",(0,t.jsx)(n.h4,{id:"extra-page-headers-in-text",children:"Extra Page Headers in Text"}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsxs)(n.li,{children:["Sometimes page header strings (chapter titles) with new lines were added in some parts of the text, they need to be removed... They can probably be removed programmatically...","\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsxs)(n.li,{children:["Get the name of all the chapters. Then look for the string ",(0,t.jsx)(n.code,{children:"r'\\s\\n\\s{chapter_name}\\s\\n\\s'"})," and delete it (AKA replace it with them empty string)"]}),"\n",(0,t.jsx)(n.li,{children:"Manually check all deletions in your git diff tool to make sure they make sense and something valuable is not being deleted"}),"\n"]}),"\n"]}),"\n"]}),"\n",(0,t.jsx)(n.h3,{id:"content-to-dos",children:"Content To Dos"}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsxs)(n.li,{children:[(0,t.jsx)(n.strong,{children:"Examples"}),": We need expanded examples and ",(0,t.jsx)(n.strong,{children:"Explanations"})," for most of the content in the reference."]}),"\n",(0,t.jsxs)(n.li,{children:["Please take a look at the ",(0,t.jsx)(n.a,{href:"/docs/contribute",children:"Dictionary Items To Do's"})," in particular"]}),"\n",(0,t.jsx)(n.li,{children:(0,t.jsx)(n.a,{href:"https://www.cliki.net/ANSI%20Clarifications%20and%20Errata",children:"https://www.cliki.net/ANSI%20Clarifications%20and%20Errata"})}),"\n",(0,t.jsx)(n.li,{children:(0,t.jsx)(n.a,{href:"https://www.lispworks.com/documentation/HyperSpec/Issues/I_Alpha.htm",children:"https://www.lispworks.com/documentation/HyperSpec/Issues/I_Alpha.htm"})}),"\n"]}),"\n",(0,t.jsx)(n.h3,{id:"new-sections",children:"New Sections"}),"\n",(0,t.jsxs)(n.p,{children:["Can you think of important topics to cover in the technical reference besides those that are in the specification and mop chapters 5 and 6? Or anything we have not yet covered? If so, please ",(0,t.jsx)(n.a,{href:"https://github.com/lisp-docs/cl-language-reference/issues",children:"submit an issue"})," or even better a pull request."]}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsx)(n.li,{children:"Concurrency is probably essential, so the bordeaux and maybe another library related to that"}),"\n",(0,t.jsx)(n.li,{children:"A lot of the trivial-* packages are probably also essential at this stage to programming in CL and they are independent (mostly) of the implementations, so it may make sense to include them"}),"\n",(0,t.jsxs)(n.li,{children:["A table comparisons of the implementations, but then we would have to think what the relevant comparisons are. Any important comparisons between the implementations that are really important to know?","\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsx)(n.li,{children:"A table of trivial-* to implementation support is important"}),"\n"]}),"\n"]}),"\n"]}),"\n",(0,t.jsx)(n.h2,{id:"tutorial",children:"Tutorial"}),"\n",(0,t.jsx)(n.p,{children:"We need help writing any of these sections:"}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsx)(n.li,{children:"Make beginner and experienced programmer tutorials"}),"\n",(0,t.jsxs)(n.li,{children:["Other items in the ",(0,t.jsx)(n.a,{href:"https://github.com/lisp-docs/lisp-docs.github.io/tree/main/docs",children:"Tutorial"})]}),"\n",(0,t.jsxs)(n.li,{children:[(0,t.jsx)(n.a,{href:"/docs/getting-started",children:"Getting Started"})," pages"]}),"\n",(0,t.jsx)(n.li,{children:(0,t.jsx)(n.a,{href:"/docs/faq",children:"FAQs"})}),"\n",(0,t.jsxs)(n.li,{children:[(0,t.jsx)(n.a,{href:"/docs/whylisp",children:"Why Lisp"})," pages for beginners and CTOs."]}),"\n",(0,t.jsx)(n.li,{children:"Finish tutorial on Structuring Large Projects both for package inferred and regular"}),"\n",(0,t.jsxs)(n.li,{children:["Add the Technical Reference and Tutorials to ",(0,t.jsx)(n.a,{href:"https://github.com/CodyReichert/awesome-cl?tab=readme-ov-file#reference",children:"https://github.com/CodyReichert/awesome-cl?tab=readme-ov-file#reference"})," and ",(0,t.jsx)(n.a,{href:"https://github.com/CodyReichert/awesome-cl?tab=readme-ov-file#reference",children:"https://github.com/CodyReichert/awesome-cl?tab=readme-ov-file#reference"})]}),"\n",(0,t.jsx)(n.li,{children:"solve google crawler indexing issue"}),"\n"]}),"\n",(0,t.jsx)(n.h2,{id:"done",children:"Done"}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsxs)(n.li,{children:[(0,t.jsxs)(n.del,{children:["All the ",(0,t.jsx)(n.em,{children:"Dictionary"})," Sections have to be separated in the docs. For example, all of section 9.2 in the original is included in our ",(0,t.jsx)(n.a,{href:"https://lisp-docs.github.io/cl-language-reference/docs/chap-9/j-b-condition-system-concepts",children:"9.1"}),". We need to split that file into two. Similarly for chapters"]}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsx)(n.li,{children:(0,t.jsx)(n.del,{children:"3.8"})}),"\n",(0,t.jsx)(n.li,{children:(0,t.jsx)(n.del,{children:"4.4"})}),"\n",(0,t.jsx)(n.li,{children:(0,t.jsx)(n.del,{children:"5.3"})}),"\n",(0,t.jsx)(n.li,{children:(0,t.jsx)(n.del,{children:"6.2"})}),"\n",(0,t.jsx)(n.li,{children:(0,t.jsx)(n.del,{children:"7.7"})}),"\n",(0,t.jsx)(n.li,{children:(0,t.jsx)(n.del,{children:"8.1?"})}),"\n",(0,t.jsx)(n.li,{children:(0,t.jsx)(n.del,{children:"9.2"})}),"\n",(0,t.jsx)(n.li,{children:(0,t.jsxs)(n.del,{children:["etc. It seems the last section of every chapter which is the ",(0,t.jsx)(n.em,{children:"Dictionary"})," was bundled into the next to last section."]})}),"\n"]}),"\n"]}),"\n",(0,t.jsx)(n.li,{children:(0,t.jsx)(n.del,{children:"Fix the Dictionary Entry pages that were not parsed correctly."})}),"\n",(0,t.jsx)(n.li,{children:(0,t.jsx)(n.del,{children:"Indent Common Lisp code blocks."})}),"\n",(0,t.jsx)(n.li,{children:(0,t.jsx)(n.del,{children:"Break the Glossary into 27 pages, one per letter, plus the introduction and non alphabetical symbols page."})}),"\n",(0,t.jsx)(n.li,{children:(0,t.jsxs)(n.del,{children:["Add ",(0,t.jsx)(n.code,{children:"<ClLinks>"})," wrapping to all italic and bolded text in the specification except for that inside code blocks and in titles, to provide links and tooltips..."]})}),"\n",(0,t.jsxs)(n.li,{children:[(0,t.jsx)(n.del,{children:"Add Tooltips with definitions from the glossary everywhere that is relevant"}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsx)(n.li,{children:"The best way to do it is by creating a hash table of the dictionary, and everywhere an italizaed word is present, suroound it with a React Component which will check if there's a definition for that text in the glossary and it will add a tooltip if it's relevant"}),"\n",(0,t.jsx)(n.li,{children:"This should probably be a react component so that it can also be used in oher Lisp Docs pages and tutorials and shared easily across projects"}),"\n",(0,t.jsx)(n.li,{children:"Update the glossary to be HTML and not markdown"}),"\n",(0,t.jsx)(n.li,{children:"Finish all the dicionary item's parsing, then update the json files for the glossary and the dicionary terms"}),"\n"]}),"\n"]}),"\n",(0,t.jsx)(n.li,{children:(0,t.jsx)(n.del,{children:"We should add links to all the bolded items in the reference to go to the appropiate pages. This should be actually fairly straight forward to do with code. We can just build a hash table of the dictionary pages available, with the name of each page, then simply find all the bolded text in the reference, and if its name matches an entry in the hash table, turn in into a link for that page."})}),"\n",(0,t.jsx)(n.li,{children:(0,t.jsx)(n.del,{children:"Make the Tooltip Definition and links to dicionary pages"})}),"\n",(0,t.jsxs)(n.li,{children:["~~",(0,t.jsx)(n.a,{href:"https://docusaurus.io/docs/sidebar/multiple-sidebars",children:"Multiple Sidebars"})," for: ~~","\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsx)(n.li,{children:(0,t.jsx)(n.del,{children:"Contribute"})}),"\n",(0,t.jsx)(n.li,{children:"Why Lisp Section: For beginners, professionals, CTOs or Project Managers"}),"\n"]}),"\n"]}),"\n",(0,t.jsxs)(n.li,{children:["~~In all code blocks: ~~","\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsx)(n.li,{children:"replace &gt; and &lt; with ><,"}),"\n",(0,t.jsx)(n.li,{children:"remove extra white lines script run..."}),"\n",(0,t.jsx)(n.li,{children:"run indent code blocks again"}),"\n",(0,t.jsx)(n.li,{children:"any place that is not inside a code block, or in a title, which is italics and or bold, add the react lisp-docs util..."}),"\n",(0,t.jsx)(n.li,{children:"get react router, test if it works, check base name, then do local route... Links..."}),"\n",(0,t.jsx)(n.li,{children:"go through all dictionary items, check that they are all there"}),"\n"]}),"\n"]}),"\n",(0,t.jsxs)(n.li,{children:[(0,t.jsx)(n.del,{children:"make code blocks from sections named example which have figures..."}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsxs)(n.li,{children:["title of section is examples","\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsx)(n.li,{children:"has a figure"}),"\n"]}),"\n"]}),"\n",(0,t.jsx)(n.li,{children:"no figure, then in that section, find first line which is either \\n\\s*( or \\n\\s;, and start code block from there till the end of the section"}),"\n"]}),"\n"]}),"\n"]})]})}function c(e={}){const{wrapper:n}={...(0,s.a)(),...e.components};return n?(0,t.jsx)(n,{...e,children:(0,t.jsx)(d,{...e})}):d(e)}},1151:(e,n,i)=>{i.d(n,{Z:()=>o,a:()=>r});var t=i(7294);const s={},l=t.createContext(s);function r(e){const n=t.useContext(l);return t.useMemo((function(){return"function"==typeof e?e(n):{...n,...e}}),[n,e])}function o(e){let n;return n=e.disableParentContext?"function"==typeof e.components?e.components(s):e.components||s:r(e.components),t.createElement(l.Provider,{value:n},e.children)}}}]);