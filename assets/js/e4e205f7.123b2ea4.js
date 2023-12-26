"use strict";(self.webpackChunklisp_docs_github_io=self.webpackChunklisp_docs_github_io||[]).push([[6034],{5175:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>l,contentTitle:()=>o,default:()=>h,frontMatter:()=>r,metadata:()=>a,toc:()=>d});var s=n(5893),i=n(1151);const r={slug:"2023-11-26-status",title:"Current Status",authors:"daniel",tags:["status","todo","beginning"]},o=void 0,a={permalink:"/blog/2023-11-26-status",editUrl:"https://github.com/facebook/docusaurus/tree/main/packages/create-docusaurus/templates/shared/blog/2023-11-26-current-status.md",source:"@site/blog/2023-11-26-current-status.md",title:"Current Status",description:"So finally, first version is out!",date:"2023-11-26T00:00:00.000Z",formattedDate:"November 26, 2023",tags:[{label:"status",permalink:"/blog/tags/status"},{label:"todo",permalink:"/blog/tags/todo"},{label:"beginning",permalink:"/blog/tags/beginning"}],readingTime:1.83,hasTruncateMarker:!1,authors:[{name:"Daniel N",title:"Lisp Docs Contributor",url:"https://github.com/daninus14",key:"daniel"}],frontMatter:{slug:"2023-11-26-status",title:"Current Status",authors:"daniel",tags:["status","todo","beginning"]},unlisted:!1,prevItem:{title:"Code Blocks and Tables",permalink:"/blog/2023/11/29/"},nextItem:{title:"Mission",permalink:"/blog/mission"}},l={authorsImageUrls:[void 0]},d=[{value:"How The Project is Structured",id:"how-the-project-is-structured",level:2}];function c(e){const t={code:"code",del:"del",h2:"h2",li:"li",ol:"ol",p:"p",strong:"strong",...(0,i.a)(),...e.components};return(0,s.jsxs)(s.Fragment,{children:[(0,s.jsx)(t.p,{children:"So finally, first version is out!"}),"\n",(0,s.jsx)(t.p,{children:"However, this is a work in progress."}),"\n",(0,s.jsx)(t.p,{children:"These are the changes that are needed for it to reach a version 1.0:"}),"\n",(0,s.jsxs)(t.ol,{children:["\n",(0,s.jsxs)(t.li,{children:[(0,s.jsx)(t.del,{children:"Fix all the markdown titles for the right side panel navigation bar. For some reason during the parsing, some of the titles got messed up in the navigation bar. The titles in the actual content are repeated. The second one must also be removed."})," ",(0,s.jsx)(t.strong,{children:"Edit: 2023/11/26"})]}),"\n",(0,s.jsx)(t.li,{children:"Need to link up all italics to the actual page in the reference."}),"\n",(0,s.jsx)(t.li,{children:"Need to fix the tables in all the files. Idea: Could find each table in the original TeX files and parse those and replace the text in the current markdown since the original is hopefully structured."}),"\n",(0,s.jsx)(t.li,{children:"Need to parse and find every bit of lisp code and wrap it in the proprer ``` backquote wrappers with the lisp syntax for pretty print. See #3, could try to see if the original TeX marked code blocks and use those with text replace."}),"\n",(0,s.jsx)(t.li,{children:"Make sure everything else is good!"}),"\n",(0,s.jsxs)(t.li,{children:[(0,s.jsxs)(t.del,{children:["Change the ",(0,s.jsx)(t.code,{children:"_category_.json"})," files to display position of chapters in sidebar correctly..."]})," ",(0,s.jsx)(t.strong,{children:"Edit: 2023/11/27"})]}),"\n",(0,s.jsxs)(t.li,{children:[(0,s.jsxs)(t.del,{children:["Change the ",(0,s.jsx)(t.code,{children:"_category_.json"})," contents to use double quotes instead of single quotes..."]})," ",(0,s.jsx)(t.strong,{children:"Edit: 2023/11/27"})]}),"\n",(0,s.jsx)(t.li,{children:"Add search plugin"}),"\n",(0,s.jsx)(t.li,{children:'"beach": "The description of each operator should probably be on a separate page, because for the language reference, more material will be added there"'}),"\n"]}),"\n",(0,s.jsx)(t.p,{children:"However, with that said, the project is actually ready for contributions!"}),"\n",(0,s.jsx)(t.h2,{id:"how-the-project-is-structured",children:"How The Project is Structured"}),"\n",(0,s.jsx)(t.p,{children:"The idea here was to separate the files with the original dpANS3r text from the community additions. Therefore, to add examples or any content, just create a new file. Please name it relevant to the section you will add it to, and then just import it in that section like all the other files are imported."}),"\n",(0,s.jsxs)(t.p,{children:["So instead of writing the examples in the dpANS3r itself, it will be a new file ",(0,s.jsx)(t.code,{children:"examples.md"})," where you can write anything you want. Then you would import it wherever relevant."]}),"\n",(0,s.jsxs)(t.p,{children:["Please note that because react doesn't seem to allow using numbers in component names, I had to convert all numbers to letters. ",(0,s.jsx)(t.code,{children:"A=0"}),", ",(0,s.jsx)(t.code,{children:"B=1"}),", etc..."]})]})}function h(e={}){const{wrapper:t}={...(0,i.a)(),...e.components};return t?(0,s.jsx)(t,{...e,children:(0,s.jsx)(c,{...e})}):c(e)}},1151:(e,t,n)=>{n.d(t,{Z:()=>a,a:()=>o});var s=n(7294);const i={},r=s.createContext(i);function o(e){const t=s.useContext(r);return s.useMemo((function(){return"function"==typeof e?e(t):{...t,...e}}),[t,e])}function a(e){let t;return t=e.disableParentContext?"function"==typeof e.components?e.components(i):e.components||i:o(e.components),s.createElement(r.Provider,{value:t},e.children)}}}]);