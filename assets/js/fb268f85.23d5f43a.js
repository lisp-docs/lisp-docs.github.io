"use strict";(self.webpackChunklisp_docs_github_io=self.webpackChunklisp_docs_github_io||[]).push([[718],{2141:(e,n,i)=>{i.r(n),i.d(n,{assets:()=>r,contentTitle:()=>t,default:()=>h,frontMatter:()=>l,metadata:()=>c,toc:()=>d});var o=i(5893),s=i(1151);const l={sidebar_position:4},t="Control Flow",c={id:"tutorial/control-flows",title:"Control Flow",description:"Introduction to Control Flow in Common Lisp",source:"@site/docs/tutorial/control-flows.md",sourceDirName:"tutorial",slug:"/tutorial/control-flows",permalink:"/docs/tutorial/control-flows",draft:!1,unlisted:!1,editUrl:"https://github.com/lisp-docs/lisp-docs.github.io/tree/main/docs/tutorial/control-flows.md",tags:[],version:"current",lastUpdatedBy:"Daniel Nussenbaum",sidebarPosition:4,frontMatter:{sidebar_position:4},sidebar:"tutorialSidebar",previous:{title:"An Informal Introduction to Common Lisp",permalink:"/docs/tutorial/informal-introduction-to-lisp"},next:{title:"Data Structures",permalink:"/docs/tutorial/data-structures"}},r={},d=[{value:"Introduction to Control Flow in Common Lisp",id:"introduction-to-control-flow-in-common-lisp",level:2},{value:"1. Conditional Forms",id:"1-conditional-forms",level:3},{value:"2. Looping Forms",id:"2-looping-forms",level:3},{value:"3. Non-Local Exits",id:"3-non-local-exits",level:3},{value:"4. Other Control Flow Constructs",id:"4-other-control-flow-constructs",level:3},{value:"1. Conditional Forms in Common Lisp",id:"1-conditional-forms-in-common-lisp",level:2},{value:"1.1 <code>if</code>: Basic Conditional Execution",id:"11-if-basic-conditional-execution",level:3},{value:"1.2 <code>when</code>: Execute a Block if True",id:"12-when-execute-a-block-if-true",level:3},{value:"1.3 <code>unless</code>: Execute a Block if False",id:"13-unless-execute-a-block-if-false",level:3},{value:"1.4 <code>cond</code>: General Conditional Execution",id:"14-cond-general-conditional-execution",level:3},{value:"1.5 <code>case</code>: Conditional Execution Based on a Key",id:"15-case-conditional-execution-based-on-a-key",level:3},{value:"1.6 <code>typecase</code>: Conditional Execution Based on Type",id:"16-typecase-conditional-execution-based-on-type",level:3},{value:"1.7 <code>ecase</code> and <code>etypecase</code>: Error-Signaling Variants",id:"17-ecase-and-etypecase-error-signaling-variants",level:3},{value:"2. Looping Forms in Common Lisp",id:"2-looping-forms-in-common-lisp",level:2},{value:"2.1 <code>loop</code>: The Versatile Looping Construct",id:"21-loop-the-versatile-looping-construct",level:3},{value:"2.1.1 Simple Iteration",id:"211-simple-iteration",level:4},{value:"2.1.2 <code>for</code> Clauses: Iteration with Variables",id:"212-for-clauses-iteration-with-variables",level:4},{value:"2.1.3 <code>while</code> and <code>until</code> Clauses: Conditional Termination",id:"213-while-and-until-clauses-conditional-termination",level:4},{value:"2.1.4 Accumulation Clauses: <code>collect</code>, <code>sum</code>, <code>count</code>, <code>minimize</code>, <code>maximize</code>",id:"214-accumulation-clauses-collect-sum-count-minimize-maximize",level:4},{value:"2.2 <code>do</code>: Parameterized Iteration",id:"22-do-parameterized-iteration",level:3},{value:"2.3 <code>dotimes</code>: Iterating a Fixed Number of Times",id:"23-dotimes-iterating-a-fixed-number-of-times",level:3},{value:"2.4 <code>dolist</code>: Iterating over a List",id:"24-dolist-iterating-over-a-list",level:3},{value:"3. Non-Local Exits in Common Lisp",id:"3-non-local-exits-in-common-lisp",level:2},{value:"3.1 <code>block</code> and <code>return-from</code>: Lexical Exits",id:"31-block-and-return-from-lexical-exits",level:3},{value:"3.2 <code>catch</code> and <code>throw</code>: Dynamic Exits",id:"32-catch-and-throw-dynamic-exits",level:3},{value:"4. Other Control Flow Constructs",id:"4-other-control-flow-constructs-1",level:2},{value:"4.1 <code>go</code> and <code>tagbody</code>: Low-Level Control Flow",id:"41-go-and-tagbody-low-level-control-flow",level:3}];function a(e){const n={code:"code",em:"em",h1:"h1",h2:"h2",h3:"h3",h4:"h4",li:"li",p:"p",pre:"pre",strong:"strong",ul:"ul",...(0,s.a)(),...e.components};return(0,o.jsxs)(o.Fragment,{children:[(0,o.jsx)(n.h1,{id:"control-flow",children:"Control Flow"}),"\n",(0,o.jsx)(n.h2,{id:"introduction-to-control-flow-in-common-lisp",children:"Introduction to Control Flow in Common Lisp"}),"\n",(0,o.jsxs)(n.p,{children:["Control flow refers to the order in which statements in a program are executed. It determines the path the program takes based on conditions and other factors. Common Lisp provides a rich set of control flow forms, offering flexibility and expressiveness in structuring your code. Unlike some languages that rely heavily on structural indentation, Common Lisp uses parentheses ",(0,o.jsx)(n.code,{children:"()"})," to define code blocks, making the logical structure explicit."]}),"\n",(0,o.jsx)(n.p,{children:"This tutorial will introduce you to the fundamental control flow concepts in Common Lisp and guide you through the most important control flow forms. We will cover conditional execution, looping, and non-local exits, providing examples and explanations to help you understand how to effectively use these tools."}),"\n",(0,o.jsx)(n.p,{children:(0,o.jsx)(n.strong,{children:"Key Control Flow Concepts:"})}),"\n",(0,o.jsxs)(n.ul,{children:["\n",(0,o.jsxs)(n.li,{children:[(0,o.jsx)(n.strong,{children:"Conditional Execution:"})," Executing different code blocks based on whether a condition is true or false."]}),"\n",(0,o.jsxs)(n.li,{children:[(0,o.jsx)(n.strong,{children:"Iteration (Looping):"})," Repeating a block of code multiple times, either a fixed number of times or until a certain condition is met."]}),"\n",(0,o.jsxs)(n.li,{children:[(0,o.jsx)(n.strong,{children:"Non-Local Exits:"})," Mechanisms for transferring control to a different part of the program, bypassing the normal sequential execution."]}),"\n"]}),"\n",(0,o.jsx)(n.p,{children:(0,o.jsx)(n.strong,{children:"Table of Contents:"})}),"\n",(0,o.jsx)(n.p,{children:"Here's a breakdown of the control flow forms we will explore:"}),"\n",(0,o.jsx)(n.h3,{id:"1-conditional-forms",children:"1. Conditional Forms"}),"\n",(0,o.jsxs)(n.ul,{children:["\n",(0,o.jsxs)(n.li,{children:[(0,o.jsx)(n.strong,{children:(0,o.jsx)(n.code,{children:"if"})}),": Basic conditional execution."]}),"\n",(0,o.jsxs)(n.li,{children:[(0,o.jsx)(n.strong,{children:(0,o.jsx)(n.code,{children:"when"})}),": Execute a block only if a condition is true."]}),"\n",(0,o.jsxs)(n.li,{children:[(0,o.jsx)(n.strong,{children:(0,o.jsx)(n.code,{children:"unless"})}),": Execute a block only if a condition is false."]}),"\n",(0,o.jsxs)(n.li,{children:[(0,o.jsx)(n.strong,{children:(0,o.jsx)(n.code,{children:"cond"})}),": General conditional execution with multiple clauses."]}),"\n",(0,o.jsxs)(n.li,{children:[(0,o.jsx)(n.strong,{children:(0,o.jsx)(n.code,{children:"case"})}),": Conditional execution based on comparing a key to specific values."]}),"\n",(0,o.jsxs)(n.li,{children:[(0,o.jsx)(n.strong,{children:(0,o.jsx)(n.code,{children:"typecase"})}),": Conditional execution based on the type of a value."]}),"\n",(0,o.jsxs)(n.li,{children:[(0,o.jsx)(n.strong,{children:(0,o.jsx)(n.code,{children:"ecase"})}),": Error-signaling variant of ",(0,o.jsx)(n.code,{children:"case"}),"."]}),"\n",(0,o.jsxs)(n.li,{children:[(0,o.jsx)(n.strong,{children:(0,o.jsx)(n.code,{children:"etypecase"})}),": Error-signaling variant of ",(0,o.jsx)(n.code,{children:"typecase"}),"."]}),"\n"]}),"\n",(0,o.jsx)(n.h3,{id:"2-looping-forms",children:"2. Looping Forms"}),"\n",(0,o.jsxs)(n.ul,{children:["\n",(0,o.jsxs)(n.li,{children:[(0,o.jsx)(n.strong,{children:(0,o.jsx)(n.code,{children:"loop"})}),": The most versatile looping construct, offering various clauses for control.","\n",(0,o.jsxs)(n.ul,{children:["\n",(0,o.jsx)(n.li,{children:"Simple iteration"}),"\n",(0,o.jsxs)(n.li,{children:[(0,o.jsx)(n.code,{children:"for"})," clauses (iterating over sequences, numbers, etc.)"]}),"\n",(0,o.jsxs)(n.li,{children:[(0,o.jsx)(n.code,{children:"while"})," and ",(0,o.jsx)(n.code,{children:"until"})," clauses (conditional termination)"]}),"\n",(0,o.jsxs)(n.li,{children:[(0,o.jsx)(n.code,{children:"collect"}),", ",(0,o.jsx)(n.code,{children:"sum"}),", ",(0,o.jsx)(n.code,{children:"count"}),", ",(0,o.jsx)(n.code,{children:"minimize"}),", ",(0,o.jsx)(n.code,{children:"maximize"})," (accumulation)"]}),"\n"]}),"\n"]}),"\n",(0,o.jsxs)(n.li,{children:[(0,o.jsx)(n.strong,{children:(0,o.jsx)(n.code,{children:"do"})}),": Parameterized iteration with variable updates."]}),"\n",(0,o.jsxs)(n.li,{children:[(0,o.jsx)(n.strong,{children:(0,o.jsx)(n.code,{children:"dotimes"})}),": Iterating a fixed number of times."]}),"\n",(0,o.jsxs)(n.li,{children:[(0,o.jsx)(n.strong,{children:(0,o.jsx)(n.code,{children:"dolist"})}),": Iterating over the elements of a list."]}),"\n"]}),"\n",(0,o.jsx)(n.h3,{id:"3-non-local-exits",children:"3. Non-Local Exits"}),"\n",(0,o.jsxs)(n.ul,{children:["\n",(0,o.jsxs)(n.li,{children:[(0,o.jsxs)(n.strong,{children:[(0,o.jsx)(n.code,{children:"block"})," and ",(0,o.jsx)(n.code,{children:"return-from"})]}),": Named blocks for exiting from nested expressions."]}),"\n",(0,o.jsxs)(n.li,{children:[(0,o.jsxs)(n.strong,{children:[(0,o.jsx)(n.code,{children:"catch"})," and ",(0,o.jsx)(n.code,{children:"throw"})]}),": Dynamic non-local exits, transferring control to a ",(0,o.jsx)(n.code,{children:"catch"})," block."]}),"\n"]}),"\n",(0,o.jsx)(n.h3,{id:"4-other-control-flow-constructs",children:"4. Other Control Flow Constructs"}),"\n",(0,o.jsxs)(n.ul,{children:["\n",(0,o.jsxs)(n.li,{children:[(0,o.jsxs)(n.strong,{children:[(0,o.jsx)(n.code,{children:"go"})," and ",(0,o.jsx)(n.code,{children:"tagbody"})]}),": Low-level control flow (generally discouraged in favor of higher-level forms)."]}),"\n"]}),"\n",(0,o.jsx)(n.p,{children:"By the end of this tutorial, you will be able to write more complex and structured Common Lisp programs using the appropriate control flow mechanisms. We will provide clear examples and explanations for each form, allowing you to quickly grasp these essential concepts."}),"\n",(0,o.jsx)(n.h2,{id:"1-conditional-forms-in-common-lisp",children:"1. Conditional Forms in Common Lisp"}),"\n",(0,o.jsx)(n.p,{children:"Conditional forms allow you to execute different parts of your code based on whether certain conditions are true or false. Common Lisp offers several conditional forms, each suited for different situations."}),"\n",(0,o.jsxs)(n.h3,{id:"11-if-basic-conditional-execution",children:["1.1 ",(0,o.jsx)(n.code,{children:"if"}),": Basic Conditional Execution"]}),"\n",(0,o.jsxs)(n.p,{children:["The ",(0,o.jsx)(n.code,{children:"if"})," form is the most fundamental conditional. It takes three arguments:"]}),"\n",(0,o.jsxs)(n.ul,{children:["\n",(0,o.jsxs)(n.li,{children:[(0,o.jsx)(n.strong,{children:"Test:"})," An expression that is evaluated to determine truth."]}),"\n",(0,o.jsxs)(n.li,{children:[(0,o.jsx)(n.strong,{children:"Then:"})," An expression to be evaluated if the test is true."]}),"\n",(0,o.jsxs)(n.li,{children:[(0,o.jsx)(n.strong,{children:"Else:"})," An optional expression to be evaluated if the test is false."]}),"\n"]}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-lisp",children:'(if (< 5 10) ; Test: Is 5 less than 10?\n    (print "5 is less than 10") ; Then: Execute this if true\n    (print "5 is not less than 10")) ; Else: Execute this if false\n'})}),"\n",(0,o.jsx)(n.p,{children:"Output:"}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-lisp",children:'"5 is less than 10"\n'})}),"\n",(0,o.jsxs)(n.p,{children:["If the ",(0,o.jsx)(n.code,{children:"else"})," clause is omitted and the test is false, ",(0,o.jsx)(n.code,{children:"if"})," returns ",(0,o.jsx)(n.code,{children:"nil"}),"."]}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-lisp",children:'(if (> 5 10)\n    (print "5 is greater than 10")) ; No else clause\n'})}),"\n",(0,o.jsx)(n.p,{children:"Evaluation result:"}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-lisp",children:"NIL\n"})}),"\n",(0,o.jsxs)(n.h3,{id:"12-when-execute-a-block-if-true",children:["1.2 ",(0,o.jsx)(n.code,{children:"when"}),": Execute a Block if True"]}),"\n",(0,o.jsxs)(n.p,{children:["The ",(0,o.jsx)(n.code,{children:"when"})," form is a simplified ",(0,o.jsx)(n.code,{children:"if"})," that executes a block of code only if the test is true. It takes a test and any number of expressions to be executed if the test is true. If the test is false, ",(0,o.jsx)(n.code,{children:"when"})," returns ",(0,o.jsx)(n.code,{children:"nil"}),"."]}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-lisp",children:"(let ((x 15))\n  (when (> x 10)\n    (print \"x is greater than 10\")\n    (print (* x 2)))) ; Multiple expressions in the 'then' block\n"})}),"\n",(0,o.jsx)(n.p,{children:"Output:"}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-lisp",children:'"x is greater than 10"\n30\n'})}),"\n",(0,o.jsxs)(n.h3,{id:"13-unless-execute-a-block-if-false",children:["1.3 ",(0,o.jsx)(n.code,{children:"unless"}),": Execute a Block if False"]}),"\n",(0,o.jsxs)(n.p,{children:[(0,o.jsx)(n.code,{children:"unless"})," is the opposite of ",(0,o.jsx)(n.code,{children:"when"}),". It executes a block of code only if the test is ",(0,o.jsx)(n.em,{children:"false"}),"."]}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-lisp",children:'(let ((x 5))\n  (unless (> x 10)\n    (print "x is not greater than 10")\n    (print (* x 2))))\n'})}),"\n",(0,o.jsx)(n.p,{children:"Output:"}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-lisp",children:'"x is not greater than 10"\n10\n'})}),"\n",(0,o.jsxs)(n.h3,{id:"14-cond-general-conditional-execution",children:["1.4 ",(0,o.jsx)(n.code,{children:"cond"}),": General Conditional Execution"]}),"\n",(0,o.jsxs)(n.p,{children:["The ",(0,o.jsx)(n.code,{children:"cond"})," form provides a more general way to handle multiple conditions. It takes a list of ",(0,o.jsx)(n.em,{children:"clauses"}),", where each clause has a test and a sequence of expressions to be executed if the test is true. The first clause whose test evaluates to true is executed, and the rest are skipped."]}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-lisp",children:'(let ((grade 85))\n  (cond ((>= grade 90) (print "A"))\n        ((>= grade 80) (print "B"))\n        ((>= grade 70) (print "C"))\n        ((>= grade 60) (print "D"))\n        (t (print "F")))) ; The \'t\' clause acts as a default (else)\n'})}),"\n",(0,o.jsx)(n.p,{children:"Output:"}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-lisp",children:'"B"\n'})}),"\n",(0,o.jsxs)(n.p,{children:["The ",(0,o.jsx)(n.code,{children:"t"})," clause is often used as the last clause to provide a default action if none of the previous tests are true."]}),"\n",(0,o.jsxs)(n.h3,{id:"15-case-conditional-execution-based-on-a-key",children:["1.5 ",(0,o.jsx)(n.code,{children:"case"}),": Conditional Execution Based on a Key"]}),"\n",(0,o.jsxs)(n.p,{children:["The ",(0,o.jsx)(n.code,{children:"case"})," form compares a ",(0,o.jsx)(n.em,{children:"key"})," to a set of ",(0,o.jsx)(n.em,{children:"keylists"}),". It's useful when you want to check for equality against specific values."]}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-lisp",children:'(let ((fruit \'apple))\n  (case fruit\n    ((apple) (print "It\'s an apple!"))\n    ((banana orange) (print "It\'s a banana or an orange!"))\n    (otherwise (print "It\'s some other fruit."))))\n'})}),"\n",(0,o.jsx)(n.p,{children:"Output:"}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-lisp",children:'"It\'s an apple!"\n'})}),"\n",(0,o.jsxs)(n.p,{children:["If the key matches a key in a keylist, the corresponding expressions are executed. The ",(0,o.jsx)(n.code,{children:"otherwise"})," keyword acts as a default case."]}),"\n",(0,o.jsxs)(n.h3,{id:"16-typecase-conditional-execution-based-on-type",children:["1.6 ",(0,o.jsx)(n.code,{children:"typecase"}),": Conditional Execution Based on Type"]}),"\n",(0,o.jsxs)(n.p,{children:["The ",(0,o.jsx)(n.code,{children:"typecase"})," form checks the ",(0,o.jsx)(n.em,{children:"type"})," of a value and executes the corresponding code."]}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-lisp",children:'(let ((value 10))\n  (typecase value\n    (integer (print "It\'s an integer!"))\n    (string (print "It\'s a string!"))\n    (t (print "It\'s something else."))))\n'})}),"\n",(0,o.jsx)(n.p,{children:"Output:"}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-lisp",children:'"It\'s an integer!"\n'})}),"\n",(0,o.jsxs)(n.h3,{id:"17-ecase-and-etypecase-error-signaling-variants",children:["1.7 ",(0,o.jsx)(n.code,{children:"ecase"})," and ",(0,o.jsx)(n.code,{children:"etypecase"}),": Error-Signaling Variants"]}),"\n",(0,o.jsxs)(n.p,{children:[(0,o.jsx)(n.code,{children:"ecase"})," and ",(0,o.jsx)(n.code,{children:"etypecase"})," are similar to ",(0,o.jsx)(n.code,{children:"case"})," and ",(0,o.jsx)(n.code,{children:"typecase"}),", respectively, but they signal an error if the key or type doesn't match any of the provided clauses. This is useful for ensuring that all possible cases are handled."]}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-lisp",children:'(let ((fruit \'grape))\n  (ecase fruit\n    ((apple) (print "It\'s an apple!"))\n    ((banana) (print "It\'s a banana!"))))\n'})}),"\n",(0,o.jsxs)(n.p,{children:["This will signal an error because ",(0,o.jsx)(n.code,{children:"grape"})," is not in any of the keylists. Similarly, ",(0,o.jsx)(n.code,{children:"etypecase"})," will signal an error if the value's type does not match any of the specified types."]}),"\n",(0,o.jsxs)(n.p,{children:["These conditional forms provide a comprehensive set of tools for controlling the flow of execution in your Common Lisp programs. Choosing the right form depends on the specific logic you need to implement. ",(0,o.jsx)(n.code,{children:"if"}),", ",(0,o.jsx)(n.code,{children:"when"}),", and ",(0,o.jsx)(n.code,{children:"unless"})," are suitable for simple conditions. ",(0,o.jsx)(n.code,{children:"cond"})," is more general for multiple conditions. ",(0,o.jsx)(n.code,{children:"case"})," and ",(0,o.jsx)(n.code,{children:"typecase"})," provide efficient ways to check against specific values or types, with ",(0,o.jsx)(n.code,{children:"ecase"})," and ",(0,o.jsx)(n.code,{children:"etypecase"})," adding error checking for robustness."]}),"\n",(0,o.jsx)(n.h2,{id:"2-looping-forms-in-common-lisp",children:"2. Looping Forms in Common Lisp"}),"\n",(0,o.jsxs)(n.p,{children:["Looping constructs allow you to repeat a block of code multiple times. Common Lisp provides several powerful looping forms, with ",(0,o.jsx)(n.code,{children:"loop"})," being the most versatile."]}),"\n",(0,o.jsxs)(n.h3,{id:"21-loop-the-versatile-looping-construct",children:["2.1 ",(0,o.jsx)(n.code,{children:"loop"}),": The Versatile Looping Construct"]}),"\n",(0,o.jsxs)(n.p,{children:["The ",(0,o.jsx)(n.code,{children:"loop"})," macro is extremely flexible and can handle a wide variety of looping needs. It uses various ",(0,o.jsx)(n.em,{children:"clauses"})," to control the loop's behavior."]}),"\n",(0,o.jsx)(n.h4,{id:"211-simple-iteration",children:"2.1.1 Simple Iteration"}),"\n",(0,o.jsxs)(n.p,{children:["The simplest form of ",(0,o.jsx)(n.code,{children:"loop"})," creates an infinite loop. You must provide a way to exit the loop using ",(0,o.jsx)(n.code,{children:"return"})," or a similar construct."]}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-lisp",children:'(loop\n  (print "This will print forever unless we stop it!")\n  (return)) ; Exit the loop immediately\n'})}),"\n",(0,o.jsxs)(n.p,{children:["This will print the message only once because of the ",(0,o.jsx)(n.code,{children:"return"}),"."]}),"\n",(0,o.jsxs)(n.h4,{id:"212-for-clauses-iteration-with-variables",children:["2.1.2 ",(0,o.jsx)(n.code,{children:"for"})," Clauses: Iteration with Variables"]}),"\n",(0,o.jsxs)(n.p,{children:["The ",(0,o.jsx)(n.code,{children:"for"})," clause introduces loop variables and specifies how they are updated in each iteration."]}),"\n",(0,o.jsxs)(n.ul,{children:["\n",(0,o.jsx)(n.li,{children:(0,o.jsx)(n.strong,{children:"Iterating over a range of numbers:"})}),"\n"]}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-lisp",children:"(loop for i from 0 to 4 ; Iterate from 0 to 4 (inclusive)\n      do (print i))\n"})}),"\n",(0,o.jsx)(n.p,{children:"Output:"}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-lisp",children:"0\n1\n2\n3\n4\n"})}),"\n",(0,o.jsxs)(n.p,{children:["You can use ",(0,o.jsx)(n.code,{children:"from"}),", ",(0,o.jsx)(n.code,{children:"to"}),", ",(0,o.jsx)(n.code,{children:"below"})," (exclusive upper bound), ",(0,o.jsx)(n.code,{children:"by"})," (step), ",(0,o.jsx)(n.code,{children:"downfrom"}),", ",(0,o.jsx)(n.code,{children:"downto"})," for different iteration patterns."]}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-lisp",children:"(loop for i from 10 downto 1 by 2\n      do (print i))\n"})}),"\n",(0,o.jsx)(n.p,{children:"Output:"}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-lisp",children:"10\n8\n6\n4\n2\n"})}),"\n",(0,o.jsxs)(n.ul,{children:["\n",(0,o.jsx)(n.li,{children:(0,o.jsx)(n.strong,{children:"Iterating over a list:"})}),"\n"]}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-lisp",children:"(loop for item in '(a b c d) ; Iterate over the elements of the list\n      do (print item))\n"})}),"\n",(0,o.jsx)(n.p,{children:"Output:"}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-lisp",children:"A\nB\nC\nD\n"})}),"\n",(0,o.jsxs)(n.h4,{id:"213-while-and-until-clauses-conditional-termination",children:["2.1.3 ",(0,o.jsx)(n.code,{children:"while"})," and ",(0,o.jsx)(n.code,{children:"until"})," Clauses: Conditional Termination"]}),"\n",(0,o.jsxs)(n.p,{children:[(0,o.jsx)(n.code,{children:"while"})," continues the loop as long as a condition is true. ",(0,o.jsx)(n.code,{children:"until"})," continues the loop until a condition becomes true."]}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-lisp",children:"(let ((x 0))\n  (loop while (< x 5) ; Loop while x is less than 5\n        do (print x)\n           (incf x))) ; Increment x\n"})}),"\n",(0,o.jsx)(n.p,{children:"Output:"}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-lisp",children:"0\n1\n2\n3\n4\n"})}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-lisp",children:"(let ((x 0))\n  (loop until (> x 5) ; Loop until x is greater than 5\n        do (print x)\n           (incf x)))\n"})}),"\n",(0,o.jsx)(n.p,{children:"Output:"}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-lisp",children:"0\n1\n2\n3\n4\n5\n"})}),"\n",(0,o.jsxs)(n.h4,{id:"214-accumulation-clauses-collect-sum-count-minimize-maximize",children:["2.1.4 Accumulation Clauses: ",(0,o.jsx)(n.code,{children:"collect"}),", ",(0,o.jsx)(n.code,{children:"sum"}),", ",(0,o.jsx)(n.code,{children:"count"}),", ",(0,o.jsx)(n.code,{children:"minimize"}),", ",(0,o.jsx)(n.code,{children:"maximize"})]}),"\n",(0,o.jsx)(n.p,{children:"These clauses allow you to accumulate results during the loop."}),"\n",(0,o.jsxs)(n.ul,{children:["\n",(0,o.jsxs)(n.li,{children:[(0,o.jsx)(n.strong,{children:(0,o.jsx)(n.code,{children:"collect"})}),": Collects the results into a list."]}),"\n"]}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-lisp",children:"(loop for i from 1 to 5\n      collect (* i i)) ; Collect the squares of numbers from 1 to 5\n"})}),"\n",(0,o.jsx)(n.p,{children:"Output:"}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-lisp",children:"(1 4 9 16 25)\n"})}),"\n",(0,o.jsxs)(n.ul,{children:["\n",(0,o.jsxs)(n.li,{children:[(0,o.jsx)(n.strong,{children:(0,o.jsx)(n.code,{children:"sum"})}),": Sums the results."]}),"\n"]}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-lisp",children:"(loop for i from 1 to 5\n      sum i) ; Sum the numbers from 1 to 5\n"})}),"\n",(0,o.jsx)(n.p,{children:"Output:"}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-lisp",children:"15\n"})}),"\n",(0,o.jsxs)(n.ul,{children:["\n",(0,o.jsxs)(n.li,{children:[(0,o.jsx)(n.strong,{children:(0,o.jsx)(n.code,{children:"count"})}),": Counts how many times a condition is true."]}),"\n"]}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-lisp",children:"(loop for i from 1 to 10\n      count (evenp i)) ; Count the even numbers from 1 to 10\n"})}),"\n",(0,o.jsx)(n.p,{children:"Output:"}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-lisp",children:"5\n"})}),"\n",(0,o.jsxs)(n.ul,{children:["\n",(0,o.jsxs)(n.li,{children:[(0,o.jsxs)(n.strong,{children:[(0,o.jsx)(n.code,{children:"minimize"})," and ",(0,o.jsx)(n.code,{children:"maximize"})]}),": Find the minimum or maximum value."]}),"\n"]}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-lisp",children:"(loop for i in '(3 1 4 1 5 9 2 6)\n      minimize i)\n"})}),"\n",(0,o.jsx)(n.p,{children:"Output:"}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-lisp",children:"1\n"})}),"\n",(0,o.jsxs)(n.h3,{id:"22-do-parameterized-iteration",children:["2.2 ",(0,o.jsx)(n.code,{children:"do"}),": Parameterized Iteration"]}),"\n",(0,o.jsxs)(n.p,{children:["The ",(0,o.jsx)(n.code,{children:"do"})," form provides more explicit control over loop variables and their updates. It takes a list of variable specifications, a termination test, and a body of code."]}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-lisp",children:'(do ((i 0 (1+ i)) ; Initialize i to 0, update it by adding 1 in each iteration\n     (j 10 (- j 2))) ; Initialize j to 10, update it by subtracting 2\n    ((> i 5) (print "Loop finished!")) ; Termination test: exit when i > 5\n  (format t "i: ~d, j: ~d~%" i j))\n'})}),"\n",(0,o.jsx)(n.p,{children:"Output:"}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-lisp",children:'i: 0, j: 10\ni: 1, j: 8\ni: 2, j: 6\ni: 3, j: 4\ni: 4, j: 2\ni: 5, j: 0\n"Loop finished!"\n'})}),"\n",(0,o.jsxs)(n.h3,{id:"23-dotimes-iterating-a-fixed-number-of-times",children:["2.3 ",(0,o.jsx)(n.code,{children:"dotimes"}),": Iterating a Fixed Number of Times"]}),"\n",(0,o.jsxs)(n.p,{children:[(0,o.jsx)(n.code,{children:"dotimes"})," is a convenient form for iterating a specific number of times."]}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-lisp",children:'(dotimes (i 5) ; Iterate 5 times, i will take values from 0 to 4\n  (format t "Iteration ~d~%" i))\n'})}),"\n",(0,o.jsx)(n.p,{children:"Output:"}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-lisp",children:"Iteration 0\nIteration 1\nIteration 2\nIteration 3\nIteration 4\n"})}),"\n",(0,o.jsxs)(n.h3,{id:"24-dolist-iterating-over-a-list",children:["2.4 ",(0,o.jsx)(n.code,{children:"dolist"}),": Iterating over a List"]}),"\n",(0,o.jsxs)(n.p,{children:[(0,o.jsx)(n.code,{children:"dolist"})," provides a simple way to iterate over the elements of a list."]}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-lisp",children:'(dolist (item \'(apple banana cherry)) ; Iterate over the list\n  (format t "Fruit: ~a~%" item))\n'})}),"\n",(0,o.jsx)(n.p,{children:"Output:"}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-lisp",children:"Fruit: APPLE\nFruit: BANANA\nFruit: CHERRY\n"})}),"\n",(0,o.jsxs)(n.p,{children:["These looping forms offer different levels of control and are suited for various situations. ",(0,o.jsx)(n.code,{children:"loop"})," is the most powerful and flexible, while ",(0,o.jsx)(n.code,{children:"do"}),", ",(0,o.jsx)(n.code,{children:"dotimes"}),", and ",(0,o.jsx)(n.code,{children:"dolist"})," provide more specialized and often more concise ways to express common looping patterns."]}),"\n",(0,o.jsx)(n.h2,{id:"3-non-local-exits-in-common-lisp",children:"3. Non-Local Exits in Common Lisp"}),"\n",(0,o.jsxs)(n.p,{children:["Non-local exits provide a way to transfer control from one part of a program to another, bypassing the usual sequential flow. Common Lisp offers two primary mechanisms for this: ",(0,o.jsx)(n.code,{children:"block"}),"/",(0,o.jsx)(n.code,{children:"return-from"})," for lexical exits and ",(0,o.jsx)(n.code,{children:"catch"}),"/",(0,o.jsx)(n.code,{children:"throw"})," for dynamic exits."]}),"\n",(0,o.jsxs)(n.h3,{id:"31-block-and-return-from-lexical-exits",children:["3.1 ",(0,o.jsx)(n.code,{children:"block"})," and ",(0,o.jsx)(n.code,{children:"return-from"}),": Lexical Exits"]}),"\n",(0,o.jsxs)(n.p,{children:[(0,o.jsx)(n.code,{children:"block"})," creates a named block of code. ",(0,o.jsx)(n.code,{children:"return-from"})," exits that block, returning a specified value. These are ",(0,o.jsx)(n.em,{children:"lexically scoped"}),", meaning ",(0,o.jsx)(n.code,{children:"return-from"})," can only exit a ",(0,o.jsx)(n.code,{children:"block"})," that is lexically visible (within the same function or a containing function)."]}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-lisp",children:'(defun my-function (x)\n  (block my-block ; Define a block named my-block\n    (loop for i from 1 to 10\n          do (if (> (* x i) 50)\n                 (return-from my-block (* x i))) ; Exit the block if the condition is met\n          (print (* x i)))\n    "Loop completed without exceeding 50")) ; This is returned if the loop finishes normally\n\n(print (my-function 5)) ; Output: 50\n(print (my-function 2)) ; Output: "Loop completed without exceeding 50"\n'})}),"\n",(0,o.jsxs)(n.p,{children:["In the first call to ",(0,o.jsx)(n.code,{children:"my-function"})," (with ",(0,o.jsx)(n.code,{children:"x = 5"}),"), the loop reaches ",(0,o.jsx)(n.code,{children:"i = 10"}),", where ",(0,o.jsx)(n.code,{children:"(* x i)"})," becomes 50. The ",(0,o.jsx)(n.code,{children:"return-from"})," exits the ",(0,o.jsx)(n.code,{children:"my-block"})," and the function immediately returns 50."]}),"\n",(0,o.jsxs)(n.p,{children:["In the second call (with ",(0,o.jsx)(n.code,{children:"x = 2"}),"), the loop completes without the condition in the ",(0,o.jsx)(n.code,{children:"if"})," being met. The function then proceeds to the last expression in the ",(0,o.jsx)(n.code,{children:"block"}),' and returns the string "Loop completed without exceeding 50".']}),"\n",(0,o.jsxs)(n.p,{children:["You can also use ",(0,o.jsx)(n.code,{children:"return"})," as a shorthand for ",(0,o.jsx)(n.code,{children:"(return-from nil ...)"})," which will return from the innermost enclosing ",(0,o.jsx)(n.code,{children:"block"})," with the name ",(0,o.jsx)(n.code,{children:"nil"})," (which is implicitly created by many forms like ",(0,o.jsx)(n.code,{children:"loop"}),", ",(0,o.jsx)(n.code,{children:"progn"}),", ",(0,o.jsx)(n.code,{children:"let"}),", etc.):"]}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-lisp",children:'(loop\n  (print "Inside the loop")\n  (return "Exiting the loop")) ; Equivalent to (return-from nil "Exiting the loop")\n\n'})}),"\n",(0,o.jsxs)(n.h3,{id:"32-catch-and-throw-dynamic-exits",children:["3.2 ",(0,o.jsx)(n.code,{children:"catch"})," and ",(0,o.jsx)(n.code,{children:"throw"}),": Dynamic Exits"]}),"\n",(0,o.jsxs)(n.p,{children:[(0,o.jsx)(n.code,{children:"catch"})," establishes a ",(0,o.jsx)(n.em,{children:"catch point"})," with a specific ",(0,o.jsx)(n.em,{children:"tag"}),". ",(0,o.jsx)(n.code,{children:"throw"})," transfers control to the nearest dynamically enclosing ",(0,o.jsx)(n.code,{children:"catch"})," with a matching tag, unwinding the call stack as needed. These are ",(0,o.jsx)(n.em,{children:"dynamically scoped"}),", meaning ",(0,o.jsx)(n.code,{children:"throw"})," can exit any ",(0,o.jsx)(n.code,{children:"catch"})," that is currently active on the call stack, regardless of the lexical structure."]}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-lisp",children:'(defun nested-calculations (x y)\n  (catch \'my-tag ; Establish a catch point with the tag \'my-tag\n    (print "Starting calculations...")\n    (let ((result (* x (nested-helper y))))\n      (print "Calculations completed.") ; This might not be reached\n      result)))\n\n(defun nested-helper (z)\n  (if (< z 0)\n      (throw \'my-tag "Negative input not allowed!") ; Throw to the \'my-tag catch\n      (* z 2)))\n\n(print (nested-calculations 5 3)) ; Output: 30, "Starting calculations...", "Calculations completed."\n(print (nested-calculations 5 -1)) ; Output: "Starting calculations...", "Negative input not allowed!"\n'})}),"\n",(0,o.jsxs)(n.p,{children:["In the first call to ",(0,o.jsx)(n.code,{children:"nested-calculations"}),", ",(0,o.jsx)(n.code,{children:"nested-helper"})," returns 6, and the multiplication and final print occur."]}),"\n",(0,o.jsxs)(n.p,{children:["In the second call, ",(0,o.jsx)(n.code,{children:"nested-helper"})," is called with ",(0,o.jsx)(n.code,{children:"-1"}),". The ",(0,o.jsx)(n.code,{children:"throw"})," form is executed, transferring control directly to the ",(0,o.jsx)(n.code,{children:"catch"})," form in ",(0,o.jsx)(n.code,{children:"nested-calculations"}),', with the value "Negative input not allowed!". The "Calculations completed." print is skipped.']}),"\n",(0,o.jsxs)(n.p,{children:[(0,o.jsx)(n.code,{children:"catch"})," and ",(0,o.jsx)(n.code,{children:"throw"})," provide a powerful way to handle exceptional situations or implement complex control flow patterns, but they should be used judiciously, as excessive use can make code harder to understand."]}),"\n",(0,o.jsx)(n.h2,{id:"4-other-control-flow-constructs-1",children:"4. Other Control Flow Constructs"}),"\n",(0,o.jsxs)(n.h3,{id:"41-go-and-tagbody-low-level-control-flow",children:["4.1 ",(0,o.jsx)(n.code,{children:"go"})," and ",(0,o.jsx)(n.code,{children:"tagbody"}),": Low-Level Control Flow"]}),"\n",(0,o.jsxs)(n.p,{children:[(0,o.jsx)(n.code,{children:"tagbody"})," establishes a block of code with labels (tags). ",(0,o.jsx)(n.code,{children:"go"})," transfers control to a specific tag within the same ",(0,o.jsx)(n.code,{children:"tagbody"}),". These are the most primitive control flow mechanisms in Common Lisp and are generally discouraged in favor of higher-level constructs like ",(0,o.jsx)(n.code,{children:"loop"}),", ",(0,o.jsx)(n.code,{children:"do"}),", ",(0,o.jsx)(n.code,{children:"cond"}),", and ",(0,o.jsx)(n.code,{children:"block"}),"/",(0,o.jsx)(n.code,{children:"return-from"}),"."]}),"\n",(0,o.jsx)(n.pre,{children:(0,o.jsx)(n.code,{className:"language-lisp",children:'(tagbody\n start\n  (print "Starting...")\n  (let ((x (read)))\n    (if (zerop x)\n        (go end))) ; Go to the \'end\' tag if x is zero\n  (print "Not zero!")\n  (go start) ; Go back to the \'start\' tag\n end\n  (print "Ending."))\n'})}),"\n",(0,o.jsxs)(n.p,{children:["This code creates a loop that prompts the user for input. If the input is zero, the program jumps to the ",(0,o.jsx)(n.code,{children:"end"}),' tag and exits. Otherwise, it prints "Not zero!" and loops back to the beginning.']}),"\n",(0,o.jsxs)(n.p,{children:["While ",(0,o.jsx)(n.code,{children:"go"})," and ",(0,o.jsx)(n.code,{children:"tagbody"})," provide a basic level of control, they can easily lead to spaghetti code that is difficult to read and maintain. The higher-level control flow forms provide better structure and readability, so they should be preferred in most cases."]}),"\n",(0,o.jsx)(n.p,{children:"This concludes the introduction to control flow in Common Lisp. By understanding these forms, you can write more complex and well-structured programs. Remember to use the most appropriate control flow construct for each situation to keep your code clear and maintainable."})]})}function h(e={}){const{wrapper:n}={...(0,s.a)(),...e.components};return n?(0,o.jsx)(n,{...e,children:(0,o.jsx)(a,{...e})}):a(e)}},1151:(e,n,i)=>{i.d(n,{Z:()=>c,a:()=>t});var o=i(7294);const s={},l=o.createContext(s);function t(e){const n=o.useContext(l);return o.useMemo((function(){return"function"==typeof e?e(n):{...n,...e}}),[n,e])}function c(e){let n;return n=e.disableParentContext?"function"==typeof e.components?e.components(s):e.components||s:t(e.components),o.createElement(l.Provider,{value:n},e.children)}}}]);