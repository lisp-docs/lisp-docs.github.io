# Why Common Lisp for Professional Software Developers?

> Author: [Slava Akhmechet](https://www.spakhm.com/about)
>
> Originally Published: [defmacro - The Nature of Lisp](https://www.defmacro.org/ramblings/lisp.html)
>
> Monday, May 8, 2006

# The Nature of Lisp

## Introduction

When I first stumbled into Lisp advocacy on various corners of the web I was already an experienced programmer. At that point I had grokked what seemed at the time a wide range of programming languages. I was proud to have the usual suspects (C++, Java, C#, etc.) on my service record and was under impression that I knew everything there is to know about programming languages. I couldn't have possibly been more wrong.

My initial attempt to learn Lisp came to a crashing halt as soon as I saw some sample code. I suppose the same thought ran through my mind that ran through thousands of other minds who were ever in my shoes: "Why on Earth would anyone want to use a language with such horrific syntax?!" I couldn't be bothered to learn a language if its creators couldn't be bothered to give it a pleasant syntax. After all, I was almost blinded by the infamous Lisp parentheses!

The moment I regained my sight I communicated my frustrations to some members of the Lisp sect. Almost immediately I was bombarded by a standard set of responses: Lisp's parentheses are only a superficial matter, Lisp has a huge benefit of code and data being expressed in the same manner (which, obviously, is a huge improvement over XML), Lisp has tremendously powerful metaprogramming facilities that allow programs to write code and modify themselves, Lisp allows for creation of mini-languages specific to the problem at hand, Lisp blurs the distinction between run time and compile time, Lisp, Lisp, Lisp... The list was very impressive. Needless to say none of it made sense. Nobody could illustrate the usefulness of these features with specific examples because these techniques are supposedly only useful in large software systems. After many hours of debating that conventional programming languages do the job just fine, I gave up. I wasn't about to invest months into learning a language with a terrible syntax in order to understand obscure features that had no useful examples. My time has not yet come.

For many months the Lisp advocates pressed on. I was baffled. Many extremely intelligent people I knew and had much respect for were praising Lisp with almost religious dedication. There had to be something there, something I couldn't afford not to get my hands on! Eventually my thirst for knowledge won me over. I took the plunge, bit the bullet, got my hands dirty, and began months of mind bending exercises. It was a journey on an endless lake of frustration. I turned my mind inside out, rinsed it, and put it back in place. I went through seven rings of hell and came back. And then I got it.

The enlightenment came instantaneously. One moment I understood nothing, and the next moment everything clicked into place. I've achieved nirvana. Dozens of times I heard Eric Raymond's statement quoted by different people: "Lisp is worth learning for the profound enlightenment experience you will have when you finally get it; that experience will make you a better programmer for the rest of your days, even if you never actually use Lisp itself a lot." I never understood this statement. I never believed it could be true. And finally, after all the pain, it made sense! There was more truth to it than I ever could have imagined. I've achieved an almost divine state of mind, an instantaneous enlightenment experience that turned my view of computer science on its head in less than a single second.

That very second I became a member of the Lisp cult. I felt something a ninjitsu master must feel: I had to spread my newfound knowledge to at least ten lost souls in the course of my lifetime. I took the usual path. I was rehashing the same arguments that were given to me for years (only now they actually made sense!), hoping to convert unsuspecting bystanders. It didn't work. My persistence sparked a few people's interest but their curiosity dwindled at the mere sight of sample Lisp code. Perhaps years of advocacy would forge a few new Lispers, but I wasn't satisfied. There had to be a better way.

I gave the matter careful thought. Is there something inherently hard about Lisp that prevents very intelligent, experienced programmers from understanding it? No, there isn't. After all, I got it, and if I can do it, anybody can. Then what is it that makes Lisp so hard to understand? The answer, as such things usually do, came unexpectedly. Of course! Teaching anybody anything involves building advanced concepts on top of concepts they already understand! If the process is made interesting and the matter is explained properly the new concepts become as intuitive as the original building blocks that aided their understanding. That was the problem! Metaprogramming, code and data in one representation, self-modifying programs, domain specific mini-languages, none of the explanations for these concepts referenced familiar territory. How could I expect anyone to understand them! No wonder people wanted specific examples. I could as well have been speaking in Martian!

I shared my ideas with fellow Lispers. "Well, of course these concepts aren't explained in terms of familiar territory", they said. "They are so different, they're unlike anything these people have learned before." This was a poor excuse. "I do not believe this to be true", I said. The response was unanimous: "Why don't you give it a try?" So I did. This article is a product of my efforts. It is my attempt to explain Lisp in familiar, intuitive concepts. I urge brave souls to read on. Grab your favorite drink. Take a deep breath. Prepare to be blown away. Oh, and may the Force be with you.

## XML Reloaded

A thousand mile journey starts with a single step. A journey to enlightenment is no exception and our first step just happens to be XML. What more could possibly be said about XML that hasn't already been said? It turns out, quite a bit. While there's nothing particularly interesting about XML itself, its relationship to Lisp is fascinating. XML is the all too familiar concept that Lisp advocates need so much. It is our bridge to conveying understanding to regular programmers. So let's revive the dead horse, take out the stick, and venture into XML wilderness that no one dared venture into before us. It's time to see the all too familiar moon from the other side.

Superficially XML is nothing more than a standardized syntax used to express arbitrary hierarchical data in human readable form. To-do lists, web pages, medical records, auto insurance claims, configuration files are all examples of potential XML use. Let's use a simple to-do list as an example (in a couple of sections you'll see it in a whole new light):

```xml
<todo name="housework">
    <item priority="high">Clean the house.</item>
    <item priority="medium">Wash the dishes.</item>
    <item priority="medium">Buy more soap.</item>
</todo>
```

What happens if we unleash our favorite XML parser on this to-do list? Once the data is parsed, how is it represented in memory? The most natural representation is, of course, a tree - a perfect data structure for hierarchical data. After all is said and done, XML is really just a tree serialized to a human readable form. Anything that can be represented in a tree can be represented in XML and vice versa. I hope you understand this idea. It's very important for what's coming next.

Let's take this a little further. What other type of data is often represented as a tree? At this point the list is as good as infinite so I'll give you a hint at what I'm getting at - try to remember your old compiler course. If you have a vague recollection that source code is stored in a tree after it's parsed, you're on the right track. Any compiler inevitably parses the source code into an abstract syntax tree. This isn't surprising since source code is hierarchical: functions contain arguments and blocks of code. Blocks of code contain expressions and statements. Expressions contain variables and operators. And so it goes.

Let's apply our corollary that any tree can easily be serialized into XML to this idea. If all source code is eventually represented as a tree, and any tree can be serialized into XML, then all source code can be converted to XML, right? Let's illustrate this interesting property by a simple example. Consider the function below:

```c
int add(int arg1, int arg2)
{
    return arg1 + arg2;
}
```

Can you convert this function definition to its XML equivalent? Turns out, it's reasonably simple. Naturally there are many ways to do this. Here is one way the resulting XML can look like:

```xml
<define-function return-type="int" name="add">
    <arguments>
        <argument type="int">arg1</argument>
        <argument type="int">arg2</argument>
    </arguments>
    <body>
        <return>
            <add value1="arg1" value2="arg2" />
        </return>
    </body>
</define>
```

We can go through this relatively simple exercise with any language. We can turn any source code into XML, and we can transform the resulting XML back to original source code. We can write a converter that turns Java into XML and a converter that turns XML back to Java. We could do the same for C++. (In case you're wondering if anyone is crazy enough to do it, take a look at [GCC-XML](http://www.gccxml.org/)). Furthermore, for languages that share common features but use different syntax (which to some extent is true about most mainstream languages) we could convert source code from one language to another using XML as an intermediary representation. We could use our Java2XML converter to convert a Java program to XML. We could then run an XML2CPP converter on the resulting XML and turn it into C++ code. With any luck (if we avoid using features of Java that don't exist in C++) we'll get a working C++ program. Neat, eh?

All this effectively means that we can use XML for generic storage of source code. We'd be able to create a whole class of programming languages that use uniform syntax, as well as write transformers that convert existing source code to XML. If we were to actually adopt this idea, compilers for different languages wouldn't need to implement parsers for their specific grammars - they'd simply use an XML parser to turn XML directly into an abstract syntax tree.

By now you're probably wondering why I've embarked on the XML crusade and what it has to do with Lisp (after all, Lisp was created about thirty years before XML). I promise that everything will become clear soon enough. But before we take our second step, let's go through a small philosophical exercise. Take a good look at the XML version of our "add" function above. How would you classify it? Is it data or code? If you think about it for a moment you'll realize that there are good reasons to put this XML snippet into both categories. It's XML and it's just information encoded in a standardized format. We've already determined that it can be generated from a tree data structure in memory (that's effectively what GCC-XML does). It's lying around in a file with no apparent way to execute it. We can parse it into a tree of XML nodes and do various transformations on it. It's data. But wait a moment! When all is said and done it's the same "add" function written with a different syntax, right? Once parsed, its tree could be fed into a compiler and we could execute it. We could easily write a small interpreter for this XML code and we could execute it directly. Alternatively, we could transform it into Java or C++ code, compile it, and run it. It's code.

So, where are we? Looks like we've just arrived to an interesting point. A concept that has traditionally been so hard to understand is now amazingly simple and intuitive. Code is also always data! Does it mean that data is also always code? As crazy as this sounds this very well might be the case. Remember how I promised that you'll see our to-do list in a whole new light? Let me reiterate on that promise. But we aren't ready to discuss this just yet. For now let's continue walking down our path.

A little earlier I mentioned that we could easily write an interpreter to execute our XML snippet of the add function. Of course this sounds like a purely theoretical exercise. Who in their right mind would want to do that for practical purposes? Well, it turns out quite a few people would disagree. You've likely encountered and used their work at least once in your career, too. Do I have you out on the edge of your seat? If so, let's move on!

## Ant Reloaded

Now that we've made the trip to the dark side of the moon, let's not leave quite yet. We may still learn something by exploring it a little more, so let's take another step. We begin by closing our eyes and remembering a cold rainy night in the winter of 2000. A prominent developer by the name of James Duncan Davidson[^1] was hacking his way through [Tomcat](http://tomcat.apache.org/) servlet container. As the time came to build the changes he carefully saved all his files and ran _make_. Errors. Lots of errors. Something was wrong. After careful examination James exclaimed: "Is my command not executing because I have a space in front of my tab?!" Indeed, this was the problem. Again. James has had enough. He could sense the full moon through the clouds and it made him adventurous. He created a fresh Java project and quickly hacked together a simple but surprisingly useful utility. This spark of genius used Java property files for information on how to build the project. James could now write the equivalent of the makefile in a nice format without worrying about the damned spaces ever again. His utility did all the hard work by interpreting the property file and taking appropriate actions to build the project. It was neat. Another Neat Tool. [Ant](http://ant.apache.org/).

After using Ant to build Tomcat for a few months it became clear that Java property files are not sufficient to express complicated build instructions. Files needed to be checked out, copied, compiled, sent to another machine, and unit tested. In case of failure e-mails needed to be sent out to appropriate people. In case of success "Bad to the Bone" needed to be played at the highest possible volume. At the end of the track volume had to be restored to its original level. Yes, Java property files didn't cut it anymore. James needed a more flexible solution. He didn't feel like writing his own parser (especially since he wanted an industry standard solution). XML seemed like a reasonable alternative. In a couple of days Ant was ported to XML. It was the best thing since sliced bread.

So how does Ant work? It's pretty simple. It takes an XML file with specific build instructions (you decide if they're data or code) and interprets them by running specialized Java code for each XML element. It's actually much simpler than it sounds. A simple XML instruction like the one below causes a Java class with an equivalent name to be loaded and its code to be executed.

```xml
<copy todir="../new/dir">
    <fileset dir="src\_dir"/>
</copy>
```

The snippet above copies a source directory to a destination directory. Ant locates a "copy" task (a Java class, really), sets appropriate parameters (todir and fileset) by calling appropriate Java methods and then executes the task. Ant comes with a set of core tasks and anyone can extend it with tasks of their own simply by writing Java classes that follow certain conventions. Ant finds these classes and executes them whenever XML elements with appropriate names are encountered. Pretty simple. Effectively Ant accomplishes what we were talking about in the previous section: it acts as an interpreter for a language that uses XML as its syntax by translating XML elements to appropriate Java instructions. We could write an "add" task and have Ant execute it when it encounters the XML snippet for addition presented in the previous section! Considering that Ant is an extremely popular project, the ideas presented in the previous section start looking more sane. After all, they're being used every day in what probably amounts to thousands of companies!

So far I've said nothing about why Ant actually goes through all the trouble of interpreting XML. Don't try to look for the answer on its website either - you'll find nothing of value. Nothing relevant to our discussion, anyway. Let's take another step. It's time to find out why.

Why XML?
--------

Sometimes right decisions are made without full conscious understanding of all the issues involved. I'm not sure if James knew why he chose XML - it was likely a subconscious decision. At the very least, the reasons I saw on Ant's website for using XML are all the wrong reasons. It appears that the main concerns revolved around portability and extensibility. I fail to see how XML helps advance these goals in Ant's case. What is the advantage of using interpreted XML over simple Java source code? Why not create a set of classes with a nice API for commonly used tasks (copying directories, compiling, etc.) and using those directly from Java source code? This would run on every platform that runs Java (which Ant requires anyway), it's infinitely extensible, and it has the benefit of having a more pleasant, familiar syntax. So why XML? Can we find a good reason for using it?

It turns out that we can (although as I mentioned earlier I'm not sure if James was consciously aware of it). XML has the property of being far more flexible in terms of introduction of semantic constructs than Java could ever hope to be. Don't worry, I'm not falling into the trap of using big words to describe incomprehensible concepts. This is actually a relatively simple idea, though it may take some effort to explain. Buckle your seat-belt. We're about to make a giant leap towards achieving nirvana.

How can we represent 'copy' example above in Java code? Here's one way to do it:

```java
CopyTask copy = new CopyTask();
Fileset fileset = new Fileset();

fileset.setDir("src\_dir");
copy.setToDir("../new/dir");
copy.setFileset(fileset);

copy.execute();
```

The code is almost the same, albeit a little longer than the original XML. So what's different? The answer is that the XML snippet introduces a special semantic construct for copying. If we could do it in Java it would look like this:

```java
copy("../new/dir")
{
    fileset("src\_dir");
}
```

Can you see the difference? The code above (if it were possible in Java) is a special operator for copying files - similar to a _for_ loop or a new _foreach_ construct introduced in Java 5. If we had an automatic converter from XML to Java it would likely produce the above gibberish. The reason for this is that Java's accepted syntax tree grammar is fixed by the language specification - we have no way of modifying it. We can add packages, classes, methods, but we cannot extend Java to make addition of new operators possible. Yet we can do it to our heart's content in XML - its syntax tree isn't restricted by anything except our interpreter! If the idea is still unclear, consider introducing a special operator 'unless' to Java:

```java
unless(someObject.canFly())
{
    someObject.transportByGround();
}
```

In the previous two examples we extend the Java language to introduce an operator for copying files and a conditional operator _unless_. We would do this by modifying the abstract syntax tree grammar that Java compiler accepts. Naturally we cannot do it with standard Java facilities, but we can easily do it in XML. Because our XML interpreter parses the abstract syntax tree that results from it, we can extend it to include any operator we like.

For complex operators this ability provides tremendous benefits. Can you imagine writing special operators for checking out source code, compiling files, running unit testing, sending email? Try to come up with some. If you're dealing with a specialized problem (in our case it's building projects) these operators can do wonders to decrease the amount of code you have to type and to increase clarity and code reuse. Interpreted XML makes this extremely easy to accomplish because it's a simple data file that stores hierarchical data. We do not have this option in Java because it's hierarchical structure is fixed (as you will soon find out, we do have this option in Lisp). Perhaps this is one of the reasons why Ant is so successful?

I urge you to take a look at recent evolution of Java and C# (especially the recently released specification for C# 3.0). The languages are being evolved by abstracting away commonly used functionality and adding it in the form of operators. New C# operators for built-in queries is one example. This is accomplished by relatively traditional means: language creators modify the accepted abstract syntax tree and add implementations of certain features. Imagine the possibilities if the programmer could modify the abstract syntax tree himself! Whole new sub-languages could be built for specialized domains (for example a language for building projects, like Ant). Can you come up with other examples? Think about these concepts for a bit, but don't worry about them too much. We'll come back to these issues after introducing a few more ideas. By then things will be a little more clear.

Almost Lisp
-----------

Let's forget about the operator business for the moment and try to expand our horizons beyond the constraints of Ant's design. I mentioned earlier that Ant can be extended by writing conventional Java classes. Ant interpreter then attempts to match XML elements to appropriately named Java classes and if the match is found the task is executed. An interesting question begs to be asked. Why not extend Ant in Ant itself? After all, core tasks contain a lot of conventional programming language constructs ('if' being a perfect example). If Ant provided constructs to develop tasks in Ant itself we'd reach a higher degree of portability. We'd be dependent on a core set of tasks (a standard library, if you will) and we wouldn't care if Java runtime is present: the core set could be implemented in anything. The rest of the tasks would be built on top of the core using Ant-XML itself. Ant would then become a generic, extensible, XML-based programming language. Consider the possibilities:

```xml
<task name="Test">
    <echo message="Hello World!"/>
</task>
<Test />
```

If ant supported the "task" construct, the example above would print "Hello World!". In fact, we could write a "task" task in Java and make Ant able to extend itself using Ant-XML! Ant would then be able to build more complicated primitives on top of simple ones, just like any other programming language! This is an example of "XML" based programming language we were talking about in the beginning of this tutorial. Not very useful (can you tell why?) but pretty damn cool.

By the way, take a look at our 'Test' task once again. Congratulations. You're looking at Lisp code. What on Earth am I talking about? It doesn't look anything like Lisp? Don't worry, we'll fix that in a bit. Confused? Good. Let's clear it all up!

## A Better XML

I mentioned in the previous section that self-extending Ant wouldn't be very useful. The reason for that is XML's verbosity. It's not too bad for data files but the moment you try writing reasonably complex code the amount of typing you have to do quickly starts to get in the way and progresses to becoming unusable for any real project. Have you ever tried writing Ant build scripts? I have, and once they get complex enough having to do it in XML becomes really annoying. Imagine having to type almost everything in Java twice because you have to close every element. Wouldn't that drive you nuts?

The solution to this problem involves using a less verbose alternative to XML. Remember, XML is just a format for representing hierarchical data. We don't have to use XML's angle brackets to serialize trees. We could come up with many other formats. One such format (incidentally, the one Lisp uses) is called an s-expression. S-expressions accomplish the same goals as XML. They're just a lot less verbose, which makes them much better suited for typing code. I will explain s-expressions in a little while, but before I do I have to clear up a few things about XML. Let's consider our XML example for copying files:

```xml
<copy todir="../new/dir">
    <fileset dir="src\_dir"/>
</copy>
```

Think of what the parse tree of this snippet would look like in memory. We'd have a 'copy' node that contains a fileset node. But what about attributes? How do they fit into our picture? If you've ever used XML to describe data and wondered whether you should use an element or an attribute, you're not alone. Nobody can really figure this out and doing it right tends to be black magic rather than science. The reason for that is that attributes are really subsets of elements. Anything attributes can do, elements can do as well. The reason attributes were introduced is to curb XML's verbosity. Take a look at another version of our 'copy' snippet:

```xml
<copy>
    <todir>../new/dir</todir>
    <fileset>
        <dir>src\_dir</dir>
    </fileset>
</copy>
```

The two snippets hold exactly the same information. However, we use attributes to avoid typing the same thing more than once. Imagine if attributes weren't part of XML specification. Writing anything in XML would drive us nuts!

Now that we got attributes out of the way, let's look at s-expressions. The reason we took this detour is that s-expressions do not have attributes. Because they're a lot less verbose, attributes are simply unnecessary. This is one thing we need to keep in mind when transforming XML to s-expressions. Let's take a look at an example. We could translate above snippet to s-expressions like this:

```clojure
(copy
    (todir "../new/dir")
    (fileset (dir "src\_dir")))
```

Take a good look at this representation. What's different? Angle brackets seem to be replaced by parentheses. Instead of enclosing each element into a pair of parentheses and then closing each element with a "(/element)" we simply skip the second parenthesis in "(element" and proceed. The element is then closed like this: ")". That's it! The translation is natural and very simple. It's also a lot easier to type. Do parentheses blind first time users? Maybe, but now that we're understand the reasoning behind them they're a lot easier to handle. At the very least they're better than arthritis inducing verbosity of XML. After you get used to s-expressions writing code in them is not only doable but very pleasant. And they provide all the benefits of writing code in XML (many of which we're yet to explore). Let's take a look at our 'task' code in something that looks a lot more like lisp:

```clojure
(task (name "Test")
    (echo (message "Hello World!")))
(Test)
```

S-expressions are called lists in Lisp lingo. Consider our 'task' element above. If we rewrite it without a line break and with comas instead of spaces it's starting to look surprisingly like a list of elements and other lists (the formatting is added to make it easier to see nested lists):

```clojure
(task, (name, "test"), (echo, (message, "Hello World!")))
```

We could do the same with XML. Of course the line above isn't really a list, it's a tree, just like its XML-alternative. Don't let references to lists confuse you, it's just that lists that contain other lists and trees are effectively the same thing. Lisp may stand for List Processing, but it's really tree processing - no different than processing XML nodes.

Whew. After much rambling we finally got to something that looks like Lisp (and is Lisp, really). By now the mysterious Lisp parentheses as well as some claims made by Lisp advocates should become more clear. But we still have a lot of ground to cover. Ready? Let's move on!

## C Macros Reloaded

By now you must be tired of all the XML talk. I'm tired of it as well. It's time to take a break from all the trees, s-expressions, and Ant business. Instead, let's go back to every programmer's roots. It's time to talk about C preprocessor. What's C got to do with anything, I hear you ask? Well, we now know enough to get into metaprogramming and discuss code that writes other code. Understanding this tends to be hard since all tutorials discuss it in terms of languages that you don't know. But there is nothing hard about the concept. I believe that a metaprogramming discussion based on C will make the whole thing much easier to understand. So, let's see (pun intended).

Why would anyone want to write a program that writes programs? How can we use something like this in the real world? What on Earth is metaprogramming, anyway? You already know all the answers, you just don't know it yet. In order to unlock the hidden vault of divine knowledge let's consider a rather mundane task of simple database access from code. We've all been there. Writing SQL queries all over the code to modify data within tables turns into repetitive hell soon enough. Even with the new C# 3.0 LINQ stuff this is a huge pain. Writing a full SQL query (albeit with a nice built in syntax) to get someone's name or to modify someone's address isn't exactly a programmer's idea of comfort. What do we do to solve these problems? Enter data access layers.

The idea is simple enough. You abstract database access (at least trivial queries, anyway) by creating a set of classes that mirror the tables in the database and use accessor methods to execute actual queries. This simplifies development tremendously - instead of writing SQL queries we make simple method calls (or property assignments, depending on your language of choice). Anyone who has ever used even the simplest of data access layers knows how much time it can save. Of course anyone who has ever written one knows how much time it can kill - writing a set of classes that mirror tables and convert accessors to SQL queries takes a considerable chunk of time. This seems especially silly since most of the work is manual: once you figure out the design and develop a template for your typical data access class you don't need to do any thinking. You just write code based on the same template over and over and over and over again. Many people figured out that there is a better way - there are plenty of tools that connect to the database, grab the schema, and write code for you based on a predefined (or a custom) template.

Anyone who has ever used such a tool knows what an amazing time saver it can be. In a few clicks you connect the tool to the database, get it to generate the data access layer source code, add the files to your project and voilà - ten minutes worth of work do a better job than hundreds of man-hours that were required previously. What happens if your database schema changes? Well, you just have to go through this short process again. Of course some of the best tools let you automate this - you simply add them as a part of your build step and every time you compile your project everything is done for you automatically. This is perfect! You barely have to do anything at all. If the schema ever changes your data access layer code updates automatically at compile time and any obsolete access in your code will result in compiler errors!

Data access layers are one good example, but there are plenty of others. From boilerplate GUI code, to web code, to COM and CORBA stubs, to MFC and ATL, - there are plenty of examples where the same code is written over and over again. Since writing this code is a task that can be automated completely and a programmer's time is far more expensive than CPU time, plenty of tools have been created that generate this boilerplate code automatically. What are these tools, exactly? Well, they are programs that write programs. They perform a simple task that has a mysterious name of metaprogramming. That's all there is to it.

We could create and use such tools in millions of scenarios but more often than not we don't. What it boils down to is a subconscious calculation - is it worth it for me to create a separate project, write a whole tool to generate something, and then use it, if I only have to write these very similar pieces about seven times? Of course not. Data access layers and COM stubs are written hundreds, thousands of times. This is why there are tools for them. For similar pieces of code that repeat only a few times, or even a few dozen times, writing code generation tools isn't even considered. The trouble to create such a tool more often than not far outweighs the benefit of using one. If only creating such tools was much easier, we could use them more often, and perhaps save many hours of our time. Let's see if we can accomplish this in a reasonable manner.

Surprisingly C preprocessor comes to the rescue. We've all used it in C and C++. On occasion we all wish Java had it. We use it to execute simple instructions at compile time to make small changes to our code (like selectively removing debug statements). Let's look at a quick example:

```c
#define triple(X)    X + X + X
```

What does this line do? It's a simple instruction written in the preprocessor language that instructs it to replace all instances of _triple(X)_ with _X + X + X_. For example all instances of '_triple(5)_' will be replaced with '_5 + 5 + 5_' and the resulting code will be compiled by the C compiler. We're really doing a very primitive version of code generation here. If only C preprocessor was a little more powerful and included ways to connect to the database and a few more simple constructs, we could use it to develop our data access layer right there, from within our program! Consider the following example that uses an imaginary extension of the C preprocessor:

```c
#get-db-schema("127.0.0.1, un, pwd");
#iterate-through-tables
#for-each-table
    class #table-name
    {
    };
#end-for-each
```

We've just connected to the database schema, iterated through all the tables, and created an empty class for each. All in a couple of lines right within our source code! Now every time we recompile the file where above code appears we'll get a freshly built set of classes that automatically update based on the schema. With a little imagination you can see how we could build a full data access layer straight from within our program, without the use of any external tools! Of course this has a certain disadvantage (aside from the fact that such an advanced version of C preprocessor doesn't exist) - we'd have to learn a whole new "compile-time language" to do this sort of work. For complex code generation this language would have to be very complex as well, it would have to support many libraries and language constructs. For example, if our generated code depended on some file located at some ftp server the preprocessor would have to be able to connect to ftp. It's a shame to create and learn a new language just to do this. Especially since there are so many nice languages already out there. Of course if we add a little creativity we can easily avoid this pitfall.

Why not replace the preprocessor language with C/C++ itself? We'd have full power of the language at compile time and we'd only need to learn a few simple directives to differentiate between compile time and runtime code!

```c
<%
    cout << "Enter a number: ";
    cin >> n;
%>
for(int i = 0; i < <%= n %>; i++)
{
    cout << "hello" << endl;
}
```

Can you see what happens here? Everything that's between `<%` and `%>` tags runs when the program is compiled. Anything outside of these tags is normal code. In the example above you'd start compiling your program in the development environment. The code between the tags would be compiled and then ran. You'd get a prompt to enter a number. You'd enter one and it would be placed inside the for loop. The for loop would then be compiled as usual and you'd be able to execute it. For example, if you'd enter 5 during the compilation of your program, the resulting code would look like this:

```c
for(int i = 0; i < 5; i++)
{
    cout << "hello" << endl;
}
```

Simple and effective. No need for a special preprocessor language. We get full power of our host language (in this case C/C++) at compile time. We could easily connect to a database and generate our data access layer source code at compile time in the same way JSP or ASP generate HTML! Creating such tools would also be tremendously quick and simple. We'd never have to create new projects with specialized GUIs. We could inline our tools right into our programs. We wouldn't have to worry about whether writing such tools is worth it because writing them would be so fast - we could save tremendous amounts of time by creating simple bits of code that do mundane code generation for us!

## Hello, Lisp

Everything we've learned about Lisp so far can be summarized by a single statement: Lisp is executable XML with a friendlier syntax. We haven't said a single word about how Lisp actually operates. It's time to fill this gap[^2].

Lisp has a number of built in data types. Integers and strings, for example, aren't much different from what you're used to. The meaning of _71_ or _"hello"_ is roughly the same in Lisp as in C++ or Java. What is of more interest to us are _symbols_, _lists_, and _functions_. I will spend the rest of this section describing these data types as well as how a Lisp environment compiles and executes the source code you type into it (this is called _evaluation_ in Lisp lingo). Getting through this section in one piece is important for understanding true potential of Lisp's metaprogramming, the unity of code and data, and the notion of domain specific languages. Don't think of this section as a chore though, I'll try to make it fun and accessible. Hopefully you can pick up a few interesting ideas on the way. Ok. Let's start with Lisp's symbols.

A symbol in Lisp is roughly equivalent to C++ or Java's notion of an identifier. It's a name you can use to access a variable (like _currentTime_, _arrayCount_, _n_, etc.) The difference is that a symbol in Lisp is a lot more liberal than its mainstream identifier alternative. In C++ or Java you're limited to alphanumeric characters and an underscore. In Lisp, you are not. For example _+_ is a valid symbol. So is _\-_, _\=_, _hello-world_, _hello+world_, _\*_, etc. (you can find the exact definition of valid Lisp symbols online). You can assign to these symbols any data-type you like. Let's ignore Lisp syntax and use pseudo-code for now. Assume that a function _set_ assigns some value to a symbol (like _\=_ does in Java or C++). The following are all valid examples:

```c
set(test, 5)            // symbol 'test' will equal an integer 5
set(=, 5)               // symbol '=' will equal an integer 5
set(test, "hello")      // symbol 'test' will equal a string "hello"
set(test, =)            // at this point symbol '=' is equal to 5
                        // therefore symbol 'test' will equal to 5
set(\*, "hello")         // symbol '\*' will equal a string "hello"
```

At this point something must smell wrong. If we can assign strings and integers to symbols like _\*_, how does Lisp do multiplication? After all, _\*_ means multiply, right? The answer is pretty simple. Functions in Lisp aren't special. There is a data-type, function, just like integer and string, that you assign to symbols. A multiplication function is built into Lisp and is assigned to a symbol _\*_. You can reassign a different value to _\*_ and you'd lose the multiplication function. Or you can store the value of the function in some other variable. Again, using pseudo-code:

```c
\*(3, 4)                 // multiplies 3 by 4, resulting in 12
set(temp, \*)            // symbol '\*' is equal to the multiply function
                        // so temp will equal to the multiply function
set(\*, 3)               // sets symbol '\*' to equal to 3
\*(3, 4)                 // error, symbol '\*' no longer equals to a function
                        // it's equal to 3
temp(3, 4)              // temp equals to a multiply function
                        // so Lisp multiplies 3 by 4 resulting in 12
set(\*, temp)            // symbol '\*' equals multiply function again
\*(3, 4)                 // multiplies 3 by 4, resulting in 12

You can even do wacky stuff like reassigning plus to minus:

set(+, -)               // the value of '-' is a built in minus function
                        // so now symbol '+' equals to a minus function
+(5, 4)                 // since symbol '+' is equal to the minus function
                        // this results in 1
```

I've used functions quite liberally in these examples but I didn't describe them yet. A function in Lisp is just a data-type like an integer, a string, or a symbol. A function doesn't have a notion of a name like in Java or C++. Instead, it stands on its own. Effectively it is a pointer to a block of code along with some information (like a number of parameters it accepts). You only give the function a name by assigning it to a symbol, just like you assign an integer or a string. You can create a function by using a built in function for creating functions, assigned to a symbol 'fn'. Using pseudo-code:

```c
fn \[a\]
{
    return \*(a, 2);
}
```

This returns a function that takes a single parameter named _'a'_ and doubles it. Note that the function has no name but you can assign it to a symbol:

```c
set(times-two, fn \[a\] { return \*(a, 2); })

We can now call this function:

times-two(5)             // returns 10
```

Now that we went over symbols and functions, what about lists? Well, you already know a lot about them. Lists are simply pieces of XML written in s-expression form. A list is specified by parentheses and contains Lisp data-types (including other lists) separated by a space. For example (this is real Lisp, note that we use semicolons for comments now):

```clojure
()                      ; an empty list
(1)                     ; a list with a single element, 1
(1 "test")              ; a list with two elements
                        ; an integer 1 and a string "test"
(test "hello")          ; a list with two elements
                        ; a symbol test and a string "hello"
(test (1 2) "hello")    ; a list with three elements, a symbol test
                        ; a list of two integers 1 and 2
                        ; and a string "hello"
```

When a Lisp system encounters lists in the source code it acts exactly like Ant does when it encounters XML - it attempts to execute them. In fact, Lisp source code is only specified using lists, just like Ant source code is only specified using XML. Lisp executes lists in the following manner. The first element of the list is treated as the name of a function. The rest of the elements are treated as functions parameters. If one of the parameters is another list it is executed using the same principles and the result is passed as a parameter to the original function. That's it. We can write real code now:

```clojure
(* 3 4)                 ; equivalent to pseudo-code *(3, 4).
                        ; Symbol '*' is a function
                        ; 3 and 4 are its parameters.
                        ; Returns 12.
(times-two 5)           ; returns 10
(3 4)                   ; error: 3 is not a function
(times-two)              ; error, times-two expects one parameter
(times-two 3 4)          ; error, times-two expects one parameter
(set + -)               ; sets symbol '+' to be equal to whatever symbol '-'
                        ; equals to, which is a minus function
(+ 5 4)                 ; returns 1 since symbol '+' is now equal
                        ; to the minus function
(* 3 (* 2 2))           ; multiplies 3 by the second parameter
                        ; (which is a function call that returns 4).
                        ; Returns 12.
```

Note that so far every list we've specified was treated by a Lisp system as code. But how can we treat a list as data? Again, imagine an Ant task that accepts XML as one of its parameters. In Lisp we do this using a quote operator _'_ like so:

```clojure
(set test '(1 2))       ; test is equal to a list of two integers, 1 and 2
(set test (1 2))        ; error, 1 is not a function
(set test '(* 3 4))     ; sets test to a list of three elements,
                        ; a symbol *, an integer 3, and an integer 4
```

We can use a built in function _head_ to return the first element of the list, and a built in function _tail_ to return the rest of the list's elements:

```clojure
(head '(* 3 4))         ; returns a symbol '*'
(tail '(* 3 4))         ; returns a list (3 4)
(head (tail '( * 3 4))) ; (tail '(* 3 4)) returns a list (3 4)
                        ; and (head '(3 4)) returns 3.
(head test)             ; test was set to a list in previous example
                        ; returns a symbol '*'
```

You can think of built in Lisp functions as you think of Ant tasks. The difference is that we don't have to extend Lisp in another language (although we can), we can extend it in Lisp itself as we did with the _times-two_ example. Lisp comes with a very compact set of built in functions - the necessary minimum. The rest of the language is implemented as a standard library in Lisp itself.

Lisp Macros
-----------

So far we've looked at metaprogramming in terms of a simple templating engine similar to JSP. We've done code generation using simple string manipulations. This is generally how most code generation tools go about doing this task. But we can do much better. To get on the right track, let's start off with a question. How would we write a tool that automatically generates Ant build scripts by looking at source files in the directory structure?

We could take the easy way out and generate Ant XML by manipulating strings. Of course a much more abstract, expressive and extensible way is to work with XML processing libraries to generate XML nodes directly in memory. The nodes can then be serialized to strings automatically. Furthermore, our tool would be able to analyze and transform existing Ant build scripts by loading them and dealing with the XML nodes directly. We would abstract ourselves from strings and deal with higher level concepts which let us get the job done faster and easier.

Of course we could write Ant tasks that allow dealing with XML transformations and write our generation tool in Ant itself. Or we could just use Lisp. As we saw earlier, a list is a built in Lisp data structure and Lisp has a number of facilities for processing lists quickly and effectively (_head_ and _tail_ being the simplest ones). Additionally Lisp has no semantic constraints - you can have your code (and data) have any structure you want.

Metaprogramming in Lisp is done using a construct called a "macro". Let's try to develop a set of macros that transform data like, say, a to-do list (surprised?), into a language for dealing with to-do lists.

Let's recall our to-do list example. The XML looks like this:

```xml
<todo name="housework">
    <item priority="high">Clean the house.</item>
    <item priority="medium">Wash the dishes.</item>
    <item priority="medium">Buy more soap.</item>
</todo>
```

The corresponding s-expression version looks like this:

```clojure
(todo "housework"
    (item (priority high) "Clean the house.")
    (item (priority medium) "Wash the dishes.")
    (item (priority medium) "Buy more soap."))
```

Suppose we're writing a to-do manager application. We keep our to-do items serialized in a set of files and when the program starts up we want to read them and display them to the user. How would we do this with XML and some other language (say, Java)? We'd parse our XML files with the to-do lists using some XML parser, write the code that walks the XML tree and converts it to a Java data structure (because frankly, processing DOM in Java is a pain in the neck), and then use this data structure to display the data. Now, how would we do the same thing in Lisp?

If we were to adopt the same approach we'd parse the files using Lisp libraries responsible for parsing XML. The XML would then be presented to us as a Lisp list (an s-expression) and we'd walk the list and present relevant data to the user. Of course if we used Lisp it would make sense to persist the data as s-expressions directly as there's no reason to do an XML conversion. We wouldn't need special parsing libraries since data persisted as a set of s-expressions is valid Lisp and we could use Lisp compiler to parse it and store it in memory as a Lisp list. Note that Lisp compiler (much like .NET compiler) is available to a Lisp program at runtime.

But we can do better. Instead of writing code to walk the s-expression that stores our data we could write a macro that allows us to treat data as code! How do macros work? Pretty simple, really. Recall that a Lisp function is called like this:

```clojure
(function-name arg1 arg2 arg3)
```

Where each argument is a valid Lisp expression that's evaluated and passed to the function. For example if we replace _arg1_ above with _(+ 4 5)_, it will be evaluated and _9_ would be passed to the function. A macro works the same way as a function, except its arguments are not evaluated.

```clojure
(macro-name (+ 4 5))
```

In this case, (+ 4 5) is not evaluated and is passed to the macro as a list. The macro is then free to do what it likes with it, including evaluating it. The return value of a macro is a Lisp list that's treated as code. The original place with the macro is replaced with this code. For example, we could define a macro plus that takes two arguments and puts in the code that adds them.

What does it have to do with metaprogramming and our to-do list problem? Well, for one, macros are little bits of code that generate code using a list abstraction. Also, we could create macros named _to-do_ and _item_ that replace our data with whatever code we like, for instance code that displays the item to the user.

What benefits does this approach offer? We don't have to walk the list. The compiler will do it for us and will invoke appropriate macros. All we need to do is create the macros that convert our data to appropriate code!

For example, a macro similar to our _triple_ C macro we showed earlier looks like this:

```clojure
(defmacro triple (x)
    '(+ ~x ~x ~x))
```

The quote prevents evaluation while the tilde allows it. Now every time _triple_ is encountered in lisp code:

```clojure
(triple 4)
```

it is replaced with the following code:

```clojure
(+ 4 4 4)
```

We can create macros for our to-do list items that will get called by lisp compiler and will transform the to-do list into code. Now our to-do list will be treated as code and will be executed. Suppose all we want to do is print it to standard output for the user to read:

```clojure
(defmacro item (priority note)
  '(block
    (print stdout tab "Priority: "
     ~(head (tail priority)) endl)
    (print stdout tab "Note: " ~note endl endl)))
```

We've just created a very small and limited language for managing to-do lists embedded in Lisp. Such languages are very specific to a particular problem domain and are often referred to as domain specific languages or _DSLs_.

## Domain Specific Languages

In this article we've already encountered two domain specific languages: Ant (specific to dealing with project builds) and our unnamed mini-language for dealing with to-do lists. The difference is that Ant was written from scratch using XML, an XML parser, and Java while our language is embedded into Lisp and is easily created within a couple of minutes.

We've already discussed the benefits of DSLs, mainly why Ant is using XML, not Java source code. Lisp lets us create as many DSLs as we need for our problem. We can create domain specific languages for creating web applications, writing massively multiplayer games, doing fixed income trading, solving the protein folding problem, dealing with transactions, etc. We can layer these languages on top of each other and create a language for writing web-based trading applications by taking advantage of our web application language and bond trading language. Every day we'd reap the benefits of this approach, much like we reap the benefits of Ant.

Using DSLs to solve problems results in much more compact, maintainable, flexible programs. In a way we create them in Java by creating classes that help us solve the problem. The difference is that Lisp allows us to take this abstraction to the next level: we're not limited by Java's parser. Think of writing build scripts in Java itself using some supporting library. Compare it to using Ant. Now apply this same comparison to every single problem you've ever worked on and you'll begin to glimpse a small share of the benefits offered by Lisp.

## What's next?

Learning Lisp is an uphill battle. Even though in Computer Science terms Lisp is an ancient language, few people to date figured out how to teach it well enough to make it accessible. Despite great efforts by many Lisp advocates, learning Lisp today is still hard. The good news is that this won't remain the case forever since the amount of Lisp-related resources is rapidly increasing. Time is on Lisp's side.

Lisp is a way to escape mediocrity and to get ahead of the pack. Learning Lisp means you can get a better job today, because you can impress any reasonably intelligent interviewer with fresh insight into most aspects of software engineering. It also means you're likely to get fired tomorrow because everyone is tired of you constantly mentioning how much better the company could be doing if only its software was written in Lisp. Is it worth the effort? Everyone who has ever learned Lisp says yes. The choice, of course, remains yours.

[^1]: I have never met James, nor does he know about my existence. The story is entirely fictional and is based on a few postings about Ant's history I found on the internet.

[^2]: Lisp has many different dialects (the most popular of which are Common Lisp and Scheme). Each dialect deals with intricate details differently yet shares the same set of basic principles. Since the goal of this article is to give you an understanding of Lisp's principles I will use Blaise for examples (which at the time of this writing is vaporware). With some minor modifications these examples can be translated to other Lisp dialects.
