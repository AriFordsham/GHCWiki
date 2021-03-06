# Moving the User's Guide away from DocBook

## The Problem


The GHC Users Guide is the primary reference resource for users of GHC. It is critical for the health of the GHC project and wider Haskell community that it remain accessible to readers as well as easy to contribute to.
As of September 2015 the Users Guide is written in DocBook.
In late 2014 there was a [brief thread](https://mail.haskell.org/pipermail/ghc-devs/2014-October/006599.html) on this list discussing the
choice of markup language used for GHC's users guide motivated by a few issues with DocBook which have been increasingly problematic,

- The format's documentation is ancient and isn't terribly approachable.

- Writing XML by hand is a terrible, terrible experience. For this reason a shocking fraction of the users' guide isn't even valid DocBook (although is largely still accepted by the toolchain)

- The tooling surrounding the format is challenging to bring up on non-Linux platforms

- Getting even a simple image displayed consistently in the PDF and HTML output is an [exercise in futility](https://ghc.haskell.org/trac/ghc/ticket/10416)


For this reason there have been a few proposals to move away from DocBook in the past,

- October 2014: [https://mail.haskell.org/pipermail/ghc-devs/2014-October/006599.html](https://mail.haskell.org/pipermail/ghc-devs/2014-October/006599.html)
- April 2015: [https://mail.haskell.org/pipermail/ghc-devs/2015-April/008844.html](https://mail.haskell.org/pipermail/ghc-devs/2015-April/008844.html)

## Alternatives to DocBook


There are a few alternatives that we could switch to (I here refers to Ben Gamari),

### Markdown


While ubiquitous, its syntax isn't nearly expressive enough to accommodate the users guide.

### asciidoc


This was the front-runner in the thread. Unfortunately, when I tried to use it in anger on the users guide things pretty quickly fell apart start to come apart. The syntax is sadly not very composable: tasks like nesting a code block inside a list becomes fragile and quite unreadable due to the need for continuation characters (as delimited blocks like code blocks must begin at column 0).
    
Despite this I did manage to get much of the way through an [asciidoc-ification](https://github.com/bgamari/ghc/blob/asciidoc/docs/users_guide/) of the users guide but only through a great deal of manual fixing. While asciidoc does strive to map one-to-one onto DocBook, my experience is that the converse is not true; a conversion to asciidoc require that we drop some of the finer distinctions between code-like inline elements. For an example of the continuation character issue, see [ ghci.asciidoc](https://github.com/bgamari/ghc/blame/asciidoc/docs/users_guide/ghci.asciidoc#L2162).

### ReStructuredText


This was a close second-place in the thread and has a fairly wide user base. The primary implementation, Sphinx, is used by Python, MathJAX, LLVM, Ubuntu, Ceph, Blender, and others. The syntax is fairly similar to Markdown and is at least as expressive as asciidoc.


I have converted the entire users guide to ReStructuredText with a modified Pandoc. While some tweaking is still certainly necessary the output from the most-mechanical conversion looks quite good,

- HTML (using a modified version of LLVM's theme), [http://smart-cactus.org/\~ben/ghc-user-manual/html/index.html](http://smart-cactus.org/~ben/ghc-user-manual/html/index.html)
- PDF produced by xetex (for convenient Unicode support), [http://smart-cactus.org/\~ben/ghc-user-manual/xetex/GHCUsersGuide.pdf](http://smart-cactus.org/~ben/ghc-user-manual/xetex/GHCUsersGuide.pdf)
- ePub (I know nothing about this format) [http://smart-cactus.org/\~ben/ghc-user-manual/epub/GHCUsersGuide.epub](http://smart-cactus.org/~ben/ghc-user-manual/epub/GHCUsersGuide.epub)
- Even Github's rendering of the source looks reasonably good, [https://github.com/bgamari/ghc/blob/doc-rst/docs/users_guide/ghci.rst](https://github.com/bgamari/ghc/blob/doc-rst/docs/users_guide/ghci.rst)


Of course, there are a few annoyances: the doctree construct doesn't quite work how one might expect, requiring one to split up files a bit more than one might like. Like asciidoc, there is no good way to express nested inlines, so we still lose some of the expressiveness of DocBook.


Another nice advantage here is that Trac has [native support](http://trac.edgewall.org/wiki/WikiRestructuredText) for rendering RST which could come in handy when pasting between documents.

## Proposal


In light of the above, ReStructuredText seems to be the most appropriate option:
the tooling is much better the DocBook, the format reasonably easy to grok and
expressive enough to accommodate the majority of the users guide unmodified.

## Execution


If we want to move forward with ReStructuredText I think we will want to
move quickly. While the conversion is mostly automated, there is some
amount of manual fiddling necessary to get things formatted nicely.
There are a few open Differentials that would need to be amended after
the change but Ben would be happy to help authors through this transition if
necessary.
