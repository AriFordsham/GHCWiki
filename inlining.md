# Inlining


Inlining is the most important compiler optimisation pass as it enables most other optimisation opportunities. The pass is simple, saturated names are replaced with their definitions, the details are complicated. The compiler must make judgements as to whether inlining a function will lead to further optimisations, if not then it is easy to increase the code size needlessly.

## Getting Started

- [Secrets of the GHC inliner](http://research.microsoft.com/en-us/um/people/simonpj/Papers/inlining/) -- quite an old paper but a great description of the main ideas
- [GHC User Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=inline#inline-and-noinline-pragmas) -- Provides a description of `INLINE`, `INLINABLE` and `NOINLINE` pragmas. 
- [Inlining and Specialisation](http://mpickering.github.io/posts/2017-03-20-inlining-and-specialisation.html) -- A blog post explaining the basic operation of the inliner and specialiser and the interaction of different pragmas and options.

## Generics and Inlining


Inlining is essential to remove intermediate representations from generic programs. There are a number of papers about the topic.

- [Optimizing Generics Is Easy! (2010)](http://dreixel.net/research/pdf/ogie.pdf)
- [Optimizing SYB Is Easy! (2014)](http://michaeldadams.org/papers/syb-opt/syb-opt-2014-pepm-authors-copy.pdf)

## Debugging the inliner


Firstly, remember that the inliner only fires with optimisations turns on (at least `-O1`). This will save you a lot of time wondering why nothing is happening!


There are several flags which are useful when working with the inliner. 

<table><tr><th> Flag </th>
<th> Usage 
</th></tr>
<tr><th> <a href="https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using.html?highlight=show-iface#ghc-flag---show-iface"> `--show-iface`</a> </th>
<th> Shows the contents of an interface file. Can be useful to check which unfoldings are being included. 
</th></tr>
<tr><th> <a href="https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/debugging.html?highlight=show-passes#ghc-flag--dshow-passes"> `-dshow-passes`</a> </th>
<th> Shows the size of the program after each optimisation pass. 
</th></tr>
<tr><th> <a href="https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/debugging.html?highlight=show-passes#ghc-flag--ddump-inlinings"> `-ddump-inlinings`</a> </th>
<th> Shows inlinings which take place 
</th></tr>
<tr><th> <a href="https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/debugging.html?highlight=show-passes#ghc-flag--ddump-simpl"> `-ddump-simpl`</a> </th>
<th> Dump the (core) output of the simplifer 
</th></tr></table>


## Relevant Tickets

See the ~inlining label.

There are also lots of old relevant tickets related to inlining. Perfect for a keen newcomer! Look for tickets labelled with both ~inlining and ~newcomer.


## Relevant Wiki Pages


- [Commentary/Compiler/DesugaringInstances](commentary/compiler/desugaring-instances) -- About how default methods can lead to poor inliner performance due to recursion
- [Proposal/SelfExplinatoryInlinePragmas](proposal/self-explinatory-inline-pragmas) 
