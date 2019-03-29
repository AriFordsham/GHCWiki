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


## Newcomer Tickets



<table><tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4960">#4960</a></th>
<td>Better inlining test in CoreUnfold</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9370">#9370</a></th>
<td>unfolding info as seen when building a module depends on flags in a previously-compiled module</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11068">#11068</a></th>
<td>Make Generic/Generic1 methods inlinable</td></tr></table>



## Relevant Tickets


There are lots of old relevant tickets related to inlining. Perfect for a keen newcomer!



Use `Keyword` = `Inlining` to ensure that a ticket ends up on these lists.



<table><tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3073">#3073</a></th>
<td>Avoid reconstructing dictionaries in recursive instance methods</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3755">#3755</a></th>
<td>Improve join point inlining</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3781">#3781</a></th>
<td>Improve inlining for local functions</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4960">#4960</a></th>
<td>Better inlining test in CoreUnfold</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5834">#5834</a></th>
<td>Allow both INLINE and INLINABLE for the same function</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5928">#5928</a></th>
<td>INLINABLE fails to specialize in presence of simple wrapper</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7109">#7109</a></th>
<td>Inlining depends on datatype size, even with INLINE pragmas</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7114">#7114</a></th>
<td>Cannot recover (good) inlining behaviour from 7.0.2 in 7.4.1</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7283">#7283</a></th>
<td>Specialise INLINE functions</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7511">#7511</a></th>
<td>Room for GHC runtime improvement &gt;~5%, inlining related</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7803">#7803</a></th>
<td>Superclass methods are left unspecialized</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7829">#7829</a></th>
<td>make better/more robust loopbreaker choices</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8099">#8099</a></th>
<td>Alternate syntax for indicating when a function is &quot;fully applied&quot; for purposes of inlining</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8589">#8589</a></th>
<td>Bad choice of loop breaker with INLINABLE/INLINE</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8662">#8662</a></th>
<td>GHC does not inline cheap inner loop when used in two places</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8668">#8668</a></th>
<td>SPECIALIZE silently fails to apply</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8774">#8774</a></th>
<td>Transitivity of Auto-Specialization</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8814">#8814</a></th>
<td>7.8 optimizes attoparsec improperly</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9020">#9020</a></th>
<td>Massive blowup of code size on trivial program</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9320">#9320</a></th>
<td>Inlining regression/strangeness in 7.8</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9370">#9370</a></th>
<td>unfolding info as seen when building a module depends on flags in a previously-compiled module</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9418">#9418</a></th>
<td>Warnings about &quot;INLINE binder is (non-rule) loop breaker&quot;</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9701">#9701</a></th>
<td>GADTs not specialized properly</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9798">#9798</a></th>
<td>Frustrating behaviour of the INLINE pragma</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10371">#10371</a></th>
<td>GHC fails to inline and specialize a function</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10421">#10421</a></th>
<td>exponential blowup in inlining (without INLINE pragmas)</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10710">#10710</a></th>
<td>More self-explanatory pragmas for inlining phase control</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10766">#10766</a></th>
<td>user manual: INLINE&apos;s interaction with optimization levels is not clear</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11068">#11068</a></th>
<td>Make Generic/Generic1 methods inlinable</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11263">#11263</a></th>
<td>&quot;Simplifier ticks exhausted&quot; that resolves with fsimpl-tick-factor=200</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12274">#12274</a></th>
<td>GHC panic: simplifier ticks exhausted</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12454">#12454</a></th>
<td>Cross-module specialisation of recursive functions</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12463">#12463</a></th>
<td>SPECIALIZABLE pragma?</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12747">#12747</a></th>
<td>INLINE vs NOINLINE vs &lt;nothing&gt; give three different results; two would be better</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13016">#13016</a></th>
<td>SPECIALIZE INLINE doesn&apos;t necessarily inline specializations of a recursive function</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13851">#13851</a></th>
<td>Change in specialisation(?) behaviour since 8.0.2 causes 6x slowdown</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14211">#14211</a></th>
<td>Compiler is unable to INLINE as well as the programmer can manually</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14275">#14275</a></th>
<td>Large Haskell value unexpectedly gets an unfolding</td></tr></table>



## Relevant Wiki Pages


- [Commentary/Compiler/DesugaringInstances](commentary/compiler/desugaring-instances) -- About how default methods can lead to poor inliner performance due to recursion
- [Proposal/SelfExplinatoryInlinePragmas](proposal/self-explinatory-inline-pragmas) 
