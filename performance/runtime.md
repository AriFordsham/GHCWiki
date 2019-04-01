# Performance of programs compiled with GHC


Here is where we track various on-going efforts to improve the runtime performance of code produced by GHC. If you are interested in the performance of the compiler itself, see [Performance/Compiler](performance/compiler).

## Relevant tickets

- #10992: `Data.List.sum` is much slower than the naive recursive definition for it.  Does not happen in 7.8.
- #6166: An alleged runtime performance regression in `mwc-random`.

- #14980 (regressed in 8.4): Runtime performance regression with binary operations on vectors 


Identify tickets by using "Runtime performance bug" for the "Type of failure field".



**Open Tickets:**

<table><tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/16340">#16340</a></th>
<td>Improve properFraction for Ratio</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/16298">#16298</a></th>
<td>Awful(?) code in AArch64 stg_BLACKHOLE entry code</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/16284">#16284</a></th>
<td>Abortion of fixed-point iteration in Demand Analyser discards sound results</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/16064">#16064</a></th>
<td>Improving Placement of Heap Checks - Avoiding Slowdowns in Hot Code</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/16040">#16040</a></th>
<td>Unboxing-Related Performance Issue with Polymorphic Functions</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/16004">#16004</a></th>
<td>Vector performance regression in GHC 8.6</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15969">#15969</a></th>
<td>Generic1 deriving should use more coercions</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15842">#15842</a></th>
<td>Exponentiation needs PrelRules</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15731">#15731</a></th>
<td>Add sortOn/coerce rule</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15727">#15727</a></th>
<td>bug: all generations are collected sequentially when compacting collection kicks in</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15717">#15717</a></th>
<td>Performance regression in for_ alternatives from GHC 8.2.2 to newer GHCs</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15652">#15652</a></th>
<td>SerializedCompact has a [(Ptr a, Word)] instead of a custom datatype</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15642">#15642</a></th>
<td>Improve the worst case performance of weak pointers</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15620">#15620</a></th>
<td>Speed up Data.Unique</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15574">#15574</a></th>
<td>C wrappers for Haskell foreign exports don&apos;t have finalizers (causes memory leak).</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15524">#15524</a></th>
<td>Performance regression when using the GHC API to evaluate code compared to 8.4</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15503">#15503</a></th>
<td>interpreter: sequence_ (replicate 100000000 (return ()))  gobbles up memory</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15366">#15366</a></th>
<td>GHC.Conc.Windows has a surprising queue</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15185">#15185</a></th>
<td>Enum instance for IntX / WordX are inefficient</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15176">#15176</a></th>
<td>Superclass `Monad m =&gt;` makes program run 100 times slower</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15153">#15153</a></th>
<td>GHC uses O_NONBLOCK on regular files, which has no effect, and blocks the runtime</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15127">#15127</a></th>
<td>Unbox around runRW#</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14980">#14980</a></th>
<td>Runtime performance regression with binary operations on vectors</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14941">#14941</a></th>
<td>Switching direct type family application to EqPred (~) prevents inlining in code using vector (10x slowdown)</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14929">#14929</a></th>
<td>Program compiled with -O2 exhibits much worse performance</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14870">#14870</a></th>
<td>Runtime performance regression in 8.4</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14827">#14827</a></th>
<td>Recognize when inlining would create a join point</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14816">#14816</a></th>
<td>Missed Called Arity opportunity?</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14797">#14797</a></th>
<td>High-residency modules during GHC build</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14789">#14789</a></th>
<td>GHCi fails to garbage collect declaration `l = length [1..10^8]` entered at prompt</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14762">#14762</a></th>
<td>Foreign.Marshal.Pool functions use inefficient O(n) operations</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14727">#14727</a></th>
<td>Unboxed sum performance surprisingly poor</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14620">#14620</a></th>
<td>Polymorphic functions not easily recognized as join points</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14610">#14610</a></th>
<td>newtype wrapping of a monadic stack kills performance</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14565">#14565</a></th>
<td>Performance degrades from -O1 to -O2</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14564">#14564</a></th>
<td>CAF isn&apos;t floated</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14509">#14509</a></th>
<td>Consider adding new stg_ap_* functions</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14461">#14461</a></th>
<td>Reuse free variable lists through nested closures</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14407">#14407</a></th>
<td>rts: Threads/caps affinity</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14383">#14383</a></th>
<td>Allocation in VS up 500%</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14359">#14359</a></th>
<td>C-- pipeline/NCG fails to optimize simple repeated addition</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14337">#14337</a></th>
<td>typeRepKind can perform substantial amounts of allocation</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14295">#14295</a></th>
<td>tagToEnum# leads to some silly closures</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14256">#14256</a></th>
<td>GHCi is faster than compiled code</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14239">#14239</a></th>
<td>Let -fspecialise-aggressively respect NOINLINE (or NOSPECIALISABLE?)</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14211">#14211</a></th>
<td>Compiler is unable to INLINE as well as the programmer can manually</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14208">#14208</a></th>
<td>Performance with O0 is much better than the default or with -O2, runghc performs the best</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14072">#14072</a></th>
<td>Code generated by GHC 8.2.1 faster than 8.0.1 but still somewhat slower than 7.10.3</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14003">#14003</a></th>
<td>Allow more worker arguments in SpecConstr</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13904">#13904</a></th>
<td>LLVM does not need to trash caller-saved registers.</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13873">#13873</a></th>
<td>Adding a SPECIALIZE at a callsite in Main.hs is causing a regression</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13851">#13851</a></th>
<td>Change in specialisation(?) behaviour since 8.0.2 causes 6x slowdown</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13763">#13763</a></th>
<td>Performance regression (~34%) in 8.2.1, poor register allocation(?) in an inner loop over an array</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13725">#13725</a></th>
<td>Remove false dependency on the destination of the popcnt instruction</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13692">#13692</a></th>
<td>Constructors and such should be able to move around seq# sometimes</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13629">#13629</a></th>
<td>sqrt should use machine instruction on x86_64</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13362">#13362</a></th>
<td>GHC first generation of GC to be as large as largest cache size by default</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13339">#13339</a></th>
<td>Arbitrarily large expressions built out of cheap primops are not floated out</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13334">#13334</a></th>
<td>Constant folding for repeated integer operation of unknown value</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13331">#13331</a></th>
<td>Worker/wrapper can lead to sharing failure</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13309">#13309</a></th>
<td>Use liftA2 in ApplicativeDo</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13296">#13296</a></th>
<td>stat() calls can block Haskell runtime</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13280">#13280</a></th>
<td>Consider deriving more Foldable methods</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13225">#13225</a></th>
<td>Fannkuch-redux time regression from join point patch</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13193">#13193</a></th>
<td>Integer (gmp) performance regression?</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13153">#13153</a></th>
<td>Several Traversable instances have an extra fmap</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13080">#13080</a></th>
<td>Memory leak caused by nested monadic loops</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13016">#13016</a></th>
<td>SPECIALIZE INLINE doesn&apos;t necessarily inline specializations of a recursive function</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13014">#13014</a></th>
<td>Seemingly unnecessary marking of a SpecConstr specialization as a loopbreaker</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13002">#13002</a></th>
<td>:set -O does not work in .ghci file</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12953">#12953</a></th>
<td>Use computed gotos in the interpreter when the compiler supports it</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12900">#12900</a></th>
<td>Common up identical info tables</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12893">#12893</a></th>
<td>Profiling defeats stream fusion when using vector library</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12817">#12817</a></th>
<td>Degraded performance with constraint synonyms</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12808">#12808</a></th>
<td>For closures, Loop Invariant Code Flow related to captured free values not lifted outside the loop...</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12798">#12798</a></th>
<td>LLVM seeming to over optimize, producing inefficient assembly code...</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12737">#12737</a></th>
<td>T12227 is failing on ghc-8.0</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12665">#12665</a></th>
<td>Make Read instances for Integral types faster, and make them fail fast</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12640">#12640</a></th>
<td>Class member functions not substituted for MultiParamTypeClasses</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12566">#12566</a></th>
<td>Memory leak</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12232">#12232</a></th>
<td>Opportunity to do better in register allocations</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12231">#12231</a></th>
<td>Eliminate redundant heap allocations/deallocations</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12181">#12181</a></th>
<td>Multi-threaded code on ARM64 GHC runtime doesn&apos;t use all available cores</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11677">#11677</a></th>
<td>Dramatic de-optimization with &quot;-O&quot;, &quot;-O1&quot;, &quot;-O2&quot; options</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11668">#11668</a></th>
<td>SPEC has a runtime cost if constructor specialization isn&apos;t performed</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11587">#11587</a></th>
<td>Place shared objects in LIBDIR</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11561">#11561</a></th>
<td>Have static ghci link against its own copy of its libraries</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11441">#11441</a></th>
<td>RFC: Inline intermediate languages (Core, STG, Cmm, even StrictCore)</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11393">#11393</a></th>
<td>Ability to define INLINE pragma for all instances of a given typeclass</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11271">#11271</a></th>
<td>Costly let binding gets duplicated in IO action value</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11226">#11226</a></th>
<td>Performance regression (involving sum, map, enumFromThenTo)</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11146">#11146</a></th>
<td>Manual eta expansion leads to orders of magnitude less allocations</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11143">#11143</a></th>
<td>Feature request: Add index/read/write primops with byte offset for ByteArray#</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11134">#11134</a></th>
<td>Limit frequency of idle GCs</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11029">#11029</a></th>
<td>Performance loss due to eta expansion</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10992">#10992</a></th>
<td>Performance regression due to lack of inlining of `foldl` and `foldl&apos;`.</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10944">#10944</a></th>
<td>powModInteger slower than computing pow and mod separately</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10922">#10922</a></th>
<td>String inlining is inconsistent</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10906">#10906</a></th>
<td>`SPECIALIZE instance` could be better</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10809">#10809</a></th>
<td>Add prefetch{Small}{Mutable}Array[0..3]#</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10804">#10804</a></th>
<td>Rules conditional on strictess properties</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10730">#10730</a></th>
<td>Spectral norm allocations increased 17% between 7.6 and 7.8</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10652">#10652</a></th>
<td>Better cache performance in Array#</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10648">#10648</a></th>
<td>Some 64-vector SIMD primitives are absolutely useless</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10626">#10626</a></th>
<td>Missed opportunity for SpecConstr</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10606">#10606</a></th>
<td>avoid redundant stores to the stack when examining already-tagged data</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10482">#10482</a></th>
<td>Not enough unboxing happens on data-family function argument</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10470">#10470</a></th>
<td>Allocating StablePtrs leads to GC slowdown even after they&apos;re freed</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10434">#10434</a></th>
<td>SPECIALISE instance does not specialize as far as SPECIALISE for type signatures</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10421">#10421</a></th>
<td>exponential blowup in inlining (without INLINE pragmas)</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10417">#10417</a></th>
<td>Rule matching not &quot;seeing through&quot; floating and type lambda (and maybe cast)</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10401">#10401</a></th>
<td>state hack-related regression</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10371">#10371</a></th>
<td>GHC fails to inline and specialize a function</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10346">#10346</a></th>
<td>Cross-module SpecConstr</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10319">#10319</a></th>
<td>Eta expand PAPs</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10229">#10229</a></th>
<td>setThreadAffinity assumes a certain CPU virtual core layout</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10124">#10124</a></th>
<td>Simple case analyses generate too many branches</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10120">#10120</a></th>
<td>Unnecessary code duplication from case analysis</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10069">#10069</a></th>
<td>CPR related performance issue</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10062">#10062</a></th>
<td>Codegen on sequential FFI calls is not very good</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10049">#10049</a></th>
<td>Lower level memcpy primop</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10016">#10016</a></th>
<td>UNPACK support for existentials</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10012">#10012</a></th>
<td>Cheap-to-compute values aren&apos;t pushed into case branches inducing unnecessary register pressure</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10005">#10005</a></th>
<td>Operations on string literals won&apos;t be inlined</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9992">#9992</a></th>
<td>Constructor specialization requires eta expansion</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9989">#9989</a></th>
<td>GHCI is slow for precompiled code</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9944">#9944</a></th>
<td>Performance issue re: simple loop</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9923">#9923</a></th>
<td>Offer copy-on-GC sliced arrays</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9809">#9809</a></th>
<td>Overwhelming the TimerManager</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9798">#9798</a></th>
<td>Frustrating behaviour of the INLINE pragma</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9792">#9792</a></th>
<td>map/coerce rule does not fire until the coercion is known</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9790">#9790</a></th>
<td>Produce coercion rules for derived Functor instances</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9786">#9786</a></th>
<td>Make quot/rem/div/mod with known divisors fast</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9701">#9701</a></th>
<td>GADTs not specialized properly</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9688">#9688</a></th>
<td>Improve the interaction between CSE and the join point transformation</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9661">#9661</a></th>
<td>Branchless ==# is compiled to branchy code</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9660">#9660</a></th>
<td>unnecessary indirect jump when returning a case scrutinee</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9659">#9659</a></th>
<td>Offer branchless conditional (CMOV) primop</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9655">#9655</a></th>
<td>Do not UNPACK strict fields that are very wide</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9646">#9646</a></th>
<td>Simplifer non-determinism leading to 8 fold difference in run time performance</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9645">#9645</a></th>
<td>Optimize range checks for primitive types</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9617">#9617</a></th>
<td>Implement `quot` and `rem` using `quotRem`; implement `div` and `mod` using `divMod`</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9601">#9601</a></th>
<td>Make the rewrite rule system more powerful</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9542">#9542</a></th>
<td>GHC-IO-Handle-Text.hPutStr&apos; and writeBlocks look like they need refactoring</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9522">#9522</a></th>
<td>SPECIALISE pragmas for derived instances</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9447">#9447</a></th>
<td>Add support for resizing `MutableByteArray#`s</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9431">#9431</a></th>
<td>integer-gmp small Integer multiplication does two multiplications on x86</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9388">#9388</a></th>
<td>Narrow the scope of the notorious &quot;state hack&quot;</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9374">#9374</a></th>
<td>Investigate Static Argument Transformation</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9353">#9353</a></th>
<td>prefetch primops are not currently useful</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9350">#9350</a></th>
<td>Consider using xchg instead of mfence for CS stores</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9349">#9349</a></th>
<td>excessive inlining due to state hack</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9342">#9342</a></th>
<td>Branchless arithmetic operations</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9320">#9320</a></th>
<td>Inlining regression/strangeness in 7.8</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9289">#9289</a></th>
<td>add anyToAddr# :: (#a#)-&gt; Addr# primop (inverse of addrToAny#)</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9279">#9279</a></th>
<td>Local wrapper function remains in final program; result = extra closure allocation</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9251">#9251</a></th>
<td>ghc does not expose branchless max/min operations as primops</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9246">#9246</a></th>
<td>GHC generates poor code for repeated uses of min/max</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9192">#9192</a></th>
<td>Add sameByteArray#</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9137">#9137</a></th>
<td>A way to match RULES only for literals</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9120">#9120</a></th>
<td>Cache intermediate powers</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9088">#9088</a></th>
<td>Per-thread Haskell thread list/numbering (remove global lock from thread allocation)</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9041">#9041</a></th>
<td>NCG generates slow loop code</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8971">#8971</a></th>
<td>Native Code Generator for 8.0.1 is not as optimized as 7.6.3...</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8955">#8955</a></th>
<td>Syscall intrinsic</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8949">#8949</a></th>
<td>switch -msse2 to be on by default</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8905">#8905</a></th>
<td>Function arguments are always spilled/reloaded if scrutinee is already in WHNF</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8903">#8903</a></th>
<td>Add dead store elimination</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8887">#8887</a></th>
<td>Double double assignment in optimized Cmm on SPARC</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8871">#8871</a></th>
<td>No-op assignment I64[BaseReg + 784] = I64[BaseReg + 784]; is generated into optimized Cmm</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8814">#8814</a></th>
<td>7.8 optimizes attoparsec improperly</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8733">#8733</a></th>
<td>I/O manager causes unnecessary syscalls in send/recv loops</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8732">#8732</a></th>
<td>Global big object heap allocator lock causes contention</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8668">#8668</a></th>
<td>SPECIALIZE silently fails to apply</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8662">#8662</a></th>
<td>GHC does not inline cheap inner loop when used in two places</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8655">#8655</a></th>
<td>Evaluate know-to-terminate-soon thunks</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8635">#8635</a></th>
<td>GHC optimisation flag ignored when importing a local module with derived type classes</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8623">#8623</a></th>
<td>Strange slowness when using async library with FFI callbacks</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8598">#8598</a></th>
<td>IO hack in demand analyzer gets in the way of CPR</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8589">#8589</a></th>
<td>Bad choice of loop breaker with INLINABLE/INLINE</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8578">#8578</a></th>
<td>Improvements to SpinLock implementation</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8457">#8457</a></th>
<td>-ffull-laziness does more harm than good</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8404">#8404</a></th>
<td>Default to turning on architecture specific optimizations in the codegen</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8354">#8354</a></th>
<td>Add INLINE (or at least INLINABLE) pragmas for methods of Ord in ghc-prim</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8336">#8336</a></th>
<td>Sinking pass could optimize some assignments better</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8327">#8327</a></th>
<td>Cmm sinking does not eliminate dead code in loops</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8326">#8326</a></th>
<td>Place heap checks common in case alternatives before the case</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8317">#8317</a></th>
<td>Optimize tagToEnum# at Core level</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8313">#8313</a></th>
<td>Poor performance of higher-order functions with unboxing</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8311">#8311</a></th>
<td>suboptimal code generated for even :: Int -&gt; Bool by NCG (x86, x86_64)</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8279">#8279</a></th>
<td>bad alignment in code gen  yields substantial perf issue</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8272">#8272</a></th>
<td>testing if SpLim=$rbp and Sp=$rsp changed performance at all</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8151">#8151</a></th>
<td>ghc-7.4.2 on OpenIndiana (Solaris) createSubprocess fails</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8048">#8048</a></th>
<td>Register spilling produces ineffecient/highly contending code</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8046">#8046</a></th>
<td>Make the timer management scale better across multicore</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8032">#8032</a></th>
<td>Worker-wrapper transform and NOINLINE trigger bad reboxing behavior</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8023">#8023</a></th>
<td>dph-examples binaries don&apos;t use all CPUs</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7977">#7977</a></th>
<td>Optimization: Shift dropped list heads by coeffecient to prevent thunk generation</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7741">#7741</a></th>
<td>Add SIMD support to x86/x86_64 NCG</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7679">#7679</a></th>
<td>Regression in -fregs-graph performance</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7647">#7647</a></th>
<td>UNPACK polymorphic fields</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7602">#7602</a></th>
<td>Threaded RTS performing badly on recent OS X (10.8?)</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7596">#7596</a></th>
<td>Opportunity to improve CSE</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7542">#7542</a></th>
<td>GHC doesn&apos;t optimize (strict) composition with id</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7511">#7511</a></th>
<td>Room for GHC runtime improvement &gt;~5%, inlining related</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7398">#7398</a></th>
<td>RULES don&apos;t apply to a newtype constructor</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7378">#7378</a></th>
<td>Identical alts/bad divInt# code</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7374">#7374</a></th>
<td>rule not firing</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7367">#7367</a></th>
<td>float-out causes extra allocation</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7309">#7309</a></th>
<td>The Ix instance for (,) leaks space in range</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7307">#7307</a></th>
<td>Share top-level code for strings</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7300">#7300</a></th>
<td>Allow CAFs kept reachable by FFI to be forcibly made unreachable for GC</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7283">#7283</a></th>
<td>Specialise INLINE functions</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7273">#7273</a></th>
<td>Binary size increase in nofib/grep between 7.6.1 and HEAD</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7206">#7206</a></th>
<td>Implement cheap build</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7114">#7114</a></th>
<td>Cannot recover (good) inlining behaviour from 7.0.2 in 7.4.1</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7109">#7109</a></th>
<td>Inlining depends on datatype size, even with INLINE pragmas</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7080">#7080</a></th>
<td>Make RULES and SPECIALISE more consistent</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7063">#7063</a></th>
<td>Register allocators can&apos;t handle non-uniform register sets</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/6092">#6092</a></th>
<td>Liberate case not happening</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/6070">#6070</a></th>
<td>Fun with the demand analyser</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5928">#5928</a></th>
<td>INLINABLE fails to specialize in presence of simple wrapper</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5834">#5834</a></th>
<td>Allow both INLINE and INLINABLE for the same function</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5775">#5775</a></th>
<td>Inconsistency in demand analysis</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5645">#5645</a></th>
<td>Sharing across functions causing space leak</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5567">#5567</a></th>
<td>LLVM: Improve alias analysis / performance</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5463">#5463</a></th>
<td>SPECIALISE pragmas generated from Template Haskell are ignored</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5444">#5444</a></th>
<td>Slow 64-bit primops on 32 bit system</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5355">#5355</a></th>
<td>Link plugins against existing libHSghc</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5344">#5344</a></th>
<td>CSE should look through coercions</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5326">#5326</a></th>
<td>Polymorphic instances aren&apos;t automatically specialised</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5302">#5302</a></th>
<td>Unused arguments in join points</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5298">#5298</a></th>
<td>Inlined functions aren&apos;t fully specialised</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5262">#5262</a></th>
<td>Compiling with -O makes some expressions too lazy and causes space leaks</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5218">#5218</a></th>
<td>Add unpackCStringLen# to create Strings from string literals</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5171">#5171</a></th>
<td>Misfeature of Cmm optimiser: no way to extract a branch of expression into a separate statement</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5075">#5075</a></th>
<td>CPR optimisation for sum types if only one constructor is used</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5059">#5059</a></th>
<td>Pragma to SPECIALISE on value arguments</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4960">#4960</a></th>
<td>Better inlining test in CoreUnfold</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4945">#4945</a></th>
<td>Another SpecConstr infelicity</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4941">#4941</a></th>
<td>SpecConstr generates functions that do not use their arguments</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4937">#4937</a></th>
<td>Remove indirections caused by sum types, such as Maybe</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4833">#4833</a></th>
<td>Finding the right loop breaker</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4831">#4831</a></th>
<td>Too many specialisations in SpecConstr</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4823">#4823</a></th>
<td>Loop strength reduction for array indexing</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4470">#4470</a></th>
<td>Loop optimization: identical counters</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4301">#4301</a></th>
<td>Optimisations give bad core for foldl&apos; (flip seq) ()</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4101">#4101</a></th>
<td>Primitive constant unfolding</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4096">#4096</a></th>
<td>New primops for indexing: index*OffAddrUsing# etc</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4081">#4081</a></th>
<td>Strict constructor fields inspected in loop</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4005">#4005</a></th>
<td>Bad behaviour in the generational GC with paraffins -O2</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3781">#3781</a></th>
<td>Improve inlining for local functions</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3767">#3767</a></th>
<td>SpecConstr for join points</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3765">#3765</a></th>
<td>Rules should &quot;look through&quot; case binders too</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3755">#3755</a></th>
<td>Improve join point inlining</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3744">#3744</a></th>
<td>Comparisons against minBound/maxBound not optimised for (Int|Word)(8|16|32)</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3606">#3606</a></th>
<td>The Ord instance for unboxed arrays is very inefficient</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3557">#3557</a></th>
<td>CPU Vector instructions in GHC.Prim</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3462">#3462</a></th>
<td>New codegen: allocate large objects using allocateLocal()</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3458">#3458</a></th>
<td>Allocation where none should happen</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3138">#3138</a></th>
<td>Returning a known constructor: GHC generates terrible code for cmonad</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3107">#3107</a></th>
<td>Over-eager GC when blocked on a signal in the non-threaded runtime</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3073">#3073</a></th>
<td>Avoid reconstructing dictionaries in recursive instance methods</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3061">#3061</a></th>
<td>GHC&apos;s GC default heap growth strategy is not as good as other runtimes</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3055">#3055</a></th>
<td>Int / Word / IntN / WordN are unequally optimized</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3034">#3034</a></th>
<td>divInt# floated into a position which leads to low arity</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2731">#2731</a></th>
<td>Avoid unnecessary evaluation when unpacking constructors</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2642">#2642</a></th>
<td>Improve SpecConstr for join points</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2625">#2625</a></th>
<td>Unexpected -ddump-simpl output for derived Ord instance and UNPACKed fields</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2607">#2607</a></th>
<td>Inlining defeats selector thunk optimisation</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2598">#2598</a></th>
<td>Avoid excessive specialisation in SpecConstr</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2465">#2465</a></th>
<td>Fusion of recursive functions</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2439">#2439</a></th>
<td>Missed optimisation with dictionaries and loops</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2387">#2387</a></th>
<td>Optimizer misses unboxing opportunity</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2374">#2374</a></th>
<td>MutableByteArray# is slower than Addr#</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2289">#2289</a></th>
<td>Needless reboxing of values when returning from a tight loop</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2273">#2273</a></th>
<td>inlining defeats seq</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2269">#2269</a></th>
<td>Word type to Double or Float conversions are slower than Int conversions</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2255">#2255</a></th>
<td>Improve SpecConstr for free variables</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2132">#2132</a></th>
<td>Optimise nested comparisons</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2028">#2028</a></th>
<td>STM slightly conservative on write-only transactions</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/1687">#1687</a></th>
<td>A faster (^)-function.</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/1600">#1600</a></th>
<td>Optimisation: CPR the results of IO</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/1544">#1544</a></th>
<td>Derived Read instances for recursive datatypes with infix constructors are too inefficient</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/1498">#1498</a></th>
<td>Optimisation: eliminate unnecessary heap check in recursive function</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/1216">#1216</a></th>
<td>Missed opportunity for let-no-esape</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/1168">#1168</a></th>
<td>Optimisation sometimes decreases sharing in IO code</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/1147">#1147</a></th>
<td>Quadratic behaviour in the compacting GC</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/932">#932</a></th>
<td>Improve inlining</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/917">#917</a></th>
<td>-O introduces space leak</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/855">#855</a></th>
<td>Improvements to SpecConstr</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/728">#728</a></th>
<td>switch to compacting collection when swapping occurs</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/605">#605</a></th>
<td>Optimisation: strict enumerations</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/149">#149</a></th>
<td>missed CSE opportunity</td></tr></table>




**Closed Tickets:**

<table><tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/16109">#16109</a></th>
<td>cabal install H under Windows 10 does not terminate</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15802">#15802</a></th>
<td>Inlining of constant fails when both cross-module and recursive</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15227">#15227</a></th>
<td>Add PrelRules for par#</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15226">#15226</a></th>
<td>GHC doesn&apos;t know that seq# produces something in WHNF</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15143">#15143</a></th>
<td>Passing an IO value through several functions results in program hanging.</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/15131">#15131</a></th>
<td>Speed up certain Foldable NonEmpty methods</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14978">#14978</a></th>
<td>GADTs don&apos;t seem to unpack properly</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14855">#14855</a></th>
<td>Implementation of liftA2 for Const has high arity</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14790">#14790</a></th>
<td>eqTypeRep does not inline</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14519">#14519</a></th>
<td>Exponential runtime performance regression in GHC 8.2 + Data.Text.Lazy + Text.RE.TDFA</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14336">#14336</a></th>
<td>ghci leaks memory</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14258">#14258</a></th>
<td>n-body runtime regressed badly due to CoreFVs patch</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14240">#14240</a></th>
<td>CSE’ing w/w’ed code regresses program runtime</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14224">#14224</a></th>
<td>zipWith does not inline</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14192">#14192</a></th>
<td>Change to 1TB VIRT allocation makes it impossible to core-dump Haskell programs</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14187">#14187</a></th>
<td>Transpose hangs on infinite by finite lists</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14140">#14140</a></th>
<td>Better treatment for dataToTag</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/14052">#14052</a></th>
<td>Significant GHCi speed regression with :module and `let` in GHC 8.2.1</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13999">#13999</a></th>
<td>Simple function not inlined within declaration marked NOINLINE</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13982">#13982</a></th>
<td>HEAD GHC+Cabal uses too much memory</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13930">#13930</a></th>
<td>Cabal configure regresses in space/time</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13690">#13690</a></th>
<td>Running profiling tests in the GHCi way is extremely slow</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13654">#13654</a></th>
<td>Optimize casMutVar# for single-threaded runtime</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13623">#13623</a></th>
<td>join points produce bad code for stream fusion</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13604">#13604</a></th>
<td>ghci no longer loads dynamic .o files by default if they were built with -O</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13566">#13566</a></th>
<td>Bigger core size in ghc8 compared to ghc7</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13536">#13536</a></th>
<td>Program which terminates instantly in GHC 8.0.2 runs for minutes with 8.2.1</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13422">#13422</a></th>
<td>INLINE CONLIKE sometimes fails to inline</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13376">#13376</a></th>
<td>GHC fails to specialize a pair of polymorphic INLINABLE functions</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13328">#13328</a></th>
<td>Foldable, Functor, and Traversable deriving handle phantom types badly</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13288">#13288</a></th>
<td>Resident set size exceeds +RTS -M limit with large nurseries</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13246">#13246</a></th>
<td>hPutBuf issues unnecessary empty write syscalls for large writes</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13228">#13228</a></th>
<td>Surprising inlining failure</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13218">#13218</a></th>
<td>&lt;$ is bad in derived functor instances</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13040">#13040</a></th>
<td>realToFrac into Complex Double has no specialization</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13025">#13025</a></th>
<td>Type family reduction irregularity (change from 7.10.3 to 8.0.1)</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/13001">#13001</a></th>
<td>EnumFromThenTo is is not a good producer</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12996">#12996</a></th>
<td>Memory leak in recursion when switching from -O1 to -O2</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12990">#12990</a></th>
<td>Partially applied constructors with unpacked fields simplified badly</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12964">#12964</a></th>
<td>Runtime regression to RTS change</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12804">#12804</a></th>
<td>forever contains a space leak</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12781">#12781</a></th>
<td>Significantly higher allocation with INLINE vs NOINLINE</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12603">#12603</a></th>
<td>INLINE and manually inlining produce different code</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12525">#12525</a></th>
<td>Internal identifiers creeping into :show bindings</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12378">#12378</a></th>
<td>Not enough inlining happens with single-method type classes</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12354">#12354</a></th>
<td>Word foldl&apos; isn&apos;t optimized as well as Int foldl&apos;</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12241">#12241</a></th>
<td>Surprising constructor accumulation</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12217">#12217</a></th>
<td>PowerPC NCG: Remove TOC save for calls.</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12129">#12129</a></th>
<td>Optimize the implementation of minusInteger in the integer-gmp package</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/12022">#12022</a></th>
<td>unsafeShiftL and unsafeShiftR are not marked as INLINE</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11989">#11989</a></th>
<td>Performance bug reading large-exponent float without explicit type</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11965">#11965</a></th>
<td>USE_PTHREAD_FOR_ITIMER causes unnecessary wake-ups</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11808">#11808</a></th>
<td>nofib&apos;s cryptarithm1 regresses due to deferred inlining of Int&apos;s Ord operations</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11795">#11795</a></th>
<td>Performance issues with replicateM_</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11725">#11725</a></th>
<td>Performance Regression from 7.8.3 to 7.10.3</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11710">#11710</a></th>
<td>Fusion of a simple listArray call is very fragile</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11707">#11707</a></th>
<td>Don&apos;t desugar large lists with build</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11701">#11701</a></th>
<td>ghc generates significant slower code</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11688">#11688</a></th>
<td>Bytestring break failing rewrite to breakByte and failing to eliminate boxing/unboxing</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11568">#11568</a></th>
<td>Regression in nofib/shootout/k-nucleotide</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11565">#11565</a></th>
<td>Restore code to handle &apos;-fmax-worker-args&apos; flag</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11533">#11533</a></th>
<td>Stack check not optimized out even if it could be</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11486">#11486</a></th>
<td>info tables are no longer aligned</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11383">#11383</a></th>
<td>CAFs lose sharing due to implicit call stacks</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11382">#11382</a></th>
<td>Optimize Data.Char</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11372">#11372</a></th>
<td>Loopification does not trigger for IO even if it could</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11365">#11365</a></th>
<td>Worse performance with -O</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11318">#11318</a></th>
<td>Data.Text.length allocates one closure per character</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11284">#11284</a></th>
<td>Lambda-lifting fails in simple Text example</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11273">#11273</a></th>
<td>PowerPC NCG: Assign all STG float and double regs to PowerPC registers</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11272">#11272</a></th>
<td>Overloaded state-monadic function is not specialised</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11222">#11222</a></th>
<td>Teach strictness analysis about `catch`-like operations</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11116">#11116</a></th>
<td>GC reports memory in use way below the actual</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/11054">#11054</a></th>
<td>GHC on Windows could not use more than 64 logical processors</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10830">#10830</a></th>
<td>maximumBy has a space leak</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10825">#10825</a></th>
<td>Poor performance of optimized code.</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10788">#10788</a></th>
<td>performance regression involving minimum</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10780">#10780</a></th>
<td>Weak reference is still alive if key is alive, but weak reference itself not reachable</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10750">#10750</a></th>
<td>silly assembly for comparing Doubles</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10744">#10744</a></th>
<td>Allow oneShot to work with unboxed types</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10720">#10720</a></th>
<td>New GHC fails to specialize imported function</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10717">#10717</a></th>
<td>fannkuch-redux allocations increase by factor of 10000 between 7.4.2 and 7.6.3</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10678">#10678</a></th>
<td>integer-gmp&apos;s runS seems unnecessarily expensive</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10677">#10677</a></th>
<td>slightly silly assembly for testing whether a Word# is 0##</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10676">#10676</a></th>
<td>silly assembly for comparing the result of comparisons that return Int# against 0#</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10649">#10649</a></th>
<td>Performance issue with unnecessary reboxing</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10457">#10457</a></th>
<td>Revise/remove custom mapM implementation for lists</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10415">#10415</a></th>
<td>ForeignPtr touched in FFI wrapper is never discarded</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10400">#10400</a></th>
<td>Run time increases by 40% in fractal plotter core loop</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10359">#10359</a></th>
<td>Tuple constraint synonym led to asymptotic performance lossage</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10291">#10291</a></th>
<td>compiling huge HashSet hogs memory</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10290">#10290</a></th>
<td>compiling huge HashSet hogs memory</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10260">#10260</a></th>
<td>last uses too much space with optimizations disabled</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10148">#10148</a></th>
<td>Optimization causes repeated computation</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10137">#10137</a></th>
<td>Rewrite switch code generation</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10129">#10129</a></th>
<td>emitCmmLitSwitch could be better</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10108">#10108</a></th>
<td>Dramatic slowdown with -O2 bytestream and list streams combined.</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10067">#10067</a></th>
<td>The Read Integer instance is too slow</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10064">#10064</a></th>
<td>Add support for &quot;foo&quot;## literals to MagicHash</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10060">#10060</a></th>
<td>The Traversable instance for Array looks unlikely to be good</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10034">#10034</a></th>
<td>Regression in mapM_ performance</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/10014">#10014</a></th>
<td>Data.Array.Base.elems needlessly calls bounds.</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9885">#9885</a></th>
<td>ghc-pkg parser eats too much memory</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9848">#9848</a></th>
<td>List.all does not fuse</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9827">#9827</a></th>
<td>void does not use &lt;$</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9801">#9801</a></th>
<td>Make listArray fuse</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9797">#9797</a></th>
<td>Investigate rewriting `&gt;&gt;=` to `*&gt;` or `&gt;&gt;` for appropriate types</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9796">#9796</a></th>
<td>Implement amap/coerce rule for `Array`</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9781">#9781</a></th>
<td>Make list monad operations fuse</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9740">#9740</a></th>
<td>D380 caused fft2 regressions</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9715">#9715</a></th>
<td>The most minimal Gloss project causes the profiler to fail silently.</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9696">#9696</a></th>
<td>readRawBufferPtr and writeRawBufferPtr allocate memory</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9676">#9676</a></th>
<td>Data.List.isSuffixOf can be very inefficient</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9638">#9638</a></th>
<td>Speed up Data.Char.isDigit</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9577">#9577</a></th>
<td>String literals are wasting space</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9546">#9546</a></th>
<td>filterM is not a good consumer for list fusion</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9540">#9540</a></th>
<td>words is not a good producer; unwords is not a good consumer</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9537">#9537</a></th>
<td>concatMap is not a good producer for list fusion</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9520">#9520</a></th>
<td>Running an action twice uses much more memory than running it once</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9510">#9510</a></th>
<td>Prelude.!! is not a good consumer</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9509">#9509</a></th>
<td>No automatic specialization of inlinable imports in 7.8</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9502">#9502</a></th>
<td>mapAccumL does not participate in foldr/build fusion</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9476">#9476</a></th>
<td>Implement late lambda-lifting</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9441">#9441</a></th>
<td>CSE should deal with letrec</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9430">#9430</a></th>
<td>implement more arithmetic operations natively in the LLVM backend</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9398">#9398</a></th>
<td>Data.List.cycle is not a good producer</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9369">#9369</a></th>
<td>Data.List.unfoldr does not fuse and is not inlined.</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9356">#9356</a></th>
<td>scanl does not participate in list fusion</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9355">#9355</a></th>
<td>scanr does not participate in stream fusion</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9345">#9345</a></th>
<td>Data.List.inits is extremely slow</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9344">#9344</a></th>
<td>takeWhile does not participate in list fusion</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9343">#9343</a></th>
<td>foldl&apos; is not a good consumer</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9339">#9339</a></th>
<td>last is not a good consumer</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9332">#9332</a></th>
<td>Memory blowing up for strict sum/strict foldl in ghci</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9326">#9326</a></th>
<td>Minor change to list comprehension structure leads to poor performance</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9291">#9291</a></th>
<td>Don&apos;t reconstruct sum types if the type subtly changes</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9234">#9234</a></th>
<td>Compiled code performance regression</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9214">#9214</a></th>
<td>UNPACK support for sum types</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9203">#9203</a></th>
<td>Perf regression in 7.8.2 relative to 7.6.3, possibly related to HashMap</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9188">#9188</a></th>
<td>quot with a power of two is not optimized to a shift</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9159">#9159</a></th>
<td>cmm case, binary search instead of jump table</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9157">#9157</a></th>
<td>cmm common block not eliminated</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9136">#9136</a></th>
<td>Constant folding in Core could be better</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9132">#9132</a></th>
<td>takeWhile&amp;C. still not fusible</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9105">#9105</a></th>
<td>Profiling binary consumes CPU even when idle on Linux.</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9075">#9075</a></th>
<td>Per-thread weak pointer list (remove global lock on mkWeak#)</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9067">#9067</a></th>
<td>Optimize clearNursery by short-circuiting when we get to currentNursery</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/9021">#9021</a></th>
<td>[CID43168] rts/linker.c has a memory leak in the dlopen/dlerror code</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8901">#8901</a></th>
<td>(very) bad inline heuristics</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8900">#8900</a></th>
<td>Strictness analysis regression</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8835">#8835</a></th>
<td>7.6.3 vs 7.8-RC performance regression</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8832">#8832</a></th>
<td>Constant-folding regression wrt `clearBit (bit 0) 0 `</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8793">#8793</a></th>
<td>Improve GHC.Event.IntTable performance</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8766">#8766</a></th>
<td>length [Integer] is twice as slow but length [Int] is 10 times faster</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8763">#8763</a></th>
<td>forM_ [1..N] does not get fused (allocates 50% more)</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8680">#8680</a></th>
<td>In STM: Variables only in left branch of orElse can invalidate the right branch transaction</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8647">#8647</a></th>
<td>Reduce allocations in `integer-gmp`</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8638">#8638</a></th>
<td>Optimize by demoting &quot;denormalized&quot; Integers (i.e. J# -&gt; S#)</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8609">#8609</a></th>
<td>Clean up block allocator</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8585">#8585</a></th>
<td>Loopification should omit stack check</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8513">#8513</a></th>
<td>Parallel GC increases CPU load while slowing down program</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8508">#8508</a></th>
<td>Inlining Unsaturated Function Applications</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8472">#8472</a></th>
<td>Primitive string literals prevent optimization</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8456">#8456</a></th>
<td>Control flow optimisations duplicate blocks</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8435">#8435</a></th>
<td>Do not copy stack after stack overflow</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8345">#8345</a></th>
<td>A more efficient atomicModifyIORef&apos;</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8321">#8321</a></th>
<td>improve basic block layout on LLVM backend by predicting stack/heap checks</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8255">#8255</a></th>
<td>GC Less Operation</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8224">#8224</a></th>
<td>Excessive system time -- new IO manager problem?</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8124">#8124</a></th>
<td>Possible leaks when using foreign export.</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8082">#8082</a></th>
<td>Ordering of assembly blocks affects performance</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/8027">#8027</a></th>
<td>Adding one call to getNumCapabilities triggers performance nose dive (6X slowdown)</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7954">#7954</a></th>
<td>Strictness analysis regression</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7923">#7923</a></th>
<td>Optimization for takeMVar/putMVar when MVar left empty</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7865">#7865</a></th>
<td>SpecConstr duplicating computations</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7850">#7850</a></th>
<td>Strangely high memory usage on optimized Ackermann function</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7837">#7837</a></th>
<td>Rules involving equality constraints don&apos;t fire</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7785">#7785</a></th>
<td>Module-local function not specialized with ConstraintKinds</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7611">#7611</a></th>
<td>Rewrite rules application prevented by type variable application (map id vs. map (\x -&gt; x))</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7561">#7561</a></th>
<td>Unnecessary Heap Allocations - Slow Performance</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7556">#7556</a></th>
<td>build/fold causes with ByteString unpack causes huge memory leak</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7460">#7460</a></th>
<td>Double literals generated bad core</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7436">#7436</a></th>
<td>Derived Foldable and Traversable instances become extremely inefficient due to eta-expansion</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7429">#7429</a></th>
<td>Unexplained performance boost with +RTS -h</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7418">#7418</a></th>
<td>Writing to stderr is 7x slower than writing to stdout</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7382">#7382</a></th>
<td>Evaluating GHCi expressions is slow following the dynamic-by-default change</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7363">#7363</a></th>
<td>runghc leaks space in IO</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7292">#7292</a></th>
<td>Optimization works for Word but not Word32 or Word64</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7284">#7284</a></th>
<td>plusAddr# x 0 isn&apos;t optimised away</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7257">#7257</a></th>
<td>Regression: pinned memory fragmentation</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7219">#7219</a></th>
<td>Reinstate constant propagation in some form</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7211">#7211</a></th>
<td>Huge space leak on a program that shouldn&apos;t leak</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7116">#7116</a></th>
<td>Missing optimisation: strength reduction of floating-point multiplication</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7091">#7091</a></th>
<td>DPH Matrix product memory usage</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7058">#7058</a></th>
<td>Add strict version of modifySTRef</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/7052">#7052</a></th>
<td>Numeric types’ Read instances use exponential CPU/memory</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/6166">#6166</a></th>
<td>Performance regression in mwc-random since 7.0.x</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/6121">#6121</a></th>
<td>Very poor constant folding</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/6111">#6111</a></th>
<td>Simple loop performance regression of 7.4.1 relative to 7.0.4</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/6110">#6110</a></th>
<td>Data.Vector.Unboxed performance regression of 7.4.1 relative to 7.0.4</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/6082">#6082</a></th>
<td>Program compiled with 7.4.1 runs many times slower than compiled with 7.2.2</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/6056">#6056</a></th>
<td>INLINABLE pragma prevents worker-wrapper to happen.</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/6000">#6000</a></th>
<td>Performance of Fibonnaci compare to Python</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5996">#5996</a></th>
<td>fix for CSE</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5991">#5991</a></th>
<td>regression: huge number of wakeups in xmonad</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5949">#5949</a></th>
<td>Demand analysis attributes manifestly wrong demand type</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5945">#5945</a></th>
<td>Lambda lifting</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5926">#5926</a></th>
<td>Add strict versions of modifyIORef and atomicModifyIORef</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5916">#5916</a></th>
<td>runST isn&apos;t free</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5888">#5888</a></th>
<td>Performance regression in 7.4.1 compared to 6.12.3</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5835">#5835</a></th>
<td>Make better use of known dictionaries</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5809">#5809</a></th>
<td>Arity analysis could be better</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5779">#5779</a></th>
<td>SPECIALISE pragma generates wrong activations</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5776">#5776</a></th>
<td>Rule matching regression</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5774">#5774</a></th>
<td>main = forever (putStrLn =&lt;&lt; getLine)   continuously saturates a CPU when compiled</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5773">#5773</a></th>
<td>main = forever (putStrLn =&lt;&lt; getLine)   continuously saturates a CPU when compiled</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5767">#5767</a></th>
<td>Integer inefficiencies</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5749">#5749</a></th>
<td>GHC 7.0.4 Performance Regression (Possibly Vector)</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5741">#5741</a></th>
<td>openFile should fail if null bytes are in the argument</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5731">#5731</a></th>
<td>Bad code for Double literals</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5715">#5715</a></th>
<td>Inliner fails to inline a function, causing 20x slowdown</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5623">#5623</a></th>
<td>GHC 7.2.1 Performance Regression: Vector</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5615">#5615</a></th>
<td>ghc produces poor code for `div` with constant powers of 2.</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5598">#5598</a></th>
<td>Function quotRem is inefficient</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5569">#5569</a></th>
<td>Ineffective seq/BangPatterns</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5549">#5549</a></th>
<td>~100% performance regression in HEAD compared to ghc6.12, ~22% compared to 7.0.4</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5505">#5505</a></th>
<td>Program runs faster with profiling than without</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5367">#5367</a></th>
<td>Program in (-N1) runs 10 times slower than it with two threads (-N2)</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5339">#5339</a></th>
<td>Data.Bits instances should use default shift instead of shiftL/shiftR</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5327">#5327</a></th>
<td>INLINABLE pragma and newtypes prevents inlining</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5237">#5237</a></th>
<td>Inefficient code generated for x^2</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5205">#5205</a></th>
<td>Control.Monad.forever leaks space</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5161">#5161</a></th>
<td>Poor performance of division; unnecessary branching</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5152">#5152</a></th>
<td>GHC generates poor code for large 64-bit literals</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5113">#5113</a></th>
<td>Huge performance regression of 7.0.2, 7.0.3 and HEAD over 7.0.1 and 6.12 (MonoLocalBinds)</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5034">#5034</a></th>
<td>Performance of Data.Graph.{preorderF, postorderF}</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/5000">#5000</a></th>
<td>Eliminate absent arguments in non-strict positions</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4986">#4986</a></th>
<td>negative Double numbers print out all wrong</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4965">#4965</a></th>
<td>60% performance regression in continuation-heavy code between 6.12 and 7</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4962">#4962</a></th>
<td>Dead code fed to CorePrep because RULEs keep it alive spuriously</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4951">#4951</a></th>
<td>Performance regression 7.0.1 -&gt; 7.0.1.20110201</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4943">#4943</a></th>
<td>Another odd missed SpecConstr opportunity</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4930">#4930</a></th>
<td>Case-of-case not eliminated when it could be</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4908">#4908</a></th>
<td>Easy SpecConstr opportunity that is nonetheless missed</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4495">#4495</a></th>
<td>GHC fails to inline methods of single-method classes</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4474">#4474</a></th>
<td>3 ways to write a function (unexpected performance difference and regression)</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4463">#4463</a></th>
<td>CORE notes break optimisation</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4448">#4448</a></th>
<td>Another case of SpecConstr not specialising</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4442">#4442</a></th>
<td>Add unaligned version of indexWordArray#</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4431">#4431</a></th>
<td>SpecConstr doesn&apos;t specialise</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4428">#4428</a></th>
<td>Local functions lose their unfoldings</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4397">#4397</a></th>
<td>RULES for Class ops don&apos;t fire in HEAD</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4365">#4365</a></th>
<td>Error handle in readProcess not closed</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4344">#4344</a></th>
<td>Better toRational for Float and Double</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4337">#4337</a></th>
<td>Better power for Rational</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4322">#4322</a></th>
<td>High CPU usage during idle time due to GC</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4306">#4306</a></th>
<td>UNPACK can lead to unnecessary copying and wasted stack space</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4285">#4285</a></th>
<td>STM bug on Windows?</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4280">#4280</a></th>
<td>Proposal: Performance improvements for Data.Set</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4279">#4279</a></th>
<td>Proposal: Performance improvements for Data.IntMap</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4278">#4278</a></th>
<td>Proposal: Add strict versions of foldlWithKey and insertLookupWithKey to Data.Map</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4277">#4277</a></th>
<td>Proposal: Significant performance improvements for Data.Map</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4276">#4276</a></th>
<td>-O0 runs in constant space, -O1 and -O2 don&apos;t</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4262">#4262</a></th>
<td>GHC&apos;s runtime never terminates unused worker threads</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4223">#4223</a></th>
<td>LLVM slower then NCG, C example</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4184">#4184</a></th>
<td>Squirrelly inliner behaviour leads to 80x slowdown</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4138">#4138</a></th>
<td>Performance regression in overloading</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4120">#4120</a></th>
<td>Iface type variable out of scope in cast</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4065">#4065</a></th>
<td>Inconsistent loop performance</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4064">#4064</a></th>
<td>SpecConstr broken for NOINLINE loops in 6.13</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4062">#4062</a></th>
<td>Bad choice of loop breaker?</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4021">#4021</a></th>
<td>Problem of Interaction Between the FreeBSD Kernel and the GHC RTS</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4018">#4018</a></th>
<td>Concurrency space leak</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4007">#4007</a></th>
<td>Look again at eta expansion during gentle simplification</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/4004">#4004</a></th>
<td>Improve performance of a few functions in Foreign.Marshal.*</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3990">#3990</a></th>
<td>UNPACK doesn&apos;t unbox data families</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3969">#3969</a></th>
<td>Poor performance of generated code on x86.</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3938">#3938</a></th>
<td>Data growth issue in System.Timeout</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3838">#3838</a></th>
<td>Performance issues with blackholes</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3772">#3772</a></th>
<td>Methods not inlined</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3738">#3738</a></th>
<td>Typechecker floats stuff out of INLINE right hand sides</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3737">#3737</a></th>
<td>inlining happens on foldl1 and does not happen on direct application of combinator</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3736">#3736</a></th>
<td>GHC specialising instead of inlining</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3735">#3735</a></th>
<td>GHC specialising instead of inlining</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3717">#3717</a></th>
<td>Superfluous seq no eliminated</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3709">#3709</a></th>
<td>Data.Either.partitionEithers is not lazy enough</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3698">#3698</a></th>
<td>Bad code generated for zip/filter/filter loop</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3697">#3697</a></th>
<td>Method selectors aren&apos;t floated out of loops</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3655">#3655</a></th>
<td>Performance regression relative to 6.10</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3627">#3627</a></th>
<td>Profiling loses eta-expansion opportunities unnecessarily</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3586">#3586</a></th>
<td>Initialisation of unboxed arrays is too slow</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3526">#3526</a></th>
<td>Inliner behaviour with instances is confusing</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3518">#3518</a></th>
<td>GHC GC rises greatly on -N8 compared to -N7</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3501">#3501</a></th>
<td>Error thunks not being exposed with &quot;B&quot; strictness</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3437">#3437</a></th>
<td>Optimizer creates space leak on simple code</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3349">#3349</a></th>
<td>poor responsiveness of ghci</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3331">#3331</a></th>
<td>control-monad-queue performance regression</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3273">#3273</a></th>
<td>memory leak due to optimisation</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3264">#3264</a></th>
<td>Real World Haskell book example issue</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3245">#3245</a></th>
<td>Quadratic slowdown in Data.Typeable</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3181">#3181</a></th>
<td>Regression in unboxing</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3123">#3123</a></th>
<td>make INLINE work for recursive definitions (generalized loop peeling/loop unrolling)</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3116">#3116</a></th>
<td>missed opportunity for call-pattern specialisation</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3076">#3076</a></th>
<td>Make genericLength tail-recursive so it doesn&apos;t overflow stack</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/3065">#3065</a></th>
<td>Reorder tests in quot to improve code</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2940">#2940</a></th>
<td>Do CSE after CorePrep</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2915">#2915</a></th>
<td>Arity is smaller than need be</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2902">#2902</a></th>
<td>Example where ghc 6.10.1 fails to optimize recursive instance function calls</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2884">#2884</a></th>
<td>Compiled code performance worsens when module names are long enough</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2840">#2840</a></th>
<td>Top level string literals</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2831">#2831</a></th>
<td>Floated error expressions get poor strictness, leaving bad arity</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2823">#2823</a></th>
<td>Another arity expansion bug</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2822">#2822</a></th>
<td>Arity expansion not working right</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2797">#2797</a></th>
<td>ghci stack overflows when ghc does not</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2785">#2785</a></th>
<td>Memory leakage with socket benchmark program</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2727">#2727</a></th>
<td>DiffArray performance unusable for advertized purpose</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2720">#2720</a></th>
<td>eyeball/inline1 still isn&apos;t optimised with -fno-method-sharing</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2712">#2712</a></th>
<td>Parallel GC scheduling problems</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2581">#2581</a></th>
<td>Record selectors not being inlined</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2463">#2463</a></th>
<td>unsafePerformIO in unused record field affects optimisations</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2462">#2462</a></th>
<td>Data.List.sum is slower than 6.8.3</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2450">#2450</a></th>
<td>Data.Complex.magnitude squares using ^(2 :: Int), which is slow</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2440">#2440</a></th>
<td>Bad code with type families</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2396">#2396</a></th>
<td>Default class method not inlined</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2329">#2329</a></th>
<td>Control.Parallel.Strategies: definitions of rnf for most collections are poor</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2325">#2325</a></th>
<td>Compile-time computations</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2280">#2280</a></th>
<td>randomR too slow</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2253">#2253</a></th>
<td>Native code generator could do better</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2236">#2236</a></th>
<td>Deep stacks make execution time go through the roof</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2185">#2185</a></th>
<td>Memory leak with parMap</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2163">#2163</a></th>
<td>GHC makes thunks for Integers we are strict in</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2105">#2105</a></th>
<td>garbage collection confusing in ghci for foreign objects</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2092">#2092</a></th>
<td>Quadratic amount of code generated</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/2078">#2078</a></th>
<td>INLINE and strictness</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/1890">#1890</a></th>
<td>Regression in mandelbrot benchmark due to inlining</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/1889">#1889</a></th>
<td>Regression in concurrency performance from ghc 6.6 to 6.8</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/1818">#1818</a></th>
<td>Code size increase vs. 6.6.1</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/1752">#1752</a></th>
<td>CSE can create space leaks by increasing sharing</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/1607">#1607</a></th>
<td>seq can make code slower</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/1434">#1434</a></th>
<td>Missing RULEs for truncate</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/1117">#1117</a></th>
<td>[2,4..10] is not a good list producer</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/955">#955</a></th>
<td>more object-code blow-up in ghc-6.8.3 vs. ghc-6.4.2 (both with optimization)</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/876">#876</a></th>
<td>Length is not a good consumer</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/783">#783</a></th>
<td>SRTs bigger than they should be?</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/650">#650</a></th>
<td>Improve interaction between mutable arrays and GC</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/635">#635</a></th>
<td>Replace use of select() in the I/O manager with epoll/kqueue/etc.</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/594">#594</a></th>
<td>Support use of SSE2 in the x86 native code genreator</td></tr>
<tr><th><a href="https://gitlab.haskell.org/ghc/ghc/issues/427">#427</a></th>
<td>Random.StdGen slowness</td></tr></table>






## Nofib results


### Austin, 5 May 2015


Full results [are here](https://gist.githubusercontent.com/thoughtpolice/498d51153240cc4d899c/raw/9a43f6bbfd642cf4e7b15188f9c0b053d311f7b9/gistfile1.txt) (updated **May 5th, 2015**)

**NB**: The baseline here is 7.6.3

### Ben, 31 July 2015

[http://home.smart-cactus.org/\~ben/nofib.html](http://home.smart-cactus.org/~ben/nofib.html)


Baseline is 7.4.2.

### Nofib outliers

#### Binary sizes

##### 7.6 to 7.8

- Solid average binary size increase of **5.3%**.

#### Allocations

##### 7.4 to 7.6

- **fannkuch-redux**: increased by factor of 10,000?!?!

  - 7.6.3: `<<ghc: 870987952 bytes, 1668 GCs (1666 + 2), 0/0 avg/max bytes residency (0 samples), 84640 bytes GC work, 1M in use, 0.00 INIT (0.00 elapsed), 2.43 MUT (2.43 elapsed), 0.00 GC (0.00 elapsed), 0.00 GC(0) (0.00 elapsed), 0.00 GC(1) (0.00 elapsed), 1 balance :ghc>>`
  - 7.4.2: `<<ghc: 74944 bytes, 1 GCs (0 + 1), 0/0 avg/max bytes residency (0 samples), 3512 bytes GC work, 1M in use, 0.00 INIT (0.00 elapsed), 2.25 MUT (2.25 elapsed), 0.00 GC (0.00 elapsed), 0.00 GC(0) (0.00 elapsed), 0.00 GC(1) (0.00 elapsed), 1 balance :ghc>>`
  - According to \[[FoldrBuildNotes](foldr-build-notes)\] this test is very sensitive to fusion
  - Filed #10717 to track this.

##### 7.6 to 7.8

- **spectral-norm**: increases by **17.0%**.

  - A **lot** more calls to `map`, over 100 more! Maybe inliner failure?
  - Over **twice** as many calls to `ghc-prim:GHC.Classes.$fEqChar_$c=={v r90O}` (& similar functions). Also over twice as many calls to `elem`, 
  - Similarly, many more calls to other specializations, like `base:Text.ParserCombinators.ReadP.$fMonadPlusP_$cmplus{v r1sr}`, which adds even more allocations (from 301 to 3928 for this one entry!)
  - Basically the same story up to `HEAD`!

##### 7.8 to 7.10

- **gcd**: increases by **20.7%**.

  - Ticky tells us that this seems to be a combination of a few things; most everything seems fairly similar, but we see a large amount of allocations attributable to 7.10 that I can't figure out where they came from, aside from the new `integer-gmp`: `integer-gmp-1.0.0.0:GHC.Integer.Type.$WS#{v rwl}` accounts for 106696208 extra bytes of allocation! It also seems like there are actual extant calls to `GHC.Base.map` in 7.10, and none in 7.8. These are the main differences.
- **pidigits**: increases by **7.4%**.

  - Ticky tells us that this seems to be, in large part, due to `integer-gmp` (which is mostly what it benchmarks anyway). I think part of this is actually an error, because before integer-gmp, a lot of things were done in C-- code or whatnot, while the new `integer-gmp` does everything in Haskell, so a lot more Haskell code shows up in the profile. So the results aren't 1-to-1. One thing that seems to be happening is that there are a lot more specializations going on that are called repeatedly, it seems; many occurrences of things like `sat_sad2{v} (integer-gmp-1.0.0.0:GHC.Integer.Type) in rfK` which don't exist in the 7.8 profiles, each with a lot of entries and allocations.
- **primetest**: went down **27.5%** in 7.6-to-7.8, but **8.8%** slower than 7.6 now - in total it got something like **36.6%** worse.

  - Much like **pidigits**, a lot more `integer-gmp` stuff shows up in these profiles. While it's still just like the last one, there are some other regressions; for example, `GHC.Integer.Type.remInteger` seems to have 245901/260800 calls/bytes allocated, vs 121001/200000 for 7.8

TODO Lots of fusion changes have happened in the last few months too - but these should all be pretty diagnosable with some reverts, since they're usually very localized. Maybe worth looking through `base` changes.

#### Runtime

##### 7.6 to 7.8

- `lcss`: increases by **12.6%**.

  - Ticky says it seems to be `map` calls yet again! These jump hugely here from 21014 to 81002.
  - Also, another inner loop with `algb` it looks like gets called a huge number of times too - `algb2` is called **2001056 times vs 7984760 times**!

    - Same with `algb` and `algb1`, which seem to be called more often too.
  - Some other similar things; a few regressions in the \# of calls to things like `Text.ParserCombinator.ReadP` specializations, I think.
  - Same story with HEAD!

##### 7.8 to 7.10

- `lcss`: decreased by \~5% in 7.10, but still **7%** slower than 7.6.

  - See above for real regressions.
- `multiplier`: increases by **7.6%**.

  - `map` strikes again? 2601324 vs 3597333 calls, with an accompanying allocation delta.
  - But some other inner loops here work and go away correctly (mainly `go`), unlike e.g. `lcss`.


 


#### Comparing integer-gmp 0.5 and 1.0


One of the major factors that has changed recently is `integer-gmp`. Namely, GHC 7.10 includes `integer-gmp-1.0`, a major rework of `integer-gmp-0.5`. I've compiled GHC 7.10.1 with `integer-gmp` 0.5 and 1.0. [Here](http://home.smart-cactus.org/~ben/nofib.html) is a nofib comparison. There are a few interesting points here,

- Binary sizes dropped dramatically and consistently (typically around 60 to 70%) from 0.5 to 1.0.
- Runtime is almost always within error. A few exceptions,

  - `binary-trees`: 6% slower with 1.0
  - `pidigits`: 5% slower
  - `integer`: 4% slower
  - `cryptarithm1`: 2.5% slower
  - `circsim`: 3% faster
  - `lcss`: 5% faster
  - `power`: 17% faster
- Allocations are typically similar. The only test that improves significantly
  is `prime` whose allocations decreased by 24% Many more tests regress
  considerably,

  - `bernoulli`: +15%
  - `gcd`: +21%
  - `kahan`: +40%
  - `mandel` +34%
  - `primetest`: +50%
  - `rsa`: +53%


The allocation issue is actually discussed in the commit message ([c774b28f76ee4c220f7c1c9fd81585e0e3af0e8a](/trac/ghc/changeset/c774b28f76ee4c220f7c1c9fd81585e0e3af0e8a/ghc)),


>
>
> Due to the different (over)allocation scheme and potentially different
> accounting (via the new `{shrink,resize}MutableByteArray#` primitives),
> some of the nofib benchmarks actually results in increased allocation
> numbers (but not necessarily an increase in runtime!).  I believe the
> allocation numbers could improve if `{resize,shrink}MutableByteArray#`
> could be optimised to reallocate in-place more efficiently.
>
>


The message then goes on to list exactly the nofib tests mentioned above. Given that there isn't a strong negative trend in runtime corresponding with these increased allocations, I'm leaning towards ignoring these for now.
