
This pages serves as a public log what I did for my GHC internship from 21 Jan 2013 to 12 April 2013.

## Helpful wiki pages

- [Commentary/Compiler/Demand](commentary/compiler/demand)

  - esp [Commentary/Compiler/Demand/RelevantParts](commentary/compiler/demand/relevant-parts)
- [Commentary/Compiler/OptOrdering](commentary/compiler/opt-ordering)
- [Building/RunningTests/Updating](building/running-tests/updating)
- [Commentary/Rts/HaskellExecution](commentary/rts/haskell-execution) nice overview
- [Commentary/Compiler/GeneratedCode](commentary/compiler/generated-code)
- [ReadingList](reading-list) - I heartily recommend the Optimisation section

  - esp "Let-floating", "Transformation-based", and "secrets" for low-level stuff (also "fast curry" from the Code generation and virtual machine section)
  - esp foldr/build, RULES, and SpecConstr for the higher-level fusion stuff

## Plan for my internship summary


Compared to [351a8c6bbd53ce07d687b5a96afff77c4c9910cc](/trac/ghc/changeset/351a8c6bbd53ce07d687b5a96afff77c4c9910cc/ghc), we implemented OPTIMIZATIONS with a cumulative effect of EFFECT on the generated code as well as EFFECT on the compiler's code. The hope is for the optimizations to have beneficial non-trivial interactions and to simplify/tidy the GHC code base.

### Core Diving

- use nofib's EXTRA_HC_OPTs="..." command line parameter to apply the -ddump-\* flags

- emacs scroll-all-mode

  - use C-o to fix up horizontal alignment without skewing the cursors

- navigate -ddump-\*'s output via the **\* headers
  **

- lots of information: -dverbose-core2core + -ddump-inlinings

  - simplCore/Simplify has many pprTraces that are commented out

- ghc --show-iface M.hi will dump its definitions

  - the unfolding of a wrapper is not shown, because it is actually not in the `.hi` file; it is reconstructed by `TcIface.tcIfaceWrapper` based on the type of it and the worker and the worker's demand information

- diff -w can highlight major changes

  - with -dppr-cols=999999999 to changes in names or number of arguments from moving lots of things around
  - -dsuppress-uniques or sed -r 's/_:alnum:?{2,4}*g' (or leave out the underscore)
    *

    - removes *most* uniques
    - this sed is handy also for diffing .ticky files
- other diffs

  - diff -y --width=180
  - diff -y --suppress-common-lines

- a strictly demanded let and a thunk with an unlifted type both become cases in the STG.

### Ticky Counters

TODO

- UNKNOWN_CALL_ctr            - put arguments on stack and call the RTS's stg_ap_\<PAT\>_fast routine for that argument pattern
- KNOWN_CALL_ctr              - "fast call": put arguments in registers and call the function's the fast entry point
- KNOWN_CALL_TOO_FEW_ARGS_ctr - "PAP": creates a Partial APplication closure
- KNOWN_CALL_EXTRA_ARGS_ctr   - like a fast call, but first push a continuation onto the stack that effectively uses stg_ap_\<PAT\>_fast for the extra args
- ALLOC_HEAP_tot is in words, and so are the allocation numbers for each id

### Core -\> STG -\> CMM

TODO and _what you can learn by looking at each one_

TODO "estimations of various run time consequences"

### The NoFib Experimental Method

- start with -O1, both for the libraries and for the individual test programs

  - proceed to -O2 once you've identified the primary interesting scenarios
  - (also saves some compile time)
- always compile with -ticky (unless you're concerned about delicate optimizations at CMMand below)
- to manage compilation of the libraries, use build.mk's GhcLibOpts
- to manage compilation of the nofib tests, use

  - nofib's EXTRA_HC_OPTs="..." command line parameter
  - or build.mk's NofibHcOpts
  - I recommend EXTRA_HC_OPTS because it's more flexible and more explicit
- there are four basic combinations

  - (1) libs w/ baseline    , (nofib) tests w/ baseline
  - (2) libs w/ baseline    , (nofib) tests w/ your change
  - (3) libs w/ your change , (nofib) tests w/ baseline
  - (4) libs w/ your change , (nofib) tests w/ your change
- ultimately the comparison that matters is (1) versus (4)
- though, (1) versus (2) and (1) versus (3) isolate the changes

  - for allocation changes, ticky usually makes isolation a moot point
  - for runtime, though, there's so many factors and so much noise that isolating the changes can benefit your sanity
- get a reproducible measurement before you start inspecting code

  - alloc is \*almost\* always consistent
  - runtime is rarely :/ so I iteratively crank up the iterations

    - use nofib's NoFibRuns command line parameter
    - alternatively, use nofib's mode=slow, mode=norm, mode=fast command line flags
- inspect the ticky per-closure allocation, per-closure entry, and the general counters to hopefully isolate the change

  - run the test (just once) with `+RTS -rFILENAME -RTS`

    - use nofib's EXTRA_RUNTEST_OPTS="+RTS -rFILENAME -RTS"
  - for allocation, the only subtlety is that allocation "inside" an LNE is assigned to the (most recent non-LNE) caller

    - (also, it seems that we're not tracking allocation by the array cloning primops, but I don't know how prevalent that is)
  - for runtime, entry counts and the general counters tracking the variety of closure entries will hopefully help

    - cf [\#TickyCounters](frisby2013-q1#ticky-counters)
- inspect the compilation outputs for differences

  - cf [\#CoreDiving](frisby2013-q1#core-diving) [\#Core-STG-CMM](frisby2013-q1#)
- allocation changes probably won't require more work (unless its delicate GC stuff, I suppose)
- for runtime, isolate the changes

  - to evaluate a change, write a simpler test that hammers just that code in order to estimate its consequences
  - see [\#Core-STG-CMM](frisby2013-q1#) for estimations of various run time consequences
  - if the change seems to be in a library, slice out the relevant code into its own module so you can mutate it to experiment


Running the full nofib suite with one set of flags and then again with another is fine for allocation, but pretty bad for runtime. The two measurements for a particular test are separated by a large amount of time, so the load is bad. My workaround has been a hacky shell script that transposes the two for-loops: outer for tests, inner for compilation method.


Moreover, I usually include the baseline variant twice. For example, I'll compare "baseline" "idea 1" "baseline again" "idea 2"; this has two benefits.

- if the two baseline runtimes are significantly difference, then there's too much noise
- (I'm unsure about this) it leaves the machine in a comparable state before executing the two ideas

## Late Lambda Float


LLF = Late Lambda Float


As the GHC optimization papers explain, it is an early design decision to \*not\* perform lambda lifting. My initial project was to investigate the effects of aggressively floating lambdas to the top-level at the end of the core2core pipeline.

- The main reason to not perform lambda lifting is that abstracting over free variables loses information and thereby inhibits \*downstream\* optimization.
- Doing it \*late\* (ie just before CorePrep) circumvents this issue.
- The original conjecture was that doing it would save allocation: a dynamically allocated closure becomes a static top-level function.
- Max Bolingbroke did a quick implementation of this idea some years ago (\~mid 2000s), but it seems it was abandoned. I don't know why.

### Notes for Write-up

- puzzle"s $fEnumItemType.$cenumFromThen has a nice demonstration: a cascade of let-no-escapes becomes a series of top-level functions, all tail-calls

### Method


We decided to implement LLF by re-using most of the [FloatOut](float-out) machinery.

[FloatOut](float-out) is structured in three phases.

1. Annotate all expressions with their free variables.
1. Consume those annotations while annotating each binder with the target "level" (essentially a depth wrt value lambdas) to which we want to float it.
1. Consume those annotations while actually relocating the bindings.


We wholesale re-use the third phase (compiler/simplCore/FloatOut) with no changes, add logic to the middle phase, and enrich the first phase with more analyses.


Most of my changes were

- Adding flags (compiler/main/DynFlags compiler/simplCore/CoreMonad compiler/simplCore/SimplCore)
- Implementing the LLF logic in the first two [FloatOut](float-out) phases (compiler/simplCore/SetLevels)
- Adding LLF to the core2core pipeline (compiler/simplCore/SimplCore)

### Overview


In order to minimize factors, I decided to float only lambdas during LLF. Thus there is no need to perform FloatIn afterwards: all of our floats are to the top-level, so there will be nothing to FloatIn.


We placed LLF as the last pass before CorePrep. After experimentation, we decided to follow it with a simplifier pass.


The basic shape of things:

```wiki
outer = CTX[let f x = RHS[x]
            in BODY[f]]
```


where `outer` is a top-level binding. LLF transforms this to:

```wiki
poly_f FVS x = RHS[x]

outer = CTX[BODY[f FVS]]
```


wbere FVS are the free variables of RHS\[x\]. We'll use `a`, `b`, `c`, ... for particular variables in FVS.


The poly prefix is vestigial: in the past, floated bindings could never cross lambdas, so the abstracted variables were only type variables. Hence the machinery that adds the new parameters was only ever adding type parameters; it was creating polymorphic functions. The naming scheme was not updated when the machinery was enriched to also abstract over values.

### Background

- join points
- let-no-escape (LNE)
- Note \[join point abstraction\]

### Discovered Detriments of LLF


These are the various negative consequences that we discovered on the way. We discuss mitigation below.

- Unapplied occurrences of f in BODY results in the creation of PAPs, which increases allocation. For example: `map f xs` becomes `map (poly_f a b c) xs`. Max had identified this issue earlier.
- Abstracting over a known function might change a fast entry call in RHS to a slow entry call. For example, if CTX binds `a` to a lambda, that information is lost in the right-hand side of poly_f. This can increase runtime.
- Replacing a floated binder's occurrence (ie `f` becomes `poly_f a b c`) can add free variables to a thunk's closure, which increases allocation.
- Abstracting over a let-no-escape binder renders it a normal let, which increases allocation.

#### Mitigating PAP Creation


This is the simplest to mitigate: we do not float `f` if it ever occurs unapplied.

#### Mitigating Thunk Growth


In nucleic2, we floated a binding with 11 free variables. But this binder occurred in about 60 thunks, so many closures grew by \~11 pointers, giving a +2.2% allocation change (as opposed to -0.9%).


We've considered three heuristics for avoiding this. In ascending complexity:

1. (easy) Limit the number of free variables the binding is allowed.
1. in-thunk: If `f` occurs inside of a thunk in `BODY`, then limit its free variables.
1. thunk-growth: Approximate the maximum number of free variables that floating `f `would add to a thunk in `BODY`, and limit that.


We did not implement the first one, since in-thunk is not very complicated. thunk-growth is significantly more complicated.

- The question of whether `f` occurs in a thunk is not simple.

  - We count non-trivial arguments as thunks; but not all non-trivial arguments end up as thunks.
  - We do not count lambda-forms as thunks, since the lambda will hopefully be floated.
- Estimating the effect of floating `f` on such a thunk's (call it `t`) closure size is more complicated.

  - Another floated function (say `g`) may also add some of `f`'s free variables to `t`; we shouldn't penal both `f` and `g` for that.
  - If `f` itself has a free variable, say `h`, which is a binder that gets floated, then floating `f` will also add `h`'s free variables to `t`.


Therefore, these are rough approximations. Being more accurate would require changing the setLevels pass instead of just the simpler first pass (the one that only analyzes the original term).


We tried limits of 32, 16, 8, and 4 to differentiate between the last two. At a limit of 8, the allocation increase in ida and puzzle were 1.3 and 2.9 percentage points better with thunk-growth than with in-thunk. But there were no differences around 10 --- which is the lowest we can go while improving nucleic2, anyway --- so we're adopting in-thunk for now.


There might have been some potential benefits to run-time from thunk-growth versus in-thunk (with limit=8, 35 percentage points better on constraint, eg), but we're not confident in those measurements.

##### Revisited


The first one was actually already implemented, so we compared in-thunk-limit=10 with late-nonrec-lam-limit=10.


We avoid PAP creation, -flate-float-simpl, and do not abstract known calls.

TODO try it with -fprotect-last-arg

```wiki
with baseline libraries

Allocations
-------------------------------------------------------------------------------
        Program          ll-baselinell-protect-ignor     ll-lam10pin      ll-it10pin
-------------------------------------------------------------------------------
         puzzle            165864160           +0.0%           +0.0%          -15.1%
```


The difference: a join point inside a letrec in $wtransfer (itself also recursive) gets floated out and then inlined back in. This ultimately eliminates several lets. A considerable amount of code duplication, but with a big pay off. The (LNE) join point has 15 free variables, but does not occur in a thunk. The free variables are probably getting parameter scrutinization discounts once floated.


Binary sizes increase +5.5% with the first, and only +2.5 with the second. It's so consistent that it's probably in the base library.


Allocation was within 0.2%, except for cacheprof (worse by 1%), but cacheprof is always wiggly.

###### With baseline libraries


Reproducible elapsed time changes

```wiki
                   baseline       baseline2        it10             lam10
           anna     0.44           -0.2%           +0.2%           -3.8%
         genfft     0.21           +0.0%           +3.9%           +5.5%
          event     1.02           +0.1%           -0.8%           +0.0%
            scs     3.57           -0.2%           +1.1%           -0.4%
```

- event - a joinpoint Int\# -\> \[...\] gets floated, gains LSSELiS parameters, called 103999 times. Also, one of the abstracted variables keeps a case binder alive.

- scs - LinearAlgebra - several changes. uses of accumArray and listArray get inlined, and then joinpoints get floated out; adding 10 abstracted variables. Other similar functions (like unit) don't do this, so I'm not totally sure what triggers it. 

- scs - Simulate - changes here too TODO

TODO inspect the event CMM to see the sort of difference that this makes; find that 1%!

TODO Why is the it10 float good in event but bad in scs? Maybe it's mostly the Simulate changes?

TODO switching on protect-ignore seems to even out scs, but it does not affect event... ACK it's that darn "inlining putStr some how slows things down" again. Why's it being inlined if the libraries are baseline?

###### With matching libraries


Elapsed time changes

```wiki
driver.sh x 15
                              baseline       in-thunk=10   late-nonrec-lam=10
           atom                 3.66           -0.8%           -1.2%
  comp_lab_zift                 1.34           +2.3%           -0.1%
         hidden                 1.97           +2.8%           +1.1%
            hpg                 0.50           +4.0%           -9.2%
            ida                 0.59           +6.2%           +2.8%
      integrate                 0.66           -0.2%           -1.9%
           para                 1.64           +0.2%           -3.0%
          solid                 0.91           -0.1%           -1.1%
      transform                 1.86           -1.9%           -2.7%
      typecheck                 0.70           +1.4%           -2.9%
   wheel-sieve2                 1.37           +0.6%           -1.0%

          boyer                 0.23           -0.6%           +2.0%
   cryptarithm1                 2.35           -0.2%           +1.8%
          power                 4.26           -1.4%           -0.3%
         simple                 1.33           -2.7%           -1.7%
       treejoin                 1.72           +1.0%           +2.4%

reproduced with local-driver x15

                              baseline      baseline again   in-thunk=10   late-nonrec-lam=10
           atom                 3.67           -0.2%           -0.9%           -1.6%
  comp_lab_zift                 1.34           -0.4%           +1.2%           -0.5%
         hidden                 1.94           -0.1%           +2.9%           +1.0%
            hpg                 0.46           +1.9%           +5.5%           -0.4%
            ida                 0.59           +0.1%           +5.2%           +3.2%
      integrate                 0.67           -0.5%           -0.4%           -3.4%
           para                 1.64           -0.0%           -0.4%           -3.0%
          solid                 0.91           -0.1%           +0.0%           -1.4%
      transform                 1.86           -0.1%           -2.1%           -2.7%
      typecheck                 0.69           +0.2%           +1.5%           -1.7%
   wheel-sieve2                 1.37           +0.3%           -0.8%           -1.7%

          boyer                 0.23           -0.3%           -0.3%           +3.2%
   cryptarithm1                 2.36           +0.0%           -0.1%           +1.5%
          power                 4.26           +0.0%           -1.5%           -0.7%
         simple                 1.31           +0.2%           -2.3%           -1.2%
       treejoin                 1.61           -0.1%           +1.2%           +2.7%
```

TODO investigate

#### Preserving Fast Entries


The first idea here was simply: do not float a binding if its RHS applies a free variable.


But since the idea was to avoid losing fast entries, this only applies to saturated and oversaturated calls. As a sanity check, however, I added two flags.

- `-f(no-)late-float-abstract-undersat-var` don't allow undersaturated applications
- `-f(no-)late-float-abstract-sat-var`      don't allow saturated or oversaturated applications


Ever since, I've been doing parameter sweeps over these as we make other refinements to the system.

- nn - do not float a binding that applies one of its free variables.
- yn - do not float a binding that applies one of its free variables saturated or oversaturated.
- ny - do not float a binding that applies one of its free variables undersaturated.
- yy - do not restrict application of the binding's free variables


There was no variant that bested the others on most programs' runtime. And the data was so noisy that it was difficult to choose which tests to investigate. I eventually developed some bash script (I'm so sorry) to transpose the NoFib loops; instead of running the entire NoFib suite for one set of switches and then running it again for the next set of switches, and so on, I build all the variants, and then run each variant's version of each program sequentially. I intend for this to reduce noise by improving the time locality of the measurements of the same test. Even so, the noise in Runtime was bad. Eventually, I turned the iterations up to the 40/50 range and found some steadiness. To my surprise, there were a couple tests that had the best Runtime if we \*do\* abstract over functions with fast calls! This happens. More on this later (cf [\#puzzle-time-issue](frisby2013-q1#)).


I also saw some surprises in allocation. Roughly, we expect that more floating means (barely) less allocation but worse runtime (by how much?) because some known calls become unknown calls. But, eg, going from nn -\> yn --- ie floating functions that undersaturate free variables instead of not floating them --- caused worse allocation! This investigation led to [\#MitigatingLNEAbstraction](frisby2013-q1#mitigating-lne-abstraction).


Based on that example, it occurred to me that we should only restrict the binding's saturation of its \*known\* free variables... duh. For example, we should floating a binding even if its RHS exactly applies a free variable when that free variable is lambda bound. Not floating in that case has no benefit, and indeed was causing knock-on effects that increase allocation (eg [\#MitigatingLNEAbstraction](frisby2013-q1#mitigating-lne-abstraction)).


After allocation leads to some code tweaks, I reran the Run time tests with high iterations. hpg and puzzle were odd cases where ny did the best, by far. Both have low run times, so I cranked the iterations up to 200. hpg's results change drastically, which I haven't yet resolved. But puzzle remained; see [\#puzzle-time-issue](frisby2013-q1#).


I have yet to determine that the preservation of fast entries is worth the trouble --- I certainly hope so... the parameter sweeps have taken a lot of time!


To enable further measurements, I have identified the semantics of some ticky counters, cf [\#TickyCounters](frisby2013-q1#ticky-counters), and started resurrecting useful ones that are no longer enabled.

#### Mitigating LNE Abstraction


We had actually already seen this for a non-lambda join point in knights, but we were preoccupied with the unintentional existence of non-lambda join points and moved on after fixing those. I re-discovered this while experimenting with the fast preservation variants above.


NB I think this will be mitigated "for free", since I'm predicting that we will never abstract variables that occur exactly saturated and an LNE binder can only be exactly saturated. If we do end up abstracting over saturated functions, we may want to consider mitigating this separately.


Using -flate-float-in-thunk-limit=10, -fprotect-last-arg, and -O1, I tested the libraries+NoFib for the four variants from [\#PreservingFastEntries](frisby2013-q1#preserving-fast-entries). In fish (1.6%), hpg (\~4.5%), and sphere (10.4%), allocation gets worse for ny and yy compared to nn and yn. The nn and ny do not change the allocation compared to the baseline library (ie no LLF).


The nn -\> ny comparison is counter to our rough idea: floating more bindings (those that saturate/oversaturate some free variables) worsens allocation. Thus, I investigate.


The sphere program hammers `hPutStr`. Its extra allocation is mostly due to a regression in `GHC.IO.Encoding.UTF8`. Here's the situation.


With the nn variant:

```wiki
outer a b c ... =
  let-no-escape f x = CTX[let-no-escape $j y = ... (f ...) ... in CTX2[$j]]
  in ...
```


In this case, `$j` is not floated because it applies `f`. With the ny variant, `$j` gets floated.

```wiki
poly_$j a b c ... f y = ...

outer a b c ... =
  let f x = CTX[CTX2[poly_$j a b c ... f]]
  in ...
```


Thus `f` cannot be let-no-escape because it now occurs as an argument to `poly_$j`.


This contributes to sphere's 1 megabyte of extra allocation for two reasons:

- `outer` is entered about 60,000 times.
- The RHS of `f` has 13 free variables, so it's closure is rather large.


13\*60,000 \~ 750,000. I suspect the rest of sphere's increase is due to a similar issue in `GHC.IO.Handle`.


In hpg, it's principally due to `GHC.IO.Encoding.UTF8` again, with a second place contributor of `GHC.IO.FD`, where the function `$wa17` is again like the `outer` example above, but with fewer free variables and thus less effect.

### Discovered Benefits of LLF


We haven't seen as much decrease in allocation as we would have liked, but there have been some nice benefits:

#### Creates Inliner Opportunities


Floating functions to the top-level creates more opportunities for the inliner. We've found two ways.

- #7663 - simulates having the inliner discount for free variables like it discounts for parameters

- It also decreases size of functions by floating out internal let-bindings (eg big join points, etc).


Both of these have been observed on puzzle, with commit feec91b71, it-thunk-limit=10, protect-last-arg. We get a big improvement in both allocation (-15.1%) and runtime (-1.4%) by allowing fast entries to be abstracted over. Oddly, if we additionally disallow undersat known calls to be abstract over, we get another runtime boost (up to -3.9%). These are both unfortunate from the fast-entry perspective, but demonstrate a big win.


In particular, the worker for the derived equality function for the `StateType` contains a join-point. When the join-point is floated, the worker's Guidance goes from

`IF_ARGS [70 70 80 0 60 60 60 0] 360 20`


to

`IF_ARGS [130 0 0 0 60 0 0 0] 220 20`


while the floated join point itself gets a Guidance of

`IF_ARGS [170 160 0 60 120 0 0] 300 60`}


The loss of parameter discounts may be bad, but the reduction in size exemplifies a good thing.


But there's a bigger change in puzzle's main loop: `$wtransfer` gets a 28% reduction in allocation. Furthermore, one of its contained letrecs gets a 56% percent reduction. This results in a %15 percent reduction for the whole program.b

TODO I need ticky to track LNEs in order to pin down what's happening there.

#### Creates Simplifier Opportunities


Floating functions to the top-level creates more opportunities for the simplifier.


Abstracted from boyer2 (where `f` is a join point):

```wiki
CTX[case a of [p1 -> let f x = ... in case a of ...]]
```


The let prevents the cases from being merged. Since LLF is so aggressive, it floats f when it otherwise wouldn't be, enabling the case merge.

#### Lifting Recursive Lambdas


I arbitrarily went with flate-rec-lam-limit=10.


We avoid creating PAPs and there is a -flate-float-simpl.

TODO try it with no free-variable restriction on recursive lams

TODO try it with -flate-protect-last-arg

TODO try it with -flate-abstract-sat-var?


Binary size increases another 1% to 1.5%.


Allocation swings \> 0.5%

```wiki
                           baseline          it10+norec     it10+rec10
           anna             70678152           -0.0%           -1.3%
           ansi               128632           -0.1%           -1.1%
         awards               292416           -0.1%           -0.5%
       calendar               838008           -0.0%           -0.9%
        circsim           1326471120           -0.0%           -0.9%
       clausify             34268616           -0.0%           -1.8%
   cryptarithm1           2051852480           -0.0%           -2.4%
         expert               373048           -0.0%           -1.0%
          fluid              9308464           -0.0%           -6.3%
         gamteb             59846096           -0.8%           -0.9%
         hidden           1165309432           -0.0%           -1.2%
          infer             29637224           -0.0%           -0.6%
          kahan            405812520           -0.0%           +1.5%
        knights              2258392           +0.3%           -0.3%
        mandel2              8970176           -0.0%           -0.8%
        mkhprog              3371224           -0.0%           -1.2%
           para            500173128           -0.4%           -1.7%
        reptile             16634048           -0.0%           -1.0%
           rfib               115688           -0.0%           -1.2%
            tak               110136           -0.1%           -3.5%
      transform            738806200           -2.2%           -2.8%
      typecheck            317783904           -0.0%           -2.5%
           x2n1              2463160           -0.0%           -3.3%

      cacheprof            476193896           +0.1%           +2.1%
    constraints           2142833608           -0.0%           +0.9%
   cryptarithm2             26560976           -0.0%          +22.4%
            cse              1257128           -0.0%           +0.5%
            fem             53059480           -0.0%           +1.0%
       fibheaps             47494928           -0.5%           +7.5%
           fish             19083736           -0.0%           +1.6%
       nucleic2             98472096           -0.9%           +0.5%
            rsa             17910680           -0.0%           +0.6%
         simple            226413112           -0.3%          +23.5%
         sphere             95134112           -0.0%           +4.3%
```


Run time changes

```wiki
x15 driver and reproduced with x15 local-driver

        Program         log-baseline        log-it10  log-it10-rec10
           anna                 0.44           -3.0%           -6.8%  good
     bernouilli                 0.92           +0.1%           -1.7%
    constraints                21.47           -0.0%           -1.5%
  comp_lab_zift                 1.34           +1.7%           +0.3%
       compress                 0.91           +1.9%           -0.5%
      compress2                 1.97           -3.2%           -9.9%  excellent
   cryptarithm1                 2.35           +0.3%           -3.9%  great
            fft                 0.23           +0.0%           -2.9%  good
         hidden                 1.95           +4.4%           -1.4%  -- reproduced-ish: it wiggled
            hpg                 0.47           +3.5%           +0.4%
            ida                 0.59           +5.7%           +3.1%
          infer                 0.36           -1.7%           -6.2%  good
        integer                 5.69           +0.8%           +0.0%
          kahan                 0.85           -0.6%           -1.7%
           lcss                 3.49           -0.7%           -1.4%
           life                 1.85           +2.8%           -2.0%
       listcopy                 0.44           +1.5%           -0.5%
     multiplier                 0.71           -0.3%           -1.6%
           para                 1.63           +0.7%           -2.4%  good
      paraffins                 0.89           -0.9%           -3.3%  good
      transform                 1.86           -2.4%           -4.5%
       treejoin                 1.72           +1.1%           +0.2%
      typecheck                 0.69           +1.0%           -0.2%
           wang                 0.89           -1.3%           -3.2%

          event                 1.02           -2.7%           -1.4%
      listcompr                 0.40           -0.2%           +2.7%
          power                 4.27           -1.6%           -0.2%
         primes                 0.45           +0.0%           +2.8%
            scs                 3.55           +2.6%           +3.2%
         simple                 1.32           -2.5%          +28.3%
   wheel-sieve1                 0.93           +0.6%           +1.9%

```

##### Analysis

- cryptarithm2 - we're lifting the letrec out of a SAT-transformed function. In particular, the `>>=` for `[]` can no longer be specialized on the second argument; this has disastrous effects

- simple - similar to cryptarithm2, so I'm not digging in for now.

- compress2 - it10-rec10 is a big win, even though it has 165 SLOW_CALL_OTHER_ctr (previously 0). Using the baseline libraries for both variants, rec10 actually worsens by 1.1%. So it's in the libraries. Ticky shows an additional $wa15 in GHC.IO.Handle.Text. TODO

TODO

##### An example where it worsens allocation

```wiki
-------------------------------------------------------------------------------
Program ll-baseline ll-protect-ignore ll-lam10pin ll-it10pin ll-lam10pin-rec10 ll-it10pin-rec10 ll-lam10pin-rec10-stab ll-it10pin-rec10-stab
-------------------------------------------------------------------------------
kahan   405812520        +0.0%           +0.0%       +0.0%         +1.5%          +1.5%           +1.5%           +1.5%
```


(Note: kahan is one of the programs Johan sees as a regression: he once had it not allocating at all, IIRC. It allocates plenty with 7.6 and HEAD, however.)


Before the late lambda lift in imaginary/kahan (some compact array munging code Johan wrote), we have

```wiki
f ... = ... letrec inner ... = ... inner ...
        in
            letrec-no-escape outer ... =
              case ... of
                False -> terminate
                True -> case inner ... of ... -> ... outer ...
            in ... outer ...
```


Both letrecs are floated to the top in a SAT'd shape (explained below).

```wiki
poly_inner ... = letrec-no-escape inner ... = ... in inner ...

poly_outer ... = letrec-no-escape outer ... = ... poly_inner ... in outer ...

f ... = ... poly_outer ...
```


Consequently, the simplifier immediately (and silently) inlines them both. However, it happens in an unfortunate order, so we end up with

```wiki
f ... = ... let-no-escape outer ... =
              case ... of
                False -> terminate
                True -> letrec inner ... = ... inner ...
                        in case inner ... of ... -> ... outer ...
            in ... outer ...
```


outer loops 2500000 times, and now allocates inner (size = 3 words) once per iteration.


The SAT'd shape of each letrec (which triggers pre-inline-unconditionally, I'm suspecting) is due to some existing code in SetLevels. This is from a match for lvlBind (Rec pairs):

```wiki
-- ToDo: when enabling the floatLambda stuff,
--       I think we want to stop doing this
  | isSingleton pairs && count isId abs_vars > 1
  = do      -- Special case for self recursion where there are
      -- several variables carried around: build a local loop:    
      --    poly_f = \abs_vars. \lam_vars . letrec f = \lam_vars. rhs in f lam_vars
      -- This just makes the closures a bit smaller.  If we don't do
      -- this, allocation rises significantly on some programs
      --
      -- We could elaborate it for the case where there are several
      -- mutually functions, but it's quite a bit more complicated
      -- 
      -- This all seems a bit ad hoc -- sigh
```


If floating a singly recursive binding with more than one abstracted value variable, this immediately does a SAT while marking it FloatMe.


The comment is doubly confusing:

- "when enabling the floatLambda stuff" seems contradictory to the guard count isId abs_vars \> 1
- I'm not sure how this improves allocation, unless ... perhaps it is an alternative way to prevent the float from precluding specialization?

  - ie It's an alternative approach to the problem that we mitigated by instead stabilizing the binding for the sake of the .hi file?
  - this SAT didn't prevent our 20% allocation explosion in cryptharithm2 because the letrec there had exactly one abs_var, and so didn't use this lvlBind alternative


Questions:

1. Is it worrisome that the chosen inlining ends up nesting a loop inside another, simultaneously spoiling the LNE? 
1. If it can be refined to avoid this sort of problem, this SAT-based transform could potentially get the benefits of both the late lambda float and specialization

  - actually, might another (normal) [FloatOut](float-out) pass float inner (back) out of outer?

### Miscellaneous Findings

#### Thunk Join Points


We discovered that the worker-wrapper was removing the void argument from join points (eg knights and mandel2). This ultimately resulted in LLF \*increasing\* allocation. A thunk was let-no-escape before LLF but not after, since it occurred free in the right-hand side of a floated binding and hence now occurred (escapingly) as an argument.


SPJ was expecting no such non-lambda join points to exist. We identified where it was happening (`WwLib.mkWorkerArgs`) and switched it off. Here are the programs that with affected allocation.

```wiki
protect-no  = allow wwlib to remove the last value argument
              (ie the previous behavior)

protect-yes = protect the last value argument from being removed
              (ie the experimental behavior)

Both are applied to both the libraries and the program.

Allocations

-------------------------------------------------------------------------------
        Program           protect-no     protect-yes
-------------------------------------------------------------------------------
        circsim           1326468688           -0.7%
         hidden           1165299720           -0.7%
            scs           1029909256           -0.1%
      transform            738757608           -0.1%
      cacheprof            478120432           +0.3%
       listcopy            334710912           -0.4%
  comp_lab_zift            330889440           -5.0%
         fulsom            321534872           -0.3%
      listcompr            304662896           -0.4%
           anna             70685104           +0.1%
         gamteb             59846096           -0.3%
         parser             32406448           +0.2%
             gg              8970344           -0.2%

        -1 s.d.                -----           -0.6%
        +1 s.d.                -----           +0.5%
        Average                -----           -0.1%
```


In circsim, put gets 1,000,000 better and Text.Read.Lex gets a tiny bit better.


In hidden, it's Text.Read.Lex, Text.ParserCombinators.ReadP, and GHC.Read.


In cacheprof, $wpreparseline gets a bit worse.


In comp_lab_zift: f_make_tree gets 2,060,000 better and f_union_br gets 1,500 better.


In anna, StrictAn6.$wsa, SmallerLattice.$w$sslDijkstra_aux, SmallerLattice.$w$sslDijkstra_unlink get worse (10,000, 400, 400).


In parser, Main.leLex gets worse (5000).


In gg, Graph.$wprintFloat gets worse (12 -\> 84).


Bigger swings in allocation (mostly good) took place in the programs themselves (eg transform.f_prmdef \~130,000 better, listcopy.f_se \~150,000 better).


Many of the Core differences were of the following form. For example, see circsim's `put` function. When protecting the last argument from being removed by `WwLib.mkWorkerArgs`, the Core looks like this:

```wiki
let x :: RealWorld# -> ...
    x = \_void -> let d = ... in Ctor(... d ...) (... d ...) ...
in CTX[x]
```


Without protection, it looks like:

```wiki
let d = ...
in CTX[Ctor(... d ...) (... d ...) ...]
```


Simon explained that it is probably the simplifier floating d out of the unprotected `x` binding \*in order to reveal `x` as let-bound to a constructor\*. Thus revealed, `x` is immediately inlined. Because of the `\_void`, this doesn't happen when the last argument is protected.


With protection, `d` isn't allocated unless `x` is entered, which might not always happen in `CTX`. This is a potential win because `x` might be let-no-escape.


A potential detriment of protection is that `x` is not exposed as a let-bound constructor. Simon conjectures that's not actually harmful. The reasoning is as follows.


These void arguments arise in two ways.

- when join points are created

- the strictness pass on constant functions


In both cases, it is unlikely that revealing the binding's RHS as a HNF will lead to any beneficial optimizations.

- Join points are unlikely to be case-scrutinized. It's unlikely that further simplification will render them scrutinized.

- Removing the value arg from constant functions would create sharing, which SPJ says is always a "dodgy" thing to do. If the programmer defines and uses a constant function, they may be trying to avoid retention of large data structures. I was concerned that such constant functions might arise upstream (eg from use of generics), but he regards that unlikely/not worth it (because the optimization is not always a good thing).

##### Affect on Expression Sizes


When we protect the void arguments from being removed by WwLib.mkWorkerArgs, we effect unfolding decisions.

- The body of the let that keeps its void argument is bigger because of the void arg's lambda
- It also gets a different result discount because of the lambda (instead of the lambda's body's result discount)
- The entire let expression itself is larger because:

  - The let's body is larger (see above)
  - The occurrences of the let have the additional application (supplying the void actual)
- Parameters' scrut discounts have also changed, but I didn't try figuring out why --- it was already clear we should do something about this.


Just turning on the -fprotect-last-arg flag was generally helpful for allocation. It was also generally helpful for runtime: somewhere between +1% and -3%. But a couple were \> +4%. In a couple of those cases, I discovered that the increased expression size was breaching the unfolding creation threshold (in a base library module). This actual prevented some inlining.


I implemented the -funfolding-ignore-RealWorld flag by ignoring arguments of type State\# RealWorld when measuring applications and lambdas. The only subtlety was that we still need to count the void args when considering whether to award the partial application result discount --- not counting the void args incorrectly awards the discount. That mistake means every invocation of a join point awards the rather large ufFunAppDiscount. In a couple programs, this increased allocation drastically because the newly inlined function's RHS included a join point that lost its LNE status at the call-site because the entire inlined RHS was case-scrutinized.

#### Measurements


Allocation; only good things happen with protect-last-arg, unfolding-ignore-RealWorld doesn't make much of a difference.

```wiki
                          baseline       protect-no-ignore   protect-ignore
        circsim           1326471120           -0.7%           -0.7%
  comp_lab_zift            330889440           -5.0%           -5.0%
         hidden           1165309432           -0.7%           -0.7%
      listcompr            304662896           -0.4%           -0.4%
       listcopy            334710912           -0.4%           -0.4%
       maillist             92431136           +0.0%           -0.7%
```


Repeatable Elapsed Times


There are some good ones in the -3 to -5% range. This table is just the bad ones/ones where ignore made things worse.

```wiki
                          baseline         baseline2     protect-no-ignore   protect-ignore

x5

           atom                 3.66           -0.1%           -2.2%           +2.0%
     bernouilli                 0.91           -0.4%           +0.7%           +1.8%
       cichelli                 0.34           +0.0%           -2.4%           +2.9%
       compress                 0.92           +0.0%           +0.4%           +1.3%
           fft2                 0.28           +0.0%           +0.0%           +5.7%
         genfft                 0.21           -0.9%           +2.8%           +3.8%
            hpg                 0.48           -1.3%           -2.5%           +4.2%
          infer                 0.36           -1.1%           -2.2%           -2.8%
       nucleic2                 0.26           +1.5%           +3.0%           +5.3%
            scs                 3.54           +0.2%           +0.8%           +2.7%
         simple                 1.33           -0.2%           -0.2%           +2.1%
          solid                 0.91           -0.0%           -1.1%           +0.4%
      transform                 1.86           -0.3%           -0.8%           +0.1%

x15

           atom                 3.66           +0.2%           -2.1%           +1.8%
     bernouilli                 0.92           -0.6%           -0.1%           +0.1%
       cichelli                 0.34           +0.0%           -2.0%           +0.4%
       compress                 0.92           -0.5%           +0.1%           +1.3%
           fft2                 0.28           -0.2%           +0.2%           +5.5%
         genfft                 0.21           +0.3%           +4.7%           +4.1%
            hpg                 0.47           +0.3%           -1.6%           +4.9%
          infer                 0.36           +0.2%           -1.3%           -2.2%
       nucleic2                 0.26           +0.5%           +5.3%           +4.0%
            scs                 3.55           -0.1%           +0.7%           +2.4%
         simple                 1.32           +0.2%           -0.1%           +2.4%
          solid                 0.91           +0.4%           -0.3%           +0.6%
      transform                 1.85           +0.5%           -0.1%           +0.9%

x50

       cichelli                 0.34           +0.0%           -1.7%           +0.5%
         genfft                 0.21           -0.5%           +4.3%           +4.1%
       nucleic2                 0.26           -0.1%           +4.0%           +4.3%
          solid                 0.91           -0.1%           -0.9%           +0.7%

```

TODO investigate

##### Interaction with LLF

TODO try with -flate-float-abstract-sat?


Baseline sizes tend to increase another 1% to 1.5%.


Allocation changes \> 0.5%:

```wiki
                           baseline             it10     it10-protect-ignore
  comp_lab_zift            330889440           -0.0%           -5.0%
         gamteb             59846096           -0.8%           -1.4%
         hidden           1165309432           -0.0%           -0.7%
       maillist             92431136           -0.2%           -0.8%
         puzzle            165864160           -0.0%          -15.1%

        rewrite             17800064           -0.0%           +3.1%
```


NB The rewrite bump is likely the same as in the non-LLF case: it happens because some recursive bindings end up in a different order for some reason and consequently some intermediate data structures don't get compiled away.


Elapsed time changes:

```wiki
x15 driver and then reproduced with x15 local-driver 

                              baseline        baseline2        it10      it10-protect-ignore
       cichelli                 0.34           +0.0%           -1.5%           -2.9%
  comp_lab_zift                 1.34           +0.1%           +1.6%           -4.4%
           life                 1.85           -1.1%           +3.0%           +0.3%
           para                 1.64           -0.1%           +0.0%           -3.0%
      paraffins                 0.89           -0.1%           -0.7%           -2.5%
         puzzle                 0.49           -0.2%           +0.8%          -10.2%
            scs                 3.56           +0.1%           +1.7%           -0.6%
   wheel-sieve2                 1.37           +0.4%           -0.7%           -1.7%

           anna                 0.44           +0.0%           -3.6%           +0.0%   -- reproduced at x50
        circsim                 5.82           -0.2%           -1.7%           +0.5%
   cryptarithm1                 2.36           -0.0%           +0.2%           +1.4%
          event                 1.02           +0.0%           -3.2%           +0.0%
         exp3_8                 1.88           -0.3%           +0.2%           +1.6%
        integer                 5.69           -0.2%           +0.7%           +2.6%
           lcss                 3.49           -0.3%           -1.1%           +0.6%
      typecheck                 0.69           -0.4%           +0.7%           +3.0%   -- reproduced at x50
```

- event - no Core diff
- integer - no Core diff
- typecheck - a few nested LNEs become top-level functions

TODO investigate the bad ones

##### Continuation


SPJ thought this may be another means of hoisting the let-no-escape functionality from the code generator into the core2core pipeline. LLF would handle let-no-escape lambdas, but it does not handle let-no-escape thunks (which we didn't initially realize were being identified).


Changing the let-no-escape thunks to \\void -\> ... closures upstream would then subject the binding to more optimisations. Formerly, it's non-lambda status meant that inlining it, eg, would lose sharing.

## Late Strictness/WW


There are two core-to-core passes related to demand (= strictness & usage & CPR):

- the demand analyzer pass (stranal/DmdAnal) and
- the worker-wrapper split (stranal/WorkWrap).


The split currently happens once in the pipeline, and the demand analysis happens immediately before it.

```wiki
sat
vectorise
specialize
float-out
float-in
simpl
HERE
float-out
cse
float-in
liberate-case;simpl
spec-constr
simpl
```


This pair of passes can be enabled/disabled by `-fstrictness` flag.


Additionally, the demand analyzer is optionally be ran before each execution of some arbitrary phases of the simplifier using the `-fstrictness-before` flag. A few direct invocations of the simplifier, eg, after vectorisation, are not affected by this flag.

### The Idea


The demands change as we optimize Core terms. The passes are careful to remove the demand info annotation when transforming a term in a way that invalidates the current demand info. Doing so, however, can hinder downstream optimizations. Running the strictness analyzer a second time may therefore be helpful.

### The Design


I added to flags `-flate-strictness` and `-flate-wwsplit`.

- `-flate-strictness` invokes the demand analyzer after SpecConstr.
- `-flat-wwsplit` implies `-flate-strictness` and immediately follows it with a worker-wrapper split.


I placed them arbitrarily in the pipeline. But phase-ordering is known to have an impact, so maybe we should try running in between all other passes.

#### Initial Core Lint error


Enabling either flag caused a core lint error when compiling `GHC.Float`. (I anticipate that `-fstrictness-before` would have done the same.) Some cleverness in the `.hi` files was surreptitiously creating an ill-typed unfolding for `GHC.Real.even`. Here's why.


The unfolding for a wrapper function is not actually stored in a `.hi` file. This unfolding can be reconstructed based on the type of the function and the demand info it had when the split was performed. Thus only the name of the worker is stored in the `.hi` file and `TcIface.tcIfaceWrapper` rebuilds the wrapper's unfolding.


However, re-running the demand analyzer after the worker wrapper split may change the wrapper's demand info. In the program offending Core Lint, the new demand info results in a worker with an arity of 4 while the old info had an arity of 5.


For now, I've merely disabled the `.hi` cleverness;thusly storing the wrapper's actual unfolding works fine.

### The initial measurements


Using different flag combinations, I built the libraries and ran nofib tests.

1. baseline        = -O1
1. late-strictness = -O1 -flate-strictness
1. late-wwsplit    = -O1 -flate-wwsplit


NB the libraries are usually compiled with -O2.


Allocation changes:

- allocation for late-strictness and late-wwsplit are always the same.

- The big changes are all good, but nothing truly spectacular.

```wiki
knights  2258392    -5.70%
fulsom   335718008  -2.50%
scs      1030151712 -1.60%
simple   226413112  -1.20%
pic      3528968    -0.20%
gamteb   59846096   -0.10%
gg       9159680    -0.10%

ansi     128632      0.10%
awards   292416      0.10%
expert   373048      0.10%
pretty   145640      0.10%
rfib     115688      0.10%
grep     72992       0.20%
mkhprog  3371224     0.20%
scc      59568       0.20%
tak      110136      0.30%
maillist 92431136    0.50%

```


Repeatable Elapsed time changes:

- These don't necessarily include all \*good\* changes, but I think I caught all the bad ones.

```wiki
              baseline   baseline2  late-strictness  wwsplit
atom          3.66       0.10%       3.00%            -1.10%
compress2     1.97       0.10%      -3.80%            -3.40%
fft           0.23       0.00%      -2.30%           -10.40%
integrate     0.66       0.30%      -0.20%            -4.10%
life          1.85       0.10%       1.90%             1.30%
para          1.65      -0.40%       0.20%            -3.40%
scs           3.59       0.60%      -0.90%           -12.50%
wave4main     1.18      -0.10%       1.90%             0.40%

cryptarithm1  2.36       0.10%      -1.90%             2.90%
fft2          0.28      -0.20%       0.10%             2.10%
genfft        0.21      -0.30%       1.90%             5.30%
hpg           0.47       0.10%      -3.00%             2.80%
integer       5.69      -0.20%      -0.70%             2.90%
transform     1.86      -0.20%      -0.20%             2.20%
treejoin      1.61       0.40%       1.10%             2.30%
```

### Analysis

#### Allocation

- knights - `possibleMoves`'s letrec includes a join point: baseline, there's no demand info, but late-strictness records that it's strict in its first argument. Thus after CoreTidy, calls `$j (case sNo {I# x -> I# (x -# 1)})` become `case sNo {I# x -> $j (I# (x -# 1))}`. By STG, the -\# 1 gets floated out, and the sat thunk's closure is instead just the `I#` constructor's closure. This has many benefits, including saving us a word since we don't need the free variable. This is approximately what the ww-split version ends up doing, too, ww-split just makes the change more explicit in the Core and STG.

- fulsom - A constructor argument `(let a = case fv1 of D# x -> case fv2 of D# y -> D# (x +## y) in GHC.Float.timesDouble a a)` becomes `case fv1 of D# x -> case fv2 of D# y -> let a = x +## y in D# (a *## a)`. The let at an unlifted type is actually a case, and hence not allocated, eliminating a two-free-variable closure altogether. Moreover, the entry count of `GHC.Float.timesDouble` is halved.

- scs - a hot recursive LNE has some lets converted to cases, since their strict demand is identified. 79,751,836 reduces to 77,930,374.

- scs - `GHC.Float.$w$sfloatToDigits1` allocates 95% as much as before (1070916 on 14098 entries).

- scs - `GHC.Float.$wfromRat''` no longer allocates, saving 170315 allocation on 37304 entries.

- simple - a number of lets become cases, partly because `revised_temperature` is newly found strict in its second arg

#### Runtime

TODO

## Make all dictionaries strict

- we also considered not newtyping dictionaries, but it had little effect

  - the only thing it prevents us from doing is "be strict in anything of kind Constraint"

- look inside used superclass dictionaries too

- abstract to \<S,U\> if we suspect that the dictionary is being reboxed

  - be suspicious reboxing, eg, if 3/4 or more of the components are Used

    - or maybe if they are UHead?
  - the consequence of \<S,U\> is that ww will use CBV but not unbox it

## Other Tasks

- Johan pointed out regressions; look into them --- there might be some easy wins

- late CSE; make lvlMFE very aggressive

- consider adding a void parameter to LNEs earlyish in the pipeline so they do not appear to share work

- use a specialized demand transformer for eliminators (record selectors, class selectors)

  - usually they get inlined, but this would help in case they're applied to a lambda-bound variable: we'd be able to better transfer its demand
  - it would be dual to the existing specialized transformer for data cons
  - cf dmdTransformDataConSig, MkId.dictSelRule
  - will need to stash which field is selected in the IdDetails (RecSelId and ClassOpId)

    - add a corresponding type synonym for Int alongside the ConTag synonym
  - it's a property that could potentially be recognized of user functions...
  - I spotted a case (interesting-programs/strict-dicts-something/SuperClasses.hs) where the worker has, eg, LU LU LU demand, but it does \*not\* use some of those arguments; this  particular case would not happen with the special transformer 
