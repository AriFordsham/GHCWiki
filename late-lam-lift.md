### TODOs

- LNE catch 22: good to lift (enables simplifications) but also bad to lift (causes a slight slow down)

  - apparently LNE calls are slightly faster than function calls --- investigate if this is totally intentional
  - some of those simplifications are because lifting simulates FV-scrutinization discounts
  - SPJ says it's reasonable to implement FV-scrut directly in the simplifier --- have a brave go at implementing this
  - another benefit from lifting an LNE comes from reducing the size of the enclosing expression --- I don't see how to recover this benefit outside of the late lambda lift
  - on the other hand, some programs get slower if we leave the LNEs in --- investigate: is this solely due to inhibited simplification?
  - so maybe lift an LNE if it's huge?
- related easy win? Reformulating a recursive function as an LNE (if that's possible for its RHS) may give a slight speed boost
- also, CPR sum for "nested" things was disrupting LNEs... we'd like to enable it
- do not use the delayed lift-cost estimation

  - currently, we delay the cost estimation so that we can take into account free variables ("freebies") added by lifting enclosing functions
  - **refinement 1** (experiment with this as a simplification that might still be effective): be very conservative

    - assume all RHS function ids are also lifted (unless obviously not, eg PAP): gather their abs_ids transitively
    - don't take freebies into account
  - **refinement 2** (future work): be more precise

    - guess about "cadres" of functions that co-occur in closures and share free variables
    - separately estimate their lift-cost as a pair
    - this may choose to inline both when individually either (or both) of them would not be lifted
  - **refinement 3** (future work): spread the rewards

    - if lifting `g` actually reduces the size of a closure (since, `g`'s abs_ids are freebies), then should lifting other functions (say `f`) be allowed to grow that closure accordingly?
    - this could be good: it might unpin other functions that fast-call `f`
    - it could be bad: if `f` wasn't pinning anything important, then we just wasted `g`'s improvement
  - **refinement 4** (experiment): ignore CorePrep floats

    - measure how much it matters that we approximate CorePrep's floats
  - **refinement 5** (not sure): integrate PAP-avoidance into the closure-growth estimates
- formulate the specification as `e ~> (ups,e')`

  - where (`f` maps to `n` in `ups`) if lifting `f` would incur the `n` more allocated words during arbitrary evaluation of `e'`. `n` can be `infinity` if there's a increase in allocation under a lambda.
  - we use the `ups` map in order to decide if we should float `f`.
- statistics

  - static: lambdas lifted, lambdas left

    - count, size, arguments, free variables (related to size but different because of `ArgRep`), number of uses, number of capturing closures
    - pinning relationships
  - dynamic: total allocation change wrt to each lambda (via ticky, I guess), etc
  - **refinement 6** (experiment): is the closure growth `n` correlated to other more easily-computed characteristics of `f`
- consider more possibilities for stabilisation
- try running it before `SpecConstr` again (I think I missed an -O2 last time)
- **refinement 7**: re-consider the partial float, if pinnings are a major issue

  - the residual PAPs though probably have a runtime cost
  - but is it any different than the PAP created by CorePrep?
  - **refinement 7.5**: partial float PAP

    - ie just wrt PAP creation avoidance, can we leave a residual PAP instead of not floating at all?
- run it at the beginning of the Core2Core pipeline to demonstrate how/why that's bad
- measure how much cardinality=1 helps us
