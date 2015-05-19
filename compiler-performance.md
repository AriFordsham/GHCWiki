## Nofib results


Full results [ are here](https://gist.githubusercontent.com/thoughtpolice/498d51153240cc4d899c/raw/9a43f6bbfd642cf4e7b15188f9c0b053d311f7b9/gistfile1.txt) (updated **May 5th, 2015**)

**NB**: The baseline here is 7.6.3

### Nofib outliers

#### Binary sizes

##### 7.6 to 7.8

- Solid average binary size increase of **5.3%**.

#### Allocations

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

TODO Build 7.10 with `integer-gmp 0.5` (not "`integer-gmp2`") to compare allocation baselines - did the compiler or the rewrite cause these failures?

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

## tests/perf/compiler\` results

### 7.6 vs 7.8

- A bit difficult to decipher, since a lot of the stats/surrounding numbers were totally rewritten due to some Testsuite API overhauls.
- The results are a mix; there are things like `peak_megabytes_allocated` being bumped up a lot, but a lot of them also had `bytes_allocated` go down as well. This one seems pretty mixed.

### 7.8 vs 7.10

- Things mostly got **better** according to these, not worse!
- Many of them had drops in `bytes_allocated`, for example, `T4801`.
- The average improvement range is something like 1-3%.
- But one got much worse; `T5837`'s `bytes_allocated` jumped from 45520936 to 115905208, 2.5x worse!

### 7.10 vs HEAD

- Most results actually got **better**, not worse!
- Silent superclasses made HEAD drop in several places, some noticeably over 2x

  - `max_bytes_used` increased in some cases, but not much, probably GC wibbles.
- No major regressions, mostly wibbles.

## The case of [\#10370](https://gitlab.haskell.org//ghc/ghc/issues/10370)

[\#10370](https://gitlab.haskell.org//ghc/ghc/issues/10370) seems to be attributable to a change in [b8392ae76a6d39c57be94b5ba041c450ab479e2b](/trac/ghc/changeset/b8392ae76a6d39c57be94b5ba041c450ab479e2b/ghc), which caused a large space leak it seems. This can be boiled down essentially to most of the allocations being done in `simplTopBinds -> completeCall -> addCoerce`, which was refactored in the aforementioned commit.


As the attachments show, this seems to be a large growth in the usage of some `IntMap` types. However this seems to really be a problem that boils down to: `substExpr` (or something below it) seems inefficient when doing simplification.


If you apply [ the following patch](https://gist.githubusercontent.com/thoughtpolice/26bb202459e747db5cd5/raw/2c506e8b615455bdab1e442aaf8b161310d0d007/T10370.patch), and run a profiled compiler over the testcase in [\#10370](https://gitlab.haskell.org//ghc/ghc/issues/10370)\#comment:3, you get a result like this:

```wiki
COST CENTRE        MODULE       %time %alloc

CoreTidy           HscMain       25.7   24.1
CompleteCall       Simplify       9.3    7.7
AddCoerceSubstExpr Simplify       9.0   22.2
tc_rn_src_decls    TcRnDriver     5.2    5.4

...
            SimpleExprF1App                       Simplify                  1584      198000    0.2    0.2    14.9   26.6
             SimpleExprF1Cast                     Simplify                  1669        6000    0.0    0.0     0.1    0.0
              SimpleCoercion                      Simplify                  1671        6000    0.1    0.0     0.1    0.0
             SimpleExprF1Lam                      Simplify                  1590        3000    0.0    0.0     0.0    0.0
             ApplyToVal                           Simplify                  1588      120000    0.0    0.0     0.0    0.0
             ApplyToTy                            Simplify                  1587       78000    0.0    0.0     0.0    0.0
             SimpleExprF1Var                      Simplify                  1585      159000    0.4    0.4    14.6   26.3
              SimpleExprF1Cast                    Simplify                  1658        9000    0.0    0.0     0.5    0.3
               SimpleCoercion                     Simplify                  1660        9000    0.4    0.3     0.4    0.3
              SimpleExprF1Lit                     Simplify                  1593        3000    0.0    0.0     0.0    0.0
              CompleteCall                        Simplify                  1586      123000    2.1    1.5    13.7   25.6
               Typecheck-Rename                   HscMain                   1664           0    0.0    0.0     0.0    0.0
                tcRnImports                       TcRnDriver                1665           0    0.0    0.0     0.0    0.0
               SimpleExprF1Cast                   Simplify                  1659           0    0.0    0.0     0.1    0.0
                AddCoerce                         Simplify                  1662        9000    0.1    0.0     0.1    0.0
                 AddCoerceArgSe                   Simplify                  1677        3000    0.0    0.0     0.0    0.0
                 AddCoercePair                    Simplify                  1663        6000    0.0    0.0     0.0    0.0
                SimpleCoercion                    Simplify                  1661           0    0.0    0.0     0.0    0.0
               SimpleExprF                        Simplify                  1648       21000    0.1    0.0    11.0   23.8
                SimpleExprF1Cast                  Simplify                  1651        6000    0.0    0.0     9.8   22.9
                 AddCoerce                        Simplify                  1653       21000    0.5    0.5     9.5   22.8
                  AddCoercePairApplyToTy          Simplify                  1667        6000    0.0    0.0     0.0    0.0
                  AddCoerceSplitForAllTy          Simplify                  1666        6000    0.0    0.0     0.0    0.0
                  AddCoercePairApplyToVal         Simplify                  1657        9000    0.0    0.0     0.0    0.0
                  AddCoerceSubstExpr              Simplify                  1656        9000    9.0   22.2     9.0   22.2
                  AddCoerceDecomposeCo            Simplify                  1655        9000    0.0    0.0     0.0    0.0

```


... which points to this SCC:

```
simplCast::SimplEnv->InExpr->Coercion->SimplCont->SimplM(SimplEnv,OutExpr)simplCast env body co0 cont0
  =do{ co1 <-{-# SCC "SimpleCoercion" #-} simplCoercion env co0
        ;-- pprTrace "simplCast" (ppr co1) $
          simplExprF env body (addCoerce co1 cont0)}where
       addCoerce co cont ={-# SCC "AddCoerce" #-} add_coerce co (coercionKind co) cont
       ...
       add_coerce co (Pair s1s2 t1t2)(ApplyToVal{ sc_arg = arg, sc_env = arg_se
         ...={-# SCC "AddCoercePairApplyToVal" #-}ApplyToVal{ sc_arg  = mkCast arg' (mkSymCo co1), sc_env  = zapSubstEnv arg_se
                      , sc_dup  = dup
                      , sc_cont = addCoerce co2 cont }where...
           arg'       ={-# SCC "AddCoerceSubstExpr" #-} substExpr (text "move-cast") arg_se' arg
           ...
```


in `Simplify.hs`

TODO But we don't really use `IntMap` almost anywhere in GHC, except for the `TrieMap` module perhaps (and by extension a few places in `coreSyn`)! Need to trace this down more.

TODO I'm not sure if this scaled before; maybe the commit increased the amount of work `substExpr` now does, and it was always Somewhat inefficient?

## Compile/build times


(NB: Sporadically updated)

**As of April 22nd**:

- GHC HEAD: 14m9s  (via 7.8.3) (because of Joachim's call-arity improvements)
- GHC 7.10: 15m43s (via 7.8.3)
- GHC 7.8:  12m54s (via 7.8.3)
- GHC 7.6:  8m19s  (via 7.4.1)


Random note: GHC 7.10's build system actually disabled DPH (half a dozen more packages and probably a hundred extra modules), yet things \*still\* got slower over time!


Relevant tickets

- [\#10370](https://gitlab.haskell.org//ghc/ghc/issues/10370): OpenGLRaw
- [\#10289](https://gitlab.haskell.org//ghc/ghc/issues/10289): 2.5k static HashSet takes too much memory to compile
- [\#9669](https://gitlab.haskell.org//ghc/ghc/issues/9669): slow compilation with lots of deriving clauses
- [\#9583](https://gitlab.haskell.org//ghc/ghc/issues/9583), [\#9630](https://gitlab.haskell.org//ghc/ghc/issues/9630): code blowup in Generics/Binary
- [\#10228](https://gitlab.haskell.org//ghc/ghc/issues/10228): regression from 7.8.4 to 7.10.1
- [\#7450](https://gitlab.haskell.org//ghc/ghc/issues/7450), [\#7258](https://gitlab.haskell.org//ghc/ghc/issues/7258): deriving Read generates gigantic code. Better now, but still not linear.
- [\#7428](https://gitlab.haskell.org//ghc/ghc/issues/7428): Non-linear compile time: addFingerprint??
- [\#2346](https://gitlab.haskell.org//ghc/ghc/issues/2346): desugaring let-bindings 
