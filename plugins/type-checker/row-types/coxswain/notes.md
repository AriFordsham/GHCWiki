
I have a free week in 2018 May that I hope to spend revisiting the architecture of the [\`coxswain\` plugin](plugins/type-checker/row-types/coxswain). This page contains my notes.

## Using type families as syntax


One core idea is to use closed empty type families as syntax. The plugin instead of instances provides their semantics.

```wiki
infix 3 .=

data Col (kl :: Type) (kt :: Type)

type family (l :: kl) .= (t :: kt) = (col :: Col kl kt) | col -> l t where {}

data Row (kl :: Type) (kt :: Type)

type family Row0 :: Row kl kt where {}

infixl 2 .&
type family (p :: Row kl kt) .& (col :: Col kl kt) = (q :: Row kl kt) where {}
```


I don't really see an alternative to this.

### Blocking SEQSAME et al


Unfortunately, type families as syntax interferes with the GHC constraint simplifier. For example, the ambiguity check for the `DLacks` dctor fails.

```wiki
data B

data DLacks p l where
  DLacks :: (p ~ (q .& B .= ()),Lacks p l) => DLacks p l
```


The constraints look like

```wiki
given
  [G] $d~_a4oC {0}:: p[sk:2] ~ p[sk:2] (CDictCan)
  [G] $d~~_a4oD {0}:: p[sk:2] ~~ p[sk:2] (CDictCan)
  [G] $dLacks_a4on {0}:: Lacks p[sk:2] l[sk:2] (CDictCan)
  [G] co_a4oy {0}:: (B .= ()) ~# fsk_COL[fsk:0] (CFunEqCan)
  [G] co_a4oA {0}:: (q[sk:2] .& fsk_COL[fsk:0]) ~# fsk_ROW[fsk:0] (CFunEqCan)
  [G] co_a4oB {1}:: fsk_ROW[fsk:0] ~# p[sk:2] (CTyEqCan)
derived
wanted
  [WD] hole{co_a4oM} {3}:: (alpha[tau:2] .& fsk_COL[fsk:0]) ~# p[sk:2] (CNonCanonical)
untouchables [fsk_ROW[fsk:0], fsk_COL[fsk:0], l[sk:2], q[sk:2], p[sk:2]]
touchables (alpha[tau:2], alpha[tau:2])
```


(I opened [\#15147](https://gitlab.haskell.org//ghc/ghc/issues/15147) because the `fsk` is supposedly unexpected in a
Wanted.)


Via `-ddump-tc-trace`, we see that the Wanted was a `CTyEqCan` before it
was unflattened and zonked prior to being handed to the plugin.

```wiki
  {[WD] hole{co_a4oM} {3}:: (s_a4oE[fmv:0] :: Row * *)
                            GHC.Prim.~# (p[sk:2] :: Row * *) (CTyEqCan)}
```


The simplification rule `SEQSAME` from Fig.23 of jfp-outsidein would
fire for `co_a4oB` and the (canonical) `hole{co_a4oM}` if both equalities
were swapped such that `p` were on the left. Note that `Note [Canonical orientation for tyvar/tyvar equality constraints]` says "If
either is a flatten-meta-variables, it goes on the left" and also "If
one is a flatten-skolem, put it on the left so that it is substituted
out"; so that explains the orientation we see.


Because we're using "type families as syntax", this impedance of `SEQSAME` will be common.


And it affects other rules too.

```wiki
f :: (p ~ (u .& A .= ()),p ~ (v .& B .= ())) => Proxy p -> Proxy u -> Proxy v
f _ _ = Proxy
```


When simplifying the givens for `f`, the orientation similarly blocks the `EQSAME` interaction rule from Fig.22 for `co_a4qk` and `co_a4qt`.

```wiki
  [G] co_a4qq {0}:: (B .= ()) ~# (fsk_a4qp[fsk:0]) (CFunEqCan)
  [G] co_a4qh {0}:: (A .= ()) ~# (fsk_a4qg[fsk:0]) (CFunEqCan)
  [G] co_a4qs {0}:: (v_a4p4[sk:2] .& fsk_a4qp[fsk:0]) ~# (fsk_a4qr[fsk:0]) (CFunEqCan)
  [G] co_a4qj {0}:: (u_a4p3[sk:2] .& fsk_a4qg[fsk:0]) ~# (fsk_a4qi[fsk:0]) (CFunEqCan)
  [G] co_a4qk {1}:: (fsk_a4qi[fsk:0]) ~# (p_a4p2[sk:2]) (CTyEqCan)
  [G] co_a4qt {1}:: (fsk_a4qr[fsk:0]) ~# (p_a4p2[sk:2]) (CTyEqCan)
```

### Equal Untouchable Variables


The plugin is only responsible for *non-trivial* row/set equalities. The example here demonstrates that a var-var equality is still trivial (i.e. GHC will completely handle it) even if both variables are untouchable.


If the givens contain an equality for two untouchable variables, the other givens will only contain one of those variables; GHC (8.4.1) will have replaced the rest. (GHC does this "the hard way" via the `EQSAME` interaction rule instead of via unification because two variables, as untouchables, cannot be unified.) For example,

```wiki
f :: C (a,b) => a :~: b -> ()
f Refl = ()
```


for some class `C` with no instances gives

```wiki
========== 3 ==========
given
  [G] $dC_a1uk {0}:: C (a_a1ub[sk:2], a_a1ub[sk:2]) (CDictCan)
  [G] co_a1uf {0}:: (b_a1uc[sk:2] :: *)
                    ~# (a_a1ub[sk:2] :: *) (CTyEqCan)
given_uf_tyeqs
  ([G] co_a1uf {0}:: (b_a1uc[sk:2] :: *)
                     ~# (a_a1ub[sk:2] :: *) (CTyEqCan),
   b_a1uc[sk:2],
   a_a1ub[sk:2])
derived
wanted
```


Note that `_a1uk` is now `C (a,a)` instead of the `C (a,b)` from the original source.


GHC will also have applied the renaming to any wanteds via `SEQSAME`.

### Generating "Hidden" Givens While Solving Wanteds


If I recall correctly, new givens cannot be generated while simplifying wanteds (the comment on [ \`TcInteract.runTcPluginsWanted\`](https://github.com/ghc/ghc/blob/57858fc8b519078ae89a4859ce7588adb39f6e96/compiler/typecheck/TcInteract.hs#L269) sounds like I'm recalling correctly). I think this should be allowed, if only as a convenience to plugin authors. They could at least use it to memoize any forwarding chaining/reasoning done by their plugin as it solves wanteds. (For my particular plugin, it might also be nice if these hidden givens could be accompanied by similarly hidden universally quantified variables; in other words, binding this evidence might reveal an existentially quantified variable. This seems like a bigger ask...)


It might also enable the plugin to cache state specific to the givens, based on an otherwise unused constraint like `MyStateCacheIdentifier 42`. (This is probably only wise for memoization.) And if that cached state contains type variable uniques, then another otherwise unused constraint `Rename '[v1,v2,...]`, would let the plugin track how the uniques on the type variables `v1,v2,...` during GHC's turn. Uniques on evidence variables could not be similarly tracked, which is problematic. That deficiency of this workaround suggests that reporting updated uniques to a plugin deserves genuine support in the API.

### Set Types


I'm pursuing "set types" instead of "row types". There's ways to embed `Set k` into `Row k Void` or some such, but it seems more delicate (eta-contraction issues with lifted types?) than I'd like to try at first. And one of my prime interests is a view of algebraic data types as named summands, which I think will be less redundant via type-indexed-sums than via row-based variants.


This is my current sketch for the "syntax" types.

```wiki
data Set (n :: Nat) (k :: *)
type family Set0 :: Set 0 k
type family (s :: Set n k) .& (e :: k) :: Set (1+n) k where {}
```


I need a better word than "set" --- it's way to overloaded. But it's so temptingly short...


The kind for future row types might have a set index, as in `Row (Set n kl) kt`.

### Simplifying `Lacks` constraints


There are two fundamental simplifications to `Lacks` constraints, based on the operationally-useful semantics of a `Lacks x b` dictionary being the number of elements in `x` that are "less than" `b`.

```wiki
[Lacks0] (d : Lacks Set0 b) = 0
[LacksE] (d : Lacks (x .& a) b) = (d1 : Lacks x b) + (d2 : Lacks (Set0 .& a) b)
```


And here is a slightly less obvious rule, which emphasizes that singleton `Lacks` constraints are declarations of inequality where the type promises that `a` and `b` are unequal while only the evidence indicates the concrete relative ordering.

```wiki
[Lacks1] (d : Lacks (Set0 .& a) b) = 1 - (d1 : Lacks (Set0 .& b) a)
```


By the orderless semantics of set forms, the `[LacksE]` rule generalizes to decompositions like the following.

```wiki
(d : Lacks (x .& u .& v .& w) b) = (d1 : Lacks (x .& u .& w) b) + (d2 : Lacks (Set0 .& v) b)
(d : Lacks (x .& u .& v .& w) b) = (d1 : Lacks (x .& v .& w) b) + (d2 : Lacks (Set0 .& u) b)
(d : Lacks (x .& u .& v .& w) b) = (d1 : Lacks (x .& v) b)      + (d2 : Lacks (Set0 .& u .& w) b)
(d : Lacks (x .& u .& v .& w) b) = (d1 : Lacks (x .& v) b)      + (d2 : Lacks (Set0 .& u) b       + (d3 : Lacks (Set0 .& w) b)
...
```


Note the multi-modality of the numbers in (both sides of) those equations: knowing all but one number determines the other, using addition and/or subtraction.


Also note that, in an otherwise empty context, `(Lacks (Set0 .& u) b,Lacks (Set0 .& v) b)` is a stronger constraint than `Lacks (Set0 .& u .& v) b`: the evidence of the second says 0-2 of `u` and `v` are less than `b`, while the evidence of the first tells us the exact relationship of `u` to `b` and of `v` to `b`.


Because we can't solve eg `Lacks (Set0 .& u) b` from `Lacks (Set0 .& u .& v) b`, we must decompose wanteds prematurely; the decomposition must be driven by what evidence is already available and/or already needed.
