
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