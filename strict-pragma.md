# The Strict Language Pragma


This page explains the motivation, semantics, and implementation of a new language pragma named `Strict`.

## The Problem


High-performance Haskell code (e.g. numeric code) can sometimes be littered with bang patterns, making it harder to read. The reason is that laziness isn't the right default in this particular code, but the programmer has no way to say that except by repeatedly adding bang patterns. This page proposes a new language pragma that allows the programmer to switch the default on a per module basis.

## Semantics


Informally the `-XStrict` language extension switches functions, data types, and bindings to be strict by default, allowing optional laziness by adding `~` in front of a variable. This essentially reverses the present situation where laziness is default and strictness can be optionally had by adding `!` in front of a variable.

- **Function definitions.**  When the user writes

  ```wiki
  f x = ...
  ```

  we interpret it as if she had written

  ```wiki
  f !x = ...
  ```

  Adding `~` in front of `x` gives the old lazy behavior.

- **Let/where bindings.**  When the user writes

  ```wiki
  let x = ...
  let pat = ...
  ```

  we interpret it as if she had written

  ```wiki
  let !x = ...
  let !pat = ...
  ```

  Adding `~` in front of `x` gives the old lazy behavior.  Notice that we do *not* put bangs on nested patterns. For example

  ```wiki
   let (p,q) = if flob then (undefined, undefined) else (True, False)
   in ...
  ```

  will behave like

  ```wiki
   let !(p,q) = if flob then (undefined, undefined) else (True, False)
  ```

  which will strictly evaluate the RHS, and bind `p` and `q` to the components of the pair.  But the pair itself is lazy (unless we also compile the Prelude with `-XStrict`; see "Modularity" below).  So `p` and `q` may end up bound to `undefined`.  See also "Recursive and polymorphic let bindings" below.

- **Top level bindings** are unaffected by `-XStrict`.  For example:

  ```wiki
   x = factorial 20
   (y,z) = if x > 10 then True else False
  ```

  Here `x` and the pattern binding `(y,z)` remain lazy.  Reason: there is no good moment to force them, until first use.  **Simon**: Johan, do you agree?

- **Data types.** When the user writes

  ```wiki
  data T = C a
  ```

  we interpret it as if she had written

  ```wiki
  data T = C !a
  ```

  Haskell doesn't allow for `~` patterns in data constructor definitions today. We'll add support for such patterns and have it give the old lazy behavior.

- **Newtypes.**  There is no effect on newtypes, which simply rename existing types.  For example:

  ```wiki
  newtype T = C a
  f (C x)  = rhs1
  g !(C x) = rhs2
  ```

  In ordinary Haskell , `f` is lazy in its argument and hence in `x`; and `g` is strict in its argument and hence also strict in `x`.  With `-XStrict`, both become strict because `f`'s argument gets an implict bang.

### Modularity


The pragma only affects definitions *in this module*. Functions and data types imported from other modules are unaffected. For example, we won't evaluate the argument to `Just` before applying the constructor. Similarly we won't evaluate the first argument to `Data.Map.findWithDefault` before applying the function.


This is crucial to preserve correctness. Entities defined in other modules might rely on laziness for correctness (whether functional or performance).

### Recursive and polymorphic let bindings


Consider a banged let-binding

```wiki
  let !pat = rhs in body
```


Bang patterns in let bindings today (GHC 7.8.3 and earlier) behave as [described in the user manual](http://www.haskell.org/ghc/docs/7.8.3/html/users_guide/bang-patterns.html):

- The binding cannot be recursive
- The variables bound by the pattern always get monomorphic types
- The complete pattern is matched before evaluation of `body` begins


The intent was that it is valid to desugar such a binding to

```wiki
  case rhs of pat -> body
```


This currently applies even if the pattern is just a single variable, so that the `case` boils down to a `seq`.


Continuing with this rule would mean that `-XStrict` would not allow recursive or polymoprhic pattern bindings *at all*.  So instead we propose the following revised specification.  

- **Static semantics.** Exactly as in Haskell, unaffected by `-XStrict`.

- **Dynamic semantics.** Consider the rules in the box of [ Section 3.12 of the Haskell report](http://www.haskell.org/onlinereport/exps.html#sect3.12).  Replace these rules with the following ones, where `v` stands for a variable.

  - **(FORCE)**.  Replace any binding `!p = e` with `v = e; p = v` and replace `e0` with `v `seq` e0`, where `v` is fresh.  This translation works fine if `p` is already a variable `x`, but can obviously be optimised by not introducing a fresh variable `v`.
  - **(SPLIT)**. Replace any binding `p = e`, where `p` is not a variable, with `v = e; x1 = case v of p -> x1; ...; xn = case v of p -> xn`, where `v` is fresh and `x1`..`xn` are the bound variables of `p`.

>
> The result will be a (possibly) recursive set of bindings, binding only simple variables on the LHS.  (One could go one step further, as in the Hsakell Report and make the recursive bindings non-recursive using `fix`, but we do not do so in Core, and it only obfuscates matters, so we do not do so here.)


Examples of how this translation works.  The first expression of each sequence is Haskell source; the subsequent ones are Core.

```wiki
(1)  let x :: Int     -- Non-recursive
         !x = factorial y
     in body
 ===> (FORCE)
     let x = factorial y in x `seq` body
 ===> (inline seq)
     let x = factorial y in case x of x -> body
 ===> (inline x)
     case factorial y of x -> body

(2)  letrec xs :: [Int]  -- Recursive
            !xs = factorial y : xs
     in body
 ===> (FORCE)
     letrec xs = factorial y : xs in xs `seq` body
 ===> (inline seq)
     letrec xs = factorial y : xs in case xs of xs -> body
 ===> (eliminate case of value)
     letrec xs = factorial y : xs in body

 (3) let f :: forall a. [a] -> [a]    -- Polymorphic
         !f = fst (reverse, True)
     in body
 ===> (FORCE)
     let f = /\a. fst (reverse a, True) in f `seq` body
        -- Notice that the `seq` is added only in the translation to Core
        -- If we did it in Haskell source, thus
        --    let f = ... in f `seq` body
        -- then f's polymorphic type would get intantiated, so the Core
        -- translation woudl be
        --    let f = ... in f Any `seq` body

 ===> (inline seq, inline f)
     case (/\a. fst (reverse a, True)) of f -> body

 (4) let f :: forall a. Eq a => a -> [a] -> Bool    -- Overloaded
         !f = fst (member, True)
     in body
 ===> (FORCE)
     let f = /\a \(d::Eq a). fst (member, True) in f `seq` body
 ===> (inline seq, case of value)
     let f = /\a \(d::Eq a). fst (member, True) in body
        -- Note that the bang has no effect at all in this case
```

## Implementation

TODO Find all the places where we do special things for bang patterns and list them here.

## Raw chat log between SPJ, Johan, and Duncan

**Simon** I think this is unnecessary now.  Delete?


Probably doesn't make sense to anyone else!

```wiki
Johan Tibell: https://github.com/tibbe/ghc/tree/strict/compiler/deSugar
Simon Peyton Jones: Just (x,y) = e
Simon Peyton Jones: !Just (x,y) = e
Simon Peyton Jones: f :: (Int,Int) -> Int
Johan Tibell: f !x = ...
Simon Peyton Jones: f x = case x of (a,b) -> a `seq` b `seq` ...
Simon Peyton Jones: f2 :: Int -> Int -> INt
Johan Tibell: data MyPair a b = MP a b
Johan Tibell: f :: MyPair -> …
Johan Tibell: let x = ....
Johan Tibell: let !x = ...
Johan Tibell: case … of
Johan Tibell: let (x, y) = ....
Simon Peyton Jones: let !(a,b) = e ...
Johan Tibell: let x@(a,b) = … in x `seq` …
Simon Peyton Jones: {-# LANGUAGE BangPatterns #-}
module Bang where

f y = let !x = reverse y in reverse x
Simon Peyton Jones: ~/5builds/HEAD-2/inplace/bin/ghc-stage1 -c -ddump-ds Bang.hs

==================== Desugar (before optimization) ====================
Result size of Desugar (before optimization)
  = {terms: 14, types: 20, coercions: 0}

Rec {
Bang.f :: forall a_arI. [a_arI] -> [a_arI]
[LclIdX, Str=DmdType]
Bang.f =
  \ (@ a_arK) ->
    (\ (@ a_arI) ->
       letrec {
         f_aru :: [a_arI] -> [a_arI]
         [LclId, Str=DmdType]
         f_aru =
           \ (y_apy :: [a_arI]) ->
             let {
               x_apz :: [a_arI]
               [LclId, Str=DmdType]
               x_apz = GHC.List.reverse @ a_arI y_apy } in
             case x_apz of x_apz { __DEFAULT ->
             GHC.List.reverse @ a_arI x_apz
             }; } in
       f_aru)
      @ a_arK
end Rec }




==================== Desugar (after optimization) ====================
Result size of Desugar (after optimization)
  = {terms: 9, types: 12, coercions: 0}

Bang.f :: forall a_arI. [a_arI] -> [a_arI]
[LclIdX, Str=DmdType]
Bang.f =
  \ (@ a_arK) (y_apy :: [a_arK]) ->
    case GHC.List.reverse @ a_arK y_apy of x_apz { __DEFAULT ->
    GHC.List.reverse @ a_arK x_apz
    }



simonpj@cam-05-unx:~/tmp$
Simon Peyton Jones: f y = let !(x,z) = (reverse y, y) in reverse x
Simon Peyton Jones:          f_arv =
           \ (y_apy :: [a_arL]) ->
             let {
               ds_drW :: ([a_arL], [a_arL])
               [LclId, Str=DmdType]
               ds_drW = (GHC.List.reverse @ a_arL y_apy, y_apy) } in
             case ds_drW of wild_00 { (x_apz, z_apA) ->
             GHC.List.reverse @ a_arL x_apz
             }; } in
Simon Peyton Jones: let x = 1+2 in ....
Simon Peyton Jones: x :: Num a => a
Simon Peyton Jones: TcBinds lines 1299
Simon Peyton Jones:     strict_pat_binds = any isStrictHsBind binds
       -- Strict patterns (top level bang or unboxed tuple) must not
       -- be polymorphic, because we are going to force them
       -- See Trac #4498, #8762
Simon Peyton Jones: let !pat = e
Simon Peyton Jones: let (# a,b #) = e
Johan Tibell: let pat
Johan Tibell: let !pat
Simon Peyton Jones: let !pat = e in b
Simon Peyton Jones: case e of pat -> b
Johan Tibell: let pat = …
Simon Peyton Jones: let (a,b) = (reverse, filter)
Johan Tibell: let !x :: a = ….
Simon Peyton Jones: let !x = if ... then reverse else id
Simon Peyton Jones: let x :: forall a. [a] -> [a] = case ... of True -> reverse; False -> id
Simon Peyton Jones: in case x ?? of _ -> body
Simon Peyton Jones: case x Any of _ -> body
Simon Peyton Jones: let !(x,y) = if ... then (p,q) else (r,s)
Simon Peyton Jones: let p :: forall a. (..., ...) = /\a. if .. then (p,q) else (r,s)
Simon Peyton Jones:    x :: forall a. ....  = /\a . fst (p a)
Simon Peyton Jones:   y :: forall. ... = /\a. snd (p a)
Simon Peyton Jones: case (p Any) of _ -> ..
Simon Peyton Jones: let !pat = e in b
Simon Peyton Jones: let p = e; x = case p of pat -> x; y = case p of pat -> y 
in p `seq` b
Simon Peyton Jones: This translation works ok for recursive bang pats
Simon Peyton Jones: and polymorphic ones
Simon Peyton Jones: 1.  make Strict language behave EXTACTLY like let !p
Simon Peyton Jones: 2. Improve let !p as above
Simon Peyton Jones: (i.e. to wwork for rec and poly cases)
Johan Tibell: case x of _ -> ...
Johan Tibell: case undefined of ...
Simon Peyton Jones: case e of pi -> ei
Simon Peyton Jones: ist reated like
Simon Peyton Jones: case e of !pi -> ei
Johan Tibell: case e of !_ -> ei
Simon Peyton Jones: case e of !(Just x) -> b
Simon Peyton Jones: is eqiv to
Simon Peyton Jones: case e of (Just x) -> b
Simon Peyton Jones: newtype Age = MkAge Int
Simon Peyton Jones: case e of MkAge y -> ...
Simon Peyton Jones: this is strict in y
Simon Peyton Jones: Put that in the spec too!
Johan Tibell: data T = C ~Int
Simon Peyton Jones: HsType
Simon Peyton Jones: HsBangTy
```