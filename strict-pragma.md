# The Strict Language Pragma


This page explains the motivation, semantics, and implementation of a new language pragma named `Strict`.

## The Problem


High-performance Haskell code (e.g. numeric code) can sometimes be littered with bang patterns, making it harder to read. The reason is that laziness isn't the right default in this particular code, but the programmer has no way to say that except by repeatedly adding bang patterns. This page proposes a new language pragma that allows the programmer to switch the default on a per module basis.

## Semantics


Informally the `Strict` pragma switches functions, data types, and bindings to be strict by default, allowing optional laziness by adding `~` in front of a variable. This essentially reverses the present situation where laziness is default and strictness can be optionally had by adding `!` in front of a variable.

### Modularity


The pragma only affects definitions \*in this module\*. Functions and data types imported from other modules are not affected. For example, we won't evaluate the argument to `Just` before applying the constructor. Similarly we won't evaluate the first argument to `Data.Map.findWithDefault` before applying the function.


This is crucial to preserve correctness. Entities defined in other modules might rely on laziness for correctness (whether functional or performance).

### Function definitions


When the user writes

```wiki
f x = ...
```


we interpret it as if she had written

```wiki
f !x = ...
```


Adding `~` in front of `x` gives the old lazy behavior.

### Let bindings


When the user writes

```wiki
let x = ...
```


we interpret it as if she had written

```wiki
let !x = ...
```


Adding `~` in front of `x` gives the old lazy behavior.

### Data types


When the user writes

```wiki
data T = C a
```


we interpret it as if she had written

```wiki
data T = C !a
```


Haskell doesn't allow for `~` patterns in data constructor definitions today. We'll add support for such patterns and have it give the old lazy behavior.

### Newtypes


Newtypes are also strict when pattern matching e.g.

```wiki
newtype T = C a
case e of
    C x -> ...
```


is strict in `x`.

### Recursive definitions and polymorphism


Today strict bindings cannot be polymorphic. We have a translation that would allow them to be, one that also works in the presence of recursion.

TODO(SPJ): The below comments from SPJ contains the translation, but it's somewhat interleaved with other stuff so I forgot what it was.

## Implementation

TODO Find all the places where we do special things for bang patterns and list them here.

## Raw chat log between SPJ, Johan, and Duncan


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