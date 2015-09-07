
This page explains the motivation and implementation of unpacking for sum types.

## Motivation


GHC does a good job of unpacking product types. Given a declaration like

```wiki
data T1 a b = C1 a b
data T2 a b = C2 {-# UNPACK #-} !(T1 a b)
```

`C2` will have a representation where all the overhead of the `C1` constructor, both the pointer to it in the `C2` constructor and the info table pointer in the `C1` constructor, has been removed. This saves two words and one indirection  compared to a packed representation, which uses five words.


Unfortunately, a similar example using sum types cannot be unpacked today:

```wiki
data T1 a = Some a | None
data T2 a = C !(T1 a)  -- Cannot UNPACK here
```


Here the representation of the `C` constructor will contain a pointer to e.g. the `Some` constructor. The `Some` constructor will be a separate heap object and thus needs one word to store its info table pointer.


In this example there is an alternative, unpacked representation that is more memory efficient and has fewer indirections. We could store a constructor tag together with the union of the fields of `T1` inside `C`. Conceptually the memory layout would look like this (in practice we might group pointer and non-pointer fields together):

<table><tr><th> T2 info table pointer </th>
<th> T1 constructor tag </th>
<th> Fields of `Some`</th>
<th> Fields of `None`</th></tr></table>


(In this case `None` has no fields.)


This representation saves one word and one indirection compared to the packed representation, which uses four words.

## Implementation


The implementation proceeds by adding a new type for unboxed sums and then using that in the unpacking of sum types.

### Core


We add a new primitive type constructor for the family of unboxed sums:

```wiki
(#|...|#)
```


A sum of n "\|"s is a n+1 ary sum. The type constructor can then be used to create a type, like so:

```wiki
(# t1 | ... | tn #)
```


The data constructor looks similar, except that we use an "_" to mark which alternative of the sum we want:

```wiki
(#...|_|...#)
```


This gets added to [compiler/prelude/TysWiredIn.hs](/trac/ghc/browser/ghc/compiler/prelude/TysWiredIn.hs), just like for unboxed tuples.


There's an construction and elimination form.


Construction:

```wiki
(# ... | x | ... #)
```


Again we count the bars to decide which alternative of the sum we are creating.


Elimination:

```wiki
case x of
    (# ... | x | ... #) -> ...
```


This matches against one of the alternatives of the n-ary sum.

### Core to STG


When going to STG we need to eliminate the unboxed sums. This can be done in [compiler/simplStg/UnariseStg.hs](/trac/ghc/browser/ghc/compiler/simplStg/UnariseStg.hs), just like for tuples.


Given the Core function

```wiki
f :: (# t_1 | ... | t_n #) -> ...
```


we convert it to a call to STG which includes the tag and the maximum number of pointer and non-pointer arguments we might need. Example:

<table><tr><th> Core </th>
<th> STG 
</th></tr>
<tr><th>` f :: (# Int# | Bool #) -> ... `</th>
<th>` f :: Word -> Word -> Pointer -> ... `</th></tr>
<tr><th>` f :: (# Int# | Word# #) -> ... `</th>
<th>` f :: Word -> Word -> ... `</th></tr></table>

### Code generation


We need to make sure we generate closure types for the constructors we unpack into. This is done in [compiler/codeGen/StgCmmCon.hs](/trac/ghc/browser/ghc/compiler/codeGen/StgCmmCon.hs).


We use the same algorithm as is used in the Core to STG step to compute the maximum number of pointer and non-pointer fields we might need.

### Unpacking


Given

```wiki
data T1 a = Some a | None
data T2 a = C {-# UNPACK #-} !(T1 a)
```


we generate a "worker" constructor

```wiki
C (# a | (# #) #)
```


(`(# #)` playing the role of void.)


We then translate the construction of `C` as follows:

```wiki
C x

===> (translates to)

case x of
    Some y -> C (# y | #)
    None   -> C (# | (# #) #)
```


We then translate the elimination of `C` as follows:

```wiki
case e of
    C x -> ... x ...

===> (translates to)

case e of
    C x' ->
        let x = case x' of
            (# y | #) -> Some y
            (# | _ #) -> None
        in ... x ...
```


This above reboxing will go away, using case-of-case and case-of-known-constructor, if we scrutinize `x` again.

---

# Exploiting nullary constructors


Joachim [ writes](https://mail.haskell.org/pipermail/ghc-devs/2015-September/009831.html): The current proposed layout for a 

```wiki
    data D a = D a {-# UNPACK #-} !(Maybe a) would be
    [D’s pointer] [a] [tag (0 or 1)] [Just’s a]
```


So the representation of

```wiki
         D foo (Just bar)     is     [D_info] [&foo] [1] [&bar]
and of   D foo Nothing        is     [D_info] [&foo] [0] [&dummy]
```


where `dummy` is something that makes the GC happy.


But assuming this dummy object is something that is never a valid heap objects of its own, then this should be sufficient to distinguish the two cases, and we could actually have that the representation of 

```wiki
         D foo (Just bar)     is     [D_info] [&foo] [&bar]
and of   D foo Nothing        is     [D_info] [&foo] [&dummy]
```


and an case analysis on D would compare the pointer in the third word with the well-known address of dummy to determine if we have Nothing or Just. This saves one word.


If we generate a number of such static dummy objects, we can generalize this tag-field avoiding trick to other data types than Maybe. It seems that it is worth doing that if

- the number of constructors is no more than the number of static dummy objects, and
- there is one constructor which has more pointer fields than all other constructors.


Also, this trick cannot be applied repeatedly: If we have

```wiki
  data D = D {-# UNPACK #-} !(Maybe a) | D'Nothing
  data E = E {-# UNPACK #-} !(D a)
```


then it cannot be applied when unpacking `D` into `E`. (Or maybe it can, but care has to be taken that `D`’s `Nothing` is represented by a different dummy object than `Maybe`’s `Nothing`.)


Anyways, this is an optimization that can be implemented once unboxed sum type are finished and working reliably.
