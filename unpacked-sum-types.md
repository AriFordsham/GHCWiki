
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

### Core


We add a new primitive type constructor for the family of unboxed sums:

```wiki
|# t_1 | ... | t_n #|_n
```


This gets added to [compiler/prelude/TysWiredIn.hs](/trac/ghc/browser/ghc/compiler/prelude/TysWiredIn.hs), just like for unboxed tuples.


There's an construction and elimination form.


Construction:

```wiki
|# x #|_i_n
```


This creates the *i*th option of an n-ary sum.


Elimination:

```wiki
case x of
    |# x_1 #|_i_n -> ...
```


This matches against the *i*th option of an n-ary sum.

### Core to STG


When going to STG we need to eliminate the unboxed sums. This can be done in [compiler/simplStg/UnariseStg.hs](/trac/ghc/browser/ghc/compiler/simplStg/UnariseStg.hs), just like for tuples.


Given the Core function

```wiki
f :: |# t_1 | ... | t_n #| -> ...
```


we convert it to a call to STG which includes the tag and the maximum number of pointer and non-pointer arguments we might need. Example:

<table><tr><th> Core </th>
<th> STG 
</th></tr>
<tr><th>` f :: |# Int#, Bool #|_n -> ... `</th>
<th>` f :: Word -> Word -> Pointer -> ... `</th></tr>
<tr><th>` f :: |# Int#, Word# #|_n -> ... `</th>
<th>` f :: Word -> Word -> ... `</th></tr></table>

### Code generation


We need to make sure we generate closure types for the constructors we unpack into. This is done in [compiler/codeGen/StgCmmCon.hs](/trac/ghc/browser/ghc/compiler/codeGen/StgCmmCon.hs).


We use the same algorithm as is used in the Core to STG step to compute the maximum number of pointer and non-pointer fields we might need.

### Unpacking


Given

```wiki
data T1 a = Some a | None
data T2 a = C !(T1 a)  -- Cannot UNPACK here
```


we generate a "worker" constructor

```wiki
C |# a, (# #) #|_2
```


(`(# #)` playing the role of void.)


We then translate the construction of `C` as follows:

```wiki
C x

===> (translates to)

case x of
    Some y -> C (|# #|_1_2 y)
    None   -> C (|# #|_2_2 (# #))
```


We then translate the elimination of `C` as follows:

```wiki
case e of
    C x -> ... x ...

===> (translates to)

case e of
    C x' ->
        let x = case x' of
            |# #|_1_2 y -> Some y
            |# #|_2_2 _ -> None
        in ... x ...
```


This above reboxing will go away, using case-of-case and case-of-known-constructor, if we scrutinize `x` again.
