
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

### Computing the representation


There are several possible representations we could chose from. Briefly they are:

- Store a constructor tag and the union of all the fields.
- Store a constructor tag and the maximum number of needed pointer and non-pointer fields.


The former is simpler, especially when it comes to represent the types of the fields (since there's no aliasing), but the latter is more efficient. We will use the former for now.


Given

```wiki
data T1 = C1 x_1..x_n | C2 y_1..y_n
data T2 = C {-# UNPACK #-} !T1
```


computing the representation of `C` is quite simple. We just splice in the union of the fields of all the constructors of `T1`.

<table><tr><th> Info table pointer </th>
<th> T1 constructor tag </th>
<th> x_1..x_n </th>
<th> y_1..y_n 
</th></tr></table>


(In practice we might group pointers and non-pointers fields in the actual heap object representation.)

### Populating the constructor


Given

```wiki
data T1 = C1 x_1..x_n | C2 y_1..y_n
data T2 = C {-# UNPACK #-} !T1
```


and the representation

<table><tr><th> Info table pointer </th>
<th> T1 constructor tag </th>
<th> x_1..x_n </th>
<th> y_1..y_n 
</th></tr></table>


we must figure out what values to write write for e.g. the fields of `C2` when creating a `C1` value:

```wiki
mkC (C1 x_1..x_n) = C c1_tag# x_1..x_n ???
```


At the very least we need to write something into those y_1..y_n fields that contain pointers, or the GC will crash when traversing constructor. For now we will just write a pointer to `bottom` in those fields.

TODO This needs to be expanded on. Writing something in the unused fields in easy in the code gen, but at the core level we might need to conjure up some values of the right type to put in all fields.

### Avoiding reboxing in case statements


Given

```wiki
data T1 = C1 x_1..x_n | C2 y_1..y_n
data T2 = C {-# UNPACK #-} !T1
```


we might worried that doing

```wiki
case t2 of
    C t1 -> case t1 of
        C1 x_1..x_n -> ...
        C2 y_1..y_n -> ...
```


would require that we need to allocate a `C1` or `C2` constructor just to deconstruct it again. Fortunately we should be able to avoid that because when we construct either `C1` or `C2` from the unpacked representation in `C` we'd do that we a case, like so:

```wiki
unpack (C tag# x_1..x_n y_1..y_n) = case tag# of
    0# -> C1 x_1..x_n
    1# -> C2 y_1..y_n
```


This gives us

```wiki
case t2 of
    C t1 -> case t1 of
        C1 x_1..x_n -> ... x_1..x_n ...
        C2 y_1..y_n -> ... y_1..y_n ...

===>

case t2 of
    C t1 -> case unpack t2 of
        C1 x_1..x_n -> ... x_1..x_n ...
        C2 y_1..y_n -> ... y_1..y_n ...

===> (inline unpack)

case t2 of
    C tag# x_1..x_n y_1..y_n -> case (case tag# of
        0# -> C1 x_1..x_n
        1# -> C2 y_1..y_n) of
            C1 x_1..x_n -> ... x_1..x_n ...
            C2 y_1..y_n -> ... y_1..y_n ...

===> (case-of-case and case of known constructor)

case t2 of
    C tag# x_1..x_n y_1..y_n -> case tag# of
        0# -> ... x_1..x_n ...
        1# -> ... y_1..y_n ...
```

# Work in progress notes


Nothing to see here!

## Core


We add a new type of unboxed sums:

```wiki
|# t_1 | ... | t_n #|  -- stands for nth constructor out of m
```


There's an construction and elimination form. Construction:

```wiki
|#_n x_n #|
```


Elimination:

```wiki
case x of
    |#_1 x_1 #| -> ...
    ...
    |#_n x_n #| -> ...
```


code: compiler/prelude/TysWiredIn.hs

## Core to STG


Given the Core function

```wiki
f :: |# t_1 | ... | t_n #| -> ...
```


we convert it to a call to STG which includes the minimal number of arguments needed to catch all pointer and non-pointer arguments e.g.

```wiki
f args...
```


This should be done in compiler/simplStg/unariseStg.hs, just like it is for unboxed tuples.

## Code generation


code: codeGen/StgCmmCon
