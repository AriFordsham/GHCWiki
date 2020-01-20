# Pointer Tagging

In GHC we "tag" pointers to heap objects with information about the object they point to.  The tag goes in the low 2 bits (3 bits on a 64-bit platform) of the pointer, which would normally be zero since heap objects are always [word](commentary/rts/word)-aligned.

Key material
* Paper: [Faster laziness using dynamic pointer tagging](http://research.microsoft.com/en-us/um/people/simonpj/papers/ptr-tag/ptr-tagging.pdf)

* [List of pointer-tagging tickets](https://gitlab.haskell.org/ghc/ghc/issues?label_name%5B%5D=pointer+tagging), labelled with "pointer tagging".

* (2020) [Andreas K's pointer-tagging agenda](https://gitlab.haskell.org/ghc/ghc/wikis/AndreasPK#pointer-tagging-in-ghc)

## Meaning of the tag bits


The way the tag bits are used depends on the type of object pointed to:

- If the object is a **constructor**, the tag bits contain the *constructor tag*, if the number of
  constructors in the datatype is less than 4 (less than 8 on a 64-bit platform).  If the number of
  constructors in the datatype is equal to or more than 4 (resp 8), then the highest tag value
  indicates that the constructor tag must be extracted from the constructor's info table. The other
  tag values work the same as for small families (representing the constructor tag).
  See the table below under `Tagging of large and small  families` for an example.

- If the object is a **function**, the tag bits contain the *arity* of the function, if the arity fits
  in the tag bits.

- For a pointer to any other object (including a PAP), the tag bits are always zero.

### Tagging of large and small families

In the past small families where tagged with their constructor while large families, if tagged, where always `1` indicating an evaluate value.

This was changed recently, now we tag representing the constructor if it's small enough to be encoded. With the highest tag reserved to indicate any other constructor. See #14373 for more details.

Here is a table showing the difference for a platform with two bit sized tags.

| Pointing to | Tag Small Family | Tag Large Family (GHC 8.10 onwards) | Tag Large Family (GHC 8.8) |
| ---:     |  :--  | :-- | :-- | 
| Thunk | 0 | 0 | 0 | 
| Con1   | 1   | 1 | 1 - ConTag in info table | 
| Con2   | 2   | 2 | 1 - ConTag in info table | 
| Con3   | 3   | 3 - ConTag in info table | 1 - ConTag in info table | 
| Con4   | -   | 3 - ConTag in info table | 1 - ConTag in info table |

We can see how in the old scheme large families when evaluated where always tagged with `1` and the constructors tag had to be fetched from the info table.

In the new scheme this is only the case if we can't encode the constructor tag in the tag bits (while reserving the highest tag to indicate a tag needed to be fetched from the info table).

## Optimisations enabled by tag bits


The presence of tag bits enables certain optimisations:

- In a case-expression, if the variable being scrutinised has non-zero tag bits, then we know
  that it points directly to a constructor and we can avoid *entering* it to evaluate it.
  Furthermore, for datatypes with only a few constructors, the tag bits will tell us *which*
  constructor it is, eliminating a further memory load to extract the constructor tag from the
  info table.

- In a [generic apply](commentary/rts/haskell-execution/function-calls#generic-apply), if the function being applied has a tag value that indicates it has exactly the
  right arity for the number of arguments being applied, we can jump directly to the function, instead of
  inspecting its info table first.


Pointer-tagging is a fairly significant optimisation: we measured 10-14% depending on platform.  A large proportion of this comes from eliminating the indirect jumps in a case expression, which are hard to predict by branch-prediction.  The paper has full results and analysis.

### Dealing with tags in the code


Every time we dereference a pointer to a heap object, we must first zero the tag bits.  In the RTS, this is done with the inline function (previously: macro) `UNTAG_CLOSURE()`; in `.cmm` code this is done with the `UNTAG()` macro.  Surprisingly few places needed untagging to be added.


## Invariants


Pointer tagging is *not* optional, contrary to what the paper says.  We originally planned that it would be: if the GC threw away all the tags, then everything would continue to work albeit more slowly.  However, it turned out that in fact we really want to assume tag bits in some places:

- In the continuation of an algebraic case, R1 is assumed tagged
- On entry to a non-top-level function, R1 is assumed tagged


If we don't assume the value of the tag bits in these places, then extra code is needed to untag the pointer.  If we can assume the value of the tag bits, then we just take this into account when indexing off R1.


This means that everywhere that enters either a case continuation or a non-top-level function must ensure that R1 is correctly tagged.  For a case continuation, the possibilities are:

- the scrutinee of the case jumps directly to the alternative if R1 is already tagged.
- the constructor entry code returns to an alternative.  This code adds the correct tag.
- if the case alternative fails a heap or stack check, then the RTS will re-enter the alternative after
  GC.  In this case, our re-entry arranges to enter the constructor, so we get the correct tag by
  virtue of going through the constructor entry code.

## Functions (FUN closures)

For a non-top-level function, the cases are:

- unknown function application goes via `stg_ap_XXX` (see [Generic Apply](commentary/rts/haskell-execution/function-calls#generic-apply)).  
  The generic apply functions must therefore arrange to correctly tag R1 before entering the function.
- A known function can be entered directly, if the call is made with exactly the right number of arguments.
- If a function fails its heap check and returns to the runtime to garbage collect, on re-entry the closure
  pointer must be still tagged.
- the PAP entry code jumps to the function's entry code, so it must have a tagged pointer to the function
  closure in R1.  We therefore assume that a PAP always contains a tagged pointer to the function closure.


In the second case, calling a known non-top-level function must pass the function closure in R1, and this pointer *must* be correctly tagged.  The code generator does not arrange to tag the pointer before calling the function; it assumes the pointer is already tagged.  Since we arrange to tag the pointer when the closure is created, this assumption is normally safe.  However, if the pointer has to be saved on the stack, say across a call, then when the pointer is retrieved again we must either retag it, or be sure that it is still tagged.  Currently we do the latter, but this imposes an invariant on the garbage collector: all tags must be retained on non-top-level function pointers.


Pointers to top-level functions are not necessarily tagged, because we don't always know the arity of a function that resides in another module.  When optimisation is on, we do know the arities of external functions, and this information is indeed used to tag pointers to imported functions, but when optimisation is off we do not have this information.  For constructors, the interface doesn't contain information about the constructor tag, except that there may be an unfolding, but the unfolding is not necessarily reliable (the unfolding may be a constructor application, but in reality the closure may be a CAF, e.g. if any of the fields are references outside the current shared library).

## Partial applications (PAP closures)

See also the [function calls wiki page](https://gitlab.haskell.org/ghc/ghc/wikis/commentary/rts/haskell-execution/function-calls).

A partial application closure (PAP) represents an unsaturated
application of a function to one or more arguments.
The layout of a PAP is described in the [Layout of Heap Objects](https://gitlab.haskell.org/ghc/ghc/wikis/commentary/rts/storage/heap-objects) wiki page.

However PAPs are unusual in the following ways:

* The info pointer of a PAP points, as always, to an info table; but
  the entry code is "crash".  That is, a PAP should never be entered;
  it can be called only via the generic `stg_ap_pp` functions, which
  in turn call `stg_PAP_apply` (in `Apply.cmm`).

* A pointer to a PAP is always tagged 000 in its tag-bits; that is, we
  do not attempt to record, in the pointer, the evaluated-ness of the
  object.

**Invariant**: when calling a function, if the function's closure has a non-zero tag, it *must* be a FUN object (not a PAP), and the tag encodes the arity.  So we can call it by loading the arguments into registers and jumping to the closure's entry code.

The compiled code for
```
  revApp a f = f a
```
will call `stg_ap_p_fast` (passing f and a).  This RTS function will dispatch
on the tag of f: if it finds 001, it assumes that f points to FUN with arity 1,
so it pust the argument in the correct register and jumps to the function's
code.

PAPs are tagged 000. So `stg_ap_p` looks in the info table to see if
it is a thunk or a PAP (or possibly an un-tagged FUN); and behaves
accordingly.  For a PAP it jumps to `stg_PAP_apply` in `Apply.cmm`.

An alternative would be to use 111 for tagging PAPs, and make
that an illegal tag for FUN closures.

A wrinkle: see `Note [avoid intermediate PAPs]` in `GHC.StgToCmm.Layout`.


## Interaction with garbage collection

### Garbage collection with tagged pointers

The [garbage collector](commentary/rts/storage/gc) *preserves* tag bits on the pointers it traverses. It is crucial that it does not lose tagging information, both for performance (tagged pointers are faster) but also for correctness: see "Invariants" above.

Additionally, when the GC eliminates an indirection it takes the tag bits from the pointer inside the indirection.  Pointers to indirections always have zero tag bits.

The paper (section 6.1.4) suggests that the garbage collection could turn an untagged pointer to a constructor or function closure into a tagged pointer, since it is traversing the heap anyway.  However *we do not do this* for several reasons.

* Reconstructing tag bits would require that the GC knows not only the tag of the constructor (which is in the info table), but also the family size (which is currently not in the info table); see "Tagging of large and small families" above. To make this practical we would probably need different closure types for "small family" and "large family" constructors, and we already subdivide the constructor closures types by their layout. (Suggestion from SG: Encode small/large in a tag bit in the constructor tag field of the info table. But given the impact on mutator code and the next point, this is not worthwhile to pursue.)

* It is vanishingly rare to find an untagged pointer to a contructor.  [This comment](https://gitlab.haskell.org/ghc/ghc/merge_requests/1692#note_245358) found only 14 out of 18M such pointers were untagged.



### Compacting GC


Compacting GC also uses tag bits, because it needs to distinguish between a heap pointer and an info pointer quickly.  The compacting GC has a complicated scheme to ensure that pointer tags are retained, see the comments in [rts/sm/Compact.c](https://gitlab.haskell.org/ghc/ghc/blob/master/rts/sm/Compact.c).



## Gotchas where we surprisingly don't have tagged pointers.

Since pointer tagging is an important optimization GHC makes sure to apply it often. However there are a few cases where this surprisingly doesn't hold. This surfaced among other things in the issues #15155 and #14677

There are two different issues here:

1. Suppose a pointer P points to a constructor object. Then I would like the invariant that P is properly tagged -- eg it never has tag 0 meaning "thunk". I believe this is very nearly the case today, (see [this commeent](https://gitlab.haskell.org/ghc/ghc/merge_requests/1692#note_245358) in !1692, and may be the case *always* once we have nailed #17004.

1. Suppose a data constructor has a pointer Q in a strict field of type T, where T is a data type. Then Q is a properly-tagged pointer to a data constructor (of type T). This is what #15155 is about.

These are not the same! We could have (1) without (2) or (2) without (1).

### Failure to tag imported bindings

When a module refers to a top level binding from a different module this *won't* be tagged except for trivial cases where we reference nullary constructors with an available unfolding.

This was documented in #14677. !1530 is a WIP but is currently stalled on not directly related work.

### Strict fields containing untagged pointers - #15155

One might assume that strict fields by their nature can only contain tagged pointers. This is however not true as they only require to uphold their semantics which allows indirections (or even thunks in theory).

In particular we can (and do) end up with indirections in strict fields. This is discussed in #15155. But imported top level bindings could also end up (untagged) inside strict fields.

So the (known) sources of untagged pointers in strict fields are:
* Untagged static constructor pointers (see `Failure to tag imported bindings` on this page).
* Static indirections which are the result of things like coercions. See #16831 for an example.
* Indirections resulting from float out.

The last one comes from code code like this:

```
bar = <thunk>
foo = SJust bar

baz = case bar of
   _ -> ... foo ...
```

Clearly bar *is* evaluated before `foo` is demanded. But `foo` will still only contain an indirection to `bar`, and we never tag indirections.

Changing this would be beneficial for performance as it allows avoidance of the "check tag and enter" code when accessing strict fields.

!1472 is a WIP patch for one approach to changing this working at the STG level. In extreme cases like set lookups for Data.Set this improved performance by >10%. However the implementations is also far from trivial so it's not yet clear if this is the best approach. See also the related [ticket](https://gitlab.haskell.org/ghc/ghc/issues/16970#strict-fields)