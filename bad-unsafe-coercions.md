## Rationale

`unsafeCoerce#` allow unsafe coercions between different types.
According to the [ documentation in GHC.Prim](http://hackage.haskell.org/package/ghc-prim-0.3.1.0/docs/GHC-Prim.html#v:unsafeCoerce-35-), it is safe only for following uses:

- Casting any lifted type to `Any`

- Casting `Any` back to the real type

- Casting an unboxed type to another unboxed type of the same size (but not coercions between floating-point and integral types)

- Casting between two types that have the same runtime representation. One case is when the two types differ only in "phantom" type parameters, for example `Ptr Int` to `Ptr Float`, or `[Int]` to `[Float]` when the list is known to be empty. 

- Casting between a newtype of a type `T` and `T` itself.  (**RAE:** This last usecase is subsumed by `Data.Coerce.coerce`, at least when the newtype constructor is in scope.)


However GHC doesn't check if it's safe to use `unsafeCoerce#` as a result bugs can appear, see [9035](https://gitlab.haskell.org//ghc/ghc/issues/9035).
In order to solve this problem a solution was proposed by Simon in [9122](https://gitlab.haskell.org//ghc/ghc/issues/9122), quote:

>
> I think it would be a great idea for Core Lint to check for uses of `unsafeCoerce` that don't obey the rules. It won't catch all cases, of course, but it would have caught [\#9035](https://gitlab.haskell.org//ghc/ghc/issues/9035). 


This proposal is about implementation of the task.

## Progress


Current progress could be found on [ D637](https://phabricator.haskell.org/D637)([ https://phabricator.haskell.org/D637](https://phabricator.haskell.org/D637)). It implements
proposed checks modulo few questions mentioned in this proposal. 


The solution introduces following
changes in the core specification, `docs/core-spec/CoreLint.ott` in the source tree ([ PDF here](https://github.com/ghc/ghc/blob/master/docs/core-spec/core-spec.pdf)):

```wiki
-G |-ty t1 : k
------------------------------ :: UnivCo
-G |-co t1 ==>!_R t2 : t1 ~R k t2
+G |-ty t1 : k1
+G |-ty t2 : k2
+isUnLiftedTy t1 = isUnLiftedTy t2
+(not (isUnLiftedTy t1)) \/ ((activeSizeTy t1 = activeSizeTy t2) /\ (isFloatingTy t1 = isFloatingTy t2))
+-------------------------------------------------------------------------------------- :: UnivCo
+G |-co t1 ==>!_R t2 : t1 ~R k2 t2
```


Basically it introduces three new predicates in UnivCo rule:

1. Both types should be lifted or both types should be unlifted (Qnikst: note that original task forbids coercion between lifted and *unboxed*)

1. If types are unlifted then their activeSize should be equal. (**SPJ**: what is "active size"?)

1. If types are unlifted then they either should be both floating or both integral

## Questions


There are few dark places in this semantics change that should be clarified

### Size of value


GHC has 2 different sizes: word aligned size of values, and active size in bytes that actually used.  **SPJ**: where do you see these two different sizes in GHC's source code?


The question is if we need to allow coercion between values with same word size, but different active size.
(Qnikst. current implementation forbids it, as values with different active size can contain garbage, however coercion from value with bigger active size to value with smaller potentially should be fine).

### Unboxed Tuples


A big question is how to treat unboxed tuples if they have same size, can we coerce between `(# Int, Int64 #)` and \`(\# Int64, Int \#)'?

**SPJ**: I think it should be ok to coerce from `(# a, b #)` to `(# c,d #)` if it's safe to coerce from `a` to `c`, and ditto `b` to `d`.  The tuples must have the same length.


How to check is value is floating in this case?  **SPJ** I don't understand the question.


(Qnikst. current implementation allow such coercions and doesn't check "floatiness" of values)

### User programs


Should those check be applied only to internal GHC's transformations, for user programs should also be
checked?

**SPJ** Both ideally.  Emit a warning for uer programs with visible problems.  And check in Lint.  Start with the latter.

**RAE** So that means that a warning would be issued, followed by a CoreLint failure. This violates the invariant that CoreLint catches only GHC's mistakes. I loosely agree with this approach here (because I want to allow users to do terrible things if they really want to), but we'll have to be careful about wording the error message that CoreLint spits out. This also implies that users who are actively trying to shoot themselves in the foot will have to avoid `-dcore-lint`, which is slightly dissatisfying. Maybe add a flag asking whether or not CoreLint should perform these checks? I guess my tension stems from the fact that we want to protect most users from mistakes and want to detect mistakes in GHC, while still allowing crazy things to happen. (Like still exporting [ this function](https://github.com/haskell/bytestring/blob/2530b1c28f15d0f320a84701bf507d5650de6098/Data/ByteString/Internal.hs#L599).)

## Implementors


implementation: Alexander Vershilov / Qnikst


advisor: Richard Eisenberg / goldfire / RAE
