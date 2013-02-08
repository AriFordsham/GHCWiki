# Implementing primitive Bool\#


This page gathers the notes about implementing primitive logical operations and thus resolving ticket [\#6135](https://gitlab.haskell.org//ghc/ghc/issues/6135).

## The problem


Consider following fragment of code:

```wiki
 case (x <# 0#) || (x >=# width) || (y <# 0#) || (y >=# height) of
  True  -> E1
  False -> E2
```


This kind of code is common in image processing (and array programming in general) where one needs to check whether the `(x,y)` coordinates are within the image. Primitive comparison operators `<#` and `>=#` have type `Int# -> Int# -> Bool`. Logical OR operator `(||)` is defined as:

```wiki
(||)       :: Bool -> Bool -> Bool
True  || _ =  True
False || x =  x
```


in GHC.Classes (ghc-prim library) which is equivalent of:

```wiki
(||) x y = case x of
            True  -> True
            False -> y
```


During the compilation process (assuming the optimizations are turned on) the definition of `(||)` gets inlined and then case-of-case transform is performed successively. This results in following Core (cleaned up for clarity):

```wiki
case <# x 0 of _ {
  False ->
    case >=# x width of _ {
      False ->
        case <# y 0 of _ {
          False ->
            case >=# y height of _ {
              False -> E2
              True  -> E1
            };
          True -> E1
        };
      True -> E1
    };
  True -> E1
};
```


and in following assembler code:

```wiki
.Lc1rf:
        testq %r14,%r14
        jl .Lc1rk
        cmpq %rdi,%r14
        jge .Lc1rp
        testq %rsi,%rsi
        jl .Lc1ru
        cmpq %r8,%rsi
        jge .Lc1rz
        movl $Main_g2_closure+1,%ebx
        jmp *0(%rbp)
.Lc1rk:
        movl $Main_g1_closure+1,%ebx
        jmp *0(%rbp)
.Lc1rp:
        movl $Main_g1_closure+1,%ebx
        jmp *0(%rbp)
.Lc1ru:
        movl $Main_g1_closure+1,%ebx
        jmp *0(%rbp)
.Lc1rz:
        movl $Main_g1_closure+1,%ebx
        jmp *0(%rbp)
```


There are five possible branches to take, although four of them have the same result. This is caused by code duplication introduced by case-of-case transform (see [ this blog post](http://ics.p.lodz.pl/~stolarek/blog/2013/01/taking-magic-out-of-ghc-or-tracing-compilation-by-transformation/) for a step by step derivation). According to Ben Lippmeier, who submitted the original bug report, mis-predicted branches are bad in object code because they stall the pipeline.

## Workarounds


It is possible to work around the issue of code duplication by using GHC primops `tagToEnum#` and `dataToTag#`. These allow to distinguish between `True` and `False` by means of accessing the tag of a data type constructor. This means that `dataToTag#` can convert `True` to `1#` and `False` to `0#`, while `tagToEnum#` does the opposite (see paper [ Faster Laziness Using Dynamic Pointer Tagging](http://research.microsoft.com/en-us/um/people/simonpj/papers/ptr-tag/index.htm) for more details): 

```wiki
ghci> import GHC.Exts
ghci> import GHC.Prim
ghci> :set -XMagicHash
ghci> I# (dataToTag# True)
1
ghci> I# (dataToTag# False)
0
ghci> (tagToEnum# 0#) :: Bool
False
ghci> (tagToEnum# 1#) :: Bool
True
ghci>
```


Having the possibility of converting `Bool` to an unboxed `Int#` allows us to compute results of logical expression by means of logical bitwise operations. The result can be converted back to a `Bool` so this is transparent on the Haskell source level, except for the fact that defined logical binary operators will be strict in both their arguments. 

**NOTE: Validity of this solution is based on assumption that `True` will always have a tag of `1#`, while `False` will have a tag of `0#`. Changing this invariant in the future would make these primitive logical operators invalid.**

### First workaround


First workaround assumes converting each result of comparison into an unboxed `Int` and replacing `||` with `+#`:

```wiki
 case (dataToTag# (x <# 0#)) +# (dataToTag# (x >=# width)) +#
      (dataToTag# (y <# 0#)) +# (dataToTag# (y >=# height)) of
  0# -> E2 -- note that branch order is reversed
  _  -> E1
```


This compiles to:

```wiki
case +#
       (+#
          (+# (dataToTag# (<# x 0)) (dataToTag# (>=# x width)))
          (dataToTag# (<# y 0)))
       (dataToTag# (>=# y height))
of _ {
  __DEFAULT -> E1;
  0 -> E2
}
```


Similarly we can convert logical && into multiplication.

### Second workaround


The above workaround is a bit clumsy: `dataToTag#`s make the code verbose and it may not be very obvious what the code is doing. Hence the second workaround, that defines an alternative logical `or` operator:

```wiki
(||#) :: Bool -> Bool -> Bool
(||#) x y = let xW = int2Word# (dataToTag# x)
                yW = int2Word# (dataToTag# y)
                zI = word2Int# (yW `or#` xW)
            in tagToEnum# zI
```


This operator is defined in terms of primops `dataToTag#`, `tagToEnum#` and a bitwise or primop `or#`. Since the last one operates only on `Word`s we need to use `int2Word#` and `word2Int#` for conversion between these data types. Luckily, GHC does a good job of removing unnecessary conversions between data types. This means that:

```wiki
 case (x <# 0#) ||# (x >=# width) ||# (y <# 0#) ||# (y >=# height) of
  True  -> E1
  False -> E2
```


compiles to:

```wiki
case tagToEnum#
       (word2Int#
          (or#
             (int2Word# (dataToTag# (>=# y height)))
             (or#
                (int2Word# (dataToTag# (<# y 0)))
                (or#
                   (int2Word# (dataToTag# (>=# x width)))
                   (int2Word# (dataToTag# (<# x 0)))))))
of _ {
  False -> E2;
  True  -> E1
}
```


Primitive logical operators `&&#` and `not#` can be defined in a similar matter.

**NOTE: Neither of this two workarounds produces good object code. The reason is that comparison operators return a `Bool` as a thunk that needs to be evaluated. The real solution requires that no thunk is created.**

## Solutions


A good beginning would be to implementing second of the above workarounds as a primop. Then we need to create primops that return unboxed values instead of a thunk. The big question is should an unboxed version of Bool be introduced into the language?

### First approach


Treat `Bool` as a boxed version of primitive `Bool#`. `True` would be equivalent of `B# True#`, `False` of `B# False#`:

```wiki
data Bool = B# True# | B# False#

-- B# :: Bool# -> Bool
```


Not sure if this can be considered equivalent to what the Haskell Report says about Bool. We need to ensure that `Bool#` is populated only by `True#` and `False#` and that these two are translated to `1#` and `0#` in the Core. It should be **impossible** to write such a function at Haskell level:

```wiki
g :: Bool -> Int -> Int
g (B# b) (I# i) = I# (b + i)
```


This approach might require one additional case expression to inspect the value of `Bool` at the Core level. For example:

```wiki
f :: Int -> Int -> Int
f x y = if x > y
        then x
        else y
```


would compile to:

```wiki
case x of _ { I# xP ->
 case y of _ { I# yP ->
   case ># xP yP of _ {
     B# bP -> case bP of _ { 1# -> e1; 0# -> e2 }
   }
 }
}
```


This would complicate Core a bit but it should be possible to compile such Core to exactly the same result as with normal `Bool`. This code assumes that `>#` has type `Int# -> Int# -> Bool`, but to truly avoid branching in the Core we need `.># :: Int# -> Int# -> Bool#` so that we get a primitive value that doesn't need to be inspected using case expression but can be directly used by primitive logical operators.

### Second approach


Second approach assumes creating type `Bool#` that is independent of type `Bool`. Boxing and unboxing would have to be done explicitly via additional functions:

```wiki
data Bool = True | False -- no changes here

bBox :: Bool# -> Bool
bBox 1# = True
bBox 0# = False

bUnbox :: Bool -> Bool#
bUnbox True  = 1#
bUnbox False = 0#
```

`Bool#` could not be implemented as an ADT because it is unlifted and unboxed, while ADT value constructors need to be boxed and lifted (see comments in [compiler/types/TyCon.lhs](/trac/ghc/browser/ghc/compiler/types/TyCon.lhs)). There would need to be some magical way of ensuring that `Bool#` is populated only by `#0` and `1#` and that these values cannot be mixed with unboxed integers. Perhaps this could be done by preventing programmer from explicitly creating values of that type (can this be done?) and allow her only to use values returned from functions.


Another problem with this approach is that it would introduce primitive logical operations `||#` and `&&#` with type `Int# -> Int# -> Int#` - it is questionable whether anyone would want such operations available to the programmer. I think it is desirable to have primitive logical operators of type `Bool# -> Bool# -> Bool#`.

## Places of interest in the source code


The file [prelude/primops.txt.pp](/trac/ghc/browser/ghc/prelude/primops.txt.pp) defines PrimOps and their type signatures. An example definition looks like this:

```wiki
primop   IntGtOp  ">#"   Compare   Int# -> Int# -> Bool
   with fixity = infix 4
```


Existing definitions should remain unchanged or the code using them would break and that is a Very Bad Thing. This would require creating new PrimOps:

```wiki
primop   IntGtOpB  ".>#"   Compare   Int# -> Int# -> Bool#
   with fixity = infix 4
```


The tricky part here is `Compare`. This a value constructor of `PrimOpInfo` data type defined in [prelude/PrimOp.lhs](/trac/ghc/browser/ghc/prelude/PrimOp.lhs):

```wiki
data PrimOpInfo
  = Dyadic      OccName         -- string :: T -> T -> T
                Type
  | Monadic     OccName         -- string :: T -> T
                Type
  | Compare     OccName         -- string :: T -> T -> Bool
                Type
  | GenPrimOp   OccName         -- string :: \/a1..an . T1 -> .. -> Tk -> T
                [TyVar]
                [Type]
                Type
```


We would need new `PrimOpInfo` value to denote PrimOps of type `T -> T -> Bool#`. Appropriate functions like `primOpSig` and `getPrimOpResultInfo` would have to be adjusted accordingly.
