# Unlifted data types


This page describes the unlifted data types, i.e. algebraic data types which live in kind `#` rather than kind \*. In fact, this is a collection of standalone proposals:

1. Allow data types to be declared unlifted. (Should be easy; but has a harder subproposal to separate a new kind `Unlifted` from `#`)
1. Allow newtypes over unlifted types (giving them kind `#`). (Should be easy.)
1. Provide a built-in `Force a` which is an unlifted version of `a`, with no indirection cost. (Harder.)

## Motivation


Bob Harper [ has written](https://existentialtype.wordpress.com/2011/04/24/the-real-point-of-laziness/):

>
> Haskell suffers from a paucity of types.  It is not possible in Haskell to define the type of natural numbers, nor the type of lists of natural numbers (or lists of anything else), nor any other inductive type!


The reason, of course, is that whenever you write `data Nat = Z | S !Nat`, you define a type of strict natural numbers, AS WELL AS bottom. Ensuring that an `x :: Nat` is never bottom requires all use-sites of this type to do strict pattern matching / force the value appropriately. It would be nice if there was some type-directed mechanism which specified that a value `x` was always evaluated. This would give benefits, e.g. for code generation, where we can assume that a pointer never points to a thunk or indirection.


It would be hard to justify drastically changing Haskell to allow defining a type which doesn't include bottom, but as it turns out, GHC *already* supports such non-bottom types, in the form of unlifted types of kind `#`. In particular, we already have special strict evaluation rules for unlifted types like `Int#` and `Array#`.


The fact that a pointer never points to a thunk is especially helpful in the implementation of mutable variables: without this guarantee, code which writes to this mutable variable has to first check if the thunk is fully evaluated before actually performing the memory write. For this reason, the `MutVar#` primitive (which is a GC'd object) lives in kind `#`.

## Proposal 1: Allow data types to be declared as unlifted


A data type can be declared as unlifted by writing `data unlifted`, e.g.:

```wiki
data unlifted UBool = UTrue | UFalse
```


Such data types are always boxed, but the type does not include bottom and is operationally represented as a pointer to the value. Intuitively, if you have `x :: UBool` in scope, you are guaranteed to have `UTrue` or `UFalse`, and not bottom.


The evaluation rules for unlifted data types are identical to the existing rules we have for types kinded `#`: lets are strict, cannot be recursive, and function arguments are evaluated before calls. For example:

```wiki
unot :: UBool -> UBool
unot UTrue = UFalse
unot UFalse = UTrue

main :: IO ()
main = let y = unot (error "foo")
       in return ()
```


In this example, we get the error "foo", rather than returning `()`, because the binding of `y` must be evaluated strictly.

**Non-polymorphic unlifted types can directly be unpacked.** The following declaration is valid:

```wiki
data unlifted StrictInt = StrictInt Int#
data MyInt = MyInt {-# UNPACK #-} StrictInt
```


and is representationally equivalent to `MyInt'` here:

```wiki
data Int = Int Int#
data MyInt' = MyInt' {-# UNPACK #-} !Int
```


Of course, the constructors for `MyInt` and `MyInt'` have different types.

## Proposal 2: Polymorphism over a new Unlifted kind


Currently, we have two different kinds (ahem) of unlifted types which live in `#`: unboxed, unlifted types such as `Int#`, and boxed, unlifted types such as `Array#` and the unlifted data types we can now define.


This subproposal is to distinguish between these two kinds, calling boxed, unlifted kinds `Unlifted`. If users start defining and using unlifted types in normal code,  it is likely that they will request polymorphism over unlifted types. There are two particular cases of polymorphism we might like to support:

**Polymorphism over unlifted types in types and functions.** In data types and functions, we may want to be polymorphic over a type variable in kind `Unlifted`:

```wiki
data unlifted UList (a :: Unlifted)
  = UNil | UCons a UList
umap :: forall (a :: Unlifted) (b :: Unlifted).
        UList a -> (a -> b) -> UList b
```


We cannot be polymorphic in `#` in general, because this includes unboxed types like `Int#` which don't have uniform representation. However, we can be polymorphic over unlifted types, which do have uniform representation.

**Boxed levity polymorphism in types (and functions with extra code generation).** In data types, we may want to have a type parameter which is polymorphic over all boxed types:

```wiki
data BList (a :: Boxed)
  = BNil | BCons a BList
```

`BList` is representationally the same whether or not it is instantiated with a boxed lifted type, or a boxed unlifted type.


However, for levity polymorphism over functions we must generate code twice. Consider::

```wiki
map :: forall a (b :: Boxed). (a -> b) -> BList a -> BList b
map f (BCons x xs) = BCons (f x) (map f xs)
```


We do not know if `f x` should be evaluated strictly or lazily; it depends on whether or not `b` is unlifted or lifted. This case can be handled by specializing `map` for the lifted and unlifted cases.  The fact that the semantics of the function change depending on the type is a bit questionable (though not entirely unprecedented, c.f. type classes); additionally, there isn't any reason why we couldn't also generate copies of the code for all unboxed types, dealing with `Int#`, `Float#` and `Double#`: it's unclear when to stop generating copies. (For reference, .NET does this on the fly.)

## Proposal 3: Allow newtypes over unlifted types


To allow cost-free abstraction over unlifted types, we should allow newtypes to be written over types of kind `#`, with the resulting newtype being in kind `#`. For example:

```wiki
newtype MyInt# = MkInt# Int#
```


with `MyInt# :: #`. GHC already supports coercions over kind `#`, so this should be very simple to implement.

## Proposal 4: Allow unlifting existing data types with no overhead


Proposal 1 requires a user to define a new data type for every unlifted type they want to define. However, for every lifted data type a user can define, there is an obvious unlifted type one might be interested in: the one without bottom. Fortunately, we can define a data type to unlift an arbitrary lifted type:

```wiki
data Force :: * -> Unlifted where
  Force :: !a -> Force a

suspend :: Force a -> a
suspend a = a
```

`Force a` is the "head strict" version of `a`: if you have `x :: Force Int` in scope, it is guaranteed to have already been evaluated to an `Int`. We can also suspend these strict computations: `suspend (error "foo" :: Int#)` does not error until forced. Like `Box`, unlifted computations may not be lifted out of `suspend` without changing the semantics.

`Force` and `suspend` can be written purely as library code, however there is a cost of an indirection of `Force`. We might notice, however, that the value of type `Force a` only admits the value `Force a`: `undefined` is excluded by the `Unlifted` kind, and `Force undefined` is excluded by the strict field.  Thus, it would be great if we could represent `Force` on the heap simply as an unlifted pointer to `a`, which is never undefined.


Ideally, we would like to define the coercion `Coercible (Force a) a`, to witness the fact that `Force a` is representationally the same as `a`. However, there are two problems:

1. This coercion is ill-kinded (`Force a` has kind `Unlifted` but `a` has kind `*`), so we would need John Major equality style coercions.

1. The coercion is only valid in one direction: I can coerce from `Force a` to `a`, but not vice-versa: in the other direction, evaluation may be necessary.


My suggested implementation strategy is to bake in `Force` as a special data type, which is represented explicitly in Core, but then optimized away in STG.

## Optional extensions

**(OPTIONAL) Give some syntax for `Force`.** Instead of writing `f :: Force Int -> Force Int`, we might like to write `f :: Int! -> Int!`. We define post-fix application of bang to be a wrapping of `Force`.

**(OPTIONAL) Introduce a pattern synonym `Thunk`.**`suspend` can be generalized into the bidirectional pattern synonym `Thunk`:

```wiki
pattern Thunk a <- x | let a = Force x
  where Thunk (Force a) = a
```


For example:

```wiki
let x = Thunk (error "foo" :: Force Int) :: Int
in True
```


does not error. Pattern matching over `Thunk` forces the argument (similar to bang patterns) and returns the unlifted value (unlike bang patterns):

```wiki
let Thunk x = 3 + 5 :: Int
in x :: Force Int
```

## Dynamic semantics of unlifted types


In this section, we review the dynamic semantics of unlifted types.  These are not being added by our proposal (since they are already implemented by `#`), but since they are fairly unfamiliar to most Haskell users, I think this section will be useful.

**Case binding.** Given `case e of x1 -> e1`, where `e` is `Unlifted`, `e` is evaluated to whnf, and then the result is case-matched upon. (i.e. it is always as if it is a strict pattern match.)

**Let bindings.** Given `let x = e in e'`, where `x` is `Unlifted)`, this desugars to `let !x = e in e'` which desugars to `case e of !x -> e'`. Mutually recursive let bindings of unlifted variables are not allowed. Let bindings are evaluated bottom up (but see [\#10824](https://gitlab.haskell.org//ghc/ghc/issues/10824)).

**Conditionals.** Given `if e then e1 else e2` where `e1` and `e2` are `Unlifted`, this desugars into the obvious case. (NB: this means `e1` and `e2` are not eagerly evaluated, as they would be for an `ifthenelse` function.)

**Constructors and function application.** Given `K e` where `e` is `Unlifted`, this desugars into `case e of !x -> K x`. NB: if `K` is in kind star, then this expression admits bottom!


Intuitively, if you have an unlifted type, anywhere you let bind it or pass it to a function, you evaluate it. Strict let bindings cannot be arbitrarily floated; you must preserve the ordering of bindings and they cannot be floated beyond an expression kinded `*`.

## FAQ

**Why do you get to use error in the unlifted examples, isn't error a bottom?**`error` is specially treated to be both lifted and unlifted. It's interpretation in an unlifted setting is that it immediately causes an exception when it is evaluated (it never goes into the heap).

**Why not `!Int` rather than `Int!` as the syntax proposal?** This syntax conflicts with strict data fields. `data S a = S !a` has a constructor of type `S :: Int -> S`, taking a lifted type and evaluating it before placing it in the constructor; `data S a = S a!` has a constructor of type `S :: Force Int -> S`, which requires the *user* to have forced the integer. Representationally, the data types are the same, but the outward behavior for clients differs dramatically.

**Is `Force (Maybe (Force Int))` allowed?** No, because `Force Int` has kind `Unlifted` but `Maybe` has kind `* -> Unlifted`. A data type declaration must be explicitly written to accept an unlifted type (`data StrictMaybe (a :: Unlifted) = SMaybe a`), or simply be strict in its field (`data StrictMaybe2 a = SMaybe2 !a`).

**How does this affect inlining?** In any CBV language, inlining doesn't always preserve semantics; unlifted types are no different. For example, this errors:

```wiki
  let x = error "foo" :: Force Int
      y = suspend x :: Int
  in True
```


but this example does not:

```wiki
  let y = suspend (error "foo" :: Force Int) :: Int
  in True
```

**What's the difference between `Force Int` and `Int#`?**`Force Int` is an unlifted, boxed integer; `Int#` is an unlifted, unboxed integer.

**Why aren't strict patterns enough?** A user can forget to write a strict pattern at a use-site. Putting a type in kind unlifted forces all use-sites to act as if they had strict patterns.

**Is `Force [a]` the type of strict lists?** No. It is the type of a lazy list whose head is always evaluated and non-bottom.

**Does `foo :: Force [a] -> Force [a]` force just the first constructor or the whole spine?** You can't tell; the head not withstanding, `[a]` is still a lazy list, so you would need to look at the function body to see if any extra forcing goes on. `Force` does not induce `seq`ing: it is an obligation for the call-site.
