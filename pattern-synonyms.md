# Pattern Synonyms


Most language entities in Haskell can be named so that they can be abbreviated instead of written out in full.
This proposal provides the same power for patterns.


See the [implementation](pattern-synonyms/implementation) page for implementation details.


Relevant closed tickets:

- [\#5144](https://gitlab.haskell.org//ghc/ghc/issues/5144) (Pattern synonyms [ merged into master](https://github.com/ghc/ghc/commit/4f8369bf47d27b11415db251e816ef1a2e1eb3d8) on 20 January 2014)
- [\#8968](https://gitlab.haskell.org//ghc/ghc/issues/8968) (GADTs)
- [\#9417](https://gitlab.haskell.org//ghc/ghc/issues/9417) (Haddock)
- [\#9514](https://gitlab.haskell.org//ghc/ghc/issues/9514) (Haddock again)
- [\#8584](https://gitlab.haskell.org//ghc/ghc/issues/8584) (pattern type signatures)


Relevant open tickets:

- [\#8581](https://gitlab.haskell.org//ghc/ghc/issues/8581) (explicitly bidirectional)
- [\#8582](https://gitlab.haskell.org//ghc/ghc/issues/8582) (Record patterns)
- [\#8583](https://gitlab.haskell.org//ghc/ghc/issues/8583) (Associated patterns)
- [\#8761](https://gitlab.haskell.org//ghc/ghc/issues/8761) (template haskell support)
- [\#8779](https://gitlab.haskell.org//ghc/ghc/issues/8779) (exhaustiveness checks)
- [\#10783](https://gitlab.haskell.org//ghc/ghc/issues/10783) (partial type signatures in pattern synonym signatures)
- [\#10653](https://gitlab.haskell.org//ghc/ghc/issues/10653) (Associated pattern synonyms with types)
- [\#10932](https://gitlab.haskell.org//ghc/ghc/issues/10932) (View patterns with arguments)

## Motivating example


Here is a simple representation of types

```wiki
    data Type = App String [Type]
```


Using this representations the arrow type looks like `App "->" [t1, t2]`.
Here are functions that collect all argument types of nested arrows and recognize the `Int` type:

```wiki
   collectArgs :: Type -> [Type]
   collectArgs (App "->" [t1, t2]) = t1 : collectArgs t2
   collectArgs _ = []

   isInt (App "Int" []) = True
   isInt _ = False
```


Matching on `App` directly is both hard to read and error prone to write.


The proposal is to introduce a way to give patterns names:

```wiki
   pattern Arrow t1 t2 = App "->" [t1, t2]
   pattern Int = App "Int" []
```


And now we can write

```wiki
   collectArgs :: Type -> [Type]
   collectArgs (Arrow t1 t2) = t1 : collectArgs t2
   collectArgs _ = []

   isInt Int = True
   isInt _ = False
```


Here is a second example from [ pigworker on Reddit](http://www.reddit.com/r/haskell/comments/1kmods/patternsynonyms_ghc_trac/).
Your basic sums-of-products functors can be built from this kit.

```wiki
newtype K a        x  = K a
newtype I          x  = I x
newtype (:+:) f g  x  = Sum (Either (f x) (g x))
newtype (:*:) f g  x  = Prod (f x, g x)
```


and then you can make recursive datatypes via

```wiki
newtype Fix f = In (f (Fix f))
```


e.g.,

```wiki
type Tree = Fix (K () :+: (I :*: I))
```


and you can get useful generic operations cheaply because the functors in the kit are all `Traversable`, admit a partial zip operation, etc.


You can define friendly constructors for use in expressions

```wiki
leaf :: Tree
leaf = In (Sum (Left (K ())))
node :: Tree -> Tree -> Tree
node l r = In (Sum (Right (Prod (I l, I r))))
```


but any `Tree`-specific pattern matching code you write will be wide and obscure. Turning these definitions into pattern synonyms means you can have both readable type-specific programs and handy generics without marshalling your data between views.

## Uni-directional (pattern-only) synonyms


The simplest form of pattern synonyms is the one from the examples above.  The grammar rule is:

`pattern`*conid**varid<sub>1</sub>* ... *varid<sub>n</sub>*`<-`*pat*

`pattern`*varid<sub>1</sub>**consym**varid<sub>2</sub>*`<-`*pat*

- Each of the variables on the left hand side must occur exactly once on the right hand side 
- Pattern synonyms are not allowed to be recursive.  Cf. type synonyms.

<table><tr><th>
There have been several proposals for the syntax of defining pattern-only synonyms:

- `pattern`*conid**varid<sub>1</sub>* ... *varid<sub>n</sub>*`~`*pat*
- `pattern`*conid**varid<sub>1</sub>* ... *varid<sub>n</sub>*`:=`*pat*
- `pattern`*conid**varid<sub>1</sub>* ... *varid<sub>n</sub>*`->`*pat*
- `pattern`*conid**varid<sub>1</sub>* ... *varid<sub>n</sub>*`<-`*pat*

</th></tr></table>


Pattern synonyms can be exported and imported by prefixing the *conid* with the keyword `pattern`:

```wiki
   module Foo (pattern Arrow) where ...
```


This is required because pattern synonyms are in the namespace of constructors, so it's perfectly valid to have

```wiki
   data P = C
   pattern P = 42
```


You may also give a type signature for a pattern, but as with most other type signatures in Haskell it is optional:

`pattern`*conid*`::`*type*


E.g.

```wiki
   pattern Arrow :: Type -> Type -> Type
   pattern Arrow t1 t2 <- App "->" [t1, t2]
```


Together with [ViewPatterns](view-patterns) we can now create patterns that look like regular patterns to match on existing (perhaps abstract) types in new ways:

```wiki
import qualified Data.Sequence as Seq

pattern Empty <- (Seq.viewl -> Seq.EmptyL)
pattern x :< xs <- (Seq.viewl -> x Seq.:< xs)
pattern xs :> x <- (Seq.viewr -> xs Seq.:> x)
```

## Simply-bidirectional pattern synonyms


In cases where *pat* is in the intersection of the grammars for patterns and expressions (i.e. is valid both as an expression and a pattern), the pattern synonym can be made bidirectional, and can be used in expression contexts as well. Bidirectional pattern synonyms have the following syntax:

`pattern`*conid**varid<sub>1</sub>* ... *varid<sub>n</sub>*`=`*pat*

`pattern`*varid<sub>1</sub>**consym**varid<sub>2</sub>*`=`*pat*


For example, the following two pattern synonym definitions are rejected, because they are not bidirectional (but they would be valid as pattern-only synonyms)

```wiki
   pattern ThirdElem x = _:_:x:_
   pattern Snd y = (x, y)
```


since the right-hand side is not a closed expression of {*x*} and {*y*} respectively.


In contrast, the pattern synonyms for *Arrow* and *Int* above are bidirectional, so you can e.g. write:

```wiki
   arrows :: [Type] -> Type -> Type
   arrows = flip $ foldr Arrow
```

## Explicitly-bidirectional pattern synonyms


What if you want to use `Succ` in an expression:

```wiki
    pattern Succ n <- n1 | let n = n1 -1, n >= 0
```


It's clearly impossible since its expansion is a pattern that has no meaning as an expression.
Nevertheless, if we want to make what looks like a constructor for a type we will often want to use it in both patterns and expressions.
This is the rationale for the most complicated synonyms, the bidirectional ones.  They provide two expansions, one for patterns and one for expressions.

`pattern`*conid**varid<sub>1</sub>* ... *varid<sub>n</sub>*`<-`*pat*`where`*cfunlhs**rhs*


where *cfunlhs* is like *funlhs*, except that the functions symbol is a *conid* instead of a *varid*.


Example:

```wiki
   pattern Succ n <- n1 | let n = n1-1, n >= 0 where
      Succ n = n + 1
```

**TODO**: Rewrite this example to not use [ViewPatternsAlternative](view-patterns-alternative)


The first part as is before and describes the expansion of the synonym in patterns. The second part describes the expansion in expressions.

```wiki
   fac 0 = 0
   fac (Succ n) = Succ n * fac n 
```

## Associated pattern synonyms


Just like data types and type synonyms can be part of a class declaration, it would be possible to have pattern synonyms as well.


Example:

```wiki
   class ListLike l where
      pattern Nil :: l a
      pattern Cons :: a -> l a -> a
      isNil :: l a -> Bool
      isNil Nil = True
      isNil (Cons _ _) = False
      append :: l a -> l a -> l a

   instance ListLike [] where
      pattern Nil = []
      pattern Cons x xs = x:xs
      append = (++)

   headOf :: (ListLike l) => l a -> Maybe a
   headOf Nil = Nothing
   headOf (Cons x _) = Just x
```


One could go one step further and leave out the `pattern` keyword to obtain *associated constructors*, which are required to be bidirectional. The capitalized identifier would indicate that a pattern synonym is being defined. For complicated cases one could resort to the `where` syntax (shown above).

**TODO**: Syntax for associated pattern synonym declarations to discern between pattern-only and bidirectional pattern synonyms

## Static semantics


A unidirectional pattern synonym declaration has the form

```wiki
pattern P var1 var2 ... varN <- pat
```


The formal pattern synonym arguments `var1`, `var2`, ..., `varN` are brought
into scope by the pattern pat on the right-hand side. The declaration
brings the name `P` as a pattern synonym into the module-level scope.


The pattern synonym `P` is assigned a *pattern type* of the form

```wiki
pattern P :: CProv => CReq => t1 -> t2 -> ... -> tN -> t 
```


where `t1`, ..., `tN` are the types of the parameters `var1`, ..., `varN`, `t` is the simple type (with no context) of the thing getting matched, and `CReq` and `CProv` are type contexts.

`CReq` can be omitted if it is empty. If `CProv` is empty, but `CReq` is not, `()` is used. The following example shows cases:

```wiki
data Showable where
    MkShowable :: (Show a) => a -> Showable

-- Required context is empty
pattern Sh :: (Show a) => a -> Showable
pattern Sh x <- MkShowable x

-- Provided context is empty, but required context is not
pattern One :: () => (Num a, Eq a) => a
pattern One <- 1
```


A pattern synonym can be used in a pattern if the
instatiated (monomorphic) type satisfies the constraints of
`CReq`. In this case, it extends the context available in the
right-hand side of the match with `CProv`, just like how an
existentially-typed data constructor can extend the context.


As with function and variable types, the pattern type signature can be inferred, or it can be explicitly written out on the program. 


Here's a more complex example. Let's look at the following definition:

```wiki
{-# LANGUAGE PatternSynonyms, GADTs, ViewPatterns #-}
module ShouldCompile where

data T a where
	MkT :: (Eq b) => a -> b -> T a

f :: (Show a) => a -> Bool

pattern P x <- MkT (f -> True) x
```


Here, the inferred type of `P` is

```wiki
pattern P :: (Eq b) => (Show a) => b -> T a
```


A bidirectional pattern synonym declaration has the form

```wiki
pattern P var1 var2 ... varN = pat
```


where both of the following are well-typed declarations:

```wiki
pattern P1 var1 var2 ... varN <- pat

P2 = \var1 var2 ... varN -> pat
```


In this case, the *pattern type* of `P` is simply the pattern type
of `P1`, and its *expression type* is the type of `P2`. The name `P`
is brought into the module-level scope both as a pattern synonym and
as an expression.

## Dynamic semantics


A pattern synonym occurance in a pattern is evaluated by first
matching against the pattern synonym itself, and then on the argument
patterns. For example, given the following definitions:

```wiki
pattern P x y <- [x, y]

f (P True True) = True
f _             = False

g [True, True] = True
g _            = False
```


the behaviour of `f` is the same as

```wiki
f [x, y] | True <- x, True <- y = True
f _                             = False
```


Because of this, the eagerness of `f` and `g` differ:

```wiki
*Main> f (False:undefined)
*** Exception: Prelude.undefined
*Main> g (False:undefined)
False
```

## Typed pattern synonyms


So far patterns only had *syntactic* meaning. In comparison [ Ωmega](http://code.google.com/p/omega) has *typed* pattern synonyms, so they become first class values. For bidirectional pattern synonyms this seems to be the case

```wiki
data Nat = Z | S Nat deriving Show
pattern Ess p = S p
```


And it works:

```wiki
*Main> map S [Z, Z, S Z]
[S Z,S Z,S (S Z)]
*Main> map Ess [Z, Z, S Z]
[S Z,S Z,S (S Z)]
```

## Branching pattern-only synonyms

*N.B. this is a speculative suggestion!
*


Sometimes you want to match against several summands of an ADT simultaneously. E.g. in a data type of potentially unbounded natural numbers:

```wiki
data Nat = Zero | Succ Nat
type UNat = Maybe Nat -- Nothing meaning unbounded
```


Conceptually `Nothing` means *infinite*, so it makes sense to interpret it as a *successor* of something. We wish it to have a predecessor just like `Just (Succ Zero)`!


I suggest *branching pattern synonyms* for this purpose:

```wiki
pattern S pred <- pred@Nothing | pred@(Just a <- Just (Succ a))
pattern Z = Just Zero
```


Here `pred@(Just a <- Just (Succ a))` means that the pattern invocation `S pred` matches against `Just (Succ a)` and - if successful - binds `Just a` to `pred`.


This means we can syntactically address unbound naturals just like bounded ones:

```wiki
greetTimes :: UNat -> String -> IO ()
greetTimes Z _ = return ()
greetTimes (S rest) message = putStrLn message >> greetTimes rest message
```


As a nice collateral win this proposal handles `pattern Name name <- Person name workplace | Dog name vet` too.

## Record Pattern Synonyms


Normal pattern synonyms provide a convenient way to abstract away from ADTs by explicitly defining the meaning of the pattern and the ability to define the constructor.


Currently there is no way to similar way to project an existing datatype to a record.
Adding this feature provides completeness as pattern synonyms would become equally expressive as ordinary data type declarations. 

### Specification


The syntax for defining pattern synonyms is extened as follows

```wiki
patsyndecl_w_records ::=  patsyndecl
             |  'pattern' con '{' var1 ',' ... ',' varn '}' <- pat
```


A bidirectional record pattern synonym `P` with type `T` and arguments `f1, f2, ..., fn` which have types `t1, t2, ...., tn` should behave just as if `P` had been defined as a record constructor for `T` with the corresponding fields.


For example,

```wiki
data T ... = ... |  P { f1 :: t1, f2 :: t2, ..., fn :: tn }
```

### Design


The proposed syntax is as follows

```wiki
pattern Foo{bar, baz} = (bar, baz)
```


which overloads the syntax for named field puns. 


If a unidirectional pattern is declared then the pattern along with record selectors are provided. The following five definitions are equivalent.

```wiki
getFst1 Foo{bar} = bar

getFst2 Foo{bar=qux} = qux

getFst3 Foo{..} = bar

getFst4 (Foo v _) = v

getFst5 v = bar v
```


When a bidirectional synonym is declared then the constructor `Foo` is also declared which can be used in two ways.

```wiki
myFoo = Foo "first" 2

hisFoo = Foo { bar = "first", baz = 2 }
```


Finally we consider record updates.

```wiki
updateBaz x = x {baz = 6}
```


An unresolved design point is how record updates should be handled. Given Foo is in scope then there is an unambiguous type for this expression (as baz is uniquely a selector for `Foo`). Thus the inferred type of `updateBaz` would be `updateBaz :: (a, b) -> (Int, b)`. 


This whole construct seems quite strange as it would also seem possible to write (the currently illegal)  `(1,2) {baz = Just 6}` as well as `(Foo 1 2) { baz = Just 6}`. Currently pattern synonyms do not change the semantics of programs outside from the explicit use of the synonym. This example is slightly different as we do not use `Foo` but merely the field name `baz`. I am not sure whether this would be confusing to users.

### Tricky bits

- There is now a potential ambiguity.

```wiki
data D = MkD { foo :: Int }
pattern Pat = MkD { foo = Int }

baz = Pat { foo = 5 }
```

>
> Here, I'm intending `Pat { foo = 5 }` to be a record *update*, not a record *construction*. But it's confusing! Does this work?

#### Answer


Not currently - `baz` is parsed as a `RecordCon` which then fails as `Pat` is not a constructor with field `foo`.

- Import/export syntax has to be extended to accommodate the field labels. So, if we have

```wiki
pattern Pat { a } = Just a
```

>
> then we should be able to write any of the following in an export list: `pattern Pat`, `pattern Pat(..)`, `pattern Pat(a)`. (The last two mean the same thing.) It would only be logical to extend this syntax to also allow record data constructors to operate the same way. Question: should record data constructors be allowed to use this syntax when exported without the `pattern` keyword?

## Associating synonyms with types


See [PatternSynonyms/AssociatingSynonyms](pattern-synonyms/associating-synonyms)