# Pattern Synonyms


Most language entities in Haskell can be named so that they can be abbreviated instead of written out in full.
This proposal provides the same power for patterns.  See the [implementation](pattern-synonyms/implementation) page for implementation details.


Tickets should include `PatternSynonyms` in their Keywords to appear in these summary lists.



Open Tickets:

<table><tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/8581">#8581</a></th>
<td>Pattern synonym used in an expression context could have different constraints to pattern used in a pattern context</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/8583">#8583</a></th>
<td>Associated pattern synonyms</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/9671">#9671</a></th>
<td>Allow expressions in patterns</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/10783">#10783</a></th>
<td>Partial type signatures should work in pattern synonym signatures</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11212">#11212</a></th>
<td>Should be more liberal parsing pattern synonyms with view patterns</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11228">#11228</a></th>
<td>Interaction between ORF and record pattern synonyms needs to be resolved.</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11350">#11350</a></th>
<td>Allow visible type application in patterns</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11368">#11368</a></th>
<td>Pattern synonym name is mangled when patterns are non-exhaustive</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11461">#11461</a></th>
<td>Allow pattern synonyms to be bundled with type classes?</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11646">#11646</a></th>
<td>Make pattern synonym export type mismatch a warning</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11655">#11655</a></th>
<td>Ambiguous types in pattern synonym not determined by functional dependencies</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11955">#11955</a></th>
<td>Haddock documentation for pattern synonyms printed with explicit forall quantifiers</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11959">#11959</a></th>
<td>Importing doubly exported pattern synonym and associated pattern synonym panics</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11993">#11993</a></th>
<td>RFC, allow local bindings in pattern synonyms</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12006">#12006</a></th>
<td>Can&apos;t infer constraint of pattern synonyms</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12178">#12178</a></th>
<td>Allow inline pragmas on pattern synonyms</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12179">#12179</a></th>
<td>Incorrect parsing of a pattern synonym type</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12187">#12187</a></th>
<td>Clarify the scoping of existentials for pattern synonym signatures</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12203">#12203</a></th>
<td>Allow constructors on LHS of (implicit) bidirectional pattern synonym</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12448">#12448</a></th>
<td>Allow partial application of bidirectional pattern synonyms</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12975">#12975</a></th>
<td>Suggested type signature for a pattern synonym causes program to fail to type check</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13042">#13042</a></th>
<td>Allow type annotations / visible type application in pattern synonyms</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13307">#13307</a></th>
<td>Record pattern synonym fields have to be manually exported</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13363">#13363</a></th>
<td>Wildcard patterns and COMPLETE sets can lead to misleading redundant pattern-match warnings</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13572">#13572</a></th>
<td>Add ArgMin / ArgMax pattern synonyms</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13717">#13717</a></th>
<td>Pattern synonym exhaustiveness checks don&apos;t play well with EmptyCase</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13778">#13778</a></th>
<td>explicitly bidirectional patterns should not report Recursive definition&quot; when used in view pattern expression position</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13964">#13964</a></th>
<td>Pattern-match warnings for datatypes with COMPLETE sets break abstraction</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13965">#13965</a></th>
<td>COMPLETE sets nerf redundant pattern-match warnings</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13975">#13975</a></th>
<td>GHC can&apos;t infer pattern signature, untoucable kinds</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14059">#14059</a></th>
<td>COMPLETE sets don&apos;t work at all with data family instances</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14133">#14133</a></th>
<td>COMPLETE pragmas seem to be ignored when using view patterns</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14253">#14253</a></th>
<td>Pattern match checker mistakenly concludes pattern match on pattern synonym is unreachable</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14422">#14422</a></th>
<td>{-# complete #-} should be able to be at least partially type directed</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14423">#14423</a></th>
<td>{-# complete #-} should be able to handle | like {-# minimal #-}</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14602">#14602</a></th>
<td>Implement the pattern synonym construction function signatures proposal</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14630">#14630</a></th>
<td>name shadowing warnings by record pattern synonyms + RecordWildCards or NamedFieldPuns</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14851">#14851</a></th>
<td>&quot;Pattern match has inaccessible right hand side&quot; with TypeRep</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15014">#15014</a></th>
<td>Exhaustivity check should suggest when COMPLETE could be helpful</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15020">#15020</a></th>
<td>PatternSynonyms: Problems with quantified constraints / foralls</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15416">#15416</a></th>
<td>Higher rank types in pattern synonyms</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15554">#15554</a></th>
<td>COMPLETE pragmas make overlapping-patterns warnings behave oddly</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15681">#15681</a></th>
<td>Take exhaustiveness checking into consideration when using MonadFailDesugaring</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15693">#15693</a></th>
<td>Abstracting out pattern into a pattern synonym fails with scary error</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15744">#15744</a></th>
<td>Existence of complete pattern synonym hides unrelated incomplete pattern warning</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15885">#15885</a></th>
<td>Enhancing COMPLETE pragma to support pattern synonyms with polymorphic (output) types</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/16155">#16155</a></th>
<td>Pattern Synonym for Ratio</td></tr></table>


There is a list of closed tickets at the bottom of the page.

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



`pattern` *conid* *varid<sub>1</sub>* ... *varid<sub>n</sub>* `<-` *pat*



`pattern` *varid<sub>1</sub>* *consym* *varid<sub>2</sub>* `<-` *pat*


- Each of the variables on the left hand side must occur exactly once on the right hand side 
- Pattern synonyms are not allowed to be recursive.  Cf. type synonyms.

<table><tr><th>
There have been several proposals for the syntax of defining pattern-only synonyms:


- <tt>pattern</tt> <i>conid</i> <i>varid<sub>1</sub></i> ... <i>varid<sub>n</sub></i> <tt>~</tt> <i>pat</i>
- <tt>pattern</tt> <i>conid</i> <i>varid<sub>1</sub></i> ... <i>varid<sub>n</sub></i> <tt>:=</tt> <i>pat</i>
- <tt>pattern</tt> <i>conid</i> <i>varid<sub>1</sub></i> ... <i>varid<sub>n</sub></i> <tt>-></tt> <i>pat</i>
- <tt>pattern</tt> <i>conid</i> <i>varid<sub>1</sub></i> ... <i>varid<sub>n</sub></i> <tt><-</tt> <i>pat</i>

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



`pattern` *conid* `::` *type*



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



`pattern` *conid* *varid<sub>1</sub>* ... *varid<sub>n</sub>* `=` *pat*



`pattern` *varid<sub>1</sub>* *consym* *varid<sub>2</sub>* `=` *pat*



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



`pattern` *conid* *varid<sub>1</sub>* ... *varid<sub>n</sub>* `<-` *pat* `where` *cfunlhs* *rhs*



where *cfunlhs* is like *funlhs*, except that the functions symbol is a *conid* instead of a *varid*.


Example, using [ViewPatterns](view-patterns):

```wiki
   pattern Succ n <- ((\x -> (x-1) <$ guard (x > 0)) -> Just n) where
      Succ n = n + 1
```


The first part as is before and describes the expansion of the synonym in patterns. The second part describes the expansion in expressions.

```wiki
   fac (Succ n) = Succ n * fac n 
   fac 0 = 1
```

## Associated pattern synonyms


Just like data types and type synonyms can be part of a class declaration, it would be possible to have pattern synonyms as well.


Example:

```wiki
   class ListLike l where
      pattern Nil :: l a
      pattern Cons :: a -> l a -> l a
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


This is because we generate the matching function at the definition site. 

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


See [PatternSynonyms/RecordPatternSynonyms](pattern-synonyms/record-pattern-synonyms)

## Associating synonyms with types


See [PatternSynonyms/AssociatingSynonyms](pattern-synonyms/associating-synonyms)

## `COMPLETE` pragmas


See [PatternSynonyms/CompleteSigs](pattern-synonyms/complete-sigs)

## Closed Tickets



Closed Tickets:

<table><tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/5144">#5144</a></th>
<td>Pattern synonyms</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/8582">#8582</a></th>
<td>Record syntax for pattern synonyms</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/8584">#8584</a></th>
<td>Pattern synonym type signatures</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/8749">#8749</a></th>
<td>Pattern synonyms crash GHCi</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/8761">#8761</a></th>
<td>Make pattern synonyms work with Template Haskell</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/8779">#8779</a></th>
<td>Exhaustiveness checks for pattern synonyms</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/8841">#8841</a></th>
<td>PatternSynonyms error gives wrong source locations</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/8968">#8968</a></th>
<td>Pattern synonyms and GADTs</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/9161">#9161</a></th>
<td>Pattern synonyms interact badly with data kinds</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/9226">#9226</a></th>
<td>Internal error when using equality constraint in pattern synonyms</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/9417">#9417</a></th>
<td>Pattern synonyms across modules broken in Haddock</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/9514">#9514</a></th>
<td>Haddock panics when exporting a module with pattern synonyms</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/9705">#9705</a></th>
<td>Panic on a pattern synonym in a class</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/9732">#9732</a></th>
<td>Pattern synonyms and unboxed values</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/9783">#9783</a></th>
<td>Pattern synonym matcher is unnecessarily strict on unboxed continuations</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/9793">#9793</a></th>
<td>Some as-patterns could be accepted in pattern synonyms</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/9803">#9803</a></th>
<td>Poor error message for unbound variable in pattern synonym</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/9867">#9867</a></th>
<td>PatternSynonyms + ScopedTypeVariables triggers an internal error</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/9889">#9889</a></th>
<td>Pattern synonym does not work in top-level pattern bind</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/9891">#9891</a></th>
<td>Fixity declarations for pattern synonyms not persisted</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/9900">#9900</a></th>
<td>Support pattern synonyms in GHCi</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/9911">#9911</a></th>
<td>Pattern synonyms with no signatures should yield warnings</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/9953">#9953</a></th>
<td>Pattern synonyms don&apos;t work with GADTs</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/9954">#9954</a></th>
<td>Required constraints are not inferred for pattern synonyms involving GADTs</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/9967">#9967</a></th>
<td>Pattern synonym type signature documentation out of date</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/9975">#9975</a></th>
<td>RecordWildcards and PatternSynonyms cause impossible bug</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/10339">#10339</a></th>
<td>PatternSynonyms confuse exhaustiveness check</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/10404">#10404</a></th>
<td>GHC panic when creating a monomorphised pattern synonym for GADT</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/10426">#10426</a></th>
<td>matchGroupArity panic with PatternSynonyms</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/10653">#10653</a></th>
<td>PatternSynonyms should be imported/exported as part of the wildcard notation</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/10747">#10747</a></th>
<td>Infix pattern synonyms fail to parse (regression)</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/10873">#10873</a></th>
<td>Bad error message for incorrect pattern synonym signature</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/10897">#10897</a></th>
<td>Incorrect ASSERT for buildPatSyn</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/10997">#10997</a></th>
<td>Pattern synonym causes Iface error.</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11039">#11039</a></th>
<td>Panic with incorrect pattern synonym signature</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11213">#11213</a></th>
<td>Incorrect reported pattern synonym signature</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11224">#11224</a></th>
<td>Program doesn&apos;t preserve semantics after pattern synonym inlining.</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11225">#11225</a></th>
<td>Unable to provide type signature for pattern synonym</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11227">#11227</a></th>
<td>Interaction between ORF and record pattern synonyms needs to be resolved.</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11233">#11233</a></th>
<td>Improve optimisation of pattern synonym matching</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11283">#11283</a></th>
<td>PatternSynonms and DisambiguateRecordFields causes panic</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11336">#11336</a></th>
<td>GHC craches on this combination of ViewPatterns and PatternSynonyms</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11351">#11351</a></th>
<td>Scoped type variables in pattern synonyms</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11367">#11367</a></th>
<td>[Regression] Only one clause allowed in (explicitly bidirectional) pattern synonyms</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11524">#11524</a></th>
<td>Something is amiss with quantification in pattern synonym type signatures</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11633">#11633</a></th>
<td>Record field order in a bidirectional pattern synonym match is order dependent</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11667">#11667</a></th>
<td>Incorrect pattern synonym types in error messages</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11727">#11727</a></th>
<td>Allow one type signature for multiple pattern synonyms</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11728">#11728</a></th>
<td>Core lint errors</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11977">#11977</a></th>
<td>ghc doesn&apos;t agree with its own inferred pattern type</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11985">#11985</a></th>
<td>Core lint error on record syntax update/pattern synonym</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11986">#11986</a></th>
<td>Record fields not defined with pattern synonym in ghci</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11987">#11987</a></th>
<td>Allow record wildcards with pattern synonyms which are defined in GHCi</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12007">#12007</a></th>
<td>Panic when loading file with nested pattern synonyms into ghci</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12017">#12017</a></th>
<td>GHC panics on pattern synonym ‘kindPrimRep’</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12024">#12024</a></th>
<td>GHC leaks GHC.Prim.~# into type</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12025">#12025</a></th>
<td>Order of constraints forced (in pattern synonyms, type classes in comments)</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12061">#12061</a></th>
<td>Allow duplicate record fields in pattern synonyms</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12094">#12094</a></th>
<td>Unlifted types and pattern synonym signatures</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12101">#12101</a></th>
<td>Regression: Pattern synonyms make GHCi 8.0.1 crash</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12108">#12108</a></th>
<td>Function type synonym fails in pattern synonym</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12109">#12109</a></th>
<td>Matching on pattern synonym succeeds compiled with ghc, fails with ghci</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12153">#12153</a></th>
<td>Bug in pattern synonyms with template haskell</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12165">#12165</a></th>
<td>Multiple pattern type signatures accepted</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12166">#12166</a></th>
<td>Pattern synonym existential variable confusion</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12366">#12366</a></th>
<td>Use TypeOperators for pattern synonyms?</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12426">#12426</a></th>
<td>Allow smart constructors their own types</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12429">#12429</a></th>
<td>Pattern synonym parse error should recommend enabling extension</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12456">#12456</a></th>
<td>Panics when making a quotation as pattern synonym</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12489">#12489</a></th>
<td>undefined in view pattern inside pattern synonym causes GHC to panic</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12548">#12548</a></th>
<td>Exported pattern synonyms does not mark top-level bindings in RHS as used</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12615">#12615</a></th>
<td>Record pattern synonyms cause spurious name shadowing warnings</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12697">#12697</a></th>
<td>Improve output of pattern synonym info</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12698">#12698</a></th>
<td>GHC panic on pattern synonym</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12746">#12746</a></th>
<td>Assertion failed with  BuildFlavour = devel2 (one more)</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12767">#12767</a></th>
<td>Pattern synonyms for Cont, Writer, Reader, State, ...</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12872">#12872</a></th>
<td>Pattern synonyms allow multiple type signatures but only use the first</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13018">#13018</a></th>
<td>TH-spliced pattern synonym declaration fails to typecheck</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13022">#13022</a></th>
<td>Pattern Synonyms using other synonyms causes ghc panic</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13071">#13071</a></th>
<td>GHCi 8.0.1 panic with PatternSynonyms</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13158">#13158</a></th>
<td>Pattern synonyms should use type annotation information when typechecking</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13188">#13188</a></th>
<td>COMPLETE pragma causes compilation to hang forever under certain scenarios</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13349">#13349</a></th>
<td>Make GHC handle orphan COMPLETE sets of conlikes better</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13350">#13350</a></th>
<td>COMPLETE sets aren&apos;t read from external packages</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13394">#13394</a></th>
<td>PatternSynonyms/OverloadedStrings regression in GHC HEAD</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13441">#13441</a></th>
<td>Type inference failure in bidirectional pattern synonym and GADT pattern match</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13449">#13449</a></th>
<td>Multiple pattern synonym declarations for one signature</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13454">#13454</a></th>
<td>Operators not allowed as fields in Record Pattern Synonyms</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13470">#13470</a></th>
<td>Pattern synonyms bind variables out of scope</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13671">#13671</a></th>
<td>Core lint error with PatternSynonyms and undefined</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13672">#13672</a></th>
<td>Pattern match on LHS of pattern synonym declaration</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13688">#13688</a></th>
<td>Allow splices in definition of pattern synonym</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13735">#13735</a></th>
<td>RankNTypes don&apos;t work with PatternSynonyms</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13752">#13752</a></th>
<td>Odd pattern synonym type errors</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13768">#13768</a></th>
<td>Incorrect warnings generated by exhaustiveness checker with pattern synonyms / GADT combination</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13969">#13969</a></th>
<td>Record pattern synonym incorrectly claims it&apos;s recursive, given unbound variable</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14058">#14058</a></th>
<td>Cannot bundle pattern synonym with exported data family</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14112">#14112</a></th>
<td>bang patterns on pattern synonyms? (left vs right hand sides)</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14114">#14114</a></th>
<td>Strange behavior when pattern variables are duplicated on pattern synonym RHS</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14135">#14135</a></th>
<td>PatternSynonyms regression in GHC HEAD (expectJust mkOneConFull)</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14228">#14228</a></th>
<td>PatternSynonyms Non-exhaustive with UnboxedSums</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14241">#14241</a></th>
<td>Pattern synonyms defined through other pattern synonyms produce `impossible happened` in ghci/runhaskell</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14326">#14326</a></th>
<td>Panic on COMPLETE pragma with mismatched type variable order</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14394">#14394</a></th>
<td>Inferred type for pattern synonym has redundant equality constraint</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14395">#14395</a></th>
<td>Redefining pattern synonym in GHCi triggers &quot;‘p’ is untouchable&quot; error</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14463">#14463</a></th>
<td>Pattern synonym for appliation</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14498">#14498</a></th>
<td>GHC internal error: &quot;not in scope during TC but it passed the renamer&quot;</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14507">#14507</a></th>
<td>Core Lint error with Type.Reflection and pattern synonyms</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14552">#14552</a></th>
<td>GHC panic on pattern synonym</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14747">#14747</a></th>
<td>DisambiguateRecordFields fails for PatternSynonyms</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14803">#14803</a></th>
<td>Panic during desugaring I think</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15289">#15289</a></th>
<td>isUnliftedType GHC panic on pattern with True :: Maybe</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15685">#15685</a></th>
<td>Pattern signature not inferred</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15692">#15692</a></th>
<td>GHC panic from pattern synonyms + deferred type errors</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15886">#15886</a></th>
<td>Spurious warning about incomplete pattern with PatternSynonyms</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/16129">#16129</a></th>
<td>Incorrect non-exhaustive pattern warning with PatternSynonyms</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/16281">#16281</a></th>
<td>PatternSynonyms doesn&apos;t accept non-prenex quantified functions, doesn&apos;t float foralls</td></tr></table>



