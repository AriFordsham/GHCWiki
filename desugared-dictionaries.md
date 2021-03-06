
(WIP)

## Base proposal


Expose the constructor of the dictionary datatype created by desugaring typeclasses. This seems unlikely to require any further changes - local type-based instances are still not allowed, but arbitrary dictionaries can be constructed and passed manually. This is possible /already/, but it requires creating a seperate type which mimics the structure of the desugared dictionary. No inference changes are required, no new syntax.


Say one has the typeclass 

```wiki
class Monoid a where
    mempty :: a
    mappend :: a -> a -> a
```


It currently will desugar into (roughly) the following:

```wiki
data Monoid a = D:Monoid { mempty :: a; mappend :: a -> a -> a}
```


The problem is that the constructor, *D:Monoid*, is an invalid Haskell source token, and cannot be called explicitly. The proposal is to make the constructor visible at the source level, so one can construct instances of the type *Monoid a* at runtime. 


For example:

```wiki
let sumMonoid = Monoid 0 (+) in <some expression>
```

## What this proposal is NOT


It is not a proposal for local typeclass instances based on type. That is, this proposal allows easier access to creating dictionary terms, but not in types. This is basically just removing the need to create identical data structures as a workaround, due to there almost always being a one-to-one correspondance between a typeclass and a desired dictionary type (e.g. Monoid class being identical to a dictionary for construction of monoids at runtime; same with Category, Num, etc...)


It is not a proposal for any new syntax or runtime behaviour or types. It is not creating any new types; rather, just exposing types which are already being created behind the scenes in the desugaring process.

## Rationale


There are several examples where one would like access to desugared typeclass dictionary types. For example, creating new instances at runtime like in [reflection](https://www.fpcomplete.com/user/thoughtpolice/using-reflection#turning-up-the-magic-to-over-9000). Indeed, access to dictionary constructors would probably make the reflection package much simpler and less spooky.


Currently, typeclass dictionary constructors are [prepended with "D:"](https://github.com/ghc/ghc/blob/4d5f83a8dcf1f1125863a8fb4f847d78766f1617/compiler/basicTypes/OccName.hs#L615), ensuring that no source level Haskell code can access them.  


This proposal is about allowing source level access to the dictionary constructors. Note that it is not about local instances: There would be no way to override the type-based dictionary usage, just like the present. 


It would allow, for example, far greater ease of implementation of dictionary-based methods, such as is often needed in constructive category theory. Otherwise, it is [messy as hell](https://hackage.haskell.org/package/data-category-0.6.1/docs/Data-Category-Monoidal.html#t:MonoidObject). Additionally, this kept coming up while working on the monoidal category proposal for replacing arrow notation; it was a huge barrier to a nice interface/api.


Another issue which this addresses is the problem of a single datatype having multiple possible instances. For example, the set of integers have two simple monoids available: (1,(\*)) and (0,(+)). To choose between different monoids with the integers, one has to wrap them in newtypes (i.e. the Product and Sum newtypes) which then implement the separate instances.

## Additional proposals

- Integrate the *reflection* and parts of the *constraints* library into the core class libraries, and this: (code from [here](https://www.fpcomplete.com/user/thoughtpolice/using-reflection)) 

```wiki
newtype Lift (p :: * -> Constraint) (a :: *) (s :: *) = Lift { lower :: a }

class ReifiableConstraint p where
  reifiedIns :: Reifies s (p a) :- p (Lift p a s)


with :: p a -> (forall s. Reifies s (p a) => Lift p a s) -> a
with d v = reify d (lower . asProxyOf v)
  where
    asProxyOf :: f s -> Proxy s -> f s
    asProxyOf x _ = x

using :: forall p a. ReifiableConstraint p => p a -> (p a => a) -> a
using d m = reify d $ \(_ :: Proxy s) ->
  let replaceProof :: Reifies s (p a) :- p a
      replaceProof = trans proof reifiedIns
        where proof = unsafeCoerceConstraint :: p (Lift p a s) :- p a
  in m \\ replaceProof

```


Example usage of this:

```wiki
--These two instances can and should be automatically created by the compiler eventually, but it isn't strictly necessary
instance ReifiableConstraint Monoid where
  reifiedIns = Sub Dict

instance Reifies s (Monoid a) => Monoid (Lift Monoid a s) where
  mappend a b        = Lift $ mappend (reflect a) (lower a) (lower b)
  mempty = a where a = Lift $ mempty (reflect a)

--now to use it

using (Monoid (+) 0) $ mempty <> 10 <> 12 -- evaluates to 22
using (Monoid (*) 1) $ mempty <> 10 <> 12 -- evaluates to 120
```

## Implementation


The first shot at allowing access to the constructors is to simply replace the string "D:" with "".


Doing so reveals two things: 


1) It still does not work - the parser catches "Monoid undefined undefined undefined" with "Not in scope: data constructor Monoid". Is this because it's getting caught in the typechecker?
2) 

```wiki
spacekitteh@sophielinux:~/code/ghc-T9819$ inst/bin/ghc testsuite/tests/indexed-types/should_compile/Deriving
[1 of 1] Compiling ShouldCompile    ( testsuite/tests/indexed-types/should_compile/Deriving.hs, testsuite/tests/indexed-types/should_compile/Deriving.o )

testsuite/tests/indexed-types/should_compile/Deriving.hs:8:1: Warning:
    Tab character
/tmp/ghc13400_0/ghc13400_2.s: Assembler messages:

/tmp/ghc13400_0/ghc13400_2.s:617:0:
     Error: symbol `ShouldCompile_C_closure' is already defined

/tmp/ghc13400_0/ghc13400_2.s:811:0:
     Error: symbol `ShouldCompile_C_static_info' is already defined
```


This goes away when the "D:" is replaced with something like "MkDict" instead of a blank.
