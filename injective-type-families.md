# Injective type families


This page summarizes the design behind injective type families ([\#6018](https://gitlab.haskell.org//ghc/ghc/issues/6018)). It is a
work in progress. This page will evolve to reflect the progress made.


Person responsible for this page and the implementation: Jan Stolarek (just so
you now who is meant by "I").

## Forms of injectivity


The idea behind [\#6018](https://gitlab.haskell.org//ghc/ghc/issues/6018) is to allow users to declare that a type family is
injective. So far we have identified these forms of injectivity:

1. Injectivity in all the arguments, where knowing the result (right-hand
  side) of a type family determines all the arguments on the left-hand
  side. Examples:

```
typefamilyId a whereId a = a
```

```
typefamilyF a b c
typeinstanceFIntCharBool=BooltypeinstanceFCharBoolInt=InttypeinstanceFBoolIntChar=Char
```

1. Injectivity in some of the arguments, where knowing the RHS of a type
  family determines only some of the arguments on the LHS. Example:

```
typefamilyG a b c
typeinstanceGIntCharBool=BooltypeinstanceGIntCharInt=BooltypeinstanceGBoolIntInt=Int
```

>
> Here knowing the RHS allows us to determine first two arguments, but not the
> third one.

1. Injectivity in some of the arguments, where knowing the RHS of a type
  family and some of the LHS arguments determines other (possibly not all)
  LHS arguments. Examples:

```
typefamilyPlus a b wherePlusZ      n = n
     Plus(S m ) n =S(Plus n m)
```

>
> Here knowing the RHS and any single argument uniquely determines the
> remaining argument.

```
typefamilyH a b c
typeinstanceHIntCharDouble=InttypeinstanceHBoolDoubleDouble=Int
```

>
> Knowing the RHS and either `a` or `b` allows to uniquely determine the
> remaining two arguments, but knowing the RHS and `c` gives us no information
> about `a` or `b`.


In the following text I will refer to these three forms of injectivity as A, B
and C.

**Note that at the moment we only have practical use cases for injectivity of
form A. Because of that I propose that we implement only this form of injectivity.**
We can implement B and C when somebody comes up with compelling use cases.

## Proposed syntax


Below is a list of various syntax proposed so far for this new feature. Two
important things to consider when deciding on a concrete syntax are:

- **Backwards compatibility**: at the moment no decision has been made on
  whether injectivity will become a part of existing `TypeFamilies` extension
  or if we create a new `InjectiveTypeFamilies` extension that implies
  `TypeFamilies`. If we choose the former then we need to come up with syntax
  that is backwards compatible. (Perhaps this is the other way around: if we
  end up having backwards incompatible syntax then we must create a new
  language extension).

- **Future extensibility**: if we only implement A we still want to be able
  to add B and C in the future without breaking A.

### Proposal 1


Use syntax similar to functional dependencies. The injectivity declaration
begins with `|` following type family declaration head. `|` is followed by a
list of comma-separated injectivity conditions. Each injectivity condition has
the form:

```
resultA->B
```


where `A` is a possibly-empty list of type variables declared in type family
head and `B` is non-empty list of said type variables. Things on the left and
right of `->` are called LHS and RHS of an injectivity condition,
respectively. `result` becomes a restricted word that cannot be used as a type
variable's identifier in a type family head.  I think this is identical to how
the `role` word is treated. Examples (taken from the previous section):

```
typefamilyId a     | result -> a wheretypefamilyF a b c  | result -> a b c
typefamilyG a b c  | result -> a b wheretypefamilyPlus a b | result a -> b, result b -> a wheretypefamilyH a b c  | result a -> b c, result b -> a c
```


Pros:

- has natural reading: `result a -> b` reads as "knowing the result and the
  type variable a determines b".

- extensible for the future


Cons:

- steals `result` identifier in the type family head. This means it would be
  illegal to have a type variable named `result` in a type family.

- the above makes this proposal backwards incompatible with `TypeFamilies`
  extension.


Further proposals are based on this one in an attempt to solve the problem of
stealing syntax and backwards incompatibility.

### Proposal 2


Use `Result` instead of `result`:

```
typefamilyId a     |Result-> a wheretypefamilyF a b c  |Result-> a b c
typefamilyG a b c  |Result-> a b wheretypefamilyPlus a b |Result a -> b,Result b -> a wheretypefamilyH a b c  |Result a -> b c,Result b -> a c
```


Pros:

- has natural reading

- extensible for the future

- allows to have type variables named `result` (does not steal syntax)


Cons:

- all other keywords in Haskell are lower case. This would be the first one
  that is capitalized.

- confusion could arise if we have a type `Result` and use it in the type
  family head.


Unknowns (for me):

- not sure if it would be possible to avoid name clash between `Result` and
  type of the same name. If not then this proposal would also be backwards
  incompatible with `TypeFamilies`.

### Proposal 3


Use `type` instead of `result`:

```
typefamilyId a     |type-> a wheretypefamilyF a b c  |type-> a b c
typefamilyG a b c  |type-> a b wheretypefamilyPlus a b |type a -> b,type b -> a wheretypefamilyH a b c  |type a -> b c,type b -> a c
```


Pros:

- extensible for the future

- no syntax stealing


Cons:

- no natural reading

- usage of `type` here might seem unintuitive

### Proposal 4


Use name of the type family instead of `result`:

```
typefamilyId a     |Id-> a wheretypefamilyF a b c  |F-> a b c
typefamilyG a b c  |G-> a b wheretypefamilyPlus a b |Plus a -> b,Plus b -> a wheretypefamilyH a b c  |H a -> b c,H b -> a c
```


Pros:

- extensible for the future

- no syntax stealing


Cons:

- writing something like `Plus a` might be confusing. It looks as if `Plus`
  could be partially applied.

### Proposal 5


A major drawback of all the above proposals is that if we only implement A injectivity then writing things like `result -> a b c d` is a lot of boilerplate (since there is only one possible injectivity condition anyway). We could avoid that by introducing `injective` keyword:

```
injectivetypefamilyId a whereinjectivetypefamilyF a b c
injectivetypefamilyG a b c  whereinjectivetypefamilyPlus a b whereinjectivetypefamilyH a b c
```


Pros:

- no boilerplate for declaring A injectivity

- probably backwards compatible, since there is only one place where the `injective` keyword may go.

- could be extended in the future be allowing any of the proposals 1-4 to be used alternatively


Cons:

- steals "injective" identifier in type family head (I think)

- not directly extensible. Introducing syntax from proposals 1-4 sometime in the future will end up with redundancy

## Real-life use cases

*If you can supply more examples (type family declarations + their usage that
currently doesn't work but should work with injectivity) please add them here.*

**Example 1**

```
typefamilyF a | result -> a whereFChar=BoolFBool=IntFInt=CharidChar::(F a ~Bool)=> a ->CharidChar a = a
```


GHC should infer that `a` is in fact `Char`. Right now this program is rejected.

**Example 2** (taken from [6018\#comment:5](https://gitlab.haskell.org//ghc/ghc/issues/6018))

```
dataNat=Zero|SucNatdataCoNat=CoNat|InfinitytypefamilySucc(t ::CoNat)::CoNattypeinstanceSucc(Co n)=Co(Suc n)typeinstanceSuccInfinity=Infinity
```


GHC can't infer `Succ n ~ Succ m => n ~ m` because it can't see that `Succ` is injective.

**Example 3** (taken from [6018\#comment:26](https://gitlab.haskell.org//ghc/ghc/issues/6018))

```
classManifold' a wheretypeField a
    typeBase  a
    typeTangent a
    typeTangentBundle a
    typeDimension a ::NattypeUsesMetric a ::Symbol
    project :: a ->Base a
    unproject ::Base a -> a
    tangent :: a ->TangentBundle a
    cotangent :: a ->(TangentBundle a ->Field a)-- this worksid':: forall a.(Manifold' a )=>Base a ->Base a
id' input = project out
  where
    out :: a
    out = unproject input

-- but this requires injective type familiesid':: forall a.(Manifold' a )=>Base a ->Base a
id'= project . unproject

```

## Implementation outline

### Verifying correctness of user's injectivity declaration


Once the user declares type family to be injective GHC must verify that this
declaration is correct, ie. type family really is injective. Below is an outline
of the algorithm. It is then follow with a discussion and rationale. *Note: I
am not too familiar with GHC's type checking so please tell me if I'm making any
wrong assumptions here. Also, I might be using incorrect terminology. If so
please complain or fix.*

**Important**: this algorithm is only for the injectivity of type A. RHS
refers to the right-hand side of the type family being checked for
injectivity. LHS refers to the arguments of that type family.

**Algorithm**

1. If a RHS contains a call to a type family we conclude that the type family is
  not injective. I am not certain if this extends to self-recursion -- see
  discussion below.


Open type families:

1. When checking equation of an open type family we try to unify its RHS with
  RHSs of all equations we've seen so far. If we succeed this means that this type
  family is not injective because two non-overlapping equations can produce types
  that can be unified.


Closed type families

1. If a type family has only one equation we declare it to be injective (unless
  case 1 above applies).

1. If a type family has more equations then we check them one by one. Checking
  each equation consists of these steps:

>
> 0) verify 1) for the current equation.

>
> a) attempt to unify RHS with RHSs of all previous equations. If this does not
> succeed proceed to next equation. If this was the last equation then type
> family is injective.

>
> b) if unification in a) succeeds and no substitutions are required then type
> family is not injective and we report an error to the user. By "no
> substitution" I mean situation when there are no type variables involved
> ie. both sides declare the same concrete type (eg. Int).

>
> c) if unification succeeds and there are type variables involved we substitute
> unified type variables on the LHS and check whether this LHS overlaps with
> any of the previous equations. If it does we proceed to the next equation
> (if this was the last equation then type family is injective). If it
> doesn't then type family is not injective and we report an error to the
> user. See examples and discussion below for more details.

**Discussion**


Case 1 above is conservative and may incorrectly reject injectivity declaration
for some type families. For example type family `I`:

```
typefamilyI a whereI a =Id a -- Id defined earlier
```


is injective but restriction in case 1 will reject it. Why such a restriction?
Consider this example. Let us assume that we have type families `X` and `Y` that
we already know are injective, and type constructors `Bar :: * -> *` and
`Baz :: * -> * -> *`. Now we declare:

```
typefamilyFoo a whereFoo(Bar a)=X a
     Foo(Baz a b)=Y a b
```


Here we would need to check whether results of `X a` and `Y a b` can overlap and
I don't see a good way of doing this once the RHS has nested calls to type
families. Second of all if we see `Foo a ~ Bool` during type checking GHC can't
solve that without trying each equation one by one to see which one returns
`Bool`. This takes a lot of guessing and I believe GHC should refuse to do that.
See also [\#4259](https://gitlab.haskell.org//ghc/ghc/issues/4259).


What about self-recursion? Consider this type family:

```
-- assumes promoted Maybe and NattypefamilyNatToMaybe a whereNatToMaybeZ=NothingNatToMaybe(S n)=Just(NatToMaybe n)
```


Using Case 4a we will infer correctly that this type family is injective. Now
consider this:

```
typefamilyBan a whereBanZ=ZBan(S n)=Ban n
```


To verify injectivity in this case we need to ask whether `Z ~ Ban n` (case 4a
above). We can only answer such question when a type family is injective. Here
we have not confirmed that `Ban` is injective so we may rightly refuse to answer
`Z ~ Ban n` and conclude (correctly) that this type family is not injective. So it
seems to me that we could allow self-recursion - I have not yet identified any
corner cases that would prevent us from doing so.


Here's an example of case 4c in action:

```
typefamilyBak a whereBakInt=CharBakChar=IntBak a    = a
```


Equation 2 falls under case 4a. When we reach equation 3 we attempt to verify
it's RHS with each RHS of the previous equations. Attempt to unify `a` with
`Char` succeeds. Since unification was successful we substitute `Char` on the LHS
of the 3rd equation and get `Bak Char`. Now we must check if this equation
overlaps with any of the previous equations. It does overlap with the second
one, so everything is fine: we know that 3rd equation will never return a `Char`
(like 1st equation) because this case will be caught by the 2nd equation. We
know repeat the same steps for the second equations and conclude that 3rd
equation will never produce an `Int` because this will be caught by the 1st
equation. We thus conclude that `Bak` is injective.

### Other implementation details


The implementation needs to check the correctness of injectivity conditions
declarations. This includes checking that:

- only in-scope type variables are used. For example
  `type family F a | result -> b` should result with "not in scope: b" error.

- there are no identical conditions (this wouldn't hurt, but the user deserves
  a warning about this)

- type variables are not repeated on either LHS or RHS of the injectivity
  condition. For example `result a a -> ...` or `... -> a b a` should generate
  a warning. Note that it probably is OK to have the same variable both on the
  LHS and RHS of an injectivity condition: in the above examples it is true
  that `type family G a b c | result c -> a b c`. The question is whether this
  has any practical relevance.

- injectivity conditions don't overlap (eg. `result -> a b` overlaps
  `result -> a`). This probably deserves a warning.


I am not certain at the moment how to treat these injectivity conditions
declarations:

- `result -> a, result -> b` is technically correct but we could just say
  `result -> a b`. Do the two separate declarations have the same power as the
  combined one?

### Inferring injectivity

[Here](https://gitlab.haskell.org//ghc/ghc/issues/6018) it was suggested by Simon that we could infer
injectivity for closed type families. This is certainly possible for injectivity
A, but not for B or C, where the number of possible injectivity conditions is
exponential in the number of arguments. I personally am against inferring
injectivity. The reasons are:

- Before 7.10 comes out there will already be some code in the wild that uses
  closed type families introduced in GHC 7.8. None of these type families
  require injectivity to work because GHC 7.8 does not support injectivity. If
  we attempt to infer injectivity for all these already existing closed type
  families we will only increase compilation time of existing code with
  absolutely no gain in functionality of the code. There were some complaints
  about GHC's performance decreasing with each release and I don't want to add
  to that.

- I believe that requiring explicit injectivity annotations is a valuable
  source code documentation for the programmer.

- Annotations also make it explicit which code will compile with GHC 7.10 and
  which will not.

- I don't like the idea of mismatch between open type families and closed type
  families, meaning that injectivity of open type families would be openly
  documented whereas for closed type families it would be hidden.

## Other notes

- This page does not mention anything about associated types. The intention is
  that injectivity will also work for those. I just haven't thought about the
  details yet.

- [Here](https://gitlab.haskell.org//ghc/ghc/issues/6018) Richard mentions head-injectivity used by
  Agda. We don't plan to follow that direction.

- I'm not yet sure what is the exact scope of things we want to do with
  injectivity. Certainly if we have `F a ~ F b` and `F` is injective then we
  can declare `a ~ b`. (Note: Simon remarks that in such case `a` must be a
  skolem constant and `b` a unification variable. I don't know the difference
  between the two). Moreover if we have `F a ~ Int` then we can infer `a`. But
  what if we also have injective type family `G` and see a constraint like
  `F a ~ G b c`? For injective type families there exists at most one solution
  but I think that GHC should refuse to solve such riddles.
