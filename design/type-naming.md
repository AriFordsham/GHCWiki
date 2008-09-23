# Naming types in Haskell


Haskell currently allows you to use the same name for
a type and a data constructor, thus

```wiki
  data Age = Age Int
```


In any context, it is clear which is meant, thus

```wiki
  foo :: Age -> Int     -- Type constructor Age
  foo (Age i) = i       -- Data constructor Age
```


However, as we extend Haskell's type system (or at least
GHC's), there are occasions in which the distinction is
less clear.  This page summarises the issues, and proposes
solutions. 


NB: the whole page is purely about *syntax*.


Please comment on glasgow-haskell-users@…, or
by adding notes to this page.

## The issues


There are several distinct ways in which the type/value
distinction is becoming blurred.

**Type operators**.  With `-XTypeOperators`, GHC already allows this

```wiki
 data a :+: b = Left a | Right b
```


However, I really want to allow this too:

```wiki
 data a + b = Left a | Right b
```


That is, allow oprerators like `(+)` to be type constructors. 
You can find discussion of the merits of this proposal here.
At first it seems fairly straightforward; for example, it is
quite clear that in a type signature 

```wiki
  f :: (a + b) -> a
```


the `(+)` must be the type constructor not the value-level
multiplication.  But there's a problem with export lists:

```wiki
 module Foo( foo, (+), bar ) where ..
```


Is this export list exporting the type `(+)` or the value `(+)`?


There is a very similar issue with fixity declarations

```wiki
  infix 5 +, :+:
```


In these two contexts we need to disambiguate whether we mean
the type-level or value-level identifier. 

**Proper kinding**.  At the moment you see a lot of this
kind of nonsense:

```wiki
  data Zero
  data Succ a
  data List :: * -> * -> * where
    Cons :: a -> List n a -> List (Succ n) a
    ...etc...
```


The indexed data type `List` is only supposed to get
`Zero` or `Succ` as its first arugments, 
the stupid type `(List Int Int)` is, alas,
well kinded.  Obviously what we want is to give a proper
kind to `List`.  My current proposal is to allow value-level
data constructors to be re-used at the type level, thus:

```wiki
  data Nat = Zero | Succ Nat
  data List :: Nat -> * -> * where
    Cons :: a -> List n a -> List (Succ n) a
```


Again, I don't want to elaborate all the details here, but
the point is that a data constructor (`Succ`) is being used
in a type.  If there also happened to be a type constructor
`Succ`, it'd be unclear which you meant, and you really might
want either.

**Type-level lists and tuples**.  Folllowing on from the 
preceding thought, we can presumably re-use tuples at the
type level.  So if we write the type `(T (Int,Bool))` do 
mean that

- `T :: * -> *`, and we are instantiating it with the type `(Int,Bool) :: *`?
- `T :: (*,*) -> *`, and we are instantiating it with the pair types `Int::*` and `Bool::*`?


If you write it prefix, thus `(T ((,) Int, Bool))`, we can
see that this the same as the `Succ` question above: 
in this type do we mean to name the *type* constructor `(,)`
or the *data* constructor `(,)`.


Exactly same questions can be asked about the special purpose
list syntax `[a,b,c]`.  When we write `(T [])` do we mean
the type constructor `[]` or the data constructor `[]`?
But there is a bit more here, because `[a,b,c]` is
syntactic sugar. 

## Proposals


I make two proposals:

- Disambiguation in export lists and fixity declarations
- Disambiguation in types

### Proposal 1: disambiguation in export lists and fixity declarations

- Extend export lists and fixity declarations to permit the disambiguating specifier `data`, `type`, and `class`.
- The specifier is always permitted, but only required if the situation would otherwise be ambiguous.
- The specifier must match the corresponding declaration, except that the specifier `data` matches a `newtype` declaration too.


Thus you can say

```wiki
  module Foo( data T(T1,T2), S, class C ) where
    data T = T1 | T2
    data S = S1 | S2
    class C a where ...
```


In this case the `data` and `class` specifiers are both optional.
But they are not always optional (that is the point):

```wiki
  module Foo( data (%%%)(...) ) where
    infix 4 data (%%%)  -- The type constructor
    infix 6 (%%%)       -- The function
    data a %%% b = a :%%% b
    a %%% b = a :%%% b
```


Looking just at the export lists, you can see this proposal as a 
baby step towards the export list becoming a proper module signature.

### Proposal 2: disambiguation in types

- Value-level data constructors in types may be disambiguated by a shift operator `%`.
- This disambiguation is compulsory only if there is a like-named type constructor in scope.


Suppose the following data types are available

```wiki
  data Nat = Zero | Succ Nat
  data Succ = A | B
  data List :: Nat -> * where ...
  data T :: [Nat] -> * where ...
```


Now here are the interpretation of various types

```wiki
  List Zero :: *            -- Zero means the data constructor
                            --   (since there is no type constructor Zero)
  List (Succ Zero)          -- Succ means the type constructor
                            -- hence ill-kinded
  List (%Succ Zero)  :: *   -- %Succ means the data constructor
  List (%Succ %Zero) :: *   -- %Zero is also legal to mean the data constr
  
  T []                      -- [] means the list type constructor
                            -- hence ill-kinded
  T %[] :: *                -- %[] means the data constructor []

  T [Zero]                  -- [..] means the list type
                            -- hence ill-kinded
  T %[Zero] :: *                -- %[..] means list syntax (Zero : [])

  [(Int,Bool)] :: *         -- The ordinary H98 type
  [%(Int,Bool)]             -- Ill kinded
  %[%(Int,Bool)] :: [(*,*)]
  () :: *                   -- The ordinary H98 type
  %() :: ()
```


The principles are

- Just as with Haskell 98, if the lexical binding is unambiguous, 
  there is no need for a disambiguating shift operator (although one
  is always permitted)

- Just as with Haskell 98, disambiguation is purely lexical; it does
  not take advantage of kind checking.


Whether "`%`" is the best notation isn't clear to me, but the
notation must be reasonably quiet.

### Alternatives to proposal 2

- One alternative would be simple but brutal: simply have 
  no "`%`" escape notation.  In the above examples, saying
  `Succ` at the the type level would mean the data type `Succ`,
  and there would be no way to get to the data constructor.
  You lose.

- Another alternative would be to allow the type name to
  disambiguate.  Thus `Nat.Succ` would name the data construtor.
  (Obvious question: the overlap with the module qualifiers.)


Neither of these alternatives seem compatible with lists and 
tuples at the type level. Maybe they can still use the "`%`" notation?
