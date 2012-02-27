# Syntax Directed Name Resolution


The idea is a \# prefix for identifiers.  `#a` is a "type directed function".
It requires the type of its argument to be monomorphic.  An application
`#a :: forall x. M.X -> x` is desugared to `M.a :: M.X -> Y` where `Y` depends
on the concrete type of `M.a`.  All it does is strip the module part off off
the argument type, and look for the symbol `a` in that module.  The module
must be imported qualified for the resolution to happen, so compiling a
certain module only needs to look at its direct dependents.  As a degenerate case,
if the argument type is defined in the current module then typing `#a` will resolve to just
`a`.  This is nice because if you define a bunch of code in the same module as
the record you can then copy-paste it into a new module without having to update
every single reference, as long as you used `#` consistently.


Everything else remains the same.  Records wishing the same name must live in
separate modules, but field access looks like: `(#b . #a) record`.  People
who like unqualified imports can still use them, since `import A.B.C` will
bring `A.B.C.a` into scope as well as `a`.  If there are two `a`s from
separate modules, an unqualified `a` will give an ambiguous error as always,
but `#a` will use its argument type to automatically qualifiy it to the right
module.  Since `#a` is desugared to `A.B.C.a` it's a separate symbol from `a`
and they can both exist in the same module provided, of course, that you
imported `A.B.C` qualified.


This is enough for field access, but to support convenient modification, some
notion of lenses has to be built in and the rule has to be modified, from
`#a :: forall x. M.X -> x` ==\> `M.a :: M.X -> Y` to look at the first argument
of a `Lens` class:
`#a :: forall x. Lens M.X x` ==\> `M.a :: Lens M.X Y`.  Effectively this is
just substituting `Lens` for `(->)`.  Maybe it could be extended to look at
the first argument of any type constructor `forall m x. m M.X x`, that way
the syntax can be used to resolve not just record fields, but any function,
or any member of Category, or whatever.


Here's an example using plain function "\# resolution":

```wiki
import qualified Inner
import qualified Outer
-- data Outer.Outer = Outer { a :: Inner.Inner }
-- data Inner.Inner = Inner { b :: Int }

record :: Outer.Outer
record = Outer.Outer (Inner.Inner 42)

val :: Int
val = (#b.#a) record

-- Type of 'record' must already be known:
(#b.#a) :: Outer.Outer -> Int
-- Due to the definition of (.) and its 2nd argument type being known:
#a :: Outer.Outer -> b
-- New "# Resolution" rule: The first argument is in module Outer, so
-- resolve to Outer.a, and now we know the full type of Outer.a:
Outer.a :: Outer.Outer -> Inner.Inner
-- Due to the definition of (.) and its 1st argument type being known:
#b :: Inner.Inner -> c
-- # Resolution:
Outer.b :: Inner.Inner -> Int
-- So the Ints match, and val is (effectively) rewritten as:
val = (Inner.b . Outer.a) record
```


This works to set fields of a record as well.  Let `M.a` be a function from
a record to a lens focused on a certain field of that record.  Now you can
write:

```wiki
M.a :: M.Record -> Lens M.Record Int
set :: record -> Lens record field -> field -> record

record :: M.Record
record = M.Record (6*9)

val :: M.Record
val = set (#a record) 42

-- Due to the type of set:
(#a record) :: record -> Lens record field -> record -> record
-- Type of 'record' must already be known:
#a :: M.Record -> b
-- # Resolution:
M.a :: M.Record -> Lens M.Record Int
-- The Ints match, and val is:
val = set (M.a record) 42
```


Unfortunately, it doesn't work with a lens get:

```wiki
get :: Lens record field -> record -> field
val = get #a (M.Record 42)
-- Due to type of 'get' and known type of 'record':
#a :: Lens M.Record field
-- Oops, can't apply # reduction, because its first argument type is 'Lens ...'
-- not 'M.Record ...'.
```


The \# resolution needs the \#a to be a function with a known first argument
type.  We could say that's fine because lenses are only really needed for
setting since getting is already a plain composable function, and write gets
like `#a_ record`.  But that's ugly and unsatisfying, and you can't pass
around the getter and setter as one unit.


Also, composed set will run into problems.  So lets build lenses into the
language.  Let's alter \# resolution so it expects a type `Lens a b` where
`a` is known, and looks in the module that defines `a`.  To make nicer
looking examples, I'll also assume we can write `deriving (Lens)` to
make ghc generate lenses for the fields instead of get functions.

```wiki
import qualified M
-- In M.hs: data Record = Record { a :: Int } deriving (Lens)

get :: Lens record field -> record -> field
val = get #a (M.Record 42)

-- Due to type of 'get' and known type of 'record':
#a :: Lens M.Record field
-- # resolution on Lens M.Record:
M.a :: Lens M.Record Int
-- val is
val = get M.a record
```


Let's try with composed lenses.

`set ((#b.#a) record) 42` should become `set ((Inner.b . Outer.a) record) 42`

```wiki
import qualified Outer
-- Outer.hs: data Outer = Outer { a :: Inner.Inner } deriving (Lens)
import qualified Inner
-- Inner.hs: data Inner = Inner { b :: Int } deriving (Lens)

-- A lens composition operator.  Most lens libraries overload (.) with this,
-- so we'd either need to make a new operator or move Control.Category into
-- the Prelude.  Let's go with a new operator:
(!) :: Lens b c -> Lens a b -> Lens a c

-- This is the more usual argument order for 'set'.  It makes it easier to
-- partially apply as well:
set :: Lens record field -> field -> record -> record

setB :: Outer.Outer -> Outer.Outer
setB = set (#b!#a) 42

-- Due to the type of 'set' and the type 'Outer.Outer' being declared in 'setB':
(#b!#a) :: Lens Outer.Outer field
-- Due to the definition of (!) and its return type being known:
#a :: Lens Outer.Outer b
-- # resolution on Lens Outer.Outer:
Outer.a :: Lens Outer.Outer Inner.Inner
-- Due to the second argument of (!) being known:
#b :: Lens Inner.Inner c
-- # resolution on Lens
Inner.b :: Lens Inner.Inner Int
-- result is
setB = set (Inner.b ! Outer.a) 42
```


For those unfamiliar with lenses, all `deriving (Lens)` would do is:

```wiki
data Record = Record { a :: A, b :: B } deriving (Lens)
-- becomes
data Record = Record { a_ :: A, b_ :: B } -- or a_, b_ could be gensyms
a = lens a_ (\rec a -> rec { a_ = a })
b = lens b_ (\rec b -> rec { b_ = b })
```


This is the same as TH macros for existing lens libraries.  And, of course, the old record update syntax is still down there, since the lens is built on top of it.  We can just stop using it so much.  Since it's backward compatible, we can gradually convert existing programs, there is no need for a giant patch of doom that has to simultaneously update an important record to the new records and to update all its call sites.


Note that there must be a known monomorphic type for the \#a so this may require
some type declarations:

```wiki
-- no type declared
set42 = set #a 42

-- Due to type of 'set':
#a :: Lens a b
-- 'a' is not monomorphic, error!
```


However, they should be rare in practice, since if `set42` is passed to a
function that gives it a monomorphic type or applied to a M.Record then the
ambiguity is resolved.

## discussion


If we can generalize \#-resolution to work on an arbitrary binary type constructor
argument like `(~>) Known x` then we don't need to build a Lens class in.  That's
nice because people can keep experimenting with libraries, especially since
different lens libraries have different features (e.g. StateT integration,
partial lenses, etc.).  However, in practice records aren't very satisfying
without a nice update syntax, so perhaps a lens library should be promoted to
the platform at least.  Can the new "generalized deriving" feature replace
ugly TH splices with a nice deriving clause or would that need to be built in?  At the least I think it would need an extension to suppress record declaration's automatic creation of get functions.


An example of an advantage of using lens libraries: fclabels has a notion of "partial lenses" that can return Nothing.  People who like that feature can have the lens deriver generate a partial lenses for `x` but a total lens for `y` in
`data R = R1 { x, y :: Int } | R2 { y :: Int }` which would solve the problem in a nice typesafe way rather than generating a `x` function that fails at runtime.  If we have built in lenses, or a record system with a built-in way of generating record accessors (morally equivalent), then we are stuck with whatever choice was baked into ghc.  Hopefully it's Maybe rather than runtime errors, but at least using an external lens library lets you retroactively fix things like that.

## pros

1. No effect on (.) operator, which is composition as always.  No "binds tighter than functions" or left-to-right vs. right-to-left controversy, and partial application works as it always did.
1. Record declaration syntax remains exactly the same.  Totally backward compatible, we can gradually convert existing programs.  Even convert an existing record field by field, no need for a single giant patch to update everything at once.
1. Works on any function, so it doesn't tie you to the implementation of a record, you can remove a field and add a compatibility shim.  So no tension between directly exposing the record implementation vs. writing a bunch of set/modify boilerplate.
1. It's not just record types, any lens can go in the lens composition, e.g. one for Data.Map.  So you can translate imperative `record.a.b[c].d = 42` to `set (#d . Map.lens c . #b . #a) 42 record`.  Make a new operator `(.>) = flip (.)` if you like left to right.
1. Module export list controls access over record fields as always.
1. Orthogonal to records: any function can be addressed.
1. "Support" for polymorphic and higher-ranked fields, via existing record update syntax.  It's a cheat because it's also con [\#2](https://gitlab.haskell.org//ghc/ghc/issues/2), but I think it's a valid design to build on top of the existing syntax instead of replacing it.  Maybe it can be extended to support fancy stuff later, but meanwhile it solves the simple case while not precluding the complicated one.

## cons

1. Still can't have two records with the same field name in the same module since it relies on modules for namespacing.
1. Lenses can't handle updates that change the type, e.g. from `Rec a` to `Rec b`.  If the `set` function is `Lens rec field -> field -> rec -> rec` then you can't change the type of `rec`.  I'm sure if this is solvable without the set being builtin syntax, or if a fancier lens implementation could admit `Lens rec1 rec2 field -> field -> rec1 -> rec2`.
1. It's another way to resolve a name to a different function body that's not typeclasses.  One way should be enough.  But typeclasses are fundamentally global.
1. The function to resolve must be monomorphic, so there is no "structural polymorphism" e.g. `getName :: (Has Name a) => a -> String`


From my point of view (elaforge), the pros are compelling, especially how
record fields aren't a built-in concept but are just normal haskell
identifiers.


My spin on the cons:

1. I don't mind.  To my mind, modules are there for namespace control and so two things shouldn't be able to have the same name in one module by definition.  I have very few records that really want to have the same field name \*and\* live in the same module, but for those who have more of those, nested modules would be an orthogonal feature that would satisfy them. You could argue that this is a necessary development, because modules are \*the\* user-controlled namespace and access control mechanism (typeclasses are always global and hence not that controlled), then the solution must be modules.  Any other solution is either going to be unsatisfactory because you can't control the scope of the names, or unsatisfactory because it's a non-module way of namespacing.

1. It's not too satisfying, but you can fall back to `rec { field = ... }`.  In practice I'd define a type specific update function as I do now, to avoid being tied `field` always existing in the record.  Still, it's worth thinking about whether this proposal could later be extended to support type-changing updates if there were further language support.

1. The global-ness of typeclass seems hard to reconcile with the idea of wanting to control export of record fields, so maybe this is unavoidable.

1. I'm not real fond of structural polymorphism anyway, I believe parametric and typeclass polymorphism are more principled.
