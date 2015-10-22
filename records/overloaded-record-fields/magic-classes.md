# Magic classes for overloaded record fields


This page describes new built-in magic typeclasses that form part 3 of the [OverloadedRecordFields proposal](records/overloaded-record-fields). These are not a language extension as such, rather they are classes with special-purpose solver behaviour (like `Coercible` and `Typeable`).

## Design


Under this proposal, when GHC encounters a data type declaration with record fields, it generates (or behaves as if it generates -- see "Implementation" below) instances of two new classes, `HasField` and `FieldUpdate`.  For example, given

```wiki
data T = MkT { x :: Int }
```


GHC will generate

```wiki
instance HasField "x" T where
  type FieldType "x" T = Int
  getField _ (MkT x) = x

instance FieldUpdate "x" T Int where
  type UpdatedRecordType "x" T Int = T
  setField _ (MkT _) x = MkT x
```


These two classes are defined like this:

```wiki
-- | HasField x r means that r is a record type with a field x
class HasField (x :: Symbol) r where
  -- | The type of the field x in the record r
  type FieldType x r
  -- | Extract the field from the record
  getField :: Proxy# x -> r -> FieldType x r

-- | FieldUpdate x r a means that r is a record type with a field x
--   that can be assigned a value of type a
class HasField x r => FieldUpdate (x :: Symbol) r a where
  -- | The type of the updated record
  type UpdatedRecordType x r a
  -- | Set the field in the record
  setField :: Proxy# x -> r -> a -> UpdatedRecordType x r a
```


These were previously called `Has` and `Upd`, but I suggest using longer and hopefully more meaningful names. There is substantial bikeshedding to be done about the details of these definitions (names, parameter number and order, whether to use functional dependencies or type families), but it should not substantially alter the proposal. Note that these classes correspond to the `FieldOwner` class in the `record` library.


More precisely, 

- GHC will generate a `HasField` instance whenever it currently generates a selector.  Not every field has a selector today, because of existentials (see [ user manual](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/data-type-extensions.html#existential-records)).  Moreover, it will only generate a `HasField` instance if the field type is rank-0, i.e. it has no foralls.  (Even rank-1 types are out, because they violate the functional dependency on `HasField`, and higher ranks would need impredicative polymorphism.)

- Similarly, GHC will generate a `FieldUpdate` instance only for rank-0 fields, that is, ones with no foralls.  (Same reason: impredicativity.)  Plus, of course, ones that do not mention an existential variable.  There are some slightly subtle conditions about when `FieldUpdate` permits updates to change parameter types, as describe in [the original design](records/overloaded-record-fields/design#).


Crucially, we must have some kind of relationship between the record type and field type of `HasField` (and similarly for `FieldUpdate`): otherwise, the composition of two fields (using a three-parameter version of `HasField`)

```wiki
getField (proxy# :: Proxy# "g") . getField (proxy# :: Proxy# "f")
    :: (HasField "g" b c, HasField "f" a b) => a -> c
```


has an ambiguous type variable `b`, and we get terrible type inference behaviour. The options are:

1. a three-parameter class with a functional dependency: `HasField x r a | x r -> a`

1. a three-parameter class with a type family simulating a functional dependency: `a ~ FieldType x r => HasField x r a`

1. a two-parameter class `HasField x r` with a type family `FieldType x r`


Options 2 and 3 were explored in the original `OverloadedRecordFields`, and are pretty much equivalent; we initially thought 2 might give nicer inferred types but in fact they tend to be of the form `HasField x r (FieldType x r)` so we might as well go for the two-parameter class. Option 1 should give prettier inferred types (and be easier to use with the `r { x :: t }` syntactic sugar described below), but the lack of evidence for fundeps, and the inability to mention the type of a field, may be restrictive in hard-to-predict ways. At the moment, this page is written to assume option 3.

### Back to overloaded labels


How are records and overloaded labels connected?  They are connected by the `IsLabel` instance for functions

```wiki
instance (HasField x r, a ~ FieldType x r) => IsLabel x (r -> a) where
  fromLabel = getField (proxy# :: Proxy# x)
```


which would allow us to freely use `#x` as an overloaded record selector.  Thus:

```wiki
xPlusOne r = #x r + 1::Int    -- Inferred type
                              -- xPlusOne :: (HasField "x" r, FieldType "x" r ~ Int) => r -> Int
```


Alternatively, we might choose to give this instance

```wiki
instance (Functor f, FieldUpdate x s b, a ~ FieldType x s, t ~ UpdatedRecordType x s b)
        => IsLabel x ((a -> f b) -> s -> f t) where
  fromLabel w s = setField (proxy# :: Proxy# x) s <$> w (getField (proxy# :: Proxy# x) s)
```


which would allow us to use `#x` as a lens that identifies record fields of that name, for example

```wiki
f v = over #x (+1) v -- Inferred type
                     -- f :: (FieldUpdate "x" r r a a, Num a) => r -> r
```


In either case, lens libraries other than `lens`, which have data types for their lenses, could also give `IsLabel`
instances and provide additional meanings to the `#x` syntax.  Lens library authors are free to experiment to their hearts' content with various sorts of lenes and lens generation.  Similarly, extensible records libraries can be integrated.

**Design question**: there are in fact a number of choices for the `(->)` instance for `IsLabel`:

1. provide an instance in base for `IsLabel n (r -> a)`, allowing `#x` to be used as a selector function but requiring a combinator at the use site to convert it into a van Laarhoven lens;

1. provide an instance in base for `IsLabel n ((a -> f b) -> (r -> f s))`, allowing `#x` to be used as a van Laarhoven lens but requiring a combinator to convert it into a selector function;

1. provide both instances in base, with some clever typeclass trickery to avoid incoherence (which is perfectly possible but might lead to confusing inferred types);

1. provide neither instance in base, so use of `#x` as either a selector function or a van Laarhoven lens would require either an orphan instance or conversion via a combinator.

**Design question**: we could sidestep the whole question by instead translating `#x` to a proxy of type `Proxy "x"`, avoiding the need for the `IsLabel` class, but requiring a library to provide a function to convert the symbol to a selector/lens. This is slightly simpler, but perhaps too great a sacrifice of convenience.

### Hand-written instances


It is perfectly fine to write instances of `HasField` or `FieldUpdate` yourself.  For example, suppose we have various types of geometric shapes.

```wiki
data Triangle = Tri Float Float Float  -- Base, height, angle
data Circle   = Circle Float           -- Radius
```


Then we are free to say

```wiki
instance HasField "area" Triangle where
   type FieldType "area" Triangle = Float
   getField _ (Tri b h _) = 0.5 * b * h
instance HasField "area" Circle where
   type FieldType "area" Circle = Float
   getField _ (Circle r) = pi * r * r
```


Now `#area` behaves like a virtual record selector; it is not a field of `Circle` or `Triangle` but you can treat it just like one.


The exact rules for when user-defined instances are legal will require some care, because to avoid incoherence/soundness issues we must ensure that they do not clash with any automatically generated instances. A user-defined instance `HasField x t` is legal if:

- `t` is a data type that has no fields, or

- `x` is a literal string and `t` is a data type that does not have the given field (though it may have other fields).

### Design extension: sugar for class constraints


This is not necessary to begin with, but we may want `r { x :: t }` to be syntactic sugar for `(HasField "x" r t)`, although this might conflict with syntax for anonymous records. This is easy to desugar in the typechecker, but it is slightly harder to re-apply the sugar in inferred types and error messages. A similar syntax for updates would be nice, but it's not clear what.


In general, it is hard to ensure that we get nice inferred types and error messages that don't mention the type families unless absolutely necessary. Initially we plan not to implement the syntactic sugar.

### Design extension: anonymous records


Everything here is orthogonal to, and entirely compatible with the Nikita anonymous-records story.
Nikita's `FieldOwner` becomes a use-case of `HasField(Lens)`, and without any further work a library like `record` can reuse its typeclass machinery in order to work seamlessly with the `#` syntax.  Anonymous records might well be a fine thing, but they are a *separate* thing, which is good.


Moreover, we could subsequently add a syntax for anonymous record types (for example `{| x :: Int, y :: Int |}`) which would be entirely compatible with the `#` syntax.


For example, the following should work fine:

```wiki
f :: HasField "x" r => r -> FieldType "x" r
f r = #x r

z :: [r| { x :: Int, y :: Int } |]
z = [r| { x = 3, y = 2 } |]

a :: Int
a = f z
```


For this to be possible, the `Record<n>` tuple datatypes defined by the `record` library would need to have instances for `HasField` and `FieldUpdate` that are polymorphic in the name of the field, like this:

```wiki
data Record2 (n1 :: Symbol) v1 (n2 :: Symbol) v2 =
  Record2 v1 v2

instance HasField n1 (Record2 n1 v1 n2 v2) where
  type FieldType n1 (Record2 n1 v1 n2 v2) = v1
  getField _ (Record2 x _) = x

instance HasField n2 (Record2 n1 v1 n2 v2) where
  type FieldType n2 (Record2 n1 v1 n2 v2) = v2
  getField _ (Record2 _ x) = x
```


These correspond to the existing `FieldOwner` instances in the `record` library. (Actually this doesn't quite work, because the two instances for `FieldType` overlap, but it is possible with a bit more trickery.)

## Implementation

**Instances**.  We said that the data type declaration

```wiki
data T = MkT { x :: Int }
```


generates the instance

```wiki
instance HasField "x" T where
  type FieldType "x" T = Int
  getField _ (MkT x) = x
```


but GHC doesn't *actually* have to generate and compile
a whole instance declaration.  It can simply have a
built-in constraint solving rule for `(HasField "x" T)` and `(FieldType "x" T)` where `x` is a field of data type `T`.
The programmer will not know or care, but it should remove a plethora of instances.


Similar special purpose rules in the solver deal with `(s ~ t)` and `(Coercible s t)` constraints.
It's easy to do.


Hand written instances are still useful though.

**Selectors**.  The above code shows the field-selection code as part of the instance declaration, but we may not want ot generate that code repeatedly (in the special-purpose solver).  Field selection may be less trivial than it looks in when we have UNPACK pragmas; e.g.

```wiki
data T = MkT { x :: {-# UNPACK #-} !S }
data S = MkS {-# UNPACK #-} !Int Int
```


So we'll probably generate this:

```wiki
$sel_T_x :: T -> S
$sel_T_x (MkT x) = x

instance HasField "x" T where
  type FieldType "x" T = S
  getField _ = $sel_T_x
```


The `HasField` and `FieldUpdate` classes, and `FieldType` and `UpdatedRecordType` type families, will be defined in the module `GHC.Records` in the `base` package.  Contrary to the previous design, we will not generate any dfuns/axioms for these classes \*at all\*.  Instead, the typechecker will implicitly create evidence as required.  This gets rid of a whole lot of complexity.


The only additional things that need to be generated at datatype declarations are updater functions (one per field), which correspond to the selector functions that are already generated.  So for example

```wiki
data T = MkT { x, y :: Int }
```


will generate

```wiki
$sel:x:T :: T -> Int
$sel:x:T (MkT x _) = x

$upd:x:T :: T -> Int -> T
$upd:x:T (MkT _ y) x = MkT x y
```


The updater function will always have a name prefixed with `$upd:`, regardless of whether `OverloadedRecordFields` is enabled.

### GADT record updates


Consider the example

```wiki
data W a where
    MkW :: a ~ b => { x :: a, y :: b } -> W (a, b)
```


It would be nice to generate

```wiki
-- $upd:x:W :: W (a, b) -> a -> W (a, b)
$upd:x:W s e = s { x = e }
```


but this record update is rejected by the typechecker, even though it is perfectly sensible, because of [\#2595](https://gitlab.haskell.org//ghc/ghc/issues/2595). The currently implemented workaround is instead to generate the explicit update

```wiki
$upd:x:W (MkW _ y) x = MkW x y
```


which is fine, but rather long-winded if there are many constructors or fields. Essentially this is doing the job of the desugarer for record updates.


Note that `W` does not admit type-changing single update for either field, because of the `a ~ b` constraint. Without it, though, type-changing update should be allowed.

### Unused bindings


Unused local bindings are tricky in the presence of the magic type classes, as the following example illustrates:

```wiki
module M (f) where

data S = MkS { foo :: Int }
data T = MkT { foo :: Int }

f = #foo (MkS 3)
g x = #foo x
```


The renamer calculates the free variables of each definition, to produce a list of `DefUses`. This is then used to report unused local bindings.  However, we will not discover until the typechecker that `f` uses `S(foo)`, and hence may report it as unused. Note that `g` uses neither `S(foo)` nor `T(foo)`.


It doesn't really make sense to have an occurrence of an overloaded label in an expression return as free variables all the selectors it might refer to, because in the new story, an overloaded label might not have anything to do with fields. Moreover, the typechecker might encounter and solve a `HasField` constraint that was introduced without using the overloaded label syntax.

TODO can we defer reporting of unused local bindings until after the typechecker, and make use of the information it records?
