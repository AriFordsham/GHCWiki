# Overloaded record fields: a modest proposal


This is an attempt to redesign and clarify the design of the [OverloadedRecordFields](records/overloaded-record-fields) extension, in order to develop a plan for implementation.  It has benefited from the extensive discussion surrounding [Nikita Volkov's record library](records/volkov).  The following design choices are not set in stone, but are intended as a concrete proposal for further discussion.  For reference, here is the [previous design](records/overloaded-record-fields/design).

[Implementation notes here](records/overloaded-record-fields/implementation).


See also the [ high-level summary of the current plan on the Well-Typed blog](http://www.well-typed.com/blog/2015/03/overloadedrecordfields-revived/).

## Part 1: Records with duplicate fields

### Use existing Haskell records


The `AllowDuplicateRecordFields` extension permits existing Haskell records to use duplicate field labels.  Thus the following is legal in a single module:

```wiki
data Person  = Person  { personId :: Int, name :: String }
data Address = Address { personId :: Int, address :: String }
```


While we might choose to add anonymous records later, they are not central to the design.  In particular, this means that

- all existing features of Haskell datatypes, such as multiple constructors, strictness and unpacking, are supported unchanged;

- abstraction and representation hiding work just as in normal Haskell: if a field selector is not exported, client code cannot observe it;

- application code can use `AllowDuplicateRecordFields` even with libraries that do not;

- no new declaration syntax is added.


For each field in each record datatype, regardless of whether the extension is enabled, a selector function and an update function will be generated (at present, only a selector function is generated).

### No changes to record selectors, construction or update


Bare uses of the field refer only to the selector function, and work only if this is unambiguous.  Thus, in the above example `name :: Person -> String` but bare use of `personId` leads to a name resolution error.  This means that turning on `AllowDuplicateRecordFields` for an existing module is a conservative extension: since the module can have no duplicate field names, everything still works.  Moreover, changes to GHC's renamer should be minimal.  In addition, uses of fields that are always unambiguous (because they mention the constructor, e.g. construction and pattern-matching) may freely use duplicated names.


Even though a field label is duplicated in its defining module, it may be possible to use the selector unambiguously elsewhere. For example, another module could import `Person(personId)` but not `Address(personId)`, and then use `personId` unambiguously. Thus it is not enough simply to avoid generating selector functions for duplicated fields.


We propose *no change whatsoever to how Haskell 98 records are constructed* (e.g. `MkT { x = 3, y = True }`). Moreover, we propose *no change to how records are updated*, which remains monomorphic (e.g. `t { y = False }`).  If there are many `y` fields in scope, the type of the context must fix which one is intended, or a type annotation must be supplied.  This is a soft spot, but there is really no way around it because Haskell's type-changing update requires modifying multiple fields simultaneously.

## Part 2: Overloaded labels


Bare field names in expressions refer to the selector function only if unambiguous, so how are we want to select and update overloaded fields?

### Digression: implicit parameters


First, let's review Haskell's existing and long-standing *[ implicit parameters](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/other-type-extensions.html#implicit-parameters)*.
Here is how they work in GHC today.

- There is a class `IP` defined thus in `GHC.IP`:

  ```wiki
  class IP (x :: Symbol) a | x -> a where
    ip :: a

  -- Hence ip's signature is
  --    ip :: forall x a. IP x a => a
  ```
- When you write `?x` in an expression, what GHC does today is to replace it with `(ip @ "x" @ alpha)`, where `alpha` is a unification variable and `@` is type application.  (This isn't valid source Haskell, which doesnt have type application, but GHC certainly does have type application internally, so we don't need proxy arguments here.

- Of course, that call `(ip @ "x" @ alpha)` gives rise to a constraint `IP "x" alpha`, which must be satisfied by the context.

- The form `?x` in an expression is only valid with `{-# LANGUAGE ImplicitParameters #-}`

- The pretty printer displays the constraint `IP x t` as `?x::t`.

- The functional dependency `x->a` on class `IP` implements the inference rules for implicit parameters. (See the [ orginal paper](http://galois.com/wp-content/uploads/2014/08/pub_JL_ImplicitParameters.pdf).)

- There is some magic with implicit-parameter bindings, of form `let ?x = e in ...`, which in effect brings into scope a local instance declaration for `IP`.


And that's really about it.  The class `IP` is treated specially in a few other places in GHC.  If you are interested, grep for the string "`isIP`".

### Implicit values


Now consider the following class:

```wiki
class IsLabel (x :: Symbol) a where
  fromLabel :: a
```


Exactly like `IP` but without the functional dependency. It is also rather similar to a version of the `IsString` class from `OverloadedStrings`, but with an additional parameter making the string available at the type level.


It behaves like this:

- When you write `#x` in an expression, what GHC does is to replace it with `(fromLabel @ "x" @ alpha)`, where `alpha` is a unification variable and `@` is type application.   Just like implicit parameters, in fact.

- Of course the call `(fromLabel @ "x" @ alpha)` gives rise to a constraint `(IsLabel "x" alpha)` which must be satisfied by the context.

- The form `#x` in an expression is only valid with `{-# LANGUAGE OverloadedLabels #-}` (which is implied by `OverloadedRecordFields`).

- The pretty printer could print `IsLabel "x" t` as `#x::t` (**AMG**: I don't plan to implement this initially).

- There is no functional dependency, and no equivalent to the implicit-parameter `let ?x=e` binding.  So overloaded labels are much less special than implicit parameters.


Notice that overloaded labels might be useful for all sorts of things that are nothing to do with records; that is why they don't mention "record" in their name.


User code can never (usefully) call `fromLabel` (or `ip`) directly, because without explicit type application there is no way to fix `x`.

**AMG**: I've switched to the name `OverloadedLabels` rather than `ImplicitValues`. The connection to implicit parameters is nice from an implementation point of view, but I'm not sure how relevant it is to users, and the behaviour is rather more like a souped-up `OverloadedStrings`.

## Part 3: Polymorphism over record fields

### Overloaded record fields


Under this proposal, when GHC encounters a data type declaration with record fields, it generates (or behaves as if it generates -- see "Implementation notes" below) instances of two new classes, `HasField` and `FieldUpdate`.  For example, given

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

### Syntax

**Design question**: it's not absolutely necessary to use `#x` for a field.  Here are some alternatives:

- We could use `@x`, though that would prevent it being used for explicit type application (which is common practice in writing, even if the extension to permit it in Haskell syntax hasn't made much progress). This is the syntax used by [ record-preprocessor](http://hackage.haskell.org/package/record-preprocessor).

- We could say "if there is at least one data type in scope with a field `x`, then `x` is treated like `(fromLabel @ "x" @ alpha)`".  But I hate it.  And it doesn't work for virtual fields like `#area` above.

- (Suggested by Edward K.)  We could define a magic module `GHC.ImplicitValues`, and say that if you say

  ```wiki
  import GHC.ImplicitValues( p, q, area )
  ```

  then all occurrences of `p`, `q`, `area` will be treated as implicit values (written `#p`, `#q`, `#area` above).  That has the merit that it works fine for virtual fields like `area`, and it removes the `#p` syntactic clutter.

>
> It leaves open questions.  If you declare a H98 record with fields `p`, etc, do you have to import `p` from `GHC.ImplicitValues` as well?  Presumably not?  What if you *import* such a record?


But *neither of these exploit the similarity to implicit parameters*.
I really really like the similarity between the models, and I think it'd be a pity to lose it.
And would implicit parameters *really* be better (from a software engineering point of view) if we replaced `?x` notation with `import GHC.ImplicitParameters( x )`?


Note that the `#x` form only behaves specially if you have `OverloadedLabels` or `OverloadedRecordFields` enabled. So existing libraries that use `#` as an operator will work fine.  If you want `OverloadedRecordFields` as well, you'll have to put a space between an infix `#` and its second argument, thus `(a # b)` not `(a #b)`.  But that's not so bad. And exactly the same constraint applies with `MagicHash`: you must put a space between the `a` and the `#`, not `(a# b)`.  I don't think this is a big deal.


The downside of the `#x` syntax is that uses of lenses like `foo^.bar.baz` become something like `foo ^. #bar . #baz` or `foo ^. xx #bar . xx #baz` (if we need a combinator `xx` to turn an implicit value into a lens). However, this can be mitigated to some extent by users by making their own definitions `bar = xx #bar; baz = xx #baz`.

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

### Implementation notes

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

### Reflections


An `IsLabel` constraint is, in effect, rather like a (family of) single-method type classes.  Instead of

```wiki
f :: Ix a => a -> a -> a -> Bool
f i u l = inRange (l,u) i
```


which uses only one method from `Ix`, you could write the finer-grained function

```wiki
f :: (IsLabel "inRange" ((a,a) -> a -> Bool))
  => a -> a -> Bool
f i u l = #inRange (l,u) i
```


Note that this example has nothing to do with records, which is part of the point.
Perhaps `IsLabel` will find other uses.
It is rather reminiscent of Carlos Camaro's [ System CT](http://homepages.dcc.ufmg.br/~camarao/CT/).

## Summary


We propose three essentially orthogonal additions to GHC:

1. `HasField` and `FieldUpdate` typeclasses, with special-purpose constraint solving behaviour (just like `Coercible`, we do not require a special extension to enable this, as its effect is limited to code that imports the relevant module);
1. an extension `OverloadedLabels` to enable the `#x` syntax, interpreted with the `IsLabel` typeclass;
1. an extension `AllowDuplicateRecordFields` to permit the same field name to be used multiple times in the same module.


The `OverloadedRecordFields` extension is then defined as the combination of `OverloadedLabels` and `AllowDuplicateRecordFields`.


These are all useful independently, but complement each other:

- Without either of the extensions, the special typeclasses allow users to write code that works for all datatypes with particular fields (albeit without a nice built-in syntax).
- `OverloadedLabels` uses the special typeclasses through the instance for `IsLabel x (r -> a)`, but is also useful when used at other types (e.g. we could give an instance `IsLabel x (Proxy x)` to allow implicit values to represent Symbol proxies).
- `AllowDuplicateRecordFields` is perfectly sensible without `OverloadedLabels`: it allows duplicate field names provided they are not used ambiguously.
