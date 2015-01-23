# Overloaded record fields: a modest proposal


This is an attempt to redesign and clarify the design of the [OverloadedRecordFields](records/overloaded-record-fields) extension, in order to develop a plan for implementation.  It has benefited from the extensive discussion surrounding [Nikita Volkov's record library](records/volkov).  The following design choices are not set in stone, but are intended as a concrete proposal for further discussion.  For reference, here is the [previous design](records/overloaded-record-fields/design).

## Design choice 1: use existing Haskell records


The [OverloadedRecordFields](records/overloaded-record-fields) extension permits existing Haskell records to overload field names.  Thus the following is legal in a single module:

```wiki
data Person  = Person  { personId :: Int, name :: String }
data Address = Address { personId :: Int, address :: String }
```


While we might choose to add anonymous records later, they are not central to the design.  In particular, this means that

- all existing features of Haskell datatypes, such as multiple constructors, strictness and unpacking, are supported unchanged;

- abstraction and representation hiding work just as in normal Haskell: if a field selector is not exported, client code cannot observe it;

- application code can use [OverloadedRecordFields](records/overloaded-record-fields) even with libraries that do not;

- no new declaration syntax is added.


For each field in each record datatype, regardless of whether the extension is enabled, a selector function and an update function will be generated (at present, only a selector function is generated).

## Design choice 2 (Plan A): no changes to record selectors


In a change to previous versions of [OverloadedRecordFields](records/overloaded-record-fields), I propose that bare uses of the field refer only to the selector function, and work only if this is unambiguous.  Thus, in the above example `name :: Person -> String` but bare use of `personId` leads to a name resolution error.  This means that turning on [OverloadedRecordFields](records/overloaded-record-fields) for an existing module is a conservative extension: since the module can have no duplicate field names, everything still works.  Moreover, changes to GHC's renamer should be minimal.  In addition, uses of fields that are always unambiguous (because they mention the constructor, e.g. construction and pattern-matching) may freely use duplicated names.


However, we want some way to select and update overloaded fields.  Here we take an idea from the `record` library: provide a separate syntax for treating an identifier as a field, interpreted as a lens.  The syntactic distinction means that we can identify overloaded uses of fields without complicated name resolution rules.  This syntax is open for discussion; suggestions include:

- using a prefix `#` sign (but this conflicts with use of `#` as an operator, which would require spaces);
- using a prefix `@` sign (but this has previously been earmarked for [ExplicitTypeApplication](explicit-type-application)).


For example, one could write

```wiki
f v = over #x (+1) v
```


as the function that adds 1 to the `x` field of any type, where `over` is defined in your favourite lens library.


A slightly different suggestion is not to syntactically distinguish fields, but to designate a magic module name (e.g. `GHC.Records`) and make the renamer resolve imported names from that module into fields. For example, we could permit this:

```wiki
import GHC.Records as R

view R.x      rec
set  R.x 3    rec
over R.x (+2) rec
```


However, this means that we have to use qualified names for fields, or there will be clashes with the selector functions.


The remainder of this page currently assumes the `#` syntax.

## Design choice 3 (Plan A): pick-and-mix lenses


To make interpret fields as lenses, we will have a new class

```wiki
class IsRecordField (n :: Symbol) t where
  field :: Proxy# n -> t
```


and the typechecker will syntactically expand `#x` to `field (proxy# :: Proxy# "x")` so that as far as the user is concerned, `#x :: IsRecordField "x" t => t`.  Each lens library can give an appropriate instance for `IsRecordField`, though there are a number of choices for the `(->)` instance:

1. provide an instance in base for `IsRecordField n (r -> a)`, allowing `#x` to be used as a selector function but requiring a combinator at the use site to convert it into a van Laarhoven lens;

1. provide an instance in base for `IsRecordField n ((a -> f b) -> (r -> f s))`, allowing `#x` to be used as a van Laarhoven lens but requiring a combinator to convert it into a selector function;

1. provide both instances in base, with some clever typeclass trickery to avoid incoherence (which is perfectly possible but might lead to confusing inferred types);

1. provide neither instance in base, so use of `#x` as either a selector function or a van Laarhoven lens would require either an orphan instance or conversion via a combinator.


We could also choose a canonical lens representation and make `#x` produce a lens in that representation, which is effectively what the `record` library does. This would be simpler, and removes the need for the `IsRecordField` class, but it would require a combinator to use the field as a selector or any other lens type.

## Design choice 4 (Plan A): sugar-free magic classes


In order to write the `IsRecordField` instances, we need some way to solve constraints of the form "type `r` has a field `x` of type `a`".  This is provided by the `HasField` and `FieldUpdate` classes:

```wiki
-- | HasField n r means that r is a record type with a field n
class HasField (n :: Symbol) r where
  -- | The type of the field n in the record type r
  type FieldType n r
  -- | Extract the field from the record
  getField :: Proxy# n -> r -> FieldType n r

-- | FieldUpdate n r t means that r is a record type with a field n
--   that can be assigned a value of type t
class HasField n r => FieldUpdate (n :: Symbol) r t where
  -- The type of the updated record
  type UpdatedRecordType n r t
  setField :: Proxy# n -> r -> t -> UpdatedRecordType n r t
```


These were previously called `Has` and `Upd`, but I suggest using longer and hopefully more meaningful names. There is substantial bikeshedding to be done about the details of these definitions (names, parameter number and order), but it should not substantially alter the proposal. Note that these classes correspond to the `FieldOwner` class in the `record` library (two separate classes are needed to support type-changing update).


Rather than giving instances for these classes directly, they will be implicitly created by the typechecker as required (similarly to `Coercible`), so there is no code generation overhead for datatype definitions other than the existing selector functions and a small new updater function.  Moreover, users will be permitted to write their own instances, provided they will not clash with the automatically generated ones.  This permits virtual fields, and a library like `record` could make its anonymous records work seamlessly with uses of [OverloadedRecordFields](records/overloaded-record-fields).

## Design extension: sugar for class constraints


This is not necessary to begin with, but we may want `r { x :: t }` to be syntactic sugar for `(HasField "x" r, FieldType "x" r ~ t)`, although this might conflict with syntax for anonymous records. This is easy to desugar in the typechecker, but it is slightly harder to re-apply the sugar in inferred types and error messages. A similar syntax for updates would be nice, but it's not clear what.


In general, it is hard to ensure that we get nice inferred types and error messages that don't mention the type families unless absolutely necessary.

## Design extension: anonymous records


Note that if the above extension is implemented, even without any further work a library like `record` can reuse its typeclass machinery in order to work seamlessly with the `#` syntax.  Moreover, we could subsequently add a syntax for anonymous record types (for example `{| x :: Int, y :: Int |}`) which would be entirely compatible with the `#` syntax.


For example, the following should work fine:

```wiki
f :: HasField "x" r => r -> FieldType "x" r
f r = view #x r

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


These correspond to the existing `FieldOwner` instances in the `record` library.

---

## Design choice 2/3/4 (plan B): Implicit Values


I think Adam is going in the right direction with choices 2/3/4(Plan A).
But I want to propose a modest variant that is yet more orthogonal than 2/3/4(A).  It arose
from a discussion with Lennart Augustsson and Neil Mitchell. 


Simon PJ

### Implicit parameters


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


And that's really about it.  The class `IP` is treated specially in a few other places in GHC.  If you are interested, grep for the string "`isIP`\`.

### Implicit values


OK, so what has all this got to do with records?  Well, let me just change the name of Adam's `IsRecordField` class:

```wiki
class IV (x :: Symbol) a where
  iv :: a
```


Exactly like `IP` but without the functional dependency.  The "`IV`" stands
for "implicit values" (we can argue about the name later).  It behaves like this:

- When you write `#x` in an expression, what GHC does is to replace it with `(iv @ "x" @ alpha)`, where `alpha` is a unification variable and `@` is type application.   Just like implicit parameters, in fact.

- Of course the call `(iv @ "x" @ alpha)` gives rise to a constraint `(IV "x" alpha)` which must be satisfied by the context.

- The form `#x` in an expression is only valid with `{-# LANGUAGE ImplicitValues #-}` (which is implied by `OverloadedRecordFields`).

- The pretty printer prints `IV s t` as `#x::t`.

- There is no functional dependency, and no equivalent to the implicit-parameter `let ?x=e` binding.  So implicit values are much less special than implicit parameters.


Notice that implicit values might be useful for all sorts of things that are nothing to do with records; that is why I don't mention "record" in their name.

### Haskell 98 records


So does it have *anything* to do with records?  Yes, because there is one single piece of record-specific magic.  When you write 

```wiki
data T = MkT { x :: Int, y :: Bool }
```


then you always (regardless of langauge extensions) get this instance:

```wiki
instance (a ~ Int) => IV "x" (T -> a) where
  iv (MkT { x = x }) = x     -- The record selector for x in T
```


Moreover, if you do *not* have `{-# LANGUAGE OverloadedRecordFields #-}` you also get this
top-level function definition

```wiki
x :: T -> Int
x = #x    -- The top-level record selector for x in T
```


So in H98 mode, we get the top-level record selector `x`.  But with `{-# LANGUAGE OverloadedRecordFields #-}` we do not get the top-level selector, so we are free to write (in the same module)

```wiki
data S = MkS { x :: Bool }
```


which will generate

```wiki
instance (a ~ Bool) => IV (S -> a) where
  iv (MkS x) = x
```


The effect of all this is that we can freely use `#x` as an overloaded record selector.  Thus:

```wiki
xPlusOne r = #x r + 1::Int    -- Inferred type
                              -- xPlusOne :: IV "x" (r -> Int) => r -> Int
```


Notice that there is no need for a `HasField` class (unlike 4(A) above).

### Hand-written instances


It is perfectly fine to write instances of `IV` yourself.  For example, suppose we have various types
of geometric shapes.

```wiki
data Triangle = Tri Float Float Float  -- Base, height, angle
data Circle   = Circle Float           -- Radius
```


Then we are free to say

```wiki
instance (a ~ Float) => IV "area" (Triangle -> a) where
   iv (Tri b h _) = 0.5 * b * h
instance (a ~ Float) => IV "area" (Circle -> a) where
   iv (Circle r) = pi * r * r
```


Now `#area` behaves like a virtual record selector; it is not a field of `Circle` or `Triangle` but you can treat it just like one.

### Lenses


Suppose you write

```wiki
data T = MkT { x :: Int, y :: Bool }
$(makeLenses 'T)   -- Template Haskell splice
```


The `makeLenses` Template Haskell function is already in the `lens` library, but it
would be modified a bit.  It would generate something like

```wiki
instance IV "x" (Lens T Int) where
  iv = ...code for the lens for x in T...
```


and similarly for field `y`.  I am here assuming that `Lens` is a data type or newtype (contraray to the current story), and I'm ignoring the "s/t/a/b" complexities.


Once this is in place, and you need a lens, you just say `#x`.


Notice that GHC does not need to know about any of this stuff; `#x` behaves solely and precisely as described above. Lens library authors are free to experiment to their hearts content with various sorts of lenes and lens generation.


(In due course, when a consensus emerges, we might want build `makeLenses` into GHC and auto-generate them as we auto-generate record selectors; but that's a story for a later day.)

### Record construction and update


Like 2(A) I propose *no change whatsoever to how Haskell 98 records are constructed* (e.g. `MkT { x = 3, y = True }`).


Moreover, I propose *no change to how records are updated*, which remains monomorphic (e.g. `t { y = False }`).  If there are many `y` fields in scope, the type of `t` must fix which one is intended.  This is a soft spot, I admit, but there is really no way around it becuase Haskell's type-changing update simply precludes modifying one field at at time.  


If you want polymorphic update, use lenses. I don't think the half-way house of some kind of `FieldUpdate` class pays its way.

### Higher rank fields


Records whose fields have a rank-1 type present no problem for the `IV` story.  For example:

```wiki
data R1 a = MkR1 { x :: forall b. a -> b -> b }
```


Then GHC generates this instance:

```wiki
instance (f ~ a -> b -> b) => IV "x" (R1 a -> f) where
  iv (MkR1 x) = x
```


No impredicative types or any funny business.  But *do* get problems if a field has a higher rank type:

```wiki
data HR a = MkHR { x :: (forall b. b -> b) -> a -> a }
```


Now we cannot write

```wiki
instance (f ~ (forall b. b->b) -> a -> a) 
      => IV "x" (R1 a -> f) where
  iv (MkHRx) = x
```


beause this does require impredicativity.  Maybe one day, but not today.  In that case, you simply don't
get a `IV` instance.  Similar things happen today with [ ordinary records selectors for existentials.](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/data-type-extensions.html#existential-records)

### Nikita anonymous records


Everything here is orthogonal to, and entirely compatible with the Nikita anonymous-records story.
Nikita's `FieldOwner` becomes a use-caes of `IV`, I think.  Anonymous records might well be a fine thing, 
but they are a *separate* thing, which is good.

### Syntax


It's not absolutely necessary to use `#x` for a field.  We could say "if there is at lesat one data type in scope with a field `x`, then `x` is treated like `(iv @ "x" @ alpha)`".  But I hate it.  And it doesn't work for virtual fields like `#area` above.  So we'd end up wanting a top-level declaration 

```wiki
field area
```


or something, to say "treat an occurrence of `area` as `(iv @ "area" @ alpha)`.  


But really that is truly horrible.  And (most important) *it's not how we treat implicit parameters*.  I really really like the similarity between the models.


The `#x` form only behaves specially if you have `OverloadedRecordFields`. So existing libraries that use `#` as an operator will work fine.  If you want `OverloadedRecordFields` as well, you'll have to put a space between an infix `#` and its second argument, thus `(a # b)` not `(a #b)`.  But that's not so bad. And exactly the same constraint applies with `MagicHash`: you must put a space between the `a` and the `#`, not `(a# b)`.  I don't think this is a big deal.

### Implementation notes

**Instances**.  I said that the data type declaration

```wiki
data T = MkT { x :: Int }
```


generates the instance

```wiki
instance (Int ~ f) => IV "x" (T -> f) where
  iv (MkT x) = x
```


but GHC doesn't *actually* have to generate and compile 
a whole instance declaration.  It can simply have a 
built-in constraint solving rule for `(IV "x" (T -> t))` where `x` is a field of data type `T`.
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

instance (Int ~ f) => IV "x" (T -> f) where
  iv = $sel_T_x

x = $sel_T_x   -- The H98 selector, when OverloadedRecordFields is not on
```

### Reflections


An `IV` constraint is, in effect, rather like a (family of) single-method type classes.  Instead of

```wiki
f :: Ix a => a -> a -> a -> Bool
f i u l = inRange (l,u) i
```


which uses only one method from `Ix`, you could write the finer-grained function

```wiki
f :: (IV "inRange" ((a,a) -> a -> Bool)) 
  => a -> a -> Bool
f i u l = #inRange (l,u) i
```


Note that this example has nothing to do with records, which is part of the point. 
Perhaps `IV` will find other uses.
It is rather reminiscent of Carlos Camaro's [ System CT](http://homepages.dcc.ufmg.br/~camarao/CT/).
