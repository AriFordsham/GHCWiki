# Overloaded record fields: a modest proposal


This is an attempt to redesign and clarify the design of the [OverloadedRecordFields](records/overloaded-record-fields) extension, in order to develop a plan for implementation.  It has benefited from the extensive discussion surrounding [Nikita Vokov's record library](records/volkov).  The following design choices are not set in stone, but are intended as a concrete proposal for further discussion.  For reference, here is the [previous design](records/overloaded-record-fields/design).

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

## Design choice 2: no changes to record selectors


In a change to previous versions of [OverloadedRecordFields](records/overloaded-record-fields), I propose that bare uses of the field refer only to the selector function, and work only if this is unambiguous.  Thus, in the above example `name :: Person -> String` but bare use of `personId` leads to a name resolution error.  This means that turning on [OverloadedRecordFields](records/overloaded-record-fields) for an existing module is a conservative extension: since the module can have no duplicate field names, everything still works.  Moreover, changes to GHC's renamer should be minimal.  In addition, uses of fields that are always unambiguous (because they mention the constructor, e.g. construction and pattern-matching) may freely use duplicated names.


However, we want some way to select and update overloaded fields.  Here we take an idea from the `record` library: provide a separate syntax for treating an identifier as a field, interpreted as a lens.  This syntax is open for discussion but as a straw man proposal I suggest a prefix `#` sign (which is unambiguous because `MagicHash` permits it only as a suffix).  The syntactic distinction means that we can identify overloaded uses of fields without complicated name resolution rules.


For example, one could write

```wiki
f v = over #x (+1) v
```


as the function that adds 1 to the `x` field of any type, where `over` is defined in your favourite lens library.


To make this overloading work, we will have a new class

```wiki
class IsRecordField (n :: Symbol) t where
  field :: Proxy# n -> t
```


and the typechecker will syntactically expand `#x` to `field (proxy# :: Proxy# "x")` so that as far as the user is concerned, `#x :: IsRecordField "x" t => t`.  Each lens library can give an appropriate instance for `IsRecordField`, though there are a number of choices for the `(->)` instance:

1. provide an instance in base for `IsRecordField n (r -> a)`, allowing `#x` to be used as a selector function but requiring a combinator at the use site to convert it into a van Laarhoven lens;

1. provide an instance in base for `IsRecordField n ((a -> f b) -> (r -> f s))`, allowing `#x` to be used as a van Laarhoven lens but requiring a combinator to convert it into a selector function;

1. provide both instances in base, with some clever typeclass trickery to avoid incoherence (which is perfectly possible but might lead to confusing inferred types);

1. provide neither instance in base, so use of `#x` as either a selector function or a van Laarhoven lens would require either an orphan instance or conversion via a combinator.

## Design choice 3: sugar-free magic classes


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


These were previously called `Has` and `Upd`, but I suggest using longer and hopefully more meaningful names. There is substantial bikeshedding to be done about the details of these definitions (names, parameter number and order), but it should not substantially alter the proposal.


Rather than giving instances for these classes directly, they will be implicitly created by the typechecker as required (similarly to `Coercible`), so there is no code generation overhead for datatype definitions other than the existing selector functions and a small new updater function.  Moreover, users will be permitted to write their own instances, provided they will not clash with the automatically generated ones.  This permits virtual fields, and a library like `record` could make its anonymous records work seamlessly with uses of [OverloadedRecordFields](records/overloaded-record-fields).


I propose we drop the `r { x :: t }` syntactic sugar for `(HasField "x" r, FieldType "x" r ~ t)`, because it's hard to avoid the underlying representation leaking out (e.g. when updates are involved).

## Design extension: anonymous records


Note that if the above extension is implemented, a library like `record` can reuse its typeclass machinery in order to work seamlessly with the `#` syntax.  Moreover, we could subsequently add a syntax for anonymous record types (for example `{| x :: Int, y :: Int |}`) which would be entirely compatible with the `#` syntax.
