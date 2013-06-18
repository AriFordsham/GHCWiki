# Overloaded record fields: a plan for implementation


This is a plan to implement overloaded record fields, along the lines of SPJ's [Simple Overloaded Record Fields](records/overloaded-record-fields) proposal, as a Google Summer of Code project. (See the [ GSoC project details](http://www.google-melange.com/gsoc/project/google/gsoc2013/adamgundry/23001), for reference.) The page on [Records](records) gives the motivation and many options.  In particular, the proposal for [Declared Overloaded Record Fields](records/declared-overloaded-record-fields) is closely related but makes some different design decisions.

## Design

**SLPJ** this section should be a careful design specfication.

### Motivation


A serious limitation of the Haskell record system is the inability to overload field names in record types: for example, if the data types

```wiki
data Person  = Person  { personId :: Int, name :: String }
data Address = Address { personId :: Int, address :: String }
```


are declared in the same module, there is no way to determine which type an occurrence of the personId record selector refers to. A common workaround is to use a unique prefix for each record type, but this leads to less clear code and obfuscates relationships between fields of different records. Qualified names can be used to distinguish record selectors from different modules, but using one module per record is often impractical.


Instead, we want to be able to write postfix polymorphic record projections, so that `e.personId` resolves the ambiguity using the type of `e`. In general, this requires a new form of constraint `r { x :: t }` stating that type `r` has a field `x` of type `t`. For example, the following declaration should be accepted:

```wiki
getPersonId :: r { personId :: Int } => r -> Int
getPersonId e = e.personId
```

### Record field constraints

**AMG** the following needs updating.


If the flag `-XOverloadedRecordFields` is enabled, a new form of 'record field' constraints `r { x :: t } :: Constraint` are available where `r` and `t` are types, while `x` is a literal string.  These are not typeclass constraints and do not have instances as such (though see the discussion below about virtual record fields). They can appear in contexts in the usual way (that is, they are a new primitive, like equality constraints or implicit parameter constraints). Multiple fields may be written comma-separated in a single constraint as in `r { x :: t, y :: u }`.

**SLPJ note**.  I think it's more helpful to introduce it as a type-class constraint that happens to have convenient concrete syntax:

```wiki
class Has (r::*) (s::Symbol) (t::*) where
  getFld :: r -> t

-- data T a = MkT { x :: [a] }
instance (b ~ [a]) => Has (T a) "r" b where
  getFld (MkT { x = x }) = x
```


The `(b ~ [a])` in the instance is important, so that we get an instance match from the first two fields only.


Then we have convenient syntax for `(Has r "x" t)`, namely `r { x::t }`.  Moreover, we get convenient syntax for conjunctions: `(Has r "x" tx, Has r "y" ty)` has shorthand `r { x::tx, y:: ty }`.


Don't forget that the `r` might be an arbitrary type not just a type variable or type constructor.  For example, `(Has (T (Maybe b)) "x" [Maybe v])` is a perfectly fine (and soluble) constraint.  I suppose that its shorthand looks like `T (Maybe v) { x :: [Maybe v] }`**End of SLPJ**

### Record field projections


Record field constraints are introduced by projections, which are written using a new left-associative infix operator `(.>)`.  That is, if `e :: r` then `e.>x :: r { x :: t } => t`.  This operator must always be applied to (at least) its second argument, so `(.>)` is invalid but `(.>x) :: forall a b . a { x :: b } => a -> b`. 


A constraint `R { x :: t }` is solved if `R` is a datatype that has a field `x` of type `t` in scope. (More precisely, if `R` contains `x :: s` then `t` must be an instance of `s`.) An error is generated if `R` has no field called `x`, it has the wrong type, or the field is not in scope. Otherwise, the new constraints are handled by the solver just like other types of constraint.


If multiple constructors for a single datatype use the same field name, all occurrences must have exactly the same type, as at present.

### Record selectors


Optionally, we could [add a flag \`-XNoMonoRecordFields\`](records/declared-overloaded-record-fields/no-mono-record-fields) to disable the generation of the usual monomorphic record field selector functions.  This is not essential, but would free up the namespace for other record systems (e.g. **data-lens**). Note that `-XOverloadedRecordFields` will generate monomorphic selectors by default for backwards compatibility reasons, but they will not be usable if multiple selectors with the same name are in scope.


When either flag is enabled, the same field label may be declared repeatedly in a single module (or a label may be declared when a function of that name is already in scope).

### Representation hiding


Since the new constraints are **not** typeclass constraints, it is reasonable for the constraint solver to consult the fields in scope when deciding whether a solution is valid. 

**SLPJ** As above, I'd like to say that they are just type-class constraints with special syntax.  However, maybe their instances (unlike most type-class instances) can be limited in scope; the instance is in scope iff the record field selector function is.  (Um; this sentence doesn't make so much sense if we suppress the record field selectors.) **End of SLPJ**


This enables representation hiding: exporting the field selector is a proxy for permitting access to the field. For example, consider the following module:

```wiki
module M ( R(x) ) where

data R = R { x :: Int }
data S = S { x :: Bool }
```


Any module that imports `M` will have access to the `x` field from `R` but not from `S`. Thus `R { x :: Int }` will be solved but `S { x :: Bool }` will not.

### Record update


Supporting polymorphic record update is rather more complex than polymorphic lookup. In particular:

- the type of the record may change as a result of the update;
- multiple fields must be updated simultaneously for an update to be type correct (so iterated single update is not enough); and
- records may include higher-rank components.


These problems have already been [described in some detail](records/overloaded-record-fields#record-updates). In the interests of doing something, even if imperfect, we plan to add a new syntax for monomorphic record update. This allows overloaded fields to be updated, though it requires the data constructor to be specified and cannot be abstracted over. For example,

```wiki
R { e | x = t }
```


means the same as `e { x = t }` except that the type is determined from the data constructor `R`, rather than the field name `x`. Thus it can be used where the latter is ambiguous.

**SLPJ. Not the *data* constructor, for sure.  Possibly the type constructor.  But in any case, this is mean to be a disambiguation of
**

```wiki
  e { x = t }
```


so surely it should look like

```wiki
  e {T| x = t }
```


where `T` is the type constructor.  


And there's a design choice here too.  Rather than special syntax we could resolve the ambiguity if you put a type signature in one of these two places:

```wiki
  e :: T Int { x = t }
or
  e { x = t } :: T Int
```


That's less invasive syntactially, and still does the job.
**End of SLPJ**

## Design choices

### The projection operator


As currently drafted, this proposal advocates using a new operator `.>` rather than changing the meaning of the dot operator, for reasons of backward compatibility and avoidance of a whole host of tricky parsing issues. This could change, if it is felt that the benefits of mimicking other languages outweigh the drawbacks of breaking backwards compatibility.

**SLPJ**.  I don't agree here.  Dot-notation is so convenient and so universal that I think we should use it.  And there isn't any ambiguity.  Dot notation is already space-aware: `M.x` is a qualified name whereas `M . x` is the composition of a data constructor `M` with a function `x`.  Similarly `r.x` can mean record selection, distinct from `r . x`.
**End of SLPJ**.

### Virtual record fields


The design presented above does not include virtual record fields, but it is easy to add them, by permitting typeclass-like instances:

```wiki
instance ctx => r { x :: t } where
  get = blah :: r -> t
```


The constraint solver can be extended to consider such virtual fields.  Note that if `r` is a datatype with a field `x`, the virtual field will overlap. The usual rules about overlap checking apply.

### Monomorphism restriction and defaulting


The monomorphism restriction may cause annoyance, since

```wiki
foo = \ e -> e.>x
```


would naturally be assigned a polymorphic type. If there is only one `x` in scope, perhaps the constraint solver should pick that one (analogously to the other defaulting rules). However, this would mean that bringing a new `x` into scope (e.g. adding an import) could break code. Of course, it is already the case that bringing new identifiers into scope can create ambiguity!

## Implementation details


We add a new family of constraints

```wiki
Has :: * -> Symbol -> * -> Constraint
```


where `r { x :: t }` is syntactic sugar for `Has r "x" t`, and `Symbol` is the kind of type-level strings. Evidence for `Has r l t` is just a projection function of type `r -> t`. Thus it is exactly the same as if `Has` were defined via a typeclass

```wiki
class Has r (l :: Symbol) t where
  get :: r -> t
```


However, evidence is supplied by the constraint solver, taking into account the fields (or virtual field instances) that are in scope. These are not just typeclass constraints.

### Example of constraint solving

**SLPJ** Making the first example rely on the monomorphism restriction is not a good plan!


Consider the example

```wiki
module M ( R(R, x), S(S, y), T(T, x) ) where

  data R = R { x :: Int }
  data S = S { x :: Bool, y :: Bool }
  data T = T { x :: forall a . a }

module N where
  import M

  foo e = e.>x

  qux = (.>y)
 
  bar :: Bool
  bar = foo T

  baz = foo S
```


When checking `foo`, `e` is a variable of unknown type `alpha`, and the projection generates the constraint `alpha { x :: beta }` where `beta` is fresh. 
This constraint cannot be solved immediately, so generalisation yields the type `a { x :: b } => a -> b`.


When checking `qux`, the projection has type `alpha -> beta` and generates the constraint `alpha { y :: beta }`. However, the monomorphism restriction prevents this constraint from being generalised. There is only one `y` field in scope, so defaulting specialises the type to `S -> Bool`. If the `x` field was used, it would instead give rise to an ambiguity error.


When checking `bar`, the application of `foo` gives rise to the constraint `T { x :: Bool }`, which is solved since `Bool` is an instance of `forall a . a` (the type `T` gives to `x`).


When checking `baz`, the constraint `S { x :: gamma }` is generated and rejected, since the `x` from `S` is not in scope.

### Remark

`Has` constraints are slightly more general than the syntactic sugar suggests: one could imagine building constraints of the form `Has r l t` where `l` is non-canonical, for example a variable or type family. It's hard to imagine uses for such constraints, though. One idea is giving virtual fields of all possible names to a type:

```wiki
instance Has T l () where
  get _ = ()
```