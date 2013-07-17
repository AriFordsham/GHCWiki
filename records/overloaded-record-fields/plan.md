# Overloaded record fields: a plan


This is a plan to implement overloaded record fields, along the lines of SPJ's [Simple Overloaded Record Fields](records/overloaded-record-fields) proposal, as a Google Summer of Code project. (See the [ GSoC project details](http://www.google-melange.com/gsoc/project/google/gsoc2013/adamgundry/23001), for reference.) The page on [Records](records) gives the motivation and many options.  In particular, the proposal for [Declared Overloaded Record Fields](records/declared-overloaded-record-fields) is closely related but makes some different design decisions.


This page describes the design. Separate [notes on the implementation](records/overloaded-record-fields/implementation) are available, but not necessarily comprehensible. Development of the extension is taking place on forks of the [ ghc](https://github.com/adamgundry/ghc) and [ packages-base](https://github.com/adamgundry/packages-base) repositories (on branch 'overloaded-record-fields').

### Motivation


A serious limitation of the Haskell record system is the inability to overload field names in record types: for example, if the data types

```wiki
data Person  = Person  { personId :: Int, name :: String }
data Address = Address { personId :: Int, address :: String }
```


are declared in the same module, there is no way to determine which type an occurrence of the `personId` record selector refers to. A common workaround is to use a unique prefix for each record type, but this leads to less clear code and obfuscates relationships between fields of different records. Qualified names can be used to distinguish record selectors from different modules, but using one module per record is often impractical.


Instead, we want to be able to write polymorphic record projections, so that the ambiguous identifier `personId` is resolved using the type of `e`. In general, this requires a new form of constraint `r { x :: t }` stating that type `r` has a field `x` of type `t`. For example, the following declaration should be accepted:

```wiki
getPersonId :: r { personId :: Int } => r -> Int
getPersonId e = personId e
```


A constraint `R { x :: t }` is solved if `R` is a datatype that has a field `x` of type `t` in scope. An error is generated if `R` has no field called `x`, it has the wrong type, or the field is not in scope. 

## Design


In the sequel, we will describe the `-XOverloadedRecordFields` extension, which permits multiple field declarations with the same label and introduces new record field constraints.


Previous versions of this proposal suggested changing the lexical syntax so that record projections could be written postfix, immediately following a dot. For example, `e.personId` would be roughly equivalent to `personId e`. This would be a breaking change (when the extension was enabled) as composition would need spaces around the dot operator. However, it would mean that the field name would not have to be in scope, allowing better library separation. For example, `e.personId` would be valid even if no `personId` fields were in scope.


In the light of feedback, we propose **no changes to dot syntax** for the time being. In the future, we could add a separate extension to treat [dot as postfix function application](records/declared-overloaded-record-fields/dot-postfix). Note that the [ lens](http://hackage.haskell.org/package/lens) library encourages the use of dot with no spaces, as composition is used to chain lenses.

### Projections


A record field constraints is introduced when a field is used in an expression. If all the `x`s in scope are record fields, then an occurrence of `x` has type `a { x :: b } => a -> b` instead of generating an ambiguity error. If there are any normal identifiers `x` in scope (as well as fields) then a use of `x` leads to an ambiguity error.

### Record field constraints


Record field constraints `r { x :: t }` are syntactic sugar for typeclass constraints `Get r "x" t`, where

```wiki
type family GetResult (r :: *) (f :: Symbol) :: *

class t ~ GetResult r f => Get r (f :: Symbol) t where
  getFld :: proxy f -> r -> t
```


Recall that `Symbol` is the kind of type-level strings. The notation extends to conjunctions:  `r {x :: tx, y :: ty}` means `(Get r "x" tx, Get r "y" ty)`. Note also that `r` and `t` might be arbitrary types, not just type variables or type constructors.  For example, `T (Maybe v) { x :: [Maybe v] }` means `(Get (T (Maybe b)) "x" [Maybe v])`.


Instances for the `Get` typeclass and `GetResult` type family are automatically generated (for modules with `-XOverloadedRecordFields` enabled) using the record fields that are in scope. For example, the data type

```wiki
data T a = MkT { x :: [a] }
```


has the corresponding instances

```wiki
type instance GetResult (T a) "x" = [a]
instance (b ~ [a]) => Get (T a) "x" b where
  getFld (MkT { x = x }) = x
```


The `(b ~ [a])` in the instance is important, so that we get an instance match from the first two parameters only. For example, if the constraint `Get (T c) "x" d` is encountered during type inference, the instance will match and generate the constraints `(a ~ c, b ~ d, b ~ [a])`. Moreover, the `GetResult` type family ensures that the third parameter is functionally dependent on the first two, which is needed to avoid ambiguity errors when composing overloaded fields.


The reason for using a three-parameter class, rather than just two parameters and a type family, is to support the syntactic sugar. With a two-parameter class we could easily end up inferring types like the following, and it would be hard to reapply the sugar:

```wiki
f :: (Has r "x", Has r "y", GetResult r "x" ~ Int, GetResult r "y" ~ Int) => r -> Int
f r = x r + y r :: Int
```

### Representation hiding


At present, a datatype in one module can declare a field, but if the selector function is not exported, then the field is hidden from clients of the module. It is important to support this. Typeclasses in general have no controls over their scope, but for implicitly generated `Get` instances, the instance is available for a module if `-XOverloadedRecordFields` is enabled for that module and the record field selector function is in scope. Instances are generated on the client side, rather than being exported from the defining module.


This enables representation hiding: just like at present, exporting the field selector permits access to the field. For example, consider the following module:

```wiki
module M ( R(x) ) where

data R = R { x :: Int }
data S = S { x :: Bool }
```


Any module that imports `M` will have access to the `x` field from `R` but not from `S`, because the instance `Get R "x" Int` will be available but the instance `Get S "x" Bool` will not be. Thus `R { x :: Int }` will be solved but `S { x :: Bool }` will not.

### Multiple modules and automatic instance generation


Note that `Get` instances are generated on a per-module basis, using the fields that are in scope for that module, and automatically generated instances are never exported. Thus it doesn't matter whether `-XOverloadedRecordFields` was on in the module that defined the datatype. The availability of the instances in a particular module depends only on whether the flag is enabled for that module.


Suppose module `M` imports module `N`, `N` imports module `O`, and only `N` has the extension enabled. Now `N` can project any field in scope (including those defined in `O`), but `M` cannot access any `Get` instances. 


This means that

- the extension is required whenever a `Get` constraint must be solved;
- no new mechanism for hiding instances is required; and
- records defined in existing modules (or other packages) without the extension can still be overloaded.

### Higher-rank fields


Higher-rank fields, such as

```wiki
data T = MkT { x :: forall a . a -> a }
```


cannot be overloaded. If such a field is declared in a module with `-XOverloadedRecordFields` enabled, a warning will be emitted and no selector produced. The user can always declare the selector function manually.


Bidirectional type inference for higher-rank types relies on inferring the type of functions, so that types can be pushed in to the arguments. However, the type of an overloaded field cannot immediately be inferred (as some constraint solving is required). This is why higher-rank and overloaded fields are incompatible.


Some previous variants of the design supported rank-1 universally quantified fields (but not rank-2 and above). However, these prevent the third parameter of the `Get` class from being a function of the first two, and hence obstruct type inference for compositions of selectors.

### Record update


Supporting polymorphic record update is rather more complex than polymorphic lookup. In particular:

- the type of the record may change as a result of the update;
- multiple fields must be updated simultaneously for an update to be type correct (so iterated single update is not enough); and
- records may include higher-rank components.


These problems have already been [described in some detail](records/overloaded-record-fields#record-updates). In the interests of doing something, even if imperfect, the Haskell 98 record update syntax will support only monomorphic update. For overloaded fields to be updated, a type signature may be required in order to specify the type being updated. For example,

```wiki
e { x = t }
```


currently relies on the name `x` to determine the datatype of the record. If this is ambiguous, a type signature can be given either to `e` or to the whole expression. Thus either

```wiki
  e :: T Int { x = t }
```


or

```wiki
  e { x = t } :: T Int
```


will be accepted. (Really only the type constructor is needed, whereas this approach requires the whole type to be specified, but it seems simpler than inventing a whole new syntax.)

## Design choices

### Record update: avoiding redundant annotations


In an update `e { x = t }`, if `e` is a variable whose type is given explicitly in the context, we could look it up rather than requiring it to be given again. Thus

```wiki
f :: T Int -> T Int
f v = v { x = 5 }
```


would not require an extra annotation. On the other hand, we would need an annotation on the update in  

```wiki
  \v -> (v { x = 4 }, [v, w :: T Int])
```


because the type of `v` is only determined later, by constraint solving.


Annoyingly, nested updates will require some annotations. In the following example, the outer update need not be annotated (since `v` is a variable that is explicitly given a type by the context) but the inner update must be (since `x v` is not a variable):

```wiki
  f :: T Int -> T Int
  f v = v { x = (x v){ y = 6 } }
```

### Polymorphic record update for lenses


As noted above, supporting a polymorphic version of the existing record update syntax (in its full generality) is difficult. However, even if the existing record update syntax remains monomorphic, an additional motivation for polymorphic update comes from [ lens](http://hackage.haskell.org/package/lens). If we automatically generate instances of an extra class like

```wiki
class Set (r :: *) (x :: Symbol) (t :: *) where
  setFld :: r -> t -> r
```


and supply the instance (where `&x` is used for explicit type application of the `x` argument)

```wiki
instance (Functor f, Has s x a, Set s x a, fs ~ f s, fa ~ f a) => Has (a -> fa) x (s -> fs) where
  getFld f r = setFld &s &x &a r <$> f (getFld &s &x &a r) 
```


then every record field (for which a `Set` instance can be generated) is automagically a lens. This reduces the need for the current name-mangling Template Haskell implemented in the lens library. (Note that this instance requires explicit type application, or a proxy-based workaround, in order to supply the `x` argument.)


More work is needed to identify the right way to formulate the `Set` class: type-changing update requires a slightly more general version, and there is a story for [ multiple update](https://github.com/ekmett/lens/issues/197). Higher-rank fields remain problematic.

### Trouble in paradise

[ Edward Kmett points out](http://www.haskell.org/pipermail/glasgow-haskell-users/2013-July/024064.html) that the current story falls short in one important respect: composition of polymorphic record fields leads to ambiguity errors, as the intermediate type cannot be determined. For example, suppose

```wiki
foo :: Has b "foo" c => b -> c
bar :: Has a "bar" b => a -> b
```


then

```wiki
foo . bar :: (Has a "bar" b, Has b "foo" c) => a -> c
```


and `b` is an ambiguous type variable.


We could work around this by adding a functional dependency

```wiki
class Has r (f :: Symbol) t | r f -> t where
  getFld :: r -> t
```


or using a type family

```wiki
class Has r (f :: Symbol) where
  type GetResult r f :: *
  getFld :: r -> GetResult r f
```


but either of these options prevents the integration with lenses discussed above, and we lose support for universally quantified fields (though they are dubious anyway). Pick your poison.

### User-defined `Has` instances


Should the user be allowed to write explicit `Has` instances? For example:

```wiki
instance ctx => Has r "x" t where
  getFld = blah :: r -> t
```


Even with an explicit `Has` instance as above, the name `x` will not be in scope unless a datatype has a field with name `x`. Thus it is not really useful. The previous proposal, where `(.x)` always meant "project out the `x` field", used explicit `Has` instances for virtual fields. 

### Hiding record selectors


Optionally, we could [add a flag \`-XNoRecordSelectorFunctions\`](records/declared-overloaded-record-fields/no-mono-record-fields) to suppress the record selectors. Just as `-XOverloadedRecordFields` applies to a client module, and generates `Has` instances for that module, so `-XNoRecordSelectorFunctions` in a client module would hide all the record selectors that should otherwise be in scope. The idea is that another record system could use Template Haskell to generate functions in place of selectors, and these would not clash.


Since the selectors are hidden by clients (on import) rather than on export, fields can still be used for record update and mentioned in import and export lists, to control access to them (as discussed in the [representation hiding](records/overloaded-record-fields/plan#representation-hiding) section).

### Introducing field names


An advantage of distinguishing record projections syntactically (as in `e.x`) is that `x` is always treated as a record field, regardless of what is in scope. This allows better separation of concerns, as functions that manipulate records can be defined abstractly rather than referring to particular datatypes.


One workaround is to define unused types with the appropriate field names. This is slightly odd, and we might consider adding a new declaration form **field**`x`, which declares `x` as a record field that is always polymorphic, rather like the function declaration

```wiki
x :: r { x :: t } => r -> t
x = getFld
```


but with the property that it will not clash with actual `x` fields.

## Example of constraint solving


Consider the example

```wiki
module M ( R(R, x), S(S, y), T(T, x) ) where

  data R = R { x :: Int }
  data S = S { x :: Bool, y :: Bool }
  data T = T { x :: forall a . a }

module N where
  import M

  foo e = x e
 
  bar :: Bool
  bar = foo T

  baz = foo S

  quux = y
```


When checking `foo`, `e` is a variable of unknown type `alpha`, and the projection generates the constraint `alpha { x :: beta }` where `beta` is fresh. This constraint cannot be solved immediately, so generalisation yields the type `a { x :: b } => a -> b`.


When checking `bar`, the application of `foo` gives rise to the constraint `T { x :: Bool }`, which is solved since `Bool` is an instance of `forall a . a` (the type `T` gives to `x`).


When checking `baz`, the constraint `S { x :: gamma }` is generated and rejected, since the `x` from `S` is not in scope.


When checking `quux`, the only `y` field in scope is of type `S -> Bool` so that is its type.
