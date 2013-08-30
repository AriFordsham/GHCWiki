# Overloaded record fields: a plan


This is a plan to implement overloaded record fields, along the lines of SPJ's [Simple Overloaded Record Fields](records/overloaded-record-fields) proposal, as a Google Summer of Code project. (See the [ GSoC project details](http://www.google-melange.com/gsoc/project/google/gsoc2013/adamgundry/23001), for reference.) The page on [Records](records) gives the motivation and many options.  In particular, the proposal for [Declared Overloaded Record Fields](records/declared-overloaded-record-fields) is closely related but makes some different design decisions.


This page describes the design. Separate [notes on the implementation](records/overloaded-record-fields/implementation) are available, but not necessarily comprehensible. Development of the extension is taking place on forks of the [ ghc](https://github.com/adamgundry/ghc) and [ packages-base](https://github.com/adamgundry/packages-base) repositories (on branch 'overloaded-record-fields'). A [ prototype implementation](https://github.com/adamgundry/records-prototype) is also available.

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

### Record field constraints


A record field constraint is introduced when a field is used in an expression. If every `x` in scope is a record field, then an occurrence of `x` has type `a { x :: b } => a -> b` instead of generating an ambiguity error. The overloaded `x` is translated using a typeclass, described below. If there are any normal identifiers `x` in scope (as well as fields) then a use of `x` leads to an ambiguity error. 


Record field constraints `r { x :: t }` are syntactic sugar for typeclass constraints `Has r "x" t`, where

```wiki
type family GetResult (r :: *) (f :: Symbol) :: *

class t ~ GetResult r f => Has r (f :: Symbol) t where
  getField :: proxy f -> r -> t
```


Recall that `Symbol` is the kind of type-level strings. Roughly speaking, an occurrence of a field name `x` is translated into `getField (Proxy :: Proxy "x")`. (Actually a slightly more general translation is used, as [discussed below](records/overloaded-record-fields/plan#lens-integration).)


The syntactic sugar extends to conjunctions:  `r {x :: tx, y :: ty}` means `(Has r "x" tx, Has r "y" ty)`. Note also that `r` and `t` might be arbitrary types, not just type variables or type constructors.  For example, `T (Maybe v) { x :: [Maybe v] }` means `(Has (T (Maybe b)) "x" [Maybe v])`.


Instances for the `Has` typeclass and `GetResult` type family are automatically generated (for modules with `-XOverloadedRecordFields` enabled) using the record fields that are in scope. For example, the data type

```wiki
data T a = MkT { x :: [a] }
```


has the corresponding instances

```wiki
type instance GetResult (T a) "x" = [a]

instance (b ~ [a]) => Has (T a) "x" b where 
  getField _ (MkT x) = x
```


The bare type variable `b` in the instance head is important, so that we get an instance match from the first two parameters only, then the equality constraint `(b ~ [a])` improves `b`. For example, if the constraint `Has (T c) "x" d` is encountered during type inference, the instance will match and generate the constraints `(a ~ c, b ~ d, b ~ [a])`. Moreover, the `GetResult` type family ensures that the third parameter is functionally dependent on the first two, which is needed to [avoid ambiguity errors when composing overloaded fields](records/overloaded-record-fields/plan#trouble-in-paradise).


The reason for using a three-parameter class, rather than just two parameters and a type family, is to support the syntactic sugar. With a two-parameter class we could easily end up inferring types like the following, and it would be hard to reapply the sugar:

```wiki
f :: (Has r "x", Has r "y", GetResult r "x" ~ Int, GetResult r "y" ~ Int) => r -> Int
f r = x r + y r :: Int
```

### Representation hiding


At present, a datatype in one module can declare a field, but if the selector function is not exported, then the field is hidden from clients of the module. It is important to support this. Typeclasses in general have no controls over their scope, but for implicitly generated `Has` instances, the instance is available for a module if `-XOverloadedRecordFields` is enabled for that module and the record field selector function is in scope. Instances are not exported from the module that defines the datatype, but are created implicitly when needed by the typechecker.


This enables representation hiding: just like at present, exporting the field selector permits access to the field. For example, consider the following module:

```wiki
module M ( R(x) ) where

data R = R { x :: Int }
data S = S { x :: Bool }
```


Any module that imports `M` will have access to the `x` field from `R` but not from `S`, because the instance `Has R "x" Int` will be available but the instance `Has S "x" Bool` will not be. Thus `R { x :: Int }` will be solved but `S { x :: Bool }` will not.

### Multiple modules and automatic instance generation


Note that `Has` instances are generated on a per-module basis, using the fields that are in scope for that module, and automatically generated instances are never exported. Thus it doesn't matter whether `-XOverloadedRecordFields` was on in the module that defined the datatype. The availability of the instances in a particular module depends only on whether the flag is enabled for that module.


Suppose module `M` imports module `N`, `N` imports module `O`, and only `N` has the extension enabled. Now `N` can project any field in scope (including those defined in `O`), but `M` cannot access any `Has` instances. 


This means that

- the extension is required whenever a `Has` constraint must be solved;
- no new mechanism for hiding instances is required; and
- records defined in existing modules (or other packages) without the extension can still be overloaded.

### Higher-rank fields


Higher-rank fields, such as in the declaration

```wiki
data U = MkU { x :: forall a . a -> a }
```


cannot be overloaded. If such a field is in scope for a module with `-XOverloadedRecordFields` enabled, no Has or Upd instances will be produced. The user can always declare the selector function manually. This is similar to the current situation for existentially quantified variables in fields, which do not give rise to selector functions at all.


Bidirectional type inference for higher-rank types relies on inferring the type of functions, so that types can be pushed in to the arguments. However, the type of an overloaded field cannot immediately be inferred (as some constraint solving is required). This is why higher-rank and overloaded fields are incompatible.


Some previous variants of the design supported rank-1 universally quantified fields (but not rank-2 and above). However, these prevent the third parameter of the `Has` class from being a function of the first two, and hence obstruct type inference for compositions of selectors.

### Qualified names


A qualified name must refer to a unique field; it cannot be overloaded. Consider the following example:

```wiki
module M where
  data S = MkS { foo :: Int }

module N where
  data T = MkT { foo :: Int }
  data U = MkU { foo :: Int }

module O where
  import M
  import N

  f x = M.foo x
  g x = N.foo x
  h x = foo x
```


Here `f` is okay, because `M.foo` is unambiguous, but `g` is forbidden. This is because we have no way to support polymorphism over fields only from one module. The user must write `h` instead, making it explicit that the field is not qualified.

## Record update


Supporting polymorphic record update is rather more complex than polymorphic lookup. In particular:

- the type of the record may change as a result of the update;
- multiple fields must be updated simultaneously for an update to be type correct (so iterated single update is not enough); and
- records may include higher-rank components.


These problems have already been [described in some detail](records/overloaded-record-fields#record-updates). In the interests of doing something, even if imperfect, the traditional record update syntax will support only non-overloaded update (that is, update of a unique known record type). Where overloading mean that the fields alone do not determine the type being updated, a type signature may be required. For example,

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

### Limited type-changing update


As noted above, supporting a polymorphic version of the existing record update syntax (in its full generality) is difficult. However, we can generate instances of the following class, which permits type-changing update of single fields:

```wiki
type family SetResult (r :: *) (f :: Symbol) (a :: *) :: *

class (Has r f (GetResult r f), r ~ SetResult r f (GetResult r f)) =>
          Upd (r :: *) (f :: Symbol) (a :: *) where
  setField :: proxy f -> r -> a -> SetResult r f a
```


For example, the datatype `T` would give rise to these instances:

```wiki
data T a = MkT { x :: [a] }

type instance SetResult (T a) "x" [c] = T c

instance (b ~ [c]) => Upd (T a) "x" b where
  setField _ r e = r { x = e }
```


The third parameter of the `Upd` class represents the new type being assigned to the field, unlike the `Has` class, where it represents the current type. Thus it is not functionally dependent on the first two. Consequently, we must use a bare type variable `b` in the instance declaration, with an equality constraint `b ~ [c]` postponed until after the instance matches.


If a type variable is shared by multiple fields, it cannot be changed using `setField`. Moreover, the use of the `SetResult` type family means that phantom type variables cannot be changed. For example, in

```wiki
data V a b c = MkV { foo :: (a, b), bar :: a }
```


an update to `foo` must keep `a` and `c` the same, since `a` occurs in the
type of `bar`, and `c` does not occur in the type of `foo`, but the update may change `b`.  Thus we generate:

```wiki
type instance SetResult (V a b c) "foo" (a, b') = V a b' c

instance t ~ (a, b') => Upd (V a b c) "foo" t where
  setField _ r e = r { foo = e }
```

### Lens integration


It was implied above that a field like `foo` translates into `getField (Proxy :: Proxy "foo") :: Has r "foo" t => r -> t`, but this is not quite the whole story. We would like fields to be usable as lenses (e.g. using the [ lens](http://hackage.haskell.org/package/lens) package). This requires a slightly more general translation, using

```wiki
field :: (Has r f t, Accessor p f) => proxy f -> p r t
field z = accessor z (getField z) (setField z)
```


to translate `foo` to `field (Proxy :: Proxy "foo") :: (Has r "foo" t, Accessor p "foo") => p r t`. The `Accessor` class is defined thus:

```wiki
class Accessor (p :: * -> * -> *) (f :: Symbol) where
  accessor :: proxy f -> (r -> GetResult r f) ->
              (forall a . Upd r f a => r -> a -> SetResult r f a) ->
              p r (GetResult r f)
```


An instance of `Accessor p f` means that `p` may contain a getter and setter for the field `f`. In particular, we can give an instance for functions that ignores `f` and the setter completely:

```wiki
instance Accessor (->) f where
  accessor _ getter setter = getter
```


Thus, whenever a field `foo` is used at a function type (by applying it or composing it, for example), this instance will be selected. If `z` is a proxy of type `Proxy "foo"`, then `foo` translates to `field z`, which computes to `accessor z (getField z) (setField z)`, and hence to `getField z` by the `Accessor` instance for functions.


However, `p` does not have to be the function arrow. Suppose the `lens` library defined the following newtype wrapper:

```wiki
newtype WrapLens f r a
  = MkWrapLens (forall b . Upd r f b => Lens r (SetResult r f b) a b)

instance f ~ g => Accessor (WrapLens f) g where
  accessor _ getter setter = MkWrapLens (\ w s -> setter s <$> w (getter s))

fieldLens :: Upd r f b => WrapLens f r a -> Lens r (SetResult r f b) a b
fieldLens (MkWrapLens l) = l
```


Now `fieldLens foo` is a lens whenever `foo` is an overloaded record field.


Other lens libraries can define their own instances of `Accessor`, even if they do not support type-changing update, and the same machinery enables fields to be used with them.

### Type-changing update: phantom arguments


Consider the datatype

```wiki
data T a = MkT { foo :: Int }
```


where `a` is a phantom type argument (it does not occur in the type of `foo`). The traditional update syntax can change the phantom argument, for example if `r :: T Int` then `r { foo = 3 } :: T Bool` typechecks. However, `setField` cannot do so, because this is illegal:

```wiki
type instance SetResult (T a) "foo" Int = T b
```


Note that the result of the type family involves an unbound variable `b`. 


In general, a use of `setField` can change only type variables that occur in the field type being updated, and do not occur in any of the other fields' types.

## Design choices

### Scope issues, or, why we miss dot


Consider the following example:

```wiki
f :: (Has r "g" Int) => r -> Int
f x = g x + 1
```


Q1. What happens if `g` is not in scope?

1. The code gives an error. This is where dot-notation (or another syntactic form marking a field name) is better: `f x = x.g + 1` can work even if `g` is not in scope. Observe that something similar happens with implicit parameters: `f y = y + ?x` works even if `x` is not in scope, and introduces a new constraint `(?x :: Int)`. 


Q2. What if we add `data T = MkT { g :: Char }`?

1. The code compiles correctly, even though the datatype is "obviously" irrelevant because the field `g` it declares has the wrong type, so it cannot be selected. This would not be the case if we treated `g` as an unambiguous reference to the only field of that name in scope.


Q3. What if we subsequently add another datatype with a field `g`?

1. The code still compiles correctly.


An advantage of distinguishing record projections syntactically (as in `x.g`) is that `g` is always treated as a record field, regardless of what is in scope. This allows better separation of concerns, as functions that manipulate records can be defined abstractly rather than referring to particular datatypes. We could consider using an operator less controversial than dot (for example, `(|:)` has been suggested), or a keyword such as **select**:

```wiki
f x = x|:g + 1
f x = select g x + 1
```

### Introducing field names


As noted above, sometimes one might want to write code that uses record fields without any particular record types being in scope. One workaround is to define unused types with the appropriate field names. This is slightly odd! We might consider adding a new declaration form, say **field**`g`, which declares `g` as a record field that is always polymorphic, rather like the function declaration

```wiki
g :: r { g :: t } => r -> t
g = field
```


but with the property that it will not clash with actual `g` fields.

### Unambiguous fields


What if `foo` occurs in an expression, and there is only one datatype `T` with a field `foo` in scope? There are three obvious choices:

1. Generate a polymorphic use of `field` as normal.
1. Generate a use of `field`, specialised to the type `T`, but still polymorphic in the choice of `Accessor`.
1. Use the record selector for `foo`, without any polymorphism.


The first and second options are likely to be preferred by users who wish to write polymorphic code, while the third is better if the desire is only to overload field names but not write code that is polymorphic in the choice of datatype. The third option severely hampers the integration with lenses, because a field will only be a lens if it is ambiguous. However, the third would allow higher-rank fields to be used when unambiguous. This suggests a fourth option:

1. Use the record selector for `foo` if it is applied to one or more arguments, and generate a use of `field` specialised to the type `T` otherwise. 


This makes higher-rank fields usable (though possibly requiring eta-expansion), and it allows lens integration. On the other hand, it is still an impediment to users wishing to write polymorphic code.


Oh, and there's a fifth option:

1. Generate a polymorphic use of `field` as normal, but when defaulting a constraint `Has r "foo" t`, choose the instance for `T`.


This gives the maximum amount of polymorphism and the right behaviour in the presence of the monomorphism restriction, but defaulting is evil and confusing...

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

### Hiding record selectors


Optionally, we could [add a flag \`-XNoRecordSelectorFunctions\`](records/declared-overloaded-record-fields/no-mono-record-fields) to suppress the record selectors. Just as `-XOverloadedRecordFields` applies to a client module, and generates `Has` instances for that module, so `-XNoRecordSelectorFunctions` in a client module would hide all the record selectors that should otherwise be in scope. The idea is that another record system could use Template Haskell to generate functions in place of selectors, and these would not clash.


Since the selectors are hidden by clients (on import) rather than on export, fields can still be used for record update and mentioned in import and export lists, to control access to them (as discussed in the [representation hiding](records/overloaded-record-fields/plan#representation-hiding) section).

### Syntactic sugar for `Upd` constraints


Should we have a special syntax for `Upd` constraints, just as `r { x :: t }` sugars `Has r "x" t`? What should it look like? Perhaps something like `r { x ::= t }`?

## Remarks

### Trouble in paradise

[ Edward Kmett points out](http://www.haskell.org/pipermail/glasgow-haskell-users/2013-July/022584.html) that a previous version of this proposal, where the third parameter of `Has` was not functionally dependent on the first two, fell short in an important respect: composition of polymorphic record fields would lead to ambiguity errors, as the intermediate type cannot be determined. For example, suppose

```wiki
foo :: Has b "foo" c => b -> c
bar :: Has a "bar" b => a -> b
```


then

```wiki
foo . bar :: (Has a "bar" b, Has b "foo" c) => a -> c
```


and `b` is an ambiguous type variable. This shows the need for the `GetResult` type family.

### User-defined `Has` instances


The user can write explicit `Has` instances, and they are scoped normally. For example:

```wiki
instance ctx => Has r "x" t where
  getField = blah :: proxy "x" -> r -> t
```


Even with an explicit `Has` instance as above, the name `x` will not be in scope unless a datatype has a field with name `x`. This is likely to make it less useful. However, it does allow virtual fields to be declared.

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
