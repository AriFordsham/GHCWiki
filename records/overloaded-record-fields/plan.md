# Overloaded record fields: a plan for implementation


This is a plan to implement overloaded record fields, along the lines of SPJ's [Simple Overloaded Record Fields](records/overloaded-record-fields) proposal, as a Google Summer of Code project. (See the [ GSoC project details](http://www.google-melange.com/gsoc/project/google/gsoc2013/adamgundry/23001), for reference.) The page on [Records](records) gives the motivation and many options.  In particular, the proposal for [Declared Overloaded Record Fields](records/declared-overloaded-record-fields) is closely related but makes some different design decisions.


Development of the extension is taking place on forks of the [ ghc](https://github.com/adamgundry/ghc) and [ packages-base](https://github.com/adamgundry/packages-base) repositories (on branch 'overloaded-record-fields').

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


Record field constraints `r { x :: t }` are syntactic sugar for typeclass constraints `Has r "x" t`, where

```wiki
class Has (r :: *) (x :: Symbol) (t :: *) where
  getFld :: r -> t
```


Recall that `Symbol` is the kind of type-level strings. The notation extends to conjunctions:  `r {x :: tx, y :: ty}` means `(Has r "x" tx, Has r "y" ty)`. Note also that `r` and `t` might be arbitrary types, not just type variables or type constructors.  For example, `T (Maybe v) { x :: [Maybe v] }` means `(Has (T (Maybe b)) "x" [Maybe v])`.


Instances for the `Has` typeclass are implicitly generated,  corresponding to fields in datatype definitions. For example, the data type

```wiki
data T a = MkT { x :: [a] }
```


has the corresponding instance

```wiki
instance (b ~ [a]) => Has (T a) "x" b where
  getFld (MkT { x = x }) = x
```


The `(b ~ [a])` in the instance is important, so that we get an instance match from the first two fields only. For example, if the constraint `Has (T c) "x" d` is encountered during type inference, the instance will match and generate the constraints `(a ~ c, b ~ d, b ~ [a])`.

### Projections: the dot operator


Record field constraints are introduced by projections. If there are two or more fields `x` in scope, then an occurrence of `x` has type `a { x :: b } => a -> b` instead of generating an ambiguity error. If there is a single field `x` in scope, then it refers to the usual monomorphic record selector (ensuring backwards compatibility). If there are any normal identifiers `x` in scope (as well as fields) then a use of `x` leads to an ambiguity error.

### Representation hiding


At present, a datatype in one module can declare a field, but if the selector function is not exported, then the field is hidden from clients of the module. It is important to support this. Typeclasses in general have no controls over their scope, but for implicitly generated `Has` instances, the instance is in scope iff the record field selector function is.


This enables representation hiding: exporting the field selector is a proxy for permitting access to the field. For example, consider the following module:

```wiki
module M ( R(x) ) where

data R = R { x :: Int }
data S = S { x :: Bool }
```


Any module that imports `M` will have access to the `x` field from `R` but not from `S`, because the instance `Has R "x" Int` will be in scope but the instance `Has S "x" Bool` will not be. Thus `R { x :: Int }` will be solved but `S { x :: Bool }` will not.

### Record update


Supporting polymorphic record update is rather more complex than polymorphic lookup. In particular:

- the type of the record may change as a result of the update;
- multiple fields must be updated simultaneously for an update to be type correct (so iterated single update is not enough); and
- records may include higher-rank components.


These problems have already been [described in some detail](records/overloaded-record-fields#record-updates). In the interests of doing something, even if imperfect, we plan to support only monomorphic record update. For overloaded fields to be updated, a type signature may be required in order to specify the type being updated. For example,

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


If `e` is a variable, whose type is given explicitly in the context, we could look it up rather than requiring it to be given again. Thus

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

### Virtual record fields


It is easy to support virtual record fields, by permitting explicit `Has` instances:

```wiki
instance ctx => Has r "x" t where
  getFld = blah :: r -> t
```


Note that if `r` is a datatype with a field `x`, the virtual field will overlap, and the usual rules about overlap checking apply. Explicit instances follow the usual instance scope rules, so a virtual record field instance is always exported and imported.

`Has` constraints are slightly more general than the syntactic sugar suggests: one could imagine building constraints of the form `Has r l t` where `l` is non-canonical, for example a variable or type family. It's hard to imagine uses for such constraints, though. One idea is giving virtual fields of all possible names to a type:

```wiki
instance Has T l () where
  getFld _ = ()
```

### Record selectors


Even with `-XOverloadedRecordFields` enabled, monomorphic record selector functions will be generated by default for backwards compatibility reasons, and for use when there is no ambiguity. They will not be usable if multiple selectors with the same name are in scope.


Optionally, we could [add a flag \`-XNoMonoRecordFields\`](records/declared-overloaded-record-fields/no-mono-record-fields) to disable the generation of the usual monomorphic record field selector functions.  This is not essential, but would free up the namespace for other record systems (e.g. **lens**). Even if the selector functions are suppressed, we still need to be able to mention the fields in import and export lists, to control access to them (as discussed in the [representation hiding](records/overloaded-record-fields/plan#representation-hiding) section).


We could also add a flag `-XPolyRecordFields` to generate polymorphic selector functions. This implies `-XNoMonoRecordFields`. For example, if a record with field `x` is declared then the function

```wiki
x :: Has r "x" t => r -> t
x = getFld
```


would be generated. However, these have slightly odd behaviour: if two independent imported modules declare fields with the same label, they will both generate identical polymorphic selectors, so only one of them should be brought into scope.

### Monomorphism restriction and defaulting


The monomorphism restriction may cause annoyance, since

```wiki
foo = \ e -> x e
```


would naturally be assigned a polymorphic type. If there is only one `x` in scope, perhaps the constraint solver should pick that one (analogously to the other defaulting rules). However, this would mean that bringing a new `x` into scope (e.g. adding an import) could break code. Of course, it is already the case that bringing new identifiers into scope can create ambiguity!


For example, suppose the following definitions are in scope:

```wiki
data T = MkT { x :: Int, y :: Int }
data S = MkS { y :: Bool }
```


Inferring the type of `foo = \ e -> x e` results in `alpha -> beta` subject to the constraint `alpha { x :: beta }`. However, the monomorphism restriction prevents this constraint from being generalised. There is only one `x` field in scope, so defaulting specialises the type to `T -> Int`. If the `y` field was used, it would instead give rise to an ambiguity error.

### Higher-rank fields


If a field has a rank-1 type, the `Has` encoding works fine: for example,

```wiki
data T = MkT { x :: forall a . a -> a }
```


gives rise to the instance

```wiki
instance (b ~ a -> a) => Has T "x" b
```


However, if a field has a rank-2 type or higher (so the selector function has rank at least 3), things are looking dangerously impredicative:

```wiki
data T b = MkT { x :: (forall a . a -> a) -> b }
```


would give

```wiki
instance (c ~ ((forall a . a -> a) -> b)) => Has (T b) "x" c
```


but this is currently forbidden by GHC, even with `-XImpredicativeTypes` enabled. Indeed, it would not be much use if it were possible, because bidirectional type inference relies on being able to immediately infer the type of neutral terms like `x e`, but overloaded record fields prevent this. Traditional monomorphic selector functions are likely to be needed in this case.

### Multiple modules and implicit instance generation


When should `Has` instances be implicitly generated? I can think of three options:

1. If the extension is on for a module, generate instances for all datatypes in that module when checking their declarations. This means that record projections are not available to code that imports a datatype definition from a module without the flag. Some mechanism will need to restrict the scope of instances based on import/export of selectors.
1. If the extension is on for a module, generate instances for all record selectors that are in scope, but do not export them. Thus it doesn't matter whether the flag was on in the module that defined the datatype, and the availability of the instances in a particular module depends only on whether the flag is enabled.
1. If the extension is on for a module, generate and export instances for all record selectors that are in scope and do not already have instances. This is a hybrid of (1) and (2), and also requires a mechanism to restrict instance scope based on import/export of selectors.


Note that (2) can be equivalently implemented (as far as the user is concerned) by not really generating instances at all, but solving `Has` constraints directly based on the selectors in scope, much as `SingI` constraints are solved on-the-fly.


Suppose module `M` imports module `N`, `N` imports module `O`, and only `N` has the extension enabled. Under (1), `N` can project fields it defines (but not those defined in `O`), and `M` also has access to the `Has` instances for `N` (but not the dot syntax). Under (2), `N` can project any field in scope (including those defined in `O`), but `M` cannot access any `Has` instances. Under (3), `N` can project any field in scope, and `M` has access to the `Has` instances for `N` and `O` (but not fields defined in `M`).


I think (2) is probably the right choice here, because 

- the extension is required whenever dot notation is used or a `Has` constraint must be solved;
- no new mechanism for hiding instances is required; and
- records defined in existing modules (or other packages) without the extension can still be used with dot notation. 

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
```


When checking `foo`, `e` is a variable of unknown type `alpha`, and the projection generates the constraint `alpha { x :: beta }` where `beta` is fresh. This constraint cannot be solved immediately, so generalisation yields the type `a { x :: b } => a -> b`.


When checking `bar`, the application of `foo` gives rise to the constraint `T { x :: Bool }`, which is solved since `Bool` is an instance of `forall a . a` (the type `T` gives to `x`).


When checking `baz`, the constraint `S { x :: gamma }` is generated and rejected, since the `x` from `S` is not in scope.
