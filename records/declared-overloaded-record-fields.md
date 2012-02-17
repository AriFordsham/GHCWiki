# Declared Overloaded Record Fields (DORF)


Explained in 5 wiki pages (these proposals are linked but somewhat orthogonal):

- **[No Mono Record Fields](records/declared-overloaded-record-fields/no-mono-record-fields)**   (precursor to DORF)
- ** DORF -- Application Programmer's view **     (this page)
- **[DORF -- Implementor's view](records/declared-overloaded-record-fields/implementors-view)**
- **[DORF -- Comparison to SORF](records/declared-overloaded-record-fields/c-ompare-sorf)**
- **[Dot as Postfix Funcion Apply](records/declared-overloaded-record-fields/dot-postfix)**   (***optional*** syntactic sugar)

## Application Programmer's view


This proposal is addressing the "narrow issue" of namespacing for record field names.
[ http://hackage.haskell.org/trac/ghc/wiki/Records](http://hackage.haskell.org/trac/ghc/wiki/Records)


I'm avoiding giving implementation details here -- see:

>
> \<DORF -- Implementor's view\>; and
> \<DORF -- comparison to SORF\>


I'm not saying anything about field selection via pattern matching or record construction using explicit data constructors -- those are to behave as currently (using the approach per ‑XDisambiguateRecordFields and friends).


Currently in Haskell two records in the same module can't share a field name. This is because declaring a field name within a data decl creates a monomorphic selector function; and if it's monomorphic, we can only have one. I think the wiki is characterising the problem incorrectly:

- it's not that the field name appearing in different record decls is ambiguous between the two record types
  so we need some (syntactical) way of choosing between the different definitions;

- rather, we have one field name, and we lack the syntax/semantics for sharing it between different records.


An example: let's say I have a database application with a field (meaning type) `customer_id`. Then it appears in records for name and address, pricing, order entry, etc. This is not a name 'clash', it's 'intended sharing'. (It really galls me to even put it that way for explanatory purposes. Really it's the **same**`customer_id`.)


In data model design you'd typically go about identifying all the fields (types aka attributes) and putting them in a data dictionary. Then you'd construct your records from them. You might (possibly) put the data dictionary in a distinct module, for easy maintenance. But you'd certainly want all the customer-related records in the same module. So a data decl:

```wiki
    data Customer_NameAddress = Cust_NA { customer_id :: Int, ... } 
```


is not declaring `customer_id`, it's using (or instancing) an already-declared field for `customer_id`.
Similarly, if I have a family of objects, all with a `rese`' method, that's not umpteen methods with a 'clash' of names, it's one method with umpteen instances. (And I might create a family of record structures to describe each object, and store the `reset` method within it.)


What's more, the Haskell 98 field selector (auto-created from the data decl) is half-way to what we want. It's a function:

```wiki
    customer_id :: Customer_NameAddress -> Int
```


The DORF proposal generalises that signature: if you want to share a field across different records, its selector function needs to be overloaded to this type:

```wiki
    customer_id :: r{ customer_id :: Int } => r -> Int
```


The `r{ ... }` is syntactic sugar for the constraint meaning "record `r` has field `customer_id` at type `Int`".


We need a way to declare that a name is available as an overloadable field name (roughly speaking, a class/method definition), proposed syntax:

```wiki
    fieldLabel customer_id :: r -> Int
```


(The `r{ ... }` is added by the desugarer.)


The `-> Int` means the field's domain (type) is `Int` -- it's just a type.
We might also want to constrain the record -- for example to be sure it is savable to persistent storage:

```wiki
    fieldLabel unitPrice :: (Save r, Num t) => r -> t
```


(Again the `r{ ... }` gets added as a further constraint.)


Now we can use the field in a record, and that in effect declares an instance for the field/record. All these definitions are in the same module:

```wiki
    data Customer_NameAddress = ... (as above)
    data Customer_Price a = Num a => Cust_Price {
                                       customer_id :: Int,
                                       product_id  :: Int,
                                       unit_Price  :: a,
                                       ... }
    data Customer_Order = Cust_Order { customer_id :: Int, ... }
```


Then a field selection expression like:

> `... (customer_id r) ...`          -- H98 style field application


uses familiar type instance resolution to figure out from record type `r` how to extract the `customer_id`.


\[Possibly that expression could be:

> `... r.customer_id ...`


See \<Dot as Postfix Func Apply\> for that dot notation, but note that nothing in this proposal assumes dot notation will be needed.\]


From here upwards, the `r{ ... }` constraint is just a constraint, and gets merged with other constraints. For example, you could define a function:

```wiki
    fullName r = (firstName r) ++ " " ++ (lastName r)  -- per SPJ
```


The type inferred would be:

```wiki
    fullName :: r{ firstName, lastName :: String} => r -> String
```


which is eliding:

```wiki
    fullName :: (r{ firstName :: String}, r{ lastName :: String })
                 => r -> String
```


And if you think that's very close to the type of a field selector function, you'd be right. Here's some more examples of pseudo- or 'virtual' fields, using dot notation:

```wiki
    customer.fullName
    shape.area
    date.dayOfWeek        -- not a field: calculated from the date
    name.middleInitial    -- extract from the name field
    tuple.fst             -- Prelude functions
    list.head
    list.length
```


\[Since they're just functions, they can use dot notation -- or not: personal preference.\]

### Modules and qualified names for records


Do these field selector functions have a special scope in some way? No! They're just functions. They can be exported/imported.


We can't stop some other developer creating an application/package with a field `customer_id` which is incompatible with ours. (Say a Sales Order entry application where `customer_id` is a `String`, to merge with our Accounts Receivable.) So do we have a problem if someone wants to import both?


No! This is regular business-as-usual familiar name clash, and it's what the module system is designed to handle. The field selectors are just functions, we can use them qualified:

```wiki
    (My.customer_id myCust)        <===> myCust.My.customer_id
    (Their.customer_id theirCust)  <===> theirCust.Their.customer_id
    (My.customer_id r)       -- fails if r is from the 'other' module
```

### Import/Export and Representation hiding


\[See \<No Mono Record Fields\>, which is implied by DORF.\]


Since there is only a single (overloaded) field selector function created, we either have to export it always, or hide it always (that is, we can't control which record instances get exported).


The field selector function is separately declared vs. the records and their fields, so must be exported separately. For example:

```wiki
{-# OPTIONS_GHC -XDeclaredOverloadedRecordFields             #-}
module M( x )       where
    fieldLabel x,y :: r -> Int
    data T = MkT { x, y :: Int }
```


Here only the field selector function `x` is exported. The representation is abstract, the client can't construct or dismantle a record type `T`;

>
> field `y` is hidden altogether.


If you say:

```wiki
{-# OPTIONS_GHC -XDeclaredOverloadedRecordFields
                -XNoMonoRecordFields                   #-}
module M( T( x ) )       where
    fieldLabel x,y :: r -> Int
    data T = MkT { x, y :: Int }
```


then you are exporting the `x` field within record type `T`, but _not_ the field selector `x` (nor the generated type 'peg' `Proxy_x`).


Type `T` and field label `x` are exported, but not data constructor `MkT`, so `x` is unusable.


The existence of field `y` is hidden altogether.

### Field Update for Overloadable Record Fields


You can (continue to) use pattern matching and data constructor tagging for record update:

```wiki
    case r of {
     Cust_Price {unit_Price, ..}
          -> Cust_Price {unit_Price = unit_Price * 1.05, .. }
    }         -- increases Price by 5%
```


(This uses ‑XDisambiguateRecordFields, -XRecordWildCards and ‑XNamedFieldPuns -- all mature GHC extensions.)


The new part is polymorphic record update:

```wiki
    myPrice{ unit_Price = 72 :: Int }
```


Returns a record with same fields as `myPrice`, except a different `unit_Price`. Note that the update can change the type of a field (if the record declaration is polymorphic).


Note that upon first encountering that expression, we don't know the record types (because `unit_Price` is overloaded). So the types initially inferred are:

```wiki
    <expr>  :: r { unit_Price :: Int } => r
    myPrice :: _r{ unit_Price :: t }   => _r
```


That is, the update might be changing the record type as well as the field type -- in case that the record type is parametric over the field type.


Behind the scenes, the update syntax with an expression prefix to the `{ ... }` is syntactic sugar for a call to the polymorphic record update method `set`:

```wiki
    set (undefined :: Proxy_unit_Price) (72 :: Int) myPrice
```


\[See \<DORF -- Implementor's view\> for what the Proxy is doing.\]


Normal type inference/instance resolution will find the record type for `myPrice`, and therefore the correct instance to apply the update.


You can update multiple fields at the same time:

```wiki
    myCustNA { firstName = "Fred", lastName = "Dagg" }
```


\[There's a poor story to tell here in implementation terms: we split into two calls to `set`, one nested inside the other. It's wasteful to build the intermediate record. Worse, the two fields' types might be parametric in the record type or polymorphically related (perhaps one is a method to apply to the other), then we get a type failure on the intermediate record.\]


Some discussion threads have argued that Haskell's current record update syntax is awkward. The DORF proposal is to implement field update using a polymorphic function. Once this is implemented, alternative syntax could be explored, providing it desugars to a call to `set`.
