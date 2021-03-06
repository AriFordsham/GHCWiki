# Type-Punning Declared Overloaded Record Fields (TPDORF)

## Thumbnail Sketch


This proposal is addressing the narrow issue of **namespacing for record field names** by allowing more than one record in the same module to share a field name. Furthermore, it is aiming at a more structured approach to higher-ranked type fields, so that they can be updated using the same surface syntax as for other fields. This actually means a less complex implementation (compared to DORF or SORF). This proposal is in the DORF 'stable', but sufficiently different it is worth making it a separate proposal.


Specifically each sharing field name is overloaded, and there is a Type with the same name (upshifted) so that:

- Within the same module, many record types can be declared to share the field name.
- The field name can be exported so that records in other modules can share it.
- Furthermore, other modules can create records using that field name, and share it.


The export/import of both the field name and its punned Type is under usual H98 namespace and module/qualification control, so that for the record type in an importing module:

- Some fields are both readable and updatable;
- Some are read-only;
- Some are completely hidden.


In case of 'unintended' clash (another module using the same name 'by accident'), usual H98 controls apply to protect encapsulation and representation hiding.


This proposal introduces several new elements of syntax (including some shorthands), all of which desugar to use well-established extensions of ghc. The key type-inference elelemnts of this approach have been tested in ghc v7.2.1. A full prototype is yet to be developed. In particular:

- The field name overloading is implemented through usual class and instance mechanisms.
- Field selectors are ordinary functions named for the field (but overloaded rather than H98's monomorphic), so field selection is regular function application. (There is no need for syntactically-based disambiguation at point of use.)

### Implementation: the `Has` class, with methods `get` and `set`, and punned Types


Record declarations generate a `Has` instance for each record type/field combination. There is a type argument for the record and the field.


Note that SORF introduces a third argument for the field's resulting type. (This is specifically to support higher-rank typed fields; but despite the complexity it introduces, SORF has no mechanism to update h-r fields.)


TPDORF approaches h-r fields in a different way, which supports both setting and getting those fields. (I'm not claiming this is a *solution*, more a well-principled work-round. And not a hack. It is both scalable, and supports class-constrained higher-rank types.)

**The main insight** is that to manage large-scale data models (in which namespacing becomes onerous, and name sharing would be most beneficial), there are typically strong naming conventions and representation hiding for critical fields. For example:

```wiki
    newtype Customer_id = Customer_id Int                               -- data dictionary, could be a data decl
                                                                        -- constructor named same as type
    data Customer = Customer {                                          -- likewise
         customer_id :: Customer_id                                     -- field name puns on the type
       , firstName   :: String                                          -- not a critical/shared field
       , lastName    :: String                                          -- so using H98 style
       , ...   
       }       sharing (Customer_id, ...)  deriving (...)               -- new sharing syntax
```


TPDORF makes a virtue of this punning. (So extend's H98's and `NamedFieldPuns` punning on the field name.) This allows for some syntactic shorthands, but still supporting H98-style declaring field names within the record decl for backwards compatibility.


Here is the `Has` class with instances for the above Customer record, and examples of use:

```wiki
    class Has r t                                           where
        get :: r -> t -> GetResult r t                                  -- see type instances below
        set :: t -> r -> SetResult r t                                  -- use where changing the record's type

    type family GetResult r t :: *
    type family SetResult r t :: *

    instance Has Customer Customer_id                       where       -- Has instance generated for sharing field
        get Customer{ customer_id } = customer_id                       -- DisambiguateRecordFields pattern
        set x Customer{ .. }        = Customer{ customer_id = x, .. }   -- RecordWildCards and NamedFieldPuns

    type instance GetResult r Customer_id = Customer_id                 -- same result all record types
    type instance SetResult Customer t    = Customer                    -- record type is not parametric, so doesn't change

    customer_id r = get r (undefined :: Customer_id)                    -- we can auto-decl this, see below
    customer_id :: r{ customer_id :: Customer_id } =>                   -- r{ ... } => is sugar for the Has constraint
                   r -> Customer_id -> Customer_id                      -- type inferred
                                                                        

    newtype FirstName = FirstName String                                -- newtype generated for non-sharing field
    firstName :: Customer -> FirstName -> String                        -- generated selector is monomorphic
    firstName r = get r (undefined :: FirstName)

    instance Has Customer FirstName                         where       -- Has instance generated
        get Customer{ firstName = (FirstName x) } = x                   -- DisambiguateRecordFields pattern
        set x Customer{ .. }    = Customer{ firstName = x, .. }         -- RecordWildCards and NamedFieldPuns

    type instance GetResult Customer FirstName     = String             -- specific to this record/field
    -- type instance SetResult Customer FirstName  = Customer           -- not needed/already declared above
                                                                        -- (but OK because overlaps and confluent)

    myCust :: Customer                                                  -- usual record decl
    ... myCust{ customer_id = 27, firstName = "Fred" } ...              -- **polymorphic** record update, no data constr
    ... (customer_id myCust) ...                                        -- field selection is func apply, or:
    ... myCust.customer_id ...                                          -- dot notation is sugar for reverse func apply
```


Note that the** `Has` mechanism** uses **the field's type itself** to locate the field within the record:


- Each field must be a distinct type.
- The Type must be declared once (within the scope), and is then under regular name control.
  (Probably you're doing this already for critical fields to share.)
- The type functions are not associated types, because:

  - `GetResult` for shared fields depends only on the Field's type (per `Customer_id` above);
  - `SetResult` for non-parametric record types continues the same record type.
- The field selector function also must be declared once, defined punning on the field's type.
  (See below for syntactic sugar to declare these.)
- Possible **downside:** for non-`sharing` fields, what's the risk there's already a Type with the same name (upshifted) and that the name is an 'accidental' clash?

>
> >
> >
> > It is an error to be `sharing` a record field without there being a same-named Type in scope. The desugar for the data decl would create the instance to use the Type, but then the instance would fail.
> >
> >
>


To generate the correct field selector function, there is to be a new deriving class; and for record decls a shorthand:

```wiki
    newtype Customer_id = Customer_id Int                               -- newtype or data decl, same name type and constr
                                           deriving (Has, ...)          -- generates customer_id function, per above

    data Customer = Customer { :: Customer_id, ... }                    -- auto-gen field label pun on type name
                             sharing (Customer_id, ...)
```


Polymorphic record updates, alternative syntax (note, no data constructor for the record):

```wiki
    set (Customer_id 27) (                                              -- record update desugarred from above example
               set (FirstName "Fred") myCust  )                         -- note nested

    ... myCust{ cust_id, FirstName "Fred" }                             -- equiv syntax **only** for polymorphic updates
                                                                        -- (assuming cust_id :: Customer_id)
```

**Virtual** or **pseudo-** fields are easy to create and use, because field selection is merely function application (plus unwrapping for non-shared H98-style fields). Virtual fields look like ordinary fields (but can't be updated, because there is no `Has` instance):

```wiki
    fullName r = r.firstName ++ " " ++ map toUpper r.lastName           -- example adapted from SPJ
                                                                        -- dot notation binds tighter than func apply
    fullName :: r{ firstName :: String, lastName :: String} => r -> String
                                                                        -- type inferred for fullName
                                                                        -- the Has constraints use elided syntax
```

**Technical capabilities** and limitations for the `Has` class:

- Monomorphic fields can be `get` and `set`.
- Parametric polymorphic fields can be applied in polymorphic contexts, and can be `set` including changing the type of the record.
  (This uses the `SetResult` type function. See example below.)
- Multiple fields can be updated in a single expression (using familiar H98 syntax), but this desugars to nested updates, which is inefficient.
- Pattern matching and record creation using the data constructor prefixed to { ... } work as per H98 (using `DisambiguateRecordFields` and friends).
- But the types are subtlely different vs. polymorphic update: you must explicitly wrap the types.
  (So this is not backwards compatible. Can we do this?:
  In `Constr{ fld = e }`, if `e` not type `Fld`, enwrap it with a `Fld` constructor.)
- Higher-ranked polymorphic fields (including class-constrained) can be applied in polymorphic contexts, and can be set -- providing they are wrapped in a newtype. Here is SPJ's example:

**Higher-Ranked polymorphic fields** (including class-constrained polymorphism)

```wiki
 -- data HR = HR { rev :: (forall a. [a] -> [a]) }                      -- _not_ sharing, so would generate:

    newtype Rev = Rev (forall a. [a] -> [a])
    data HR = HR { rev :: Rev }

    rev :: HR -> (forall a. [a] -> [a])                                 -- generated selector is monomorphic in HR,
                                                                        --         'cos not sharing
                                                                        -- (but Higher-Ranked result)
    rev r = let (Rev fn) = get r (undefined :: Rev) in fn               -- unwrap from the newtype

    instance Has HR Rev                         where                   -- Has instance generated
        get HR{ rev = (Rev x) } = Rev x                                 -- can't unwrap here, 'cos can't spec Poly
        set (Rev x) HR{ .. }    = HR{ rev = Rev x, .. }                 -- note x is already wrapped

    type instance GetResult r Rev     = Rev
    type instance SetResult HR t      = HR                              -- HR is not parametric

```

- Now we can now apply the wrapped function polymorphically (after unwrapping within the user code).

**Parametric polymorphic fields** (including changing the parametric record type)

```wiki
 -- data ParamR a = ParamR { paramA :: a }                              -- _not_ sharing, so would generate:

    newtype ParamA a = ParamA a
    data ParamR a = ParamR { paramA :: ParamA a }

    paramA :: ParamR a -> a                                             -- generated selector is monomorphic in ParamR,
    paramA r = get r (undefined :: ParamA a)

    instance Has (ParamR a) (ParamA _a)        where                    -- Note: different type args
        get ParamR{ paramA = ParamA x } _ = x                           -- unwrap the value from the newtype
        set (ParamA x) ParamR{ .. }     = ParamR{ paramA = ParamA x, .. }

    type instance GetResult (ParamR a) (ParamA _a)  = a                  -- take param type as is (_a is dummy)
    type instance SetResult (ParamR _a) (ParamA a)  = ParamR a           -- take param type to be (_a is dummy)

```

**Wilder thought: towards type-indexed rows?**


Since we're using the field's type pun to instance the field, perhaps we could avoid the dummy argument to `get`, like this, with a few adjustments for the parametric type:

```wiki
    class Has r t                                           where
        get :: (GetResult r t ~ t) => r -> t                            -- GetResult 'improves' get's result
        set :: t -> r -> SetResult r t                                  -- 

    paramA :: ParamR a -> a                                             -- generated selector unwraps the param type,
    paramA r = let (ParamA x) = get r in x                              -- get returns the wrapped newtype

    instance Has (ParamR a) (ParamA _a)        where                    -- Note: different type args
        get ParamR{ paramA } = paramA                                   -- don't unwrap the value from the newtype
        set (ParamA x) ParamR{ .. }     = ParamR{ paramA = ParamA x, .. }
```

`get`'s result type is effectively 'pulling' the field out of the record. This is getting close to the style of polymorphic/anonymous type-indexed rows, as described in some of the record calculi proposals (such as HList).


.
