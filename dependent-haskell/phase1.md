# Adding kind equalities to GHC


This page is a discussion of the implementation of kind equalities in GHC. It is meant as a guide to the interesting bits.

## Points of interest

### `tryTcS` is now really pure


In HEAD, `tryTcS` claims to "throw away all evidence generated". This isn't quite true. `tryTcS` can still set metavariables and may twiddle `EvBindsVar`s inside of implications. With kind equalities, this won't do. The problem is that solving may invent new coercion variables; these variables may end up in types. If a metavariable is then set to point to a type with a fresh coercion variable in it, we have a problem: after throwing away the evidence, that coercion variable is unbound. (This actually happens in practice.) So, `tryTcS` must be very careful to be properly pure. It does this by maintaining the set of filled-in metavariables *and* a way to roll back any changes to nested `EvBindsVar`s. After the inner `TcS` action is complete, `tryTcS` rolls back the changes.


This works nicely in practice, but one does have to be careful when reading a `-ddump-tc-trace`, because a `writeMetaTyVar` might not be the final word. (If that's an issue, it's easy to fix. The `TcS` monad could know whether it's pure or not and print out accordingly.)

### CUSKs


I have a sinking feeling that a type has a CUSK now only when all types **and kinds** have known types. But I can't come up with an example that shows this clearly. However, we can say that anything to the right of a `::` is known to have type `*`, so this doesn't bite hard in practice. Thus `data T (a :: k)` has a CUSK, but `data S (a :: Proxy k)` does not. Does `data U (a :: Maybe k)`? I think it does, but that's not quite as obvious. What's the easy-to-articulate rule here? (Now, it's this nice rule: a type has a CUSK iff all of its type variables are annotated; if it's a closed type family, the result kind must be annotated, too.)

### Datacon wrappers are now rejigged


In HEAD, a datacon worker differs from a datacon wrapper in two distinct ways: the worker's types are `UNPACK`ed as requested, and the worker's type is rejigged, à la
`rejigConRes`. The wrapper has the datacon's original type.


This design caused endless headaches for me. (Sadly, I can't recall exactly what the problem was -- something to do with applying kind substitutions to variables. I can easily recall going round and round trying to figure out the right datacon design, though!) So, I changed wrappers to have a rejigged type. (Workers are unchanged.) This was actually a nice simplification in several places -- specifically in GADT record update. The only annoying bit is that we have to be careful to print out the right user-facing type, which is implemented in `DataCon.dataConUserType`.

### Fewer optimizations in zonking


There are a few little optimizations in TcHsSyn around zonking. For example, after finding a filled-in metavariable, its contents are zonked and then the variable is re-set to the zonked contents. This is problematic now.


The zonking algorithm in TcHsSyn knot-ties `Id`s. Of course, coercion variables are `Id`s, and so these knot-tied bits can appear in types. We thus must be very careful never, ever to look at a zonked type, which these optimizations do. So, I removed them.


I have not yet re-examined to see if there is a way to restore this behavior. There probably is, as coercion variables can't be recursive!

### `MaybeNew` is back


In Simon's refactoring in fall 2014, the `MaybeNew` type disappeared from the solver infrastructure. I found this type useful, so I brought it back. It seemed like a better way to structure my algorithm than working without it.

### No more `instance Eq Type`


Somewhere along the way (I think in wildcard implementation?), an `instance Eq Type` slipped in. I removed it.

### `analyzeType`


Once upon a time, I embarked on a mission to reduce imports of `TyCoRep`, instead aiming to export functions to make exposing `Type`'s innards unnecessary. This effort became `analyzeType` and `mapType`, both in `Type.hs`. `mapType` is a clear win, removing gobs of zonking code and making a relatively clean interface. See simplifications in TcHsSyn and TcMType. It's not clear if `analyzeType` is paying its weight though. I could easily undo this change.

## Tasks