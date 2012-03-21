
The only "magic" thing about `GHC.TypeLits` are the instances of `NatI`.  The rest is implemented like this:

```wiki
newtype TNat (n :: Nat) = TNat Integer

tNatInteger :: TNat n -> Integer
tNatInteger (TNat n) = n
```


So, now we just need instances like these:

```wiki
instance NatI 0 where nat = TNat 0
instance NatI 1 where nat = TNat 1
instance NatI 2 where nat = TNat 2
...
```


Because we cannot generate this infinite family of instances, we have
some code in GHC which can solve `NatI` predicates on the fly.


The "proof" (aka "dictionary") for `NatI n` is just the number `n`.  This is OK because:

1. GHC uses a `newtype` to represent the dictionaries for classes that have just a single method and no super-classes.  `NatI` is just such a class.
1. `TNat` is already a `newtype` for `Integer`.


Therefore, the dictionaries for class `NatI` are just integers.
