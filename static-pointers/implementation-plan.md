## Implementation plan for `-XStaticPointers`


The `-XStaticPointers` language extension as currently envisioned is very lightweight, adding just one new syntactic form to expressions with its attendant typing rule. However, in order to implement the language extension with a smaller trusted code base, a few changes need to happen in unrelated parts of the compiler and base libraries. In particular, in order to avoid unsafe type casts in the implementation of [distributed-closure](distributed-closures), we will need the [typed Typeable](typeable) proposal. Further, in order to enable dynamic checks that static pointers are always decoded against the right type (recall that `StaticPointer a` has a phantom type parameter `a` which must match the type of the value referred to), GHC must maintain a Static Pointer Table (SPT) including one entry for each (unique) expression `e` appearing in the body of a `static e` form. This table is looked up at runtime, during decoding of static pointers.


However, there is a short-term workable plan for GHC 7.10, that does not require redefining the `Data.Typeable` class at the last minute of the release engineering cycle. We observe that:

- The typed `Typeable` proposal makes it possible to avoid unsafe type casts in the presence of existentials (as does appear in the implementation of distributed-closure), but is otherwise not required. In other words, that proposal helps reduce the size of trusted base, but does not otherwise enable any particular feature.

- The SPT is required to make the following code raise an exception gracefully, rather than segfault: `decode (encode (ptr :: StaticPtr a)) :: StaticPtr b`. The *metadata* stored in the SPT, in particular the type of each value that is stored in it, is only useful for extra dynamic checks (which some people will want to opt out of anyways for performance), but not required for implementing the functionality of the language extension (decoding can still be done without it, albeit less safely).

- Users normally do not encode / decode static pointers directly, nor even do they use most of distributed-closure directly. Instead, a higher-level application-specific framework such as distributed-process is used. In the particular case of distributed-process, the dynamic type checks that the SPT enables should normally be redundant with the checks that distributed-process itself performs. To be clear, use of the SPT means we *don't need to trust the implementation of distributed-process to avoid segfaults* (see last section below), but not making use of the SPT for now does not mean the user is at risk of segfaulting by mistake.


The proposal is therefore to:

- Implement the extension in two phases: the full version with full dynamic type checks on top of a revised `Data.Typeable` in GHC 7.12, but an interim version in GHC 7.10 *with the same API* but with the dynamic type checks enabled by the SPT missing.

- In GHC 7.10, *implement only a shim SPT*, that looks just like the real thing, but does not include `tTypeRep`s or `TypeRep`s for any value. The reason we still need an SPT in GHC 7.10 is because there needs to be something that references static expressions throughout the lifetime of the program. The reason is that any such expression may at any time be referenced by a new `StaticPtr` produced from an incoming message. In other words, in a distributed system, other nodes may refer to values that are otherwise not live on the local node. The SPT must itself be protected against garbage collection, e.g. through the use of a `StablePtr`.

- In GHC 7.10, possibly only allow identifiers in the body of a `static` form, depending on the time required to implement floating of expressions to top-level adequately. This isn't a loss of generality, but is a loss of convenience for the user: one would have to float all expressions as top-level definitions manually.

### The API of `GHC.StaticPtr`

```wiki
module GHC.StaticPtr
  ( StaticPtr(..)
  , deRefStaticPtr
  , fingerprint
  , lookup
  ) where

data StaticPtr a

deRefStaticPtr :: StaticPtr a -> a
fingerprint :: StaticPtr a -> Fingerprint
lookup :: Fingerprint -> Maybe Dynamic -- or DynStaticPtr
```

**Remarks:**

- `deRefStaticPtr` so named to make its name consistent with `deRefStablePtr` in `GHC.StablePtr`. Other possibilities include `unstatic` or `unStaticPtr`.
- This module will be added to `base`, as for other primitives exposed by the compiler. This means we cannot depend on `bytestring` or any other package except `ghc-prim`.
- As such, we should leave it up to user libraries how they wish to encode `StaticPtr`, using whatever target type they wish (e.g. `ByteString`). The solution is to *not* provide encoders / decoders to some string-like type, but instead to map to/from `GHC.Fingerprint.Fingerprint` (used as the name for each entry in the SPT), which the user can encode/decode as she wishes (that part need *not* be part of the TCB).

### Implementation notes

- `StaticPtr` is defined as follows:

  ```wiki
  data StaticPtr a = StaticPtr !Fingerprint a
    deriving (Read, Show, Typeable)
  ```
- Floating of static expressions will be done in the desugarer, not the type checker.
- Each occurrence of `static e` where `e :: a` incurs a new top-level definition:

  ```wiki
  __static_f :: Dynamic
  __static_f = toDynamic (StaticPtr (Fingerprint ...) e)
  ```


where `__static_f` is a fresh name.

- In the type checker, we add `Typeable a` to the set of constraints.
- All such `__static_*` definitions are considered SPT entries. All SPT entries are collected into the SPT for each module, constructed by the code produced by `mkModuleInit`.
- A global SPT constructed using the SPT from each module by the RTS just before `main` is invoked.
- The SPT is a hash table mapping `Fingerprint`s to `StaticPtr`s.

**Remark:** do we need `__static_f` at all? It looks like we could get `mkModuleInit` to generate the right code with all the entries, without the extra indirection.

### Appendix: notes about distributed-process


Messages sent across the wire by distributed-process fall in one of three categories:

- control messages, which always have type `NCMsg`, a monomorphic type;
- type tagged messages sent using `send`, received using `expect` or `receiveWait`. These messages always include a header containing the fingerprint of the `TypeRep` for the value (needed to that `expect` and friends can dispatch on messages of the right type on the receiving end);
- channel messages, which don't include a type tag, since the type is implied by the channel on which the message is delivered.


In each case, without an SPT, one must trust that distributed-process is deserializing at the right type when deserializing closures. One does not, however, need to trust the user in order to rule out the possibility of segfaults, so long as she is *only* a client of said library (i.e. does not use any side channel to send closures across the network).
