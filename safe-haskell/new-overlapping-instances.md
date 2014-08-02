# Safe Haskell & Overlapping Instances


Overlapping Instances cause problems for Safe Haskell. Why? Because we
don't want code compiled in the `-XSafe` language to be able to alter
the behavior of other code in a indirect manner through instance
selection.


That is, overlapping instances has the effect of changing the behavior
of existing code simply through importing a new module and bringing
new instances into scope. As a primary use case of Safe Haskell is
building new libraries for enforcing security on untrusted code, this
is problematic.

## Safe Haskell and Security


In the security setting, untrusted code is compiled with `-XSafe` to
ensure the types can be trusted and the code can't violate any
invariants enforced through the type system. The TCB of the security
enforcement library, will most likely contain code written using
`-XUnsafe`. This is to firstly allow access to all parts of the
greater GHC Haskell language and also to disallow untrusted code from
importing the module and gaining access to constructors that may allow
it to violate invariants you want enforced. This works since code
compiled with `-XSafe` can only import other `-XSafe` modules or
`-XTrustworhty` modules.


While `-XTrustworthy` modules may themselves import `-XUnsafe`
modules, the user compiling the code must trust the package that any
`-XTrustworthy` module resides in to indicate that they trust the
author of the `-XTrustworthy` module to have only exposed a safe
interface despite the unsafe internals.

## Instance Selection


In the security use-case, if we have code involving overlapping
instances, we don't want to compile any cases where instances from
`-XSafe` modules overlap instances from `-XUnsafe` modules. Why?
Because now the behavior desired by the security system author may
have been overridden by an untrusted, potentially malicious author.


To deal with this, we apply a simple rule. When resolving instance
selection during compilation, if the most specific instances comes
from a module M that was compiled with `-XSafe`, then all other
instances we considered (i.e., all the less specific, overlapped
instances) must also be from M. You could imagine relaxing this a
little and just enforcing that all overlapped instances are also from
`-XSafe` modules, but we prefer to isolate `-XSafe` modules instead
and have a stronger guarantee.


This essentially is a same-origin-policy, `-XSafe` modules can use
overlapping instances, but can only overlap themselves and no one
else.

## Instances and Safe-Inference


How do we infer safety given the above policy? We take a conservative
approach (by necessity) and consider a module that enables the
`-XOverlappingInstances` flag as `-XUnsafe`.


This is since compiling a module with `-XSafe` means those instances
can no longer overlap any instances outside the module. As overlapping
instances is only resolved at the call-site and not before, this
largely affects the consumers of a module compiled with `-XSafe` and
how it affects them changes when their own code changes. (I.e.,
removing an instance from an `-XUnsafe` module may now make a
call-site of overlapping instances compile since perhaps all possible
instances except the one removed came from the same `-XSafe` module).


As we can't change the semantics of Haskell when inferring safety, we
need to be conservative.

### Bug in Safe vs Safe-Inferred

**This bug doesn't seem to exist in 7.10 anymore. Instead, 7.10 requires that at least one of `C a` or `C Int` in module S has an overlap flag**


Interestingly, this isn't exactly true. Consider the following module:

```wiki
{-# LANGUAGE FlexibleInstances #-}
module S where

class C a where
  f :: a -> String

instance C a where
  f _ = "a"

instance C [Int] where
  f _ = "[Int]"
```


This will compile fine as-is and doesn't require a
`-XOverlappingInstances` flag as we only consider when instances
overlap at call-sites, not at declaration. It will be inferred as
safe. Now consider this module:

```wiki
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module T where

import safe S

instance C [a] where
  f _ = "[a]"

test2 :: String
test2 = f ([1,2,3,4] :: [Int])
```


The above should actually fail to compile since we have the instances
`C [Int]` from the `-XSafe` module S overlapping as the most specific
instance the other instance `C [a]` from module T. This is in
violation of our single-origin-policy.


Right now though, the above actually compiles fine but \*this is a
bug\*. Compiling module S with `-XSafe` has the right affect of causing
the compilation of module T to then subsequently fail. So we have a
discrepancy between a safe-inferred module and a `-XSafe` module,
which there should not be.


It does raise a question of if this bug should be fixed. Right now
we've designed Safe Haskell to be completely opt-in, even with
safe-inference. Fixing this of course changes this, causing
safe-inference to alter the compilation success of some cases. How
common it is to have overlapping declarations without
`-XOverlappingInstances` specified needs to be tested.

## New Overlapping Instances -- a.k.a Instance Specific Pragmas


GHC 7.10 is adding in instance specific pragmas to control overlapping
of instances. They consist of:

- `OVERLAPPABLE` -- Specifies that the instance author allows this
  instance to be overlapped by others.
- `OVERLAPPING` -- Specifies that the instance author is expecting
  this instance will overlap others.
- `OVERLAPS` -- Implies both `OVERLAPPABLE` and `OVERLAPPING`. This is
  equivalent to the old `-XOverlappingInstances` behavior.


Addressing them individually for code compiled with `-XSafe`:

- `OVERLAPPABLE` -- We will adopt the convention that all-bets
  are off with such instances. Code compiled with `-XSafe` will be
  allowed to overlap such instances, even if they are in a different
  module.
- `OVERLAPPING` -- This will keep the current instance behavior. Code
  in `-XSafe` code can only overlap instances declared `OVERLAPPING`
  if those instances are from the same module.
- `OVERLAPS` -- Ideally we'd like to remove this pragma as we think
  it's security implications (since it implies OVERLAPPABLE and
  OVERLAPPING which both have very different security properties) are
  subtle and could easily lead to developers expressing the wrong
  policy. Instead, we'd prefer developer specify both instance overlap
  policies manually.


This enables more flexibility than before with Safe Haskell. Now you
can allow your instances to be overlapped through `OVERLAPPABLE`
(i.e., somewhat analogous to being an open type class), or
simply overlap your own instances through `OVERLAPPING` but not export
that property to the world (i.e., somewhat analogous to being a closed
type class).


The nice thing is the new design also encourages library authors to be specific about the overlap property of their instances. So hopefully fixing the current bug with safe-inference and overlapping instances has even less of an effect.

### Safe Inference


Safe inference becomes a trickier question with the new pragmas.
Firstly, an easy decision:

- `-XOverlappingInstances` -- causes a module to be inferred unsafe.


What should the use of `OVERLAPPABLE`, `OVERLAPPING` and `OVERLAPS` do
though? Let's assume a module M is considered safe by default, and
when we consider each instance declared in a module M, we can either
leave M considered safe, or switch M to be unsafe (a one-way
transition). Our initial thoughts are the following:

- `OVERLAPPABLE` -- leaves M in current state (i.e., safe).
- `OVERLAPPING` -- switches M to be unsafe.
- `OVERLAPS` -- switches M to be unsafe.


Why this? Well because if we infer a module M containing only
`OVERLAPPABLE` instances as Safe, then compiling M instead with
`-XSafe` will give you a module M' with the same semantics as M when
consumed.


However, if M contains `OVERLAPPING` or `OVERLAPS` instances, then
consumers of M get slightly different behavior when M is considered
safe compared to when it is considered unsafe. Safe inference
shouldn't change the behavior of a module, so we must infer M as
unsafe.
