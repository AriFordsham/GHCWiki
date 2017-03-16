# `IO` vs. `ST` in demand analysis

`IO` and strict `ST` look fairly similar if you squint some: both of them
are single-threaded monads offering access to mutable references and arrays.


Unfortunately, that similarity breaks down almost completely when it comes to
demand analysis. A classic example involves `forever`
(see [ Russell O'Connor's SE post](http://softwareengineering.stackexchange.com/a/163840/92642))


Consider the following:

```
forever m >> n
```


This desugars to (essentially)

```
\s ->case forever m s of(# s', res #)-> n s'
```


Is this strict in `n`? It depends!

## `IO`


If we're working in `IO`, then I think we don't want to
consider it strict in `n`. It follows directly from the `Monad` laws that
`forever m >> n   >=   forever m`. If we force `n` early, then we consider
`forever (print 1) >> undefined` to be bottom. `forever (print 1)` doesn't
look much like bottom as it fills the terminal screen.


Another thing to note is that we can catch exceptions in `IO`. Suppose we write

```
f r n = writeIORef r 3>> n
```


Is this strict in `n`? You might think that if `n` is bottom, the program will end
and no one will ever know that we wrote to the `IORef`. But someone could write

```
f r n `catch`\e ->... readIORef r ...
```


and observe whether we wrote to the `IORef` before throwing an imprecise exception.

## `ST`


What if we're working in `ST`? Then things are completely different. `forever m >> n`
is not only strict in `n`, but even hyperstrict in `n`. If we
hit bottom (failing to calculate the final result pair), *that's it*. The entire `runST`
computation will be bottom. We've
been mutating all sorts of references and arrays, but the moment we hit bottom, all of those references and arrays
vanish in a puff of smoke; there are no lasting side effects. Although GHC
runs `ST` computations with `State# RealWorld`, this seems a bit wrong;
we really want to run them with `State# AlternateUniverse`! The whole I/O demand hack
is unnecessary in the context of `ST`, and we can be as aggressive as we like (ignoring
such horrors as `unsafeSTtoIO` and `unsafeIOtoST`).

## `IO` transactions?


Sometimes, we may not care about the distinctions above. Suppose we have an
action that performs several database operations in a transaction. If we hit
bottom performing any of the operations, then the whole sequence goes down
the drain (as long as we're careful to bracket appropriately to initiate and
terminate the transaction). In this case, it might be nice to allow the user
to state that these actions, despite being `IO`-like, are actually occurring
in an alternate universe. The trouble is that we could end up needing to compile
each action twice: once for when it is being performed as part of a transaction
and once for when it is being performed independently. I have no good story
for that as yet.

## Dreams


In my usual "let's break the entire world!" approach, I'd love to change the actual definition of `IO`
to something entirely different (one operational monad variant or another). I imagine the complaints
would be extremely loud. Aside from breaking the world, another potential downside of this approach is that it seems
likely harder to take advantage of knowledge that certain actions (e.g., `readIORef`) have no observable side effects.
On the plus side, we could remove absolutely all the I/O hacks from core-to-core that are necessary for
correctness, leaving only performance hacks.
