
Examples of programs benefiting from linear types fall into three categories.

# Enforcement of protocol


Linear types can be used to encode protocols, in a way very similar to 'session types'. Linearity checks ensure that the protocol is respected (so one does not backtrack or drops out).

```
type a ⊗ b =...{- see proposal -}type a ⊸ b =...{- see proposal -}typeEffect=IO()-- for examplepr::Double->Effect-- "prints" a numbertypeN a = a ⊸EffectdataClient=MulDoubleDouble(N(Double⊗Server))-- Client sends two 'Double' and-- expects a double and a new server-- session.|TerminatetypeServer=NClientexampleClient::N(NClient)exampleClient server = server $Mul1234$\(product,server')->-- do something with the product
  pr product >> server' TerminateexampleServer::ServerexampleServer client =case client ofMul x y k -> k (x*y,exampleServer)Terminate-> return ()
```

# Correctness of optimized code


Writing programs in the polarized style shown above is very useful to write efficient programs.


In general, fusion in GHC relies on the rewrite rules and the inliner.

1. Rewrite rules transform code using general recursion into a representation with no recursion (eg. church encodings)
1. The inliner kicks in and 'fuses' composition of non-recursive functions
1. Unfused code may be reverted to the original representation.


The problem with this scheme is that it involves two phases of heuristics (rules and inliner), and in practice programmers have difficulties to predict the performance of any given program.


A partial remedy to this solution is to stop relying on rewrite rules, and use directly non-recursive representations. For example the following representation from Lippmeier et al.:

```
dataSources i m e =Sources-- 'i' is the array's index type, 'e' the type of elements and 'm' the effects{ arity :: i
  , pull  :: i ->(e -> m ())-> m ()-> m ()}-- 'pull' is an iterator to apply to every elements of the array (like 'traverse')dataSinks i m e =Sinks{ arity :: i
  , push  :: i -> e -> m (), eject :: i -> m ()}
```


Such representations are typically functionals, and thus do not consume memory. One eventually gets code which is guaranteed to be 'fused'. For instance, in the following example from Lippmeier et al. [ http://benl.ouroborus.net/papers/2016-polarized/dpdf-FHPC2016-preprint.pdf](http://benl.ouroborus.net/papers/2016-polarized/dpdf-FHPC2016-preprint.pdf), neither the source nor the sink represent data in memory.

```
copySetP::[FilePath]->[FilePath]->IO()copySetP srcs dsts =do
  ss <- sourceFs srcs
  sk <- sinkFs   dsts
  drainP ss sk
```


One then faces two classes of new problems.


First, any non-linear (precisely non-affine) use of such a representation will **duplicate work**. For example:

```
example srcs dsts =do
  ss <- expensiveComputation <$> sourceFs srcs
  sk <- sinkFs  dsts
  drainP ss sk
  drainP ss sk -- expensiveComputation is run a second time here.
```


If one is not careful, one may end up with a program which does not use any intermediate memory, but duplicates a lot of intermediate computations. Linear types solve the problem by preventing such duplications. (Combinators may be still provided to duplicate computation explicitly or store intermediate results explicitly.)


Second, such representations may contain effects. In this situation, non-linear uses may produce an **incorrect program**. If one takes the example of a non-recursive representation of files, one may have two processes writing simultaneously in the same file (potentially corrupting data), or one may forget to close the file.


Quoting Lippmeier et al.:

>
> In general an object of type Sources is an abstract producer of data, and it may not even be possible to rewind it to a previous state — suppose it was connected to a stream of sensor readings. Alas the Haskell type system does not check linearity so we rely on the programmer to enforce it manually.


Literature on this style of non-recursive representations includes additionally:

- Push and Pull arrays in Feldspar [ https://scholar.google.com/scholar?oi=bibs&hl=en&cites=5417443716167196803](https://scholar.google.com/scholar?oi=bibs&hl=en&cites=5417443716167196803)
- On the duality of streams [ https://jyp.github.io/pdf/Organ.pdf](https://jyp.github.io/pdf/Organ.pdf)
- Composable Efficient Array Computations using Linear Types [ http://www.cse.chalmers.se/\~josefs/publications/vectorcomp.pdf](http://www.cse.chalmers.se/~josefs/publications/vectorcomp.pdf)

# Diminishing GC pressure


Because linear values cannot be shared, they should in principle not be subject to GC. Indeed, the consumer of the value (pattern matching) may very well perform de-allocation of the spot. Thus linear values can be stored in a heap outside of GC control. Alone, this strategy will diminish GC usage, but may increase the total running time of the program (if only because allocation in the GC heap is so efficient that it beats manual memory management for short-lived object) \[Wakeling and Runciman have experienced this effect\]. Yet, the tradeoff may be worth the trouble if long-tail in latencies is a bigger problem than absolute runtime.


There is however an improvement to be had on top of the simple strategy. Namely, to always fuse composition of linear functions. This strategy removes many short-lived objects. Fusing **always** is safe performance wise thanks to linearity. It is a good idea because it allows the programmer to predict accurately the behavior of the generated code.


A consequence of this choice is that linear data will only exist when pointed to by non-linear data structures.
