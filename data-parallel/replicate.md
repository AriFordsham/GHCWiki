# Preventing space blow-up due to replicate

## The problem


The vectorisation transformation lifts scalar computations into vector space.  In the course of this lifting, scalar constants are duplicated to fill an array, using the function 'replicateP'.  Array computations are lifted in a similar manner, which leads to array constants being replicated to form arrays of arrays, which are represented as a segmented arrays.  A simple example is our  'smvm' example code:

```wiki
smvm :: [:[: (Int, Double) :]:] -> [:Double:] -> [:Double:]
smvm m v = [: sumP [: x * (v !: i) | (i,x) <- row :] | row <- m :]
```


Here the variable 'v' is constant in the array comprehensions and will be replicated while lifting the expression `v !: i`.   In other words, for every single element in a `row`, lifting implies the allocation of a separate copy of of the entire array `v` — and this only to perform a single indexing operation on that copy of `v`.  More precisely, in the lifted code, lifted indexing (which we usually denote by `(!^)` is applied to a nested array consisting of multiple copies of `v`; i.e., it is applied to the result of `replicateP (length row) v`.  


This is clearly wasted effort and space.  However, the situation is even worse in Ben's pathological example:

```wiki
treeLookup :: [:Int:] -> [:Int:] -> [:Int:]
treeLookup table xx
  | lengthP xx == 1
  = [: table !: (xx !: 0) :]
  | otherwise
  = let len     = lengthP xx
        half    = len `div` 2
        s1      = sliceP 0    half xx
        s2      = sliceP half half  xx           
      in concatP (mapP (treeLookup table) [: s1, s2 :])
```


Here `table` is constant in `mapP (treeLookup table) [: s1, s2 :]`; hence, the entire `table` gets duplicated on each level of the recursion, leading to space consumption that is exponential in the depth of the recursion.

## What's happening here?


Replication of scalars and arrays is always a waste of time and space.  However, it is particularly problematic if the replicated structure is consumed by an indexing operation as it can change the asymptotic work complexity of the vectorised program.  This holds not only for indexing, but for any operation that consumes only a small part of its input array(s).  In other words, if a replicated structure is consumed in its entirety (for example by a fold), the asymptotic work complexity of replication matches that of consuming the structure.  For operations that only consume a small part of their input, that is not the case.  Hence, lifting, which introduces the replication, does increase asymptotic work.

## A plan to fix the problem


Generally, we don't want to copy replicated data — it's a waste of space!  We definitely don't want to do it for large data structures, and in particular, we don't want to do it for arrays.  After all, that can change the asymptotic work complexity of a program.  So, instead of having `replicateP` allocate and fill a new array with multiple copies of the same data, we use a special array representation that stores the data (once!) together with the replication count.  This is surely the most space efficient representation for a replicated array.


The downside of a special representation is that we now also need to modify all consumers of replicated arrays to accept this new representation and to handle it specially.  This leads to some code blow up (each array consumer needs to be able to dynamically decide between two input array representations), and we need to be careful not to disturb fusion.

### The trouble with indices


Although, a replicated array stores its replicated payload only once, it needs to be handled with care. When indexing into a replicated array, we still use the same indices as if the array data would have been copied multiple times.  That can be a problem in examples, such as `treeLookup` above where the replicated array iteration-space grows exponentially — even 64bit indices will eventually overflow.  However, we can circumvent that problem by taking care in code that consumes replicated arrays.


In the `treeLookup` example, the `table` is replicated and grows exponentially.  But it is a segmented structure (one segment per copy of the original array) and it is accessed in the base case by a lifted index operation.  When you look at the input to that application of lifted indexing, its first argument is huge (the replicated `table`), but the second argument contains the same *data* as the original value of `xx`, albeit segmented into an array with one segment per element.  So we have effectively got

```wiki
 [:table, table, ...., table:] !^ [:[:xx_1:], [:xx_2:], ..., [:xx_n:]:]
```


Note how the `xx_i` are unchanged.  It is only *in the implementation of `(!^)`* that the `xx_i` are blown up to index into the data vector of `[:table, table, ...., table:]` (which is `concatP [:table, table, ...., table:]`).  It is that multiplication of the `xx_i` with the prescaned segment descriptor of `[:table, table, ...., table:]` that will overflow.  Notice how that is internal to the implementation of `(!^)`.  If the first argument of `(!^)` is a replicated structure, there is no need whatsoever to perform that multiplication (and subsequent division) and the indices never overflow!

### Never take the length of a replicated array


Unfortunately, not only indices blow out, the length of replicated arrays may also overflow 64bit integers.  Hence, the consuming code must carefully avoid to take the length of such arrays.  This is only the case for `replicateP`s introduced by the vectoriser.  It is the responsibility of the DPH user to ensure that `replicateP`s that are explicit in the user code do not blow out.  (We may want to switch to 64bit indices —at least on 64bit builds— anyway.)

## Related work

- The work-efficient vectorisation paper by Prins et al.  Their approach only works in a first-order language.
- Blelloch's work on the space-behaviour of data-parallel programs.
