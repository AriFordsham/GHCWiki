# Original Idea


The [demand analyser](commentary/compiler/demand) uses two different rules for handling let bindings, as explained in the [ cardinality paper](https://www.microsoft.com/en-us/research/wp-content/uploads/2014/01/cardinality-popl14.pdf). TLDR:

- **LetDown** is used for top-level bindings, recursive bindings and local, non-recursive functions. It closely mimics semantics of an actual function call, as it unleashes the demand signature of the bound RHS at the call site within the let body.
- **LetUp** is used for local, non-recursive thunks. This works more like a type-system: Analysing the let-body reveals its demand on the identifier, in which the RHS of the bindings is analysed exactly once. Only now it is that the results from the body and the RHS get sequentially combined.


There are reasons why we currently need both rules ([ cardinality paper §3.5 and §3.6](https://www.microsoft.com/en-us/research/wp-content/uploads/2014/01/cardinality-popl14.pdf)), although the general approach of **LetDown** is more flow-sensitive and thus strictly more precise than **LetUp**.


Consider the following example:

```wiki
let y = ... 
in let x = 2*y
   in if ... then x else y
```


This evaluates `y` only once, but the **LetUp** rule (in contrast to **LetDown**) fails to recognise this:
The inner let-body has a demand environment of `[x :-> <L,1*U>, y :-> <L, 1*U>]`, which combines with the DmdEnv of the RHS `[y :-> <L, 1*U>` to a total DmdEnv for the let-expression binding `x` of `[y :-> <L,U>]`.
E.g., `y` is possibly evaluated multiple times!

## Co-call graphs


Meanwhile, [Call Arity](call-arity), using a **LetUp** strategy at let-bindings exclusively, has countered the exact same loss of precision by employing \*Co-call graphs\*. Instead of having cardinality information stored in a plain identifier map, where identifiers are assumed to be evaluated with each other, co-call graphs additionally assert that some ids are never evaluated during the same evaluation of the containing expression. 


For the above example, the co-call graph for the inner let-body would specifically *not* contain an edge between `x` and `y`, because they are evaluated on mutually exclusive code paths. Thus, substituting the co-call graph of `x`s RHS into the co-call graph of the let-body will *not* induce a loop on `y`, so y will be recognised as evaluated at most once, which is what we want here.
