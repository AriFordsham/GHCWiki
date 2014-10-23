
The grand goal of source notes is to track how the source code causes program behaviour. This is useful in a lot of situations - for debugging we want to know what caused a misbehaviour, whereas for profiling we want to find reasons for program execution inefficiency.


However, this is a hard question to answer precisely. After all, both the program's optimisation as well as its execution is complicated. For example, a code transformation might take code from two different sources and merge them together, making both sources equally responsible for the resulting program behaviour. And, say, calling a closure means that the program flow depends critically on both the caller as well as how the closure got constructed.


In this situation, cost-centre profiling/stack-tracing takes the approach to \*simplify\* both optimisation and execution in order to make it feasible to follow it "perfectly". For source notes, we take the opposite approach - make a best effort to retain as much information as possible \*without\* any such restrictions. And as it turns out, we can extract a decent amount of information even then:

- For optimisations, we can ensure that we never underestimate causality: When optimising code, we can simply "merge" annotations. We can use operational semantics to show exactly how this merge should look like, but in most cases it's obvious (add all found annotations on the new expression).

- At execution time, we can reconstruct a lot of source code information just by paying close attention to the environment: A single code pointer can often give us a wealth of information about the compilation context. This is especially true in heavily in-lined code, where the code pointer will not only identify the running code, but a number of call contexts as well. If we walk the Haskell stack, we can gain even more information.


Each step is lossy in its own way, but the idea is to make the most of the incidental data we have.

## Implementation


We implement this as follows:

- We generate source notes in the desugaring phase, just like other ticks.

- When optimising, we know that we can discard source notes where we remove effects, and merge them when we combine expressions in a way that we can't tell their effects apart any more. Merging mostly takes the form of moving all ticks on matched expression to the top level.

- For Cmm, the program gets "flattened". As ticks should be able to scope over multiple blocks, and we especially want to be able to reconstruct the full calling context (see above) we assign nested tick scopes to blocks. This allows us to work with source notes essentially as we are on the Core level, and even allow some transformations.

- Once all optimisations have run, we extract the source notes from the Cmm blocks, (re)constructing the tick scope tree in the process. This will give us a map of back-end blocks to source notes, which makes it straight-forward to generate, for example, DWARF information.
