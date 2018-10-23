## Goals

1. Collect the stable pointer table by generation (resolving [\#7670](https://gitlab.haskell.org//ghc/ghc/issues/7670)). At present, every minor collection needs to inspect every cell in the stable pointer table. Adding insult to injury, that even includes cells that haven't been occupied since the last major collection or ever.

1. Avoid the indirection and pauses inherent in the current array-doubling mechanism (see [\#15665](https://gitlab.haskell.org//ghc/ghc/issues/15665)), turning a stable pointer into a pointer directly to the relevant cell.

1. Keep the stable pointer table approximately as compact as it is today. When the table is large (and has not shrunk from its maximum historical size), we should ensure that (approximately) half the space used is devoted to actual live pointers.

1. Reduce the synchronization overhead inherent to the current array-doubling mechanism (again see [\#15665](https://gitlab.haskell.org//ghc/ghc/issues/15665)).

1. To the extent that we can do so efficiently, make the system lock-free. Two potential advantages:

  1. Non-Haskell threads can delete stable pointers during garbage collection.
  1. If a non-Haskell thread is killed while it is deleting a stable pointer, the Haskell runtime will survive.
