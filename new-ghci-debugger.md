
The ToDo list for the debugger is now in ticket #1377.

## Wishlist of features (please feel free to add your ideas here)

### Backtracing


Perhaps the most useful missing feature is the ability to see where the program has been executing just prior to a breakpoint. A lexical call stack is probably out of the question (see: [ExplicitCallStack](explicit-call-stack), and [ExplicitCallStack/StackTraceExperience](explicit-call-stack/stack-trace-experience)), but we could keep track of the most recently visited breakable expressions. A key question is what kind of information should we keep about the evaluation history. The simplest thing is to record just source locations. A more adventurous idea is to keep the local variables too. One probem with keeping local variables is that we would have to make sure the GC keeps them alive, so the data structure for storing the evaluation history would have to be traversable by the GC. Note that by keeping things alive, there is the danger of introducing space leaks. Obviously we would not want to keep the whole evaluation history, but only the last N events, where N is a configurable parameter. Keeping track of the history is also likely to incur a performance penalty, so it might be advantageous to be able to turn it off, or perhaps have it off by default, and be able to turn it on.


Another option to consider is reusing the CCS framework in a lightweight way, as sketched in [LightweightCCSCallStack](lightweight-ccs-call-stack).


It would be especially useful to be able to get backtraces when exceptions are raised.

### Temporarily disable breakpoints


Typically when we reach a breakpoint we want to inspect the values of local variables. As is often the case the values are thunks. So, to print them, we must force them in some way, and that sometimes raises more breakpoints. Often this is annoying. It would be handy if the debugger allowed us to temporarily disable all breakpoints. It should be relatively easy to implement. The main question is what is the right user interface to the feature?

---

## Implementation notes


How does the debugger work?



This has been moved to [Commentary/GHCi](commentary/ghci)


