# FFI Exports


On this page I've collected some information about how we handle FFI exports which I found while working on [\#15808](https://gitlab.haskell.org/ghc/ghc/issues/15808).


It's incomplete but should hopefully make the journey less painful for these who follow my footsteps.
Much of the information is in DsForeign as well but I found it hard to grep for it so here we go.

### The Rundown


During desugaring from HsSyn to Core we look for foreign in/exports (DsForeign.hs). If you deal with issues with FFI this is a good starting point.


After running the pipeline we get:

- The haskell function
- A stable name wrapper for it.
- The exported C function
- A initialization stub.

- The haskell function:

  - This is called by regular Haskell code. 
  - It might also be jumped to eventually when calling the exported function from C name.
- The stable name wrapper:

  - In trivial cases it can end up as a duplication of the normale function.
  - For anything doing actual work it seems to just be an indirection to the regular haskell function.
- The exported function

  - It grabs a worker from the runtime.
  - It converts the arguments to the Haskell variants
  - Applies the the arguments, returns the result, releases the worker.
  - It does the above by calling into the RTS. Using functions like rts_apply, rts_mkInt, rts_getInt, rts_lock, ... rts_unlock.
  - Important to note that the exported function always calls the stable name function.
    The reason isn't entire clear to me at this point. The comments explain what they do well enough but not much
    about why. So if you can clarify please do!
- The initialization stub:

  - This is a small snippet of C code which registers the exported function and it's stable name with the RTS.
  - It gets compiled via gcc during codegen.
