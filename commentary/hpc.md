# Haskell Program Coverage


This page describes the Haskell Program Coverage implementation inside GHC. Background information can be found in the paper [Haskell Program Coverage](http://www.ittc.ku.edu/~andygill/papers/Hpc07.pdf) by Andy Gill and Colin Runciman, and the Haskell wiki page [ Haskell program coverage](https://wiki.haskell.org/Haskell_program_coverage).


The basic idea is this

- For each (sub)expression in the Haskell Syntax, write the (sub)expression in a    
  `HsTick`
- Each `HsTick` has a module local index number.
- There is a table (The Mix data structure) that maps this index number to original source location.
- Each `HsTick` is mapped in the Desugar pass with: 

  ```wiki
    dsExpr (HsTick n e) = case tick<modname,n> of DEFAULT -> e
  ```
- This tick is a special type of `Id`, a `TickOpId` which takes no core-level argument, but has two pre-applied arguments; the module name and the module-local tick number.

  - We store both module name and tick number to allow this Id to be passed (inlined) inside other modules.
  - This `Id` has type **State\# World\#**
- The core simplifier must not remove this case, but it can move it.

  - The do-not-remove is enforced via the ... function in ....
  - The semantics are tick if-and-when-and-as you enter the `DEFAULT` case. But a chain of consecutive ticks can be executed in any order.
- The CoreToStg Pass translates the ticks into `StgTick`

  ```wiki
    coreToStgExpr (case tick<m,n> of DEFAULT -> e) = StgTick m n (coreToStgExpr e)
  ```
- The `Cmm` code generator translates `StgTick` to a 64 bit increment.


Other details

- A executable startup time, we perform a depth first traversal some module
  specific code, gathering a list of all Hpc registered modules, and the
  module specific tick table. 
- There is one table per module, so we can link the increment statically,
  without needing to know the global tick number.
- The module Hpc.c in the RTS handles all the reading of these table.
- At startup, if a .tix file is found, Hpc.c checks that this is the same
  binary as generated the .tix file, and if so, pre-loads all the tick counts
  in the module specific locations.
- (I am looking for a good way of checking the binaries for sameness)
- At shutdown, we write back out the .tix files, from the module-local tables.

### Binary Tick Boxes


There is also the concept of a binary tick box. This is a syntactical boolean, like a guard or conditional for an if.
We use tick boxes to record the result of the boolean, to check for coverage over True and False.

- Each `HsBinaryTick` is mapped in the Desugar pass with: 

  ```wiki
    dsExpr (HsBinaryTick t f e) = case e of 
                                   { True -> case tick<modname,t> of DEFAULT -> True
                                   ; False -> case tick<modname,f> of DEFAULT -> False }

  ```

- After desugaring, there is no longer any special code for binary tick box.

## Machine Generated Haskell


Sometimes, Haskell is the target language - for example, Happy and Alex. In this case, you want to be able to check for coverage
of your **original** program. So we have a new pragma.

```wiki
    {-# GENERATED "Parser" 100-2:101-4 #-} <expr>
```


This means that the expression was obtained from the given file and locations. This might be code included verbatim
(for example the actions in Happy), or be generated from a specification from this location.
