# Haskell Program Coverage


This page describes the Haskell Program Coverage implementation inside GHC.


The basic idea is this

- For each (sub)expression in the Haskell Syntax, write the (sub)expression in a HsTick
- Each `HsTick` has a module local index number.
- There is a table (The Mix datastructure) that maps this index number to original source location.
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
    .. (case tick<m,n> of DEFAULT -> e) = .. StgTick m n (... e)
  ```
- The `Cmm` code generate translates `StgTick` to a 64 bit increment.


TO BE CONTINUED.

### Binary Tick Boxes


The reason we do not translate tick boxes using.

```wiki
 if e then (tick a True) else (tick b False)
```


is tick\<a\> True is a CAF, and gets lifted to top level. This maintain the coverage information, but does not allow for entry counting. If the if/then/else is called 100 times, and no exceptions were thrown, then you would expect the binary tick count to add up to 100. We hope to use Hpc to do path optimization in the future, so real numbers are important.
 
Also, we translate the tick late to allow case-of-case to work, allowing unboxed compares to work without generating boolean intermeduates. We still need to push one optimzation into the simpifier for this to work well.
