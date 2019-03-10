
\[ Up: [Commentary/Compiler/HscMain](commentary/compiler/hsc-main) \]

# The monad for renaming, typechecking, desugaring


The renamer, typechecker, interface-file typechecker, and desugarer all share a certain amount in common: they must report errors, handle environments, do I/O, etc.  Furthermore, because of Template Haskell we have to interleave renaming and typechecking.  So all four share a common monad, called `TcRnIf`.  This infrastructure is defined by the following modules:

- [compiler/utils/IOEnv.lhs](/trac/ghc/browser/ghc/compiler/utils/IOEnv.lhs): extends the IO monad with an environment (just a simple reader monad).
- [compiler/typecheck/TcRnTypes](/trac/ghc/browser/ghc/compiler/typecheck/TcRnTypes): builds the `TcRnIf` monad on top of `IOEnv`:
- [compiler/typecheck/TcRnMonad](/trac/ghc/browser/ghc/compiler/typecheck/TcRnMonad): defines lots of access functions for the renamer, typechecker, and interface typechecker.
- [compiler/typecheck/DsMonad](/trac/ghc/browser/ghc/compiler/typecheck/DsMonad): specialises the `TcRnIf` monad for the desugarer.


The typechecker and renamer use *exactly* the same monad, `TcRn`; the desugarer and interface-file checker use differnt instantiations of `TcRnIf`.  To give you the idea, here is how the `TcRn` monad looks:

```wiki
type TcRn a       = TcRnIf TcGblEnv TcLclEnv a
type TcRnIf a b c = IOEnv (Env a b) c

data Env gbl lcl	-- Changes as we move into an expression
  = Env {
	env_top	 :: HscEnv,	-- Top-level stuff that never changes
				-- Includes all info about imported things

	env_us   :: TcRef UniqSupply,	-- Unique supply for local varibles

	env_gbl  :: gbl,	-- Info about things defined at the top level
				-- of the module being compiled

	env_lcl  :: lcl		-- Nested stuff; changes as we go into 
				-- an expression
    }

type RnM  a = TcRn a		-- Historical
type TcM  a = TcRn a		-- Historical
```


The details of the global environment type `TcGblEnv` and local environment type `TcLclEnv` are also defined in [compiler/typecheck/TcRnTypes.lhs](/trac/ghc/browser/ghc/compiler/typecheck/TcRnTypes.lhs).  Side effecting operations, such as updating the unique supply, are done with TcRefs, which are simply a synonym for IORefs. 
