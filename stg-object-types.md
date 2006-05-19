
Back to [GarbageCollectorNotes](garbage-collector-notes)


\[This page needs lots of work\]

# Object Structure

# Evaluation 101

# Interesting Object Types

- CONSTR - constructors
- THUNK - thunks (courtesy of being a lazy language)
- FUN - functions
- IND - indirections - what is left behind after thunk evalution since we dont know who all would be referring to the thunk. 
- TSO - thread state object - see discussion at [CapabilitiesAndScheduling](capabilities-and-scheduling)
- PAP - partial application - a function onject to which only some arguements have been applied.

# Backward Pointers


Backwards pointers are the cause of much heartache in writing generational GCs. They are essential pointer from older generations to newer ones. If you think about it, such a thing should never really occur in a pure functional langauge since objects cannot be updated once created. While that is true in essence, backward pointers do arise in Haskell in the following cases

- Thunk updation
- Unsafe pointer updation 

  - GHC.Prim
  - usafePerformIO : Haskell.IO.Unsafe


Types

- MUT_VAR
- MUT_ARRAY
- MVAR, TVAR
- TSO
- IND_OLDGEN

# Spark Pool and par

# Weak References

# Stable Pointers

# CAF

>
> CAF stands for Constant Applicative Form. 

# Black Holes

- Free closures environment variables early
- Loop detection at runtime
- Avoid re-evaluation of the same think by mutliple threads
