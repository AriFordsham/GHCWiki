# GHC Status October 2011


GHC is still humming along, with the 7.2.1 release (more of a "technology preview" than a stable release) having been made in August, and attention now focused on the upcoming 7.4 branch. By the time you read this, the 7.4 branch will have been created, and will be in "feature freeze". We will then be trying to fix as many bugs as possible before releasing later in the year.


Significant changes planned for the 7.4 branch are:

- **changes to the way Safe Haskell works** David Terei has improved the design of Safe Haskell since the 7.2.1 release. In particular, it will no longer cause build failures for users who do not explicitly enable it. **Is this in already?**

- **declarations at the ghci prompt** Simon Marlow has extended GHCi so that it is possible to give any declaration at the ghci prompt. For example,

  ```wiki
  Prelude> data D = D Int
  Prelude> case D 5 of D x -> print x
  5
  ```

  This has already been merged, so will definitely be in 7.4.

- **kind polymorphism** (is this Dimitrios Vytiniotis?) **TODO****Is this in already?**

- **constraint kinds** Max Bolingbroke has implemented a language extension, ConstraintKinds, which allows the use of more expressive constraints. One thing this can be used for is constraint synonyms:

  ```wiki
  type Stringy a = (Show a, Read a)
  f :: Stringy a => a -> a
  f = read . show
  ```

  For more information see his [ blog post](http://blog.omega-prime.co.uk/?p=127). This has been merged and will be in 7.4.

- **associated type synonym defaults** Max Bolingbroke has implemented this, which allow the class defining an associated type to also give a default to be used if the instance doesn't declare its own associated type instances:

  ```wiki
  class C a where
    type T a
    type T a = [a]
  ```

  This has been merged and will be in 7.4.

- **profiling and hpc overhaul** Simon Marlow ... **TODO****Is this in already?**


We continue to receive some fantastic help from a number of members from the Haskell community. Amongst those who have rolled up their sleeves recently are:

- Ben Gamari, Karel Gardas and Stephen Blackheath have been working towards getting a registerised Arm port working
- Many people, including Sergei Trofimovich, Erik de Castro Lopo, Joachim Breitner, Thorkil Naur, David M Peixotto and Ben Lippmeier, have contributed platform specific fixes for other platforms
- Reiner Pope added Template Haskell support for unresolved infix expressions and patterns
- Jose Pedro Magalhaes has replaced the old generics support with a new design
- Peter Wortmann taught GHC how to compile Objective-C++ files
- Sam Anklesaria added support for additional .ghci files
- Mikolaj Konarski and Duncan Coutts have improved GHC's event logging
- Geoffrey Mainland improved error messages for unterminated quasiquotations
- Johan Tibell implemented a "population count" primitive, and some other optimisations
- Ross Paterson has fixed some problems with Arrows
- Edward Z. Yang has been improving the RTS
- George Roldugin improved the sync-all tool used by GHC developers
- Austin Seipp has been improving some of the compiler documentation
- Miscellaneous fixes and improvements from Daniel Fischer, Michal Terepeta and Lennart Kolmodin


As ever, there is a lot still to do, and if you wait for us to do something then you may have to wait a long time. So don't wait; join in!

## Other developments


Work continues on improving GHC in various directions. Active projects we know about include:

### Cloud Haskell

**TODO**

### Supercompilation

**TODO**

### Liquid types

**TODO**

### Parallel project

**TODO**

### DPH

**TODO**