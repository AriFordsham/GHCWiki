GitLab uses labels to organize both issues and merge requests. GHC uses this mechanism to track a variety of information. Here we document the conventions we use.

# Merge request status

 * ~"backport needed": This denotes a merge request which should be backported to a stable branch before the next release.
 * ~"backport": This denotes a merge request which backports a merge request to a stable branch. The merge request description should link to the MR or issue from which the backported patch came

# Types of issues

 * ~bug: This denotes an issue which describes a bug or infelicity
 * ~"feature request": This denotes an issue which describes a desired new feature.
 * ~task: This denotes an issue which describes a task of some sort (e.g. a refactoring that should be undertaken, a test which should be introduced, etc.)

# Types of bugs

 * ~"compiler perf": A case where compilation time isn't what we would like it to be
 * ~"runtime perf": A case where the code produced by GHC isn't as performant as we would like
 * ~"compiler crash": A case where GHC itself crashes
 * 

# Miscellaneous

 * ~"CI breakage"
 * ~"core libraries"
 * ~"user facing"

# Bug environmental scope

These labels identify the environments in which a bug will manifest.

 * ~ARM: Issues affecting the ARM architecture
 * ~clang: Bugs triggered by GHC's interaction with the Clang C compiler (e.g. on Darwin)
 * ~cpp
 * ~cross-compilation
 * ~"error messages"

# Bug subsystems

These labels identify compiler subsystems which are in some way implicated in a bug.

 * ~"ambiguity check": A bug in type ambiguity checking
 * ~"API annotations"
 * ~backpack: The backpack module system
 * ~"code generation": Things pertaining to STG-to-Cmm code generation
 * ~"CPR analysis": Constructed Product Result analysis
 * ~"CSE": Common Subexpression Elimination optimisation
 * ~"Data Parallel Haskell":  The (stagnated) Data Parallel Haskell project 
 * ~debugger: The GHCi debugger
 * ~"debug information": Native debugging information and backtrace support (e.g. DWARF) 
 * ~"demand analysis": Demand (strictness) analysis
 * ~documentation
 * ~driver: The compiler driver

## External projects

These labels are generally relics of when GHC's bugtracker was also used to track issues in external projects.

 * ~external-directory: The `directory` library
 * ~external-hoopl: The `hoopl` library
 * ~external-hsc2hs: The `hsc2hs` library
 * ~external-old-time: The `old-time` library
 * ~external-pretty: The `pretty` library
 * ~external-process: The `process` library
 * ~external-random: The `random` library
 * ~external-unix: The `unix` library


# Language features

These labels identify language features which a bug is triggered by.

 * ~CAFs: Handling of Constant Applicative Forms
 * ~"compact normal forms": Compact normal forms (also known as compact regions)
 * ~concurrency: GHC's threaded runtime and concurrency primitives 
 * ~CUSKs:  Complete User-Specified Kind annotations
 * ~"custom type errors": Custom type errors support (e.g. the `TypeError` typeclass)
 * ~"deferred type errors": GHC's handling of deferred type errors (i.e. `-fdefer-type-errors`)
 * ~deriving: Typeclass deriving features 
 * ~exceptions: Synchronous and asynchronous exception support

## Language extensions

 * ~ApplicativeDo
 * ~Arrows
 * ~ConstraintKinds
 * ~DataKinds
 * ~DefaultSignatures
 * ~DerivingVia
 * ~ExistentialQuantification