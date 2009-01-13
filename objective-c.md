# Haskell Objective-C FFI

## Goals


We aim for an extensions of the Standard Haskell Foreign Function Interface to include bindings to Objective-C.  We like to make it easy to embed Haskell code in Objective-C programs as well as the other way around.  Moreover, we like to be able to use object hierarchies stored in NIB files directly in Haskell code.

### Consequences of the goals


We need to be able to subclass Objective-C classes (and in that process overwrite methods, add new class and instance methods, and add properties).  We only directly support properties with setter and getter methods and not direct access to ivars; although, the latter can be gained by using the standard C FFI.

### Pragmatics


As far as the use of Haskell code from Objective-C goes, we can imagine three kinds of implementations: (1) We can make everything fully dynamic through the ObjC runtime at application runtime or (2) we can generate `.m` stubs statically during application compile time (from within GHC), and (3) we can directly generate object files that adhere to the conventions for ObjC object files.


Option (1) has the disadvantage that all class initialisation code needs to be executed after the program has loaded and before any other code runs.  We need to arrange that for dynamically loaded objects, too.  Option (2) and (3) lead to a similar outcomes (in terms of object files), but Option (2) seems easier to implement and has some precedent in the C stubs generated for Haskell functions dynamically exported to C.


We require the programmer to supply headers for all Objective-C classes implemented in Haskell.  This enables the Objective-C compiler to do type checking and enables Interface Builder to recognises properties used as outlets.

## Subtopics


We discuss the following subtopics on separate pages:

- \[ObjectiveC/ForeignDeclarations Foreign declarations for Objective-C\]
- \[ObjectiveC/Messaging Sending messages\]
- \[ObjectiveC/Classes Objective-C classes\]
- \[ObjectiveC/Naming Naming conventions\] (they are not enforced, but recommended)
- \[ObjectiveC/MemoryManagement Memory management\] 

## Related work


Chicken scheme objc egg: [ http://chicken.wiki.br/objc](http://chicken.wiki.br/objc)

## Development team

- [ Manuel M. T. Chakravarty](http://www.cse.unsw.edu.au/~chak/)
- [ André Pang](http://algorithm.com.au/)