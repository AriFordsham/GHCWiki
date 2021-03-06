# Helpful Local Import Error Messages


Issue raised here: [https://ghc.haskell.org/trac/ghc/ticket/11418](https://ghc.haskell.org/trac/ghc/ticket/11418)

## The problem


Given these two modules:



Aaa.hs:


```
module Aaa where

import           BBb

main :: IO ()
main = putStrLn myString
```


Bbb.hs:


```
module Bbb where

myString :: String
myString = "hi"
```


There's a typo in `Aaa.hs`, the import should be `Bbb` instead of `BBb`.


Running `runhaskell Aaa.hs` results in this error:

```wiki
Aaa.hs:3:18:
    Could not find module ‘BBb’
    Use -v to see a list of the files searched for.
```


The request is to have the compiler suggest that this is a typo and that it should be `Bbb` instead.
It already does this for misspelled functions and package imports.


Because the compiler will not continue when finding an error like this, it won't be harmful to spend a little extra time looking for possible misspellings.

### Current workings


For reference, in this example GHC does the following: (source: thomie) 


Given the two files from the description, here's what happens when you run `ghc Aaa.hs`:

- GHC asks the OS to open `./Aaa.hs`, and reads its contents
- GHC figures out it needs module `BBb`
- GHC "guesses" that `BBb` is either in `./BBb.hs` or in `./BBb.lhs`
- GHC asks the OS to open those two files in order
- Since neither file exists, an error message is shown \[...\]

### Proposed solution (thomie)


Cabal already asks you to specify all known modules in either `exposed-modules` or `other-modules`.
Cabal already passes this list of modules to GHC.
GHC therefore already knows the names of all modules that could possibly be imported (not quite, see [https://github.com/haskell/cabal/issues/2982\#issuecomment-169786310](https://github.com/haskell/cabal/issues/2982#issuecomment-169786310))
This list could be used to detect import typo's.


Applied to the example above:

`ghc --make Aaa.hs` first looks through the directory and finds `Aaa` in file `Aaa.hs` and `Bbb` in file `Bbb.hs`. It then gives this information to any subsequent compilation procedures.


When `ghc` encounters `import BBb` (the spelling error) it looks in the list and the directory and notices that it cannot find `BBb`.
This is where `ghc` throws a compile error saying it cannot find the module.
Instead, `ghc` could now also look through the list for potential spelling mistakes.
Because `Bbb` is in the list of available modules, it notices that `Bbb` is there and is similar to `BBb`.
It then outputs the following helpful error message. 

```wiki
Aaa.hs:3:1:
    Could not find module BBb.
    Perhaps you meant Bbb instead of BBb at line 3 of module Aaa (Aaa.hs)
```


Perhaps it could even locate the spelling mistake:

```wiki
Actual   BBb
Expected Bbb
Diff     _b_
```


This feature could be called `-fhelpful-import-errors` and should be turned on by default.

## Old Proposed solution (No longer relevant but maybe historically useful)


Before compiling anything, ghc could scan the directory to find all the Haskell modules that are there and build a `Map ModuleName FilePath` (pseudocode).


This assumes that there is not much in the directories other than the code you're trying to compile.
It also assumes that you're not moving around modules during compilation. Both arguably valid assumptions.


Applied to the example above:

`ghc --make Aaa.hs` first looks through the directory and finds `Aaa` in file `Aaa.hs` and `Bbb` in file `Bbb.hs`. It then gives this information to any subsequent compilation procedures.


Next, when going through `Aaa`, `ghc` figures out it needs to compile `Bbb` so it looks in the map to find the filepath where it's stored. (No more lookups in the filesystem at this point.)


When `ghc` encounters `import BBb` (the spelling error) it looks in the map and notices that it cannot find `BBb`. It then goes through all the keys of the map and calculates the hamming distance/insert distance/some other metric between between the key and `BBb`. It notices that `Bbb` and `BBb` are similar and outputs the following helpful error message.

```wiki
Aaa.hs:3:1:
    Could not find module BBb.
    Perhaps you meant Bbb instead of BBb at line 3 of module Aaa (Aaa.hs)
```


Perhaps it could even locate the spelling mistake:

```wiki
Actual   BBb
Expected Bbb
Diff     _b_
```


This feature could be called `-fhelpful-import-errors`.

### Tradeoffs

#### The general idea


Implementing this change means trading off scanning a directory tree recursively for a comprehensive mapping of modules that can be imported:


Loss:

- We have to add the assumption that source directories contain little other than what is being compiled.


Gain:

- Instead of 2\*m\*n calls to look for files, ghc now only looks through these directories recursively.
  In the typical situation, that means it only has to find n files instead.
- Comprehensive mapping of modules that can be imported. (Can be used for typo-error detection.)
- No file-system calls to find modules while resolving the imports.
  This means:

  - A purer compiler
  - Less expensive import

#### Turning on the flag by default


It is debatable whether it should be turned on by default.


Turning it on by default means:


Pro:

- It is used by default, which means faster import resolution


Cons:

- It could go wrong when run in a source directory that contains much more than the modules that need to be compiled. (For example: a home directory.)

#### Caching the map \[Not an option\]


Leaving this here for the sake of history, but this is not feasible. Invalidating the cache without re-walking the directory tree would be impossible/very complicated.


Whether this map should be cached and re-used between compilations is also debatable:


Caching it means:


Pro:

- Faster import resolution if files don't get added or moved often


Cons:

- Negates the advantage of making the compiler purer.
