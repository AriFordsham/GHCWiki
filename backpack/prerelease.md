## Examples

### Hello World


To test that everything is working properly, create the following two files:

```wiki
-- hello.bkp
executable hello where
  includes: base
  main-is: hello

-- hello.hs
main = putStrLn "Hello world!"
```


and then compile with:

```wiki
ghc --backpack hello.bkp
./hello
-- Hello world!
```

### Reusable import lists


Suppose you have a Haskell file with an import `import System.Directory (doesFileExist)`; you're using an explicit import list because that's good style, or you don't want to accidentally use a new function that's not available on all versions of the package you care about.  You now want to reuse this import list in another module, but you don't want to copy it over. Here's an example of how to do this in Backpack:

```wiki
package p where
    includes: base
    exposed-modules: P Q
    required-signatures: System.Directory

executable Main where
    includes: base, directory, p
    main-is: Main

-- p/System/Directory.hsig
module System.Directory where
doesFileExist :: FilePath -> IO Bool

-- p/P.hs (the module in question)
module P(ptest) where
import System.Directory
ptest = doesFileExist "no_it_does_not" >>= print

-- p/Q.hs (the second module)
module Q(qtest) where
import System.Directory
qtest = doesFileExist "no_really" >>= print

-- Main.hs (a little test script for the module)
import P
import Q
main = ptest >> qtest
```


These files will successfully compile with `ghc --backpack importlist.bkp`.


You can check that other functions from `System.Directory` are not available by editing `P.hs` or `Q.hs` to attempt to use another function from the module, e.g. `doesDirectoryExist`.


In general, to import a subset of the interface of a module, you create an hsig file which contains the signatures you want and make sure that the original module is not in scope (notice how directory is not included in package p). Then, when you actually want to compile the module (as in the executable Main in this example) make sure you import all the packages that define the modules you defined in this way. 

**Under construction:** The error message you get when you attempt to use a function which is available from the underlying implementation but not from your signature could be improved.

## Type classes


In this example, we'll develop a sorted list of integers (akin to `Data.Map`) which requires the integers in question to have the moral equivalent of `Ord` instance. However, instead of using the `Ord` type-class (for which we can only have one), we'll use a module signature instead (in the style of [ modular type classes](http://www.mpi-sws.org/~dreyer/papers/mtc/main-long.pdf)). This will let us support multiple orderings for the integers, without having to write our list data structure twice and without having to cast integers into an alternate data type. (Note: this example can be generalized to handle arbitrary data types, but for simplicity this example is hard-coded for integers.)


Here is the code:

```wiki
-- sorted.bkp
package IntOrd-sig where  
    includes: base  
    exposed-signatures: IntOrd  
  
package SortedIntList where  
    includes: base, IntOrd-sig  
    exposed-modules: SortedIntList  
  
package IntOrd-impls where  
    includes: base  
    exposed-modules: IntOrd.Asc IntOrd.Desc  
  
executable Main where  
    includes: base  
              IntOrd-impls  
              SortedIntList (IntOrd as IntOrd.Asc,  
                             SortedIntList as SortedIntList.Asc)  
              SortedIntList (IntOrd as IntOrd.Desc,  
                             SortedIntList as SortedIntList.Desc)  
    main-is: Main

-- IntOrd-sig/IntOrd.hsig
package IntOrd-sig where  
    includes: base  
    exposed-signatures: IntOrd  
  
package SortedIntList where  
    includes: base, IntOrd-sig  
    exposed-modules: SortedIntList  
  
package IntOrd-impls where  
    includes: base  
    exposed-modules: IntOrd.Asc IntOrd.Desc  
  
executable Main where  
    includes: base  
              IntOrd-impls  
              SortedIntList (IntOrd as IntOrd.Asc,  
                             SortedIntList as SortedIntList.Asc)  
              SortedIntList (IntOrd as IntOrd.Desc,  
                             SortedIntList as SortedIntList.Desc)  
    main-is: Main

-- IntOrd-impls/IntOrd/Asc.hs
module IntOrd.Asc where
cmp :: Int -> Int -> Ordering  
cmp x y = compare x y

-- IntOrd-impls/IntOrd/Desc.hs
module IntOrd.Desc where
cmp :: Int -> Int -> Ordering
cmp x y = compare y x

-- SortedIntList/SortedIntList.hs
{-# LANGUAGE DeriveDataTypeable #-}
module SortedIntList(SortedIntList, empty, insert, toList) where

import Data.Typeable
import IntOrd

newtype SortedIntList = SL [Int]
    deriving (Show, Typeable)
 
empty :: SortedIntList
empty = SL []

insert :: Int -> SortedIntList -> SortedIntList
insert x (SL []) = SL [x]
insert x (SL ys@(y:ys')) =
    case cmp x y of
        GT -> SL (y : toList (insert x (SL ys')))
        _ -> SL (x : ys)

toList :: SortedIntList -> [Int]
toList (SL xs) = xs

-- Main.hs
{-# LANGUAGE DeriveDataTypeable #-}
module SortedIntList(SortedIntList, empty, insert, toList) where

import Data.Typeable
import IntOrd

newtype SortedIntList = SL [Int]
    deriving (Show, Typeable)
 
empty :: SortedIntList
empty = SL []

insert :: Int -> SortedIntList -> SortedIntList
insert x (SL []) = SL [x]
insert x (SL ys@(y:ys')) =
    case cmp x y of
        GT -> SL (y : toList (insert x (SL ys')))
        _ -> SL (x : ys)

toList :: SortedIntList -> [Int]
toList (SL xs) = xs
```