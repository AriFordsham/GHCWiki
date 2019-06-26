# HIE (.HI Extended) Files


.hie files are a proposed new filetype that should be written by GHC next to .hi files.

## The Problem


GHC builds up a wealth of information about haskell source as it compiles it, but throws all of it away when it's done. Any external tools that need to work with haskell source need to parse, typecheck and rename files all over again. This means haskell tooling is slow and has to rely on hacks to extract information from ghc.
Allowing GHC to dump this information to disk would simplify and speed up tooling significantly, leading to a much richer and productive haskell developer experience.


As a proof of concept, haddocks --hyperlinked-source feature will be rewritten to make use of .hie files, such that it doesn't need to recompile the source.

## File Contents

- The data structure is a simplified, source aware, annotated AST derived from the Renamed/Typechecked Source
- We traverse the Renamed and Typechecked AST to collect the following info about each SrcSpan

  - Its assigned type(s)(In increasing order of generality), if it corresponds to a binding, pattern or expression

    - The `id` in `id 'a'` is assigned types \[Char -\> Char, forall a. a -\> a\]
  - The set of Constructor/Type pairs that correspond to this span in the GHC AST
  - Details about all the identifiers that occur at this SrcSpan

- For each occurrence of an identifier(Name or ModuleName), we store its type(if it has one), and classify it as one of the following based on how it occurs:

  1. Use
  1. Import/Export
  1. Pattern Binding, along with the scope of the binding, and the span of the entire binding location(including the RHS) if it occurs as part of a top level declaration, do binding or let/where binding
  1. Value Binding, along with whether it is an instance binding or not, its scope, and the span of its entire binding site, including the RHS
  1. Type Declaration (Class or Regular) (foo :: ...)
  1. Declaration(class, type, instance, data, type family etc.)
  1. Type variable binding, along with its scope(which takes into account ScopedTypeVariables)
- It should be possible to exactly recover the source from the .hie file. This will probably be achieved by including the source verbatim in the .hie file, as recovering the source exactly from the AST might be tricky and duplicate the work on ghc-exactprint.
- The first line of the .hie file should be a human readable string containing information about the version of the format, the filename of the original file, and the version of GHC the file was compiled with. Example: (v1.0,GHC8.4.6,Foo.hs)
- The format should be fairly stable across ghc versions, so we need to avoid capturing too much information. More detailed information about the exact haskell syntactic structure a part of the tree represents could be obtained by inspecting the tokens/keywords in that part.


The RichToken type used in haddock: [https://github.com/haskell/haddock/blob/master/haddock-api/src/Haddock/Backends/Hyperlinker/Types.hs\#L35](https://github.com/haskell/haddock/blob/master/haddock-api/src/Haddock/Backends/Hyperlinker/Types.hs#L35)

## Efficient serialization of highly redundant type info


The type information in .hie files is highly repetitive and redundant. For example, consider the expression

```wiki
const True 'a'
```


The type of the overall expression is `Boolean`, the type of `const True` is `Char -> Boolean` and the type of `const` is `Boolean -> Char -> Boolean`


All 3 of these types will be stored in the .hie file


To solve the problem of duplication, we introduce a new data type that is a flattened version of `Type`

```wiki
data HieType a = HAppTy a a  -- data Type = AppTy Type Type
               | HFunTy a a  --           | FunTy Type Type
               | ...
```

`HieType` represents one layer of `Type`.


All the types in the final AST are stored in a `Array Int (HieType Int)`, where the `Int`s in the `HieType` are references to other elements of the array. Types recovered from GHC are deduplicated and stored in this compressed form with sharing of subtrees.

`Fix HieType` is roughly isomorphic to the original GHC `Type`

## Scope information about symbols


A simple scope is defined as

```wiki
data Scope =
    NoScope
  | LocalScope Span
  | ModuleScope
```


Value bindings are assigned a single `Scope`.


For pattern bindings, things get a bit more complicated, with bindings in do notation and -XViewPatterns

```wiki
do (b, a, (a -> True)) <- bar
--  ^     ^^^^^^^^^^^^    ^^^ a is not in scope here or in the span of the first `b`
--        ^ but a is in scope here
   foo a
-- ^^^^^ a is in scope here
```


So pattern bindings are assigned two `Scope`s, one for the span of the pattern binding itself, and another for the rest.


The story is most complicated for type variables, in the presence of -XScopedTypeVariables and -XInstanceSigs

```wiki
foo, bar, baz :: forall a. a -> a
```

`a` is in scope in all the definitions of `foo`, `bar` and `baz`, so we need a list of scopes to keep track of this. Furthermore, this list cannot be computed on the first go, and thus type variable scopes are defined as

```wiki
data TyVarScope =
    ResolvedScopes [Scope]
  | UnresolvedScope [Name.Name] (Maybe Span)
```


UnresolvedScopes are resolved in a separate pass, by looking up the identifier binding sites.


Consider the following case

```wiki
instance Foo Int where
  foo :: forall a. ...
  foo = ... -- a is in scope here
instance Foo Bar where
  foo = ... -- a is not in scope here
```


To handle this case, we must store the Span of the instance/class definition along with the names.

## Validation of AST


There are a few simple validation tests enabled by `-fvalidate-hie`

- The shape invariants of the AST are checked(parent node spans completely contain children node spans which are arranged in left to right order without any overlaps)
- Scope information collected is validated(by checking all symbol occurrences are in the calculated scope)
- The AST is round-tripped through the binary serialization and checked for consistency

## Use cases

- Haddocks hyperlinked source and haskell-ide-engine

  - Type information on hover
  - Local(in file) usage sites for symbols
  - Supporting global go to/view definition for every symbol in the Package Db
  - Viewing info about arbitrary nodes in the AST - does it have a type? What language construct does it correspond to?
  - Recovering the scopes of symbols, for use in completion etc.
- Along with an indexer that scans .hie files

  - Viewing the usage sites of symbols across the entirety of hackage or a local Package Db
  - Dependency analysis of symbols - what other symbols does something depend on
  - Searching for symbols, and restricting search by type. Example: search for usages of `read` with type `String -> Int` to find out where the instance for `Read Int` is being used.
- More sophisticated analysis of the AST

  - Diffing changes to the AST
  - Viewing typical invocations/example usages of functions

## Modifications to GHC

- HIE file generation will be controlled by a GHC flag(-fenable-ide-info)
- The file will be generated as soon as GHC is done typechecking a file(maybe in [hscIncrementalCompile](https://www.stackage.org/haddock/nightly-2018-05-04/ghc-8.4.2/src/HscMain.html#hscIncrementalCompile)?)
- Need to coordinate with the Hi Haddock project(Including docstrings in .hi files) as that may push the burden of resolving Names/Symbols in haddock comments onto GHC.
- Other than this, little interaction with the rest of GHC should be needed.

## Why should we be able to recover file contents exactly?


Consider the case when the .hs source file that exists on disk doesn't compile, but with still have a stale .hie file generated the last time the source compiled. We would like to recover as much information as possible from the
stale .hie file to aid the user working on the .hs file. This is possible if we recover the original, compiling source from the .hie file and cross-reference/diff it with the edited file, so that we can still answer user queries for
portions of the file that haven't been edited(Indeed, this is how haskell-ide-engine currently works, but instead of reading from a .hie file, it maintains an in-memory cache of the last good `TypecheckedModule` corresponding to the source)


## Links to additional discussion



[Initial discussion on \#ghc](https://gist.github.com/wz1000/46bb4b2121f0911bbbf4d4743fafaba8) (The .hie(.hi Extended) name suggested by mpickering, cbor serialisation suggested by hvr)



[https://github.com/haskell/haddock/issues/715](https://github.com/haskell/haddock/issues/715)



[Original GSOC Proposal](https://docs.google.com/document/d/1QP4tV-oSJd3X90JKVY4D__Dfr-ypVB57p1yDqyk2aQ8/edit?usp=sharing)


# Making use of .hie files as a tooling developer

## Structure of .hie files

The HIE file AST is essentially an interval tree, where the intervals are
`SrcSpans` from the code which correspond to nodes in the internal GHC ASTs.

Each `Node` in the tree has a `Span`, some information associated with it, and
some children, the invariant being that the spans of the children are wholly
contained in the parent span. Additionally, the elements of `nodeChildren`
are sort with respect to their spans.

```haskell
  Node { nodeInfo :: NodeInfo a
       , nodeSpan :: Span
       , nodeChildren :: [HieAST a]
       }
```

The information contained in each nodes includes:

- The type(s) assigned by GHC to this `Node`, if any
- The name of the GHC AST constructors that this Node corresponds to
- The identifiers that occur at this span
- Information about the identifiers, like:
  + Are they names or modules
  + Their type, if any
  + The context in which they occur:  Are they being defined or used,
  imported, exported, declared etc..
  + If they are being defined, the full span of the definition, and
    an approximated Scope over which their definition can be used.

## Reading .hie files

To read `.hie` files, you must use a `NameCache`. This is so that if you
are using the GHC API and already have some `Name`s, `Name`s assigned to
identifiers in the .hie file can be consistent with those.

To create a fresh `NameCache`, you can do something like:

```haskell
makeNc :: IO NameCache
makeNc = do
  uniq_supply <- mkSplitUniqSupply 'z'
  return $ initNameCache uniq_supply []
```

Once you have a `NameCache`, you can use the `readHieFile` function
from `HieBin`.
```haskell
  do nc <- makeNc
     (hiefile, nc') <- readHieFile nc path
     ...
```

## Recovering and printing types

The type information that needs to be serialized in `.hie` files is highly
repetitive and redundant. For example, consider the expression:
```haskell
const True 'a'
```
The type of the overall expression is `Bool`, the type of `const True` is
`Char -> Bool` and the type of `const` is `Bool -> Char -> Bool`.

All 3 of these types will be stored in the .hie file.

To solve the problem of duplication, we introduce a new data type that is
a flattened version of GHC's `Type`:

```haskell
data HieType a = HAppTy a a  -- data Type = AppTy Type Type
               | HFunTy a a  --           | FunTy Type Type
               | ...
```

`HieType` represents one layer of `Type`.

All the types in the final AST are stored in a `Array Int (HieType Int)`,
where the `Int`s in the `HieType` are references to other elements of the
array. Types recovered from GHC are deduplicated and stored in this compressed
form with sharing of subtrees. In the AST, all types are stored as references
into this array

`Fix HieType` is supposed to be approximately equivalent to the original GHC
`Type`.

To recover the full type, you can use the following function in `HieUtils`:

```haskell
recoverFullType :: TypeIndex -> A.Array TypeIndex HieTypeFlat -> HieTypeFix
```

To print this type out as a string, you can use the following function:

```haskell
renderHieType :: DynFlags -> HieTypeFix -> String
```

If you don't have a GHC session set up, you will need to construct some
`DynFlags` to pass to this function. To do this, you will need to have path
to the GHC `libdir`. You can get this using the `ghc-paths` package.

```haskell
import SysTools ( initSysTools )
import DynFlags ( DynFlags, defaultDynFlags )
import GHC.Paths (libdir)

dynFlagsForPrinting :: IO DynFlags
dynFlagsForPrinting = do
  systemSettings <- initSysTools libdir
  return $ defaultDynFlags systemSettings ([], [])
```

## Looking up information for a point/span in a file

It is easy to look up the AST node enclosing/enclosed by a particular
`SrcSpan`. You can use the following functions from `HieUtils`:

```haskell
selectSmallestContaining :: Span -> HieAST a -> Maybe (HieAST a)
selectLargestContainedBy :: Span -> HieAST a -> Maybe (HieAST a)
```

## Looking up information for a symbol in the file

To look up information for a particular, already known symbol, it is a good idea
to generate a "References Map" using the following function in `HieUtils`:

```haskell
generateReferencesMap :: HieASTs a -> M.Map Identifier [(Span, IdentifierDetails a)]
```

Using the map generated by this, you can lookup all the spans a given
identifier occurs in, as well as whatever information is collected about
the occurrence at that location.

## Using hiedb to work with a collection of .hie files

`hiedb` offers a library interface to query `.hie` files for references,
and to generate index databases.

You can also use the library to lookup the `.hie` file associated with
a given `Module`(as long as that file is indexed in the db), and then run
queries on the file yourself.

# Upcoming developments

## Use HIE files to understand typeclass evidence and implicit variables

Recently, I extended `.hie` files to capture information about typeclass
evidence that is generated by GHC.   This can help you debug generated evidence
for your code, as well as enable features like "jump to implementation", which
will take you to the definition of the instance(s) that will be actually used
when you invoke a class method or any other method that takes some evidence
as a parameter.

To understand this, it is best to consider an example:

```haskell
class C a where
  f :: a -> Char

instance C a => C [a] where -- Line 27
  f x = 'a'

foo :: C a => a -> Char
foo x = f [x] -- Line 31
   --   ^ We want to query information for this point
```

With the new information, you can write a query on `.hie` files to ouput the
following:
```
Evidence from SrcSpanOneLine "HieQueries.hs" 31 1 14 of type: C [a]
Is bound by a let, depending on:

Evidence of type: forall a. C a => C [a]
bound by an instance at RealSrcSpan SrcSpanOneLine "HieQueries.hs" 27 10 22

Evidence from SrcSpanOneLine "HieQueries.hs" 31 1 14 of type: C a
Is bound by a signature
```

This probably won't make it into GHC 8.8.1 though. You can track the progress
of the MR here: <https://gitlab.haskell.org/ghc/ghc/merge_requests/1286>


## More type information in the AST

GHC doesn't save type information for all the nodes in its AST. This
means that to report type information, tools like `ghc-mod`, `intero`,
`haskell-ide-engine` and GHCi's `:set +c` desugared every subexpression in the
AST in order to access its type. However, this has extremely poor performance
characteristics due to repeatedly desugaring the same expression many times.

To avoid this, currently `.hie` files only store type information for
nodes where it is already precomputed for GHC, and for leaf nodes.

The plan is to fix this so that accessing type information for arbitrary nodes
in the typechecked AST is fast and efficient, by including the relevant type
information on the AST nodes where required.

## Easier, faster pretty printing for `HieType`'s

Currently, if you want to print a HieType, you have to go through GHC's
pretty printer via conversion to `IFaceType`. If you want to print a large
number of types, this also forces you to unroll all the types in the `.hie`
file, and pretty printing doesn't take any advantage of the sharing.

It should be possible to write a specialized pretty printer for `HieType`'s
that is fast and is able to share its output, so that it is efficient to
print out large numbers of types.

