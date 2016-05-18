# Relaxed unused imports

[Unused imports](commentary/compiler/unused-imports) describes how the current unused imports behavior works. The idea behind previous proposals for unused imports is GHC, at a 90% effort, should report all imports which can be deleted while your code still compiles.  This proposal adopts two new ideas:

1. When we use an identifier in the body of a Haskell program, we consider ALL import items which provide that identifier used (as opposed to only ONE, as was the case in the previous unused imports mechanism)

1. Idea (1) causes us to fail to report warnings for some obviously redundant imports. We add a new mechanism for reporting these warnings, where we report redundancy only when it is \*textually obvious\* (that is, can be determined without looking at the source code of every module we import.)


The old import warnings can be adopted as a flag which is not turned on automatically by `-Wall`

## Motivation

[ Snoyman reports](http://www.yesodweb.com/blog/2016/05/are-unused-import-warnings-harmful) a common CPP anti-pattern in cross-GHC code:

```wiki
#if MIN_VERSION_base(4,8,0)
import Control.Applicative ((<*))
#else
import Control.Applicative ((<*), pure)
#endif
```


The reason the CPP is necessary is because in recent version of GHC, `pure` is exported by `Prelude` and thus `Control.Applicative`'s import of it is redundant. Is it really redundant? Snoyman suggests he would be happy to accept just `import Control.Applicative ((<*), pure)` even though the `pure` declaration is redundant.

[\#10117](https://gitlab.haskell.org//ghc/ghc/issues/10117) reports another problem with a common workaround:

```wiki
  module Foo (Int, Word, Monoid(..)) where

  import Data.Monoid (Monoid(..))
  import Data.Word (Word)
  import Prelude
```


Here, `Word` and `Monoid` are considered redundant because they are also exported by `Prelude`. Once again, arguably the "redundant" import here is not a big deal, and we shouldn't complain too much about it.

## Specification


Say that an *import-item* is either an entire import-all decl (eg `import Foo`),
or a particular item in an import list (eg `import Foo( ..., x, ...)`).  
The general idea is that for each use of an imported name, we will attribute
that use to **all** import-items which provide it (unlike old proposals,
which only attributed the use to one import item). Then, any import items with no
uses attributed to them are unused, and are warned about. I've highlighted
in **bold** the differences from the previous specification.
More precisely:

1.  For every `RdrName` in the program text, find all the import-items that brought it into scope.  The lookup mechanism on `RdrNames` already takes account of whether the `RdrName` was qualified, and which imports have the right qualification etc, so this step is very easy.

1. **Mark all of these import items as "used".**

1.  Now bleat about any import-items that are unused.  For a decl
  `import Foo(x,y)`, if both the `x` and `y` items are unused, it'd be better
  to bleat about the entire decl rather than the individual items.

## Textual redundancy


The problem with this specification is that it does not report errors for completely duplicate imports:

```wiki
import A
import A
```


or

```wiki
import A(x)
import B(x) -- import B is OK, do not want warning
```


We observe that in both these cases it is *textually* obvious that there is redundancy. So we can just define a new algorithm to warn in these cases.

## Textual redundancy specification


We need two new algorithms to check for these cases of textual redundancy: one for dealing with entire import declarations which are dominated by others, and one for dealing with specific identifiers from import lists:

1. (Import redundancy.) For every import statement, we say that `import [ qualified ] A [ (...) | hiding (...) ]` is subsumed by `import A`, and `import [ qualified ] A as B [ (...) | hiding (...) ]` is subsumed by `import A as B`. For every pair of distinct import declarations such that one subsumes the other, report that the subsumed import is redundant (preferring textually later in the reflexive case.) Algorithmically, imports should be partitioned by `ModuleName` before doing pairwise checks.

1. (Import list redundancy.) For every `RdrName` in scope in the program, find all the import-items that brought it into scope, as before. Filter out all import-items which brought this `RdrName` into scope \*implicitly\* (i.e., consider only those which came from import lists.) If there is more than one resulting import-item which explicitly provides this `RdrName`, bleat about the \*later\* import item as being redundant. It is modestly better to collect these altogether and warn at the end, so that if both x and y of `import A(x, y)` are redundant, we can tell the user to remove the entire import.
