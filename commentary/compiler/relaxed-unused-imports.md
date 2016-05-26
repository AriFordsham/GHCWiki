# Relaxed unused imports

[Unused imports](commentary/compiler/unused-imports) describes how the current unused imports behavior works. The idea behind previous proposals for unused imports is GHC, at a 90% effort, should report all imports which can be deleted while your code still compiles.  This proposal adopts two new ideas:

1. Some import statements explicitly bring an identifier into scope, e.g., `import A(x)`; while some import statements implicitly bring an identifier into scope, e.g., `import A` or `import A(C(..))`. When we use an identifier in the body of a Haskell program, we consider ALL import items which \*implicitly\* provide that identifier used, plus ONE import item which explicitly provides the identifier.

1. Idea (1) causes us to fail to report warnings for some obviously redundant imports, but for which redundancy is textually obvious. We define a very simple subsumption relation between imports to find these cases.


We suggest the old import warnings be preserved with a flag which is not turned on automatically by `-Wall`

## Motivation

[ Snoyman reports](http://www.yesodweb.com/blog/2016/05/are-unused-import-warnings-harmful) (with [ reddit discussion](https://www.reddit.com/r/haskell/comments/4jvtmh/are_unused_import_warnings_harmful/)) a common CPP anti-pattern in cross-GHC code:

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
or a particular item in an import list (e.g., `import Foo( ..., x, ...)`). An
imported name can be provided implicitly or explicitly by an import item; specifically
an imported name `x` is explicitly provided if it appears textually in the original
import statement, and implicitly otherwise (i.e., if there was no import list, or
it was brought into scope via ellipses `C(..)`).


Then, for each use of an imported name, we will attribute
that use to **all** import-items which implicitly provide it, AND one import-item
which explicitly provides it. Then, any import items with no
uses attributed to them are unused, and are warned about. I've highlighted
in **bold** the differences from the previous specification.
More precisely:

1.  For every `RdrName` in the program text, find all the import-items that brought it into scope.  The lookup mechanism on `RdrNames` already takes account of whether the `RdrName` was qualified, and which imports have the right qualification etc, so this step is very easy.

1. **Separate these import-items into those which explicitly and implicitly provide the `RdrName`. Mark all import-items which implicitly provide the `RdrName` as used. Choose one explicit import-item and mark it used.**

1.  Now bleat about any import-items that are unused.  For a decl
  `import Foo(x,y)`, if both the `x` and `y` items are unused, it'd be better
  to bleat about the entire decl rather than the individual items.


The import-item choosing step 2 implies that there is a total order on import-items. We say import-item A dominates import-item B if we choose A over B. We always choose the textually first one.

## Textual redundancy


The problem with this specification is that it does not report errors for completely duplicate imports when implicit imports are involved:

```wiki
import A
import A
```


or

```wiki
import A (x,y)
import A
```


We observe that in these cases it is *textually* obvious that there is redundancy. So we can just define a new algorithm to warn in this case.

## Textual redundancy specification


For every import statement, we have the following subsumption relation (read brackets as, the relation holds with and without the bracketed contents):

- `import [ qualified ] A [ (...) | hiding (...) ]` is subsumed by `import A`
- `import [ qualified ] A as B [ (...) | hiding (...) ]` is subsumed by `import A as B`
- `import qualified A [ (...) | hiding (...) ]` is subsumed by `import qualified A`
- `import qualified A as B [ (...) | hiding (...) ]` is subsumed by `import qualified A as B`


For every pair of distinct import declarations such that one subsumes the other, report that the subsumed import is redundant (preferring textually later in the reflexive case.) Algorithmically, imports should be partitioned by `ModuleName` before doing pairwise checks.


(We could possibly also handle `C(..)` and `C(X)` but this seems not worth it for now.)


(Also maybe there is a more elegant way to handle `hiding` clauses)

## Examples


Consider these examples, where Foo exports x and y, and FooPlus re-exports all of Foo, plus z: 

```wiki
  module X0 where            	   module X1 where	
    import Foo	             	     import Foo		
    import Foo( x )          	     import Foo( x )	
    bar = x	             	     bar = x+y		

  module X2 where            	   module X3 where	
    import Foo( x, y )	     	     import Foo( x, y )	
    import Foo( x )	     	     import Foo( x )	
    bar = x		     	     bar = x + y         
 
  module X4 where            	   module X5 where	      
    import Foo( x, y ) 	     	     import Foo( x, y ) as Bar 
    import Foo( x, y )	     	     import Foo( x, y )	      
    bar = x + y		     	     bar = x + Bar.y       
 
  module X6 where                  module X7 where	
    import Foo( x, y ) as Bar	     import FooPlus(x,y)	
    import Foo( x, y ) 		     import FooPlus(y,z)	
    bar = Foo.x + Bar.y		     import FooPlus(z,x)	
				     bar = (x,y,z)       

  module X8
    import Control.Monad
    import Control.Monad.State
    import Control.Monad.Reader
```


Under this proposal, we get the following behavior:

- X0: `import Foo ( x )` redundant by subsumption
- X1: `import Foo ( x )` redundant by subsumption
- X2: `import Foo ( x )` redundant as second `x` was not marked used
- X3: `import Foo ( x )` redundant as second `x` was not marked used
- X4: `import Foo ( x, y )` redundant as second `x` and `y` not marked used
- X5: `import Foo ( x, y )` redundant as second `x` and `y` not marked used
- X6: `x` in `import Foo( x, y ) as Bar` and `y` in `import Foo( x, y )` redundant as they were not marked used
- X7: `import FooPlus(z,x)` and `y` in second import redundant
- X8: NOTHING marked redundant!