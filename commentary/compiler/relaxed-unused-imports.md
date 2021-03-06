# Relaxed unused imports


See also #10117.

[Unused imports](commentary/compiler/unused-imports) describes how the current unused imports warning works.  An import is unused if it can be deleted without changing the meaning of the program; currently, GHC strives to report all such imports (although it doesn't guarantee that it will do so).


In this proposal, we identify some unused imports for which we should NOT report a warning.  The underlying motivation is that, assuming a module M compiles without unused import warnings, if one of the modules it imports adds an extra export, then M should continue to compile without warnings.  (This is equivalent to stating that a minor version upgrade under the Haskell PVP should not trigger new warnings.)


This is achieved is as follows: an import is unused if it can be deleted without changing the meaning of the program, EXCEPT if the import explicitly brings `x` into scope (e.g., `import A (x)`), and the only other ways `x` was brought into scope are implicit (e.g., `import B`), AND each such implicit import is of a different module (e.g., `import A (x)` is still redundant if we `import A`).

## Motivation

[Snoyman reports](http://www.yesodweb.com/blog/2016/05/are-unused-import-warnings-harmful) (with [ reddit discussion](https://www.reddit.com/r/haskell/comments/4jvtmh/are_unused_import_warnings_harmful/)) a common CPP anti-pattern in cross-GHC code:

```wiki
#if MIN_VERSION_base(4,8,0)
import Control.Applicative ((<*))
#else
import Control.Applicative ((<*), pure)
#endif
```


The reason the CPP is necessary is because in base-4.8, an export of `pure` was added to `Prelude`; this means that `Control.Applicative`'s import of `pure` is redundant.


Although the import is technically redundant, it is extremely inconvenient, because a minor, backwards-compatible change to a package caused a redundant import warning to surface. In order for this code to be warning-free on the newer version of base, we must omit `pure` from `Control.Applicative`; however, the older version of base will not compile without this error! Instead, we would like `import Control.Applicative ((<*), pure)` to NOT report any warnings, even if `Prelude` starts exporting `pure`.


See also #10117 for a problem with a common workaround for this problem.

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

1. **Partition the import-items for a `RdrName` by their `Module` (e.g. `A` of `import A`) and whether or not the explicitly and implicitly provide the `RdrName` (`import Prelude` implicitly provides `Just`; `import Prelude (Just)` and `import Prelude (Maybe(..))` explicitly provides `Just`.).  For each `Module`, mark one implicit import-item (if it exists) as used. Among the explicit imports of a `RdrName` whose `Module`s had no implicit import-items associated with them, pick one and mark it as used.**

1.  Now bleat about any import-items that are unused.  For a decl
  `import Foo(x,y)`, if both the `x` and `y` items are unused, it'd be better
  to bleat about the entire decl rather than the individual items.


Step (2) is nondeterministic in two ways: the selection of the implicit import-item (per `Module`), and the selection of the explicit import item (across all `Module`s.)  We need some total order over import-items to let us decide which to pick: we just prefer the textually first.


The addition is a bit involved, but essentially we want to compute some set of import-items to mark as used.  Here are some important motivating examples:

**For each module, mark one implicit import-item as used.**

```wiki
-- Prelude and Control.Applicative export pure

-- We want no errors here; thus, we need to mark both Prelude
-- and Control.Applicative as used. These are different modules,
-- so we mark each of their implicit imports as used.
import Prelude
import Control.Applicative
bar = pure

-- We want to report a warning here; for any module (in this case
-- Control.Applicative) we only mark one import-item as used.
import Control.Applicative
import Control.Applicative
bar = pure
```

**Among the explicit imports whose `Module`s had no implicit import-items associated with them, pick one and mark it as used.**

```wiki
-- We want to report the second import as redundant.  Thus, because
-- M has an implicit "import M", we do not mark the second import as
-- used.
import M
import M (x)

-- We don't want to report any warning here. We mark Control.Applicative
-- as used because there is no "import Control.Applicative"
import Prelude
import Control.Applicative (pure)

-- We want to report one import as redundant.  Thus, among the eligible
-- explicit import-items (across module names), we only mark one as used.
import M (x)
import N (x)
```

**`import Prelude (Maybe(..))` is an explicit import of `Just`.**

```wiki
-- Prelude and Data.Maybe export Maybe(Just, Nothing)

-- We want to report a warning here; since Maybe(..) is an explicit
-- import we only pick one import item to mark as used.
import Prelude (Maybe(..))
import Data.Maybe (Just)
bar = Just
```

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
    -- using 'when' (exported by Control.Monad and .State),
    -- 'State' and 'Reader'
```


Under this proposal, we get the following behavior:

- X0: `import Foo ( x )` redundant (there is an implicit import of Foo).
- X1: `import Foo ( x )` redundant (there is an implicit import of Foo).
- X2: `y of import Foo ( x, y )` redundant, and `import Foo ( x )` redundant (in both cases, we only picked one explicit import to mark as used).
- X3: `import Foo ( x )` redundant (ditto).
- X4: Second `import Foo ( x, y )` redundant (ditto).
- X5: `import Foo ( x, y )` redundant (only first import's y can be marked as used by `Bar.y` name).
- X6: `x` in `import Foo( x, y ) as Bar` and `y` in `import Foo( x, y )` redundant as they were not marked used
- X7: `import FooPlus(z,x)` and `y` in second import redundant
- X8: NOTHING marked redundant! (Use of `when` marks both `Control.Monad` and `Control.Monad.State` as used)
