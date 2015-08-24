
This pages outlines a new proposed extension to GHC - `-XOpenQualifiedImports` - that allows one to locally add more symbols into the environment scoped over by `let` and `where` bindings.


Under this extension a user can open a previously qualified import (defined at the module level).

**Example**

```wiki
{-# LANGUAGE OpenQualifiedImports #-} 

import qualified Data.Text as T
import qualified Data.Text.IO as T

helloFile :: FilePath -> IO ()
helloFile = do
  contents <- readFile "name"
  return $ pack "Hello, " <> contents
  where open T
```

**New syntax**


A new keyword is added that is only valid in the definition of let/where bindings: `open`. This keyword is similar in functionality to the existing `import` statement, but rather than taking a module name, it takes the name of a qualified import (declared along with other imports at the module level).

*Usage*

```wiki
import qualified M
import qualified M as N

-- later
...
  where
    open M             -- add all symbols
    open M (a, B)      -- add just a and B
    open N hiding (C)  -- add everything except C
```


The `open` statement is valid in either `where` or `let` bindings.

**Behavior**

*Shadowing*


Opening an import should shadow symbols created in enclosing scopes. In the first example, `readFile` unambiguously refers to `Data.Text.IO.readFile`, as the `open` statement shadows any other `readFile` symbols.


Note that if another symbol of the same name is introduced in the same scope as an open statement then that \*is\* ambiguous:

```wiki
let open T (readFile)
     readFile = ...
in readFile ... -- ambiguous
```


or

```wiki
let open T (readFile)
    open Prelude (readFile)
in readFile -- ambiguous
```


This matches the behaviour of import statements and top-level definitions.

*Scope*


When opening a qualified import, the symbols imported are valid to both:

1. the body the bindings scope over (the function owning a `where` statement, or the body of a `let ... in` statement), 
1. the other sibling bindings.


Point 2 is consistent with the behavior of importing in a module and defining top-level definitions - the top-level definitions have access to imported symbols.