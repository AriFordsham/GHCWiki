# The renamer


The renamer's Number One task is to replace [RdrNames](commentary/compiler/rdr-name-type) with [Names](commentary/compiler/name-type).  For example, consider

```haskell
module K where
  f x = True

module N where
  import K

module M where
  import N( f ) as Q
  f = (f, M.f, Q.f, \f -> f)
```


(where all the variables are `RdrName`s).  The result of renaming module M is:

```haskell
M.f = (M.f, M.f, K.f, \f_22 -> f_22)
```


where all these names are now `Name`s.


- The top-level unqualifed `RdrName` "`f`" has become the `External` `Name` `M.f`.  
- The occurrences "`f`" and "`M.f`" are both bound to this `Name`.  
- The qualified `RdrName` "`Q.f`" becomes the `Name` `K.f`, because the function is defined in module K.  
- The lambda-bound "`f`" becomes an `Internal` name, here written `f_22`.  (All the `External` names have uniques too, but we often do not print them.)


In addition, the renamer does the following things:

- Sort out fixities. The parser parses all infix applications as **left-associative**, regardless of fixity.  For example "`a + b * c`" is parsed as "`(a + b) * c`".  The renamer re-associates such nested operator applications, using the fixities declared in the module.

- Dependency analysis for mutually-recursive groups of declarations.  This divides the declarations into strongly-connected components.

- Lots of lexical error checking: variables out of scope, unused bindings, unused imports, patterns that use the same binder many times, etc.


The renamer sits between the parser and the typechecker. However, its operation is quite tightly interwoven with the typechecker. This is mainly due to support for Template Haskell, where spliced code has to be renamed and type checked. In particular, top-level splices lead to multiple rounds of renaming and type checking.  It uses the [same monad as the typechecker](commentary/compiler/tc-rn-monad).

## The global renamer environment, `GlobalRdrEnv`


A big part of the renamer's task is to build the **global rdr-env** for the module, of type `GlobalRdrEnv`.  This environment allows us to take a qualified or un-qualified `RdrName` and figure out which `Name` it means.  The global rdr-env is built by looking at all the imports, and the top-level declarations of the module.


You might think that the global rdr-env would be a mapping from `RdrName` to `Name`, but it isn't.  Here is what it looks like, after at least three iterations (all in [compiler/GHC/Types/Name/Reader.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Types/Name/Reader.hs)):

```haskell
type GlobalRdrEnv = OccEnv [GlobalRdrElt]
  -- An (OccEnv a) is a mapping from OccName to a

data GlobalRdrElt = GRE { gre_name :: Name
                        , gre_prov :: Provenance
                        , gre_par :: Parent }

data Provenance = LocalDef | Imported [ImportSpec]

data ImportSpec = ImpSpec { is_decl :: ImpDeclSpec, is_item ::  ImpItemSpec }

data Parent = NoParent | ParentIs Name
```


Here is how to understand these types:

- The environment (`GlobalRdrEnv`) maps an `OccName` to a list of all entities with that occurrence name that are in scope (in any way).  

- Each of these is represented by a `GlobalRdrElt`, which gives the entity's `Name` plus a specification of how it is in scope, its `Provenance`.  

- The `Provenance` has one of two forms.  Either it is in scope because it is defined in this module (`LocalDef`), or because it is imported.  In the latter case, the `[ImportSpec]` describes all the import statements that bring it into scope. 

- An `ImportSpec` has two components: 

  - An `ImpDeclSpec` that describes the entire import declaration. This is shared between all entities brought into scope by a particular import declaration.
  - An `ImpItemSpec` that describes the import item that brought the entity into scope.

  For example, given

  ```haskell
  import qualified M( x, T(g) ) as Q
  ```
  the `ImpDeclSpec` would describe the `qualified` and `as` part, while the `ImpItemSpec` describes the `T(g)` part.  You can look in `RdrName.hs` to see what an `ImportDeclSpec` and `ImpItemSpec` are like!

- The `Parent` of an entity is the `Name` under which it is grouped when the forms `T(..)` or `T(C,D)` are used in an export or import list.  In the `T(..)` form, all the things whose `Parent` is `T` are chosen.  In the `T(C,D)` form, it is required that `C` and `D` have `T` as parents.  
  For example, 

  - The `Parent` of a data constructor is its data type
  - The `Parent` of a record field selector is its data type
  - The `Parent` of a class operation is its class


With all that information, we can give good error messages, especially in the case where an occurrence "f" is ambiguous (i.e. different entities, both called "f", were imported by different import statements).


The global rdr-env is created by [compiler/GHC/Rename/Names.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Rename/Names.hs).


It is important to note that the global rdr-env is created  *before* the renamer actually descends into the top-level bindings of a module. In other words, before `GHC.Tc.Module.rnTopSrcDecls` performs the renaming of a module by way of `GHC.Rename.Module.rnSrcDecls`, it uses `GHC.Rename.Names.importsFromLocalDecls` to set up the global rdr-env environment, which contains `Names` for all imported and all locally defined toplevel binders.  Hence, when the helpers of `rnSrcDecls` come across the defining occurences of a toplevel `RdrName`, they don't rename it by generating a new name, but they simply look up its name in the global rdr-env.

## Unused imports


See [how the renamer reports unused imports](commentary/compiler/unused-imports)

## Name Space Management


(too much detail?)


As anticipated by the variants `Orig` and `Exact` of `RdrName`, some names should not change during renaming, whereas others need to be turned into unique names. In this context, the two functions `GHC.Rename.Env.newTopSrcBinder` and `GHC.Rename.Utils.newLocalBndrRn` are important:

```haskell
newTopSrcBinder :: Module -> Maybe Name -> Located RdrName -> RnM Name
newLocalBndrRn :: Located RdrName -> RnM Name
```


The two functions introduces new toplevel and new local names, respectively, where the first two arguments to newTopSrcBinder determine the currently compiled module and the parent construct of the newly defined name. Both functions create new names only for [RdrNames](commentary/compiler/rdr-name-type) that are neither exact nor original. 

## Rebindable syntax


(ToDo: Not fully proof-read.)


In Haskell when one writes "3" one gets "fromInteger 3", where "fromInteger" comes from the Prelude (regardless of whether the Prelude is in scope). If you want to completely redefine numbers, that becomes inconvenient. So GHC lets you say "-fno-implicit-prelude"; in that case, the "fromInteger" comes from whatever is in scope. (This is documented in the User Guide.) 


This feature is implemented as follows (I always forget). 

- Names that are implicitly bound by the Prelude, are marked by the type `GHC.Hs.Expr.SyntaxExpr`. Moreover, the association list `GHC.Hs.Expr.SyntaxTable` is set up by the renamer to map rebindable names to the value they are bound to.
- Currently, five constructs related to numerals (`GHC.Hs.Expr.NegApp`, `GHC.Hs.Pat.NPat`, `GHC.Hs.Pat.NPlusKPat`, `GHC.Hs.Lit.HsIntegral`, and `GHC.Hs.Lit.HsFractional`) and two constructs related to do-expressions (`GHC.Hs.Expr.BindStmt` and `GHC.Hs.Expr.ExprStmt`) have rebindable syntax.
- When the parser builds these constructs, it puts in the built-in Prelude Name (e.g. `PrelNum.fromInteger`). 
- When the renamer encounters these constructs, it calls `GHC.Rename.Env.lookupSyntaxName`. This checks for `-fno-implicit-prelude`; if not, it just returns the same Name; otherwise it takes the occurrence name of the Name, turns it into an unqualified `RdrName`, and looks it up in the environment. The returned name is plugged back into the construct.
- The typechecker uses the `Name` to generate the appropriate typing constraints. 
