# Note about this page

*Apparently, this page is out of date and the issue has been settled in favour of the syntax:*

```wiki
import "somepackage" Some.Module
```

See also:

[https://downloads.haskell.org/\~ghc/latest/docs/html/users_guide/glasgow_exts.html\#ghc-flag--XPackageImports](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#ghc-flag--XPackageImports)

# Explicit package imports


This proposal is one possibility for addressing the question of identifying which package is meant in an import declaration.  For the context, read the [GHC packages summary page](commentary/packages/ghc-packages-proposal) first.


The main idea of this proposal is to allow the programmer to specify the source package in the import line, something like this:

```wiki
  import A.B.C from "base" ( map, filter )
```


That would presumably get the most recent installed incarnation of the `base` package. If you want a particular version of the package, we could allow

```wiki
  import A.B.C from "base-3.4" ( map, filter )
```


The exact syntax is unimportant. The important thing is that the programmer can specify the package in the source text.  Note that this fundamentally conflicts with the second assumption we started with.  We were trying to avoid specifying "provenance" at the same time as "purpose", on the grounds that we wanted to avoid editing lots of source text when the provenance changed.  (And so it begs the question, if we need to edit the source anyway, why separate the syntax of packages from modules at all?)


If we adopt the idea that an import statement can specify the source package, several design choices arise:

## Is the 'from \<package\>' compulsory?


If you want to import A.B.C, a module exported by package "foo", can you say just `import A.B.C`, or must you say `import A.B.C from "foo"`?


We think of this as rather like the question "If you import f from module M, can you refer to it as plain "f", or must you refer to it as "M.f"?  The answer in Haskell 98 is that you can refer to it as plain "f" so long as plain "f" is umambiguous; otherwise you can use a qualified reference "M.f" to disambiguate.


We propose to adopt the same principle for imports. That is, an import with no package specified, such as "`import A.B.C`", means: 

- Find all modules A.B.C exported by all exposed packages, or the package or program being compiled. If there is exactly one such module, that's the one to import. Otherwise report "ambiguous import".


If the reference to A.B.C is ambiguous, you can qualify the import by adding "`from "foo"`".

## Package versions


We probably want some special treatment for multiple versions of the same package.  What if you have both "foo-3.9" and "foo-4.0" installed, both exporting A.B.C?  This is jolly useful when you want to install new packages, but keep old ones around so you can try your program with the older one.  So we propose that this is not regarded as ambiguous: importing A.B.C gets the latest version, unless some compiler flag (-hide-package) takes it of the running.


In short, an installed package can be of two kinds:

- **Exposed**: the package's modules populate the global module namespace, and can be imported without mentioning the pacckage name explicitly (`import A.B.C`).  Explicit "from" imports may be used to resolve ambiguity.
- **Available**, but not exposed: the package can be used only by an explicit "from" import.  This is rather like "`import qualified M`, except at the package level.  


Typically, if multiple versions of the same package are installed, then all will be available, but only one will be exposed.


GHC's command-line flags (`-hide-package`, `-package`) can be used to manipulate which packages are exposed, but typically an entire package or program will be compiled with a single set of such flags.  GHC does not curretly support in-module control, thus ` {-# OPTIONS_GHC -hide-package foo #-} `, and we do not propose to change that.


Simon suggested that an installed package might be hidden (so that it cannot be used at all) but I'm not sure why we need that.

## Importing from the home package


If A.B.C is in the package being compiled (which we call "the home package"), and in an exposed package, and you say `import A.B.C`, do you get an "ambiguous import" error , or does the current package override.  And if the former, how can you say "import A.B.C from the current package"?  


One possibility is to reuqire the code to know its own package name, and mention that in the import. For exmaple, in a module that is being compiled as part package "foo", you'd say `import A.B.C from "foo"`.  What about modules that are part of the main program (not a package at all).  Perhaps you could then say `import A.B.C from "main"`.


Another way is to have a special package name meaning "the home package".  The special name could be

- ""
- "home"
- "this"
- this (with no quotes)

## The 'as P' alias


We propose to maintain the local, within-module "as P" alias mechanism unchanged.  Thus:

```wiki
   import A.B.C( T ) from "foo" as M
   type S = M.T -> M.T
```


Here, the qualified name "M.T" refers to the T imported from A.B.C in package "foo".

## Qualified names


We propose that the default qualified name of an entity within a module is just the module name plus the entity name.  Thus

```wiki
  import A.B.C( T ) from "foo" 
  type S = A.B.C.T -> A.B.C.T
```


If you want to import multiple A.B.C's (from different packages) then perhaps they define different entities, in which case there is no problem:

```wiki
  import A.B.C( T1 ) from "foo" 
  import A.B.C( T2 ) from "bar" 
  type S = A.B.C.T1 -> A.B.C.T2
```


But if they both export entities with the same name, there is no alternative to using the 'as M' mechanism:

```wiki
  import A.B.C( T ) from "foo" as M1
  import A.B.C( T ) from "bar" as M2
  type S = M1.T -> M2.T
```

## Exporting modules from other packages


It is perfectly OK to export entities, or whole modules, imported from other packages:

```wiki
  module M( f, g, module Q ) where
  import A.B( f, g ) from "foo"
  import X.Y.Z from "bar" as Q
```

## Syntax


Should package names be in quotes?  Probably yes, because they have a different lexcal syntax to the rest of Haskell.  ("foo-2.3" would parse as three tokens, "foo", "-",  and "2.3".  


It's been suggested that one might want to import several modules from one package in one go:

```wiki
    from "base" import
        Prelude hiding (length)
        Control.Exception
        qualified Data.List as List
```


What we don't like about that is that it needs a new keyword "`from`".  Perhaps all imports can start with the keyword `import`, and then we are free to use extra (context-specific) keywords.  (Haskell already has several of these, such as `hiding`.  Something like this:

```wiki
    import from "base" {
        Prelude hiding (length) ;
        Control.Exception ;
        qualified Data.List as List }
    import from "foo" M( x, y )
```


Here the layout is explicit, but the braces and semicolons could be avoided by making use of the layout rule as usual.


Indeed, we could allow this multiple form even for ordinary imports:

```wiki
   import { A(f); B(g); C(S,T) }
```


It is clear from the above examples that the keyword `from` is redundant - the presence of a string literal (or special keyword to denote the home package) after the keyword `import` is sufficient to distinguish per-package imports from the ordinary shared-namespace imports, so the above could instead be written as

```wiki
    import "base"
        Prelude hiding (length)
        Control.Exception
        qualified Data.List as List
    import "foo" M( x, y )

    import
           A(f)
           B(g)
           C(S,T)
```

### Syntax formalised and summarised


A possible syntax which covers everything in this proposal is therefore:


>
>
> **import** \[*package-name*\] **{** *import-specifier* \[**;** *import-specifier*\] **}**
>
>


where *package-name* is a string literal or the keyword `home`, the *import-specifier* corresponds to everything that is currently allowed after the keyword `import`, and the braces and semicolons would be added by the layout rule.

```wiki
    import "base" -- searches in "base" package only
        Prelude hiding (length)
        Control.Exception
        qualified Data.List as List

    import ""   -- searches in home package only
        A.B.C

    import P.Q.R -- searches in home + exposed packages
```
