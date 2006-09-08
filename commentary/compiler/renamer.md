
\[ Up: [Commentary/Compiler/HscMain](commentary/compiler/hsc-main) \]

# The renamer


The renamer's Number One taks is to replace [RdrNames](commentary/compiler/rdr-name-type) with [Names](commentary/compiler/name-type).  For example, consider

```wiki
module K where
  f x = True

module N where
  import K

module M where
  import N( f ) as Q
  f = (f, M.f, Q.f, \f -> f)
```

>
> (where all the variables are `RdrName`s).  The result of renaming module M is:
>
> ```wiki
> M.f = (M.f, M.f, K.f, \f_22 -> f_22)
> ```


(where all these names are now `Name`s).

- The top-level unqualifed `RdrName` "`f`" has become the `External``Name``M.f`.  
- The occurrences "`f`" and "`M.f`" are both bound to this `Name`.  
- The qualified `RdrName` "`Q.f`" becomes the `Name``K.f`, because the function is defined in module K.  
- The lambda-bound "`f`" becomes an `Internal` name, here written `f_22`.  


(All the `External` names have uniques too, but we often do not print them.)


In addition, the renamer does the following things:

- Sort out fixities. The parser parses all infix applications as **right-associative**, regardless of fixity.  For example "`a * b + c`" is parsed as "`a * (b + c)`".  The renamer re-associates such nested operator applications, using the fixities declared in the module.

- Dependency analysis for mutually-recursive groups of declarations.  This divides the declarations into strongly-connected components.

- Lots of lexical error checking: variables out of scope, unused bindings, unused imports, patterns that use the same binder many times, etc.
