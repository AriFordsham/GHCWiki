# Improving LLVM Alias Analysis


This page tracks the information and progress relevant to improving the alias analysis pass for the LLVM backend of GHC.


This correspond to bug [\#5567](https://gitlab.haskell.org//ghc/ghc/issues/5567).

## LLVM Alias Analysis Infrastructure


Some links to the various documentation on LLVM's AA support:

- [ LLVM Alias Analysis Infrastructure](http://llvm.org/docs/AliasAnalysis.html)
- [ LLVM's Analysis and Transform Passes](http://llvm.org/docs/Passes.html)
- [ The Often Misunderstood GEP Instruction](http://llvm.org/docs/GetElementPtr.html)
- [ LLVM Language Reference](http://llvm.org/docs/LangRef.html)
- [ LLVM Dev List: Comparison of Alias Analysis in LLVM](http://groups.google.com/group/llvm-dev/browse_thread/thread/2a5944692508bcc2/363c96bb1c6a506d?show_docid=363c96bb1c6a506d&pli=1)

## Max's Work


Max had a crack at writing a custom alias analysis pass for LLVM, relevant links are:

- [ Email to LLVM dev](http://lists.cs.uiuc.edu/pipermail/llvmdev/2011-September/043603.html)
- [ Blog post about results](http://blog.omega-prime.co.uk/?p=135)

## TBAA


LLVM as of version 2.9 includes Type Based Alias Analysis. This mean using metadata you can specify a type hierarchy (with alias properties between types) and annotate your code with these types to improve the alias information. This should allow us to improve the alias analysis without any changes to LLVM itself like Max made.

- [ LLVM TBBA Doc](http://llvm.org/docs/LangRef.html#tbaa)

## STG / Cmm Alias Properties

**Question** (David Terei): What alias properties does the codegen obey? Sp and Hp never alias? R\<n\> registers never alias? ....

**Answer** (Simon Marlow): Sp\[\] and Hp\[\] never alias, R\[\] never aliases with Sp\[\], and that's about it.

## LLVM type system


The above aliasing information can be encoded as follows:

```wiki
!0 = metadata !{ metadata !"top" }
!1 = metadata !{ metadata !"heap", metadata !0 }
!2 = metadata !{ metadata !"stack", metadata !0 }
!3 = metadata !{ metadata !"rx", metadata !1 }
!4 = metadata !{ metadata !"base", metadata !0 }
```


The fact that `R[]` never aliases with `Sp[]` is never used as the one way relation isn't expressible in LLVM.


Stores/loads needs to be annotated with `!tbaa` and one of the above four types e.g.

```wiki
%ln1NH1 = load i64* %Sp_Arg, align 8, !tbaa !2
```

## Progress


David and Johan plan to have a crack at this at the start of 2012.
