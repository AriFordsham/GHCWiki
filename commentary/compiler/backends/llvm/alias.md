# Improving LLVM Alias Analysis


This page tracks the information and progress relevant to improving the alias analysis pass for the LLVM backend of GHC.

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
```


The fact that `R[]` never aliases with `Sp[]` is never used as the one way relation isn't expressible in LLVM.


Stores/loads needs to be annotated with `!tbaa` and one of the above three types e.g.

```wiki
%ln1NH1 = load i64* %Sp_Arg, align 8, !tbaa !2
```

**Question** (Johan Tibell): Which instructions need to be instrumented? Just loads and stores, or also getelementptr?

**Question** (Johan Tibell): Should all loads and stores that are not annotated as "stack" be allocated as "heap", or is there a third class (e.g. unknown)? 

## Progress


David and Johan plan to have a crack at this at the start of 2012.
