# Records in Haskell


This [ Yesod blog post](http://www.yesodweb.com/blog/2011/09/limitations-of-haskell), and accompanying [ Reddit discussion](http://www.reddit.com/r/haskell/comments/k4lc4/yesod_the_limitations_of_haskell/) brought to the surface again the thorny issue of records in Haskell.


There are two rather different sets of issues:

- The narrow issue: **namespacing for record field names**. Currently in Haskell two records in the same module can't share a field name.  This is sometimes extremely painful.  This page is about the narrow issue.

- The broad issue: **first class record types**.  In Haskell there is no "record type" per se. Rather, you can simply give names to the fields of a constructor.  Records are not extensible and there is no polymorphism on records. 


This page focuses exclusively on the first, narrow issue of disambiguating record field names.  We have a separate Wiki page, [ExtensibleRecords](extensible-records), on the broad issue of first class record types.


On this page I'd like to summarise the problem, and specify alternative designs.  So far it is mostly a skeleton: please fill it out.  The idea is to hold a discussion by email (ghc-users?) but to collect results (alternative designs, trade-offs, pros and cons) here, because email threads quickly get lost.  Simon PJ.

## The problem: record name spacing


(Quoting the Yesod blog.)  Consider

```wiki
data Record = Record { a :: String }
data RecordClash = RecordClash { a :: String }
```


Compiling this file results in:

```wiki
record.hs:2:34:
    Multiple declarations of `Main.a'
    Declared at: record.hs:1:24
                 record.hs:2:34
```


In the Persistent data store library, Yesod works around the issue by having the standard of prefixing every record field with the record name (`recordA` and `recordClashA`). But besides being extremely verbose, it also limits us from experimenting with more advanced features like a partial record projection or an unsaved and saved record type.


The verbose name-spacing required is an in-your-face, glaring weakness telling you there is something wrong with Haskell. This issue has been solved in almost every modern programming languages, and there are plenty of possible solutions available to Haskell.

## Solutions


So we have decided to avoid the extensible record debate, but how can we have multiple record field selectors in scope and correctly resolve the type of the record?

1. Overloading: polymorphic selection & update; see [Records/OverloadedRecordFields](records/overloaded-record-fields)
1. Namespacing: simple name-spacing & type resolution; see below

**Are there any other approaches?**


The discussion has many similarities with the original Type directed name resolution proposal: the question seems to be largely about nailing down a concrete implementation; see below and [ TDNR](http://hackage.haskell.org/trac/haskell-prime/wiki/TypeDirectedNameResolution). The original TDNR proposal had Overloading in mind, but Namespacing ends up having similarities.


The benefit of Overloading over Namespacing is being able to write code that works against any Record with a given field. So I can have a function:

```wiki
getA = r.a
```


and that can work for both Record and RecordClash because they both have a field a.
With Namespacing this will fail to type check unless the compiler can determine the type of r. The advantage of Namespacing is that the implementation is clear, straightforward, and has already been done (whereas there are still questions as to the feasibility of Overloading). Namespacing also has other benefits related to namespacing that are not as directly related to solving the records issue.
In the words of the Frege author, who abandoned Overloading:

- only very inefficient code could be generated, if you have to access or update a field of some unknown record. In the end, every record type was basically a map.
- it turned out that errors stemming from mistyping a field name often could not be diagnosed at the point where they were committed, but led to inferred types with crazy signatures and an incomprehensible type error at the use side of the function that contained the error.
- the extra constraints complicated the type checker and did not play well with higher kinded type variables (at least in the code I had then, I do not claim that this is nessecarily so).

### Better name spacing & simple type resolution


Note that the name-spacing and simple type resolution approach is an attempt to port the records solution in [ Frege](http://code.google.com/p/frege/), a haskell-like language on the JVM. See Sections 3.2 (primary expressions) and 4.2.1 (Algebraic Data type Declaration - Constructors with labeled fields) of the [ Frege user manual](http://code.google.com/p/frege/downloads/detail?name=Language-202.pdf)


In Haskell, you can look at an occurrence of any identifier `f` or `M.f` and decide where it is bound without thinking about types at all.  Broadly speaking it works like this:

- For qualified names, `M.f`, find an import that binds `M.f`.
- For unqualified names, `f`, find the innermost binding of `f`; or, if that takes you to top level, look for top level binding of `f` or an import that binds `f`.


If there is ambiguity (eg two imports both import something called `f`) then an error is reported.  And that's what happens for the `Record` and `RecordClash` example above.


So one solution for record field names is to specify more precisely which one you mean.  There are two schools of thought:

- **Optionally use the type name**.  So you could say `Record.a` or `RecordClash.a` rather than `a`, to specify which field selector you mean.  Apart from verbosity the difficulty here is that it's hard to know whether you are writing `<module-name>.f` or `<type-name>.f`.  That is, is `Record` the name of a type or of a module?  (Currently it legally could be both.)

>
> The module/record ambiguity is dealt with in Frege by preferring modules and requiring a module prefix for the record if there is ambiguity. So if your record named Record was inside a module named Record you would need `Record.Record.a`. Programmers will avoid this by doing what they do now: structuring their programs to avoid this situation. We can try and give the greater assistance in this regard by providing simpler ways for them to alter the names of import types.

>
> Verbosity is solved in Frege by using the TDNR concept. In `data Record = Record {a::String};r = Record "A"; r.a` The final `r.a` resolves to `Record.a r`. See the simple type resolution discussion below.

- **Use the module name space mechanism**; after all that's what it's for.  But putting each record definition in its own module is a bit heavyweight. So maybe we need local modules (just for name space control) and local import declarations.  Details are unclear. (This was proposed in 2008 in [ this discussion](http://www.haskell.org/pipermail/haskell-cafe/2008-August/046494.html) on the Haskell cafe mailing list and in [\#2551](https://gitlab.haskell.org//ghc/ghc/issues/2551). - Yitz).

>
> Rather than strictly re-use modules it may make more sense to have a name-spacing implementation construct that is shared between both records and modules - hopefully this would make implementation easier and unify behavior. In the Frege approach, each data declaration is its own namespace - if we were to go this far (instead of stopping purely at records) there may be much less need for local namespaces. Overall this seems to be more of an implementation detail that may have a side effect of making local modules easier to implement than a concrete design proposal relating to records. -- Greg Weber.

### Simple type resolution

>
> Frege has a detailed explanation of the semantics of its record implementation, and the language is \*very\* similar to Haskell. After reading the Frege manual sections, one is still left wondering: how does Frege implement type resolution for its TDNR syntax. The answer is fairly simple: overloaded record fields are not allowed (you can't write code that works against multiple record types). Frege uses fairly normal type resolution, and it won't always be able to resolve the type (currently 1/3 or sparsely annotated code will need more type annotations, but we should be able to improve this). I will put down some more details here...

### Type directed name resolution


All of the name-space mechanisms require some level of user-supplied disambiguation: if there are two fields `a` in scope, you must use a qualified name to disambiguate them.  What is tantalising about this is that the *type* of the argument immediately specifies which one you mean. There is really no ambiguity at all, so it is frustrating to have to type qualified names to redundantly specify that information.  Object-oriented languages take for granted this form of type-directed disambiguation.


One particular way of integrating this idea into Haskell is called [ Type Directed Name Resolution](http://hackage.haskell.org/trac/haskell-prime/wiki/TypeDirectedNameResolution) (TDNR). Proposed a couple of years ago, the Haskell community didn't like it much.  (But I still do; SLPJ.)


I believe the community rejected TDNR because they wanted extensible records. I think it is a shame that the desire for \*extensible\* records is holding us back from getting anything done now, but I do think that the current TDNR proposal seems a little weak for some reasons pointed out in the proposal itself, but also because it proposes not to solve name-spacing record updates. Note that the Frege proposal incorporates the TDNR syntax, and it tackles record updates. -- Greg Weber
