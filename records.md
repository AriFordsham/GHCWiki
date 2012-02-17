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


So we have decided to avoid the extensible record debate, but how can we have multiple record field selectors in scope and correctly resolve the type of the record?  There are two main mechanisms on offer:

- **Plan A**: Name spacing.  This uses qualified names to disambiguate record field names.
- **Plan B**: Types.  This uses types to disambiguage record field names.

1. **[Simple Overloaded Record Fields](records/overloaded-record-fields) (SORF)**.  Pure (Plan B).
1. **[ Type Directed Name Resolution](http://hackage.haskell.org/trac/haskell-prime/wiki/TypeDirectedNameResolution) (TDNR)**.  Pure (Plan B), but without abstraction over fields of the same name.
1. **[Agda-derived Records](records/name-spacing) (ADR)** Pure (Plan A)
1. **[Frege-derived Records](records/name-spacing) (FDR)**.  Uses (Plan A) + (Plan B).
1. **[Declared Overloaded Record Fields](records/declared-overloaded-record-fields) (DORF)**. Tweak to SORF. (Plan B)

1. **Are there any other approaches?**

### Similarities


All records solutions are planning on using the dot operator for normal record field selection. We need to consider the [future usage of the dot, particularly as a function composition operator](records/dot-operator).
(DORF doesn't insist on dot notation: it's to be syntactic sugar for reverse function application.)

### Comparisons


The benefit of abstracting over field names in Overloading is being able to write code that works against any Record with a given field. So I can have a function:

```wiki
getA = r.a
```


and that can work for both `Record` and `RecordClash` if they are defined in the same module because they both have a field `a`.
With other approaches (including TDNR) this will fail to type check unless the compiler can determine the type of r is either `Record` or `RecordClash`. Note that we already can accomplish this on an opt-in basis with Type Classes: making this automatic is not required and could give the unwary user weakly-typed code.


The advantage of Namespacing is that the implementation is clear, straightforward, and has already been done in Agda and Frege. We can either stop with name-spacing (Agda) or continue on with automatically resolving the field when the dot operator is used. Overloading has seen downsides in practice. In the words of the Frege author, who abandoned Overloading:

- only very inefficient code could be generated, if you have to access or update a field of some unknown record. In the end, every record type was basically a map.
- it turned out that errors stemming from mistyping a field name often could not be diagnosed at the point where they were committed, but led to inferred types with crazy signatures and an incomprehensible type error at the use side of the function that contained the error.
- the extra constraints complicated the type checker and did not play well with higher kinded type variables (at least in the code I had then, I do not claim that this is nessecarily so).

### Type directed name resolution


The discussion has many similarities with the original Type directed name resolution proposal: the question seems to be largely about nailing down a concrete implementation. The original TDNR proposal had Overloading in mind, but Namespacing ends up having similarities. -- Greg Weber


All of the name-space mechanisms require some level of user-supplied disambiguation: if there are two fields `a` in scope, you must use a qualified name to disambiguate them.  What is tantalising about this is that the *type* of the argument immediately specifies which one you mean. There is really no ambiguity at all, so it is frustrating to have to type qualified names to redundantly specify that information.  Object-oriented languages take for granted this form of type-directed disambiguation.


One particular way of integrating this idea into Haskell is called  (TDNR). Proposed a couple of years ago, the Haskell community didn't like it much.  (But I still do; SLPJ.)
