# Records in Haskell


This [ Yesod blog post](http://www.yesodweb.com/blog/2011/09/limitations-of-haskell), and accompanying [ Reddit discussion](http://www.reddit.com/r/haskell/comments/k4lc4/yesod_the_limitations_of_haskell/) brought to the surface again the thorny issue of records in Haskell.


There are two rather different sets of issues:

- The narrow issue: **namespacing for record field names**. Currently in Haskell two records in the same module can't share a field name.  This is sometimes extremely painful.  This page is about the narrow issue.

- The broad issue: **first class record types**.  In Haskell there is no "record type" per se. Rather, you can simply give names to the fields of a constructor.  Records are not extensible and here is no polymorphism on records. 


This page focues exclusively on the first, narrow issue of dismbiguating record field names.  We have a separate Wiki page, [ExtensibleRecords](extensible-records), on the broad issue of first class record types.


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


I know of two sorts of solutions:

1. Better name spacing
1. Type directed name resolution

**Are there any approaches?**

---

### Better name spacing


In Haskell, you can look at an occurrence of any identifier `f` or `M.f` and decide where it is bound without thinking about types at all.  Broadly speaking it works like this:

- For qualified names, `M.f`, find an import that binds `M.f`.
- For unqualified names, `f`, find the innermost binding of `f`; or, if that takes you to top level, look for top level binging of `f` or an import that binds `f`.


If there is ambiguity (eg two imports both import something called `f`) then an error is reported.  And that's what happens for the `Record` and `RecordClash` example above.


So one solution for record field names is to specify more precisely which one you mean.  There are two schools of thought:

- Optionally use the type name.  So you could say `Record.a` or `RecordClash.a` rather than `a`, to specify which field selector you mean.  Apart from verbosity the difficulty here is that it's hard to know whether you are writing `<module-name>.f` or `<type-name>.f`.  That is, is `Record` the name of a type or of a module?  (Currently it legally could be both.)

- Use the module name space mechanism; after all that's what it's for.  But putting each record definition in its own module is a bit heavyweight. So maybe we need local modules (just for name space control) and local import declarations.  Details are unclear.

**Anyone who likes these designs, please fill out a detailed design, either here or on another page**.

---

### Type directed name resolution


All of the name-space mechanisms require some level of user-supplied disambiguation: if there are two fields `a` in scope, you must use a qualified name to disambiguate them.  What is tantalising about this is that the *type* of the argument immediately specifies which one you mean. There is really no ambiguity at all, so it is frustrating to have to type qualified names to redundantly specify that information.  Object-oriented languages take for granted this form of type-directed disambiguation.


One particular way of integrating this idea into Haskell is called [ Type Directed Name Resolution](http://hackage.haskell.org/trac/haskell-prime/wiki/TypeDirectedNameResolution) (TDNR).  Proposed a couple of years ago, the Haskell community didn't like it much.  (But I still do; SLPJ.)
