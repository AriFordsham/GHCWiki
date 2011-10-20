# Records in Haskell


This [ Yesod blog post](http://www.yesodweb.com/blog/2011/09/limitations-of-haskell), and accompanying [ Reddit discussion](http://www.reddit.com/r/haskell/comments/k4lc4/yesod_the_limitations_of_haskell/) brought to the surface again the thorny issue of records in Haskell.


There are two rather different sets of issues:

- The narrow issue: **namespacing for record field names**. Currently in Haskell two records in the same module can't share a field name.  This is sometimes extremely painful.  This page is about the narrow issue.

- The broad issue: **first class record types**.  In Haskell there is no "record type" per se. Rather, you can simply give names to the fields of a constructor.  Records are not extensible and here is no polymorphism on records. 


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


I know of three sorts of solutions:

1. Better name spacing; see below
1. Type directed name resolution; see below and [ TDNR](http://hackage.haskell.org/trac/haskell-prime/wiki/TypeDirectedNameResolution)
1. Nonextensible records with polymorphic selection & update; see below and [Records/OverloadedRecordFields](records/overloaded-record-fields)

**Are there any other approaches?**

---

### Better name spacing


In Haskell, you can look at an occurrence of any identifier `f` or `M.f` and decide where it is bound without thinking about types at all.  Broadly speaking it works like this:

- For qualified names, `M.f`, find an import that binds `M.f`.
- For unqualified names, `f`, find the innermost binding of `f`; or, if that takes you to top level, look for top level binding of `f` or an import that binds `f`.


If there is ambiguity (eg two imports both import something called `f`) then an error is reported.  And that's what happens for the `Record` and `RecordClash` example above.


So one solution for record field names is to specify more precisely which one you mean.  There are two schools of thought:

- Optionally use the type name.  So you could say `Record.a` or `RecordClash.a` rather than `a`, to specify which field selector you mean.  Apart from verbosity the difficulty here is that it's hard to know whether you are writing `<module-name>.f` or `<type-name>.f`.  That is, is `Record` the name of a type or of a module?  (Currently it legally could be both.)

- Use the module name space mechanism; after all that's what it's for.  But putting each record definition in its own module is a bit heavyweight. So maybe we need local modules (just for name space control) and local import declarations.  Details are unclear. (This was proposed in 2008 in [ this discussion](http://www.haskell.org/pipermail/haskell-cafe/2008-August/046494.html) on the Haskell cafe mailing list and in [\#2551](https://gitlab.haskell.org//ghc/ghc/issues/2551). - Yitz)

**Anyone who likes these designs, please fill out a detailed design, either here or on another page**.

---

### Type directed name resolution


All of the name-space mechanisms require some level of user-supplied disambiguation: if there are two fields `a` in scope, you must use a qualified name to disambiguate them.  What is tantalising about this is that the *type* of the argument immediately specifies which one you mean. There is really no ambiguity at all, so it is frustrating to have to type qualified names to redundantly specify that information.  Object-oriented languages take for granted this form of type-directed disambiguation.


One particular way of integrating this idea into Haskell is called [ Type Directed Name Resolution](http://hackage.haskell.org/trac/haskell-prime/wiki/TypeDirectedNameResolution) (TDNR).  Proposed a couple of years ago, the Haskell community didn't like it much.  (But I still do; SLPJ.)

---

### Nonextensible records with polymorphic selection & update


The ideas in "first class record types" still work in the case of nonextensible records. Using a simplified version of Labels [\#2104](https://gitlab.haskell.org//ghc/ghc/issues/2104) we can implement truly polymorphic selection and update, which would be more expressive than TDNR and wouldn't need a whole new type resolution mechanism.  Here is a concrete proposal (Barney Hilken); see also [Records/OverloadedRecordFields](records/overloaded-record-fields):

1. Introduce a built-in class `Label`, whose members are strings at the type level. We need a notation for them; I will use double single quotes, so `''string''` is treated as if it was defined by

  ```wiki
  data ''string''

  instance Label ''string''
  ```


This has global scope, so `''string''` is the same type in all modules. You can't define other instances of `Label`.

1. Define a class (in a library somewhere)

  ```wiki
  class Label n => Contains r n where
  	type Field r n :: *
  	select :: r -> n -> Field r n
  	update :: r -> n -> Field r n -> r
  ```
1. Declarations with field labels such as

  ```wiki
  data C = F {l1 :: t1, l2 :: t2} | G {l2 :: t2}
  ```


are syntactic sugar for

```wiki
data C = F t1 t2 | G t2

instance Contains C ''l1'' where
	Field C ''l1'' = t1
	select (F x y) _ = x
	update (F x y) _ x' = F x' y

instance Contains C ''l2'' where
	Field C ''l2'' = t2
	select (F x y) _ = y
	select (G y) _ = y
	update (F x y) _ y' = F x y'
	update (G y) _ y' = G y'
```

1. Selector functions only need to be defined once, however many types they are used in

  ```wiki
  l1 :: Contains r ''l1'' => r -> Field r ''l1''
  l1 = select r (undefined ::''l1'')

  l2 :: Contains r ''l2'' => r -> Field r ''l2''
  l2 = select r (undefined ::''l2'')
  ```
1. Constructors are exactly as they are currently. I can't see any use for polymorphic constructors when records are nonextensible.

1. Updates such as

  ```wiki
  r {l1 = x}
  ```


are syntactic sugar for

```wiki
update r (undefined::''l1'') x
```