# Static Data


WIP proposal allowing to store static data/objects into programs (see [\#5218](https://gitlab.haskell.org//ghc/ghc/issues/5218)).

## Desugaring static strings


With OverloadedStrings, when we have:

```
"My text"::String"My text"::Text"123456"::ByteString
```


it is desugared to:

```
   fromString "My text"::String
   fromString "My text"::Text
   fromString "123456"::ByteString
```


Let's add a StaticStrings extension that desugars to this instead:

```
[string|My text]::String[text|My text]::Text[byte|123456]::ByteString
```


The quasiquoter is selected from the type. I.e.,

```
classStringQuote a where
      fromStringQuote ::QuasiQuoter-- or directly
      fromStringQuote ::String->QExp
```


It makes string literals quasiquoters in disguise. It's only for syntactic convenience (the type can be inferred, hence the quasiquoter can be inferred).

### Builtin StringQuote


Not all GHC support Template Haskell. Hence some StringQuote instances could be builtin or provided by compiler plugins. E.g.,

- String (current string storage)
- Text (UTF-16 encoding)

## Storing raw bytes


We want to be able to store raw bytes ("static data") into programs (e.g., to
embed resources like images, arrays, etc.).


Let's add a `StaticData#` primitive type. `StaticData#` have a size in bytes, an
alignment constraint and can be:

- initialized (we know their contents) and immutable
- initialized and mutable
- non initialized and mutable


They support at least the following primops which are compiled as constants or
symbols:

```
   staticDataAddr#::StaticData#->Addr#
   staticDataSize#::StaticData#->Word#
   staticDataAlignment#::StaticData#->Word#
   staticDataIsMutable#::StaticData#->Int#
```


The API to create static data is available from Template Haskell. Hence a
quasiquoter can use it.


Internal representation in GHC:

```
dataStaticData=StaticData{ staticDataSize      ::Word, staticDataAlignment ::Word-- ^ Alignment constraint (1 if none), staticDataMutable   ::Bool-- ^ Is the data mutable?, staticDataContents  ::MaybeByteString}
```


Static data end up in appropriate sections of the program (.bss, .data, .rodata, etc.).
Immutable data should be shared (with least common multiple alignment) to reduce
binary size.


Note that only the contents of the static data is stored in programs: data size
isn't stored! (no overhead)

## Storing Haskell objects


Some packages would like to store static Haskell object (e.g., `ByteArray#`) and not raw bytes (which are retrieved from an
`Addr#`).


We could use a static compact region embedded in the program. Let's add a
`StaticObject#` primitive type which is a reference to an object in the static
compact region.

`StaticObject#` supports the following primop:

```
   fromStaticObject#::StaticObject#-> a
```


The linter should ensure that the type of the static object is `a`.


We provide the following GHC/Template Haskell API:

```
-- | Add an object in compact normal form in the static compact region
   addStaticObject :: a ->QStaticObject#-- | Helper: use addStaticObject and produce "fromStaticObject# obj :: a" expression
   asStaticObject :: a ->QExpr
```


Static compact region would be automatically size expanded to be able to contain
these data.


AFAIK compact regions only support immutable data. Perhaps we could relax this
to support storage of `MutableByteArray#` which don't have references to other data
(in order to support mutable unboxed vectors for instance).

## Desugaring usual strings


Instead of using a specific string-literal, we can use generic static data.


Desugaring becomes:

```
   unpackCString#(staticDataAddr# d)
```


Most rules should still match. Some would need a minor refactoring (e.g.,
builtin rules).
