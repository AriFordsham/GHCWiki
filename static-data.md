# Static Data


WIP proposal allowing to store static data/objects into programs (see #5218). It would allow large resources data (images, sounds, etc.) to be embedded (see #14741).

## Step 1: support dependent object files

[https://phabricator.haskell.org/D4217](https://phabricator.haskell.org/D4217)


We need to be able to link with arbitrary object files. Object files can contain arbitrary data in their data sections (data, rodata, bss, etc.).

## Step 2: provide helpers to generate object files


Provide helpers (in GHC and in TH) to generate object files (ELF, etc.) containing a data section with specified properties: read-only or mutable, section alignment, allow section merging, etc.


Provide helper to generate unique data symbols and to add symbols referring to the data in the symbol table of the object file.



At this step, we can already use TH to embed data. We can retrieve the data address from the symbol with:


```
foreign import ccall "&" myDataSymbol :: Ptr ()
```

## Step 3: add GHC primitive


To make the interface easier to use even when TH is not supported, let's add a `StaticData#` primitive type.

`StaticData#` have a size in bytes, an alignment constraint and can be:

- initialized (we know their contents) and immutable
- initialized and mutable
- non initialized and mutable


They support at least the following primops which are compiled as constants or
symbols:


```
   staticDataAddr#      :: StaticData# -> Addr#
   staticDataSize#      :: StaticData# -> Word#
   staticDataAlignment# :: StaticData# -> Word#
   staticDataIsMutable# :: StaticData# -> Int#
```


The API to create static data is available from Template Haskell. Hence a
quasiquoter can use it.



Internal representation in GHC:


```
   data StaticData = StaticData
      { staticDataSize      :: Word
      , staticDataAlignment :: Word    -- ^ Alignment constraint (1 if none)
      , staticDataMutable   :: Bool    -- ^ Is the data mutable?
      , staticDataContents  :: Maybe ByteString -- ^ Initialized or not? (maybe support 0-initialized data too?)
      , staticDataSymbol    :: String
      }
```


Static data end up in appropriate sections (.bss, .data, .rodata, etc.) in dependent object files.


Identical immutable data should be shared (with least common multiple alignment) to reduce binary size.


Note that only the contents of the static data is stored in programs: data size
isn't stored! (no overhead)

## Step 4: Storing Haskell objects


Some packages would like to store static Haskell object (e.g., `ByteArray#`) and not raw bytes (which are retrieved from an
`Addr#`).


We could use a static compact region embedded in object files (reusing the `StaticData#` machinery). Let's add a
`StaticObject#` primitive type which is a reference to an object in the static
compact region.



`StaticObject#` supports the following primop:


```
   fromStaticObject# :: StaticObject# -> a
```


The linter should ensure that the type of the static object is `a`.



We provide the following GHC/Template Haskell API:


```
   -- | Add an object in compact normal form in the static compact region
   addStaticObject :: a -> Q StaticObject#

   -- | Helper: use addStaticObject and produce "fromStaticObject# obj :: a" expression
   asStaticObject :: a -> Q Expr
```


Static compact region would be automatically size expanded to be able to contain
these data.


AFAIK compact regions only support immutable data. Perhaps we could relax this
to support storage of `MutableByteArray#` which don't have references to other data
(in order to support mutable unboxed vectors for instance).

## Step 5: revamp string literals


We would like string literals to be `StaticData#` like other data.



Currently with OverloadedStrings, the following:


```
   "My text" :: String
   "My text" :: Text
   "123456"  :: ByteString
```


is desugared into:


```
   fromString "My text" :: String
   fromString "My text" :: Text
   fromString "123456"  :: ByteString
```


Let's add a `StaticStrings` extension that desugars to this instead:


```
   [string|My text|] :: String
   [text|My text|]   :: Text
   [byte|123456|]    :: ByteString
```


Note that the \*quasiquoter is selected from the type\*. I.e., we have:


```
   class StringQuote a where
      fromStringQuote :: QuasiQuoter

   instance StringQuote ByteString where ...
   instance StringQuote String where ...
   instance StringQuote Text where ...
   instance StringQuote (Ptr a) where ...
```


It makes string literals quasiquoters in disguise. It's only for syntactic convenience: the type can be inferred, hence the quasiquoter can be inferred.

### Builtin StringQuote


Not all GHC support Template Haskell. Hence some StringQuote instances could be builtin or provided by compiler plugins. E.g.,

- String (current string storage)
- Text (UTF-16 encoding)

### Desugaring usual strings



When the StaticStrings extension is not in use, instead of desugaring into a specific string-literal, we can desugar into the following:


```
   unpackCString# (staticDataAddr# d)
```


Most rules should still match. Some would need a minor refactoring (e.g., builtin rules).
