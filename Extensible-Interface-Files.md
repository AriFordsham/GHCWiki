## Abstract

GHC uses so called interface files (.hi files) to capture information about modules
to be used when compiling modules that depend on previously compiled modules.
By allowing to include additional metadata in .hi files, we can capture, and extract
additional information in a more structured format, instead of writing this info into
custom files. By reusing .hi files, existing tooling does not need to be adapted.

## Haskell Interface Files

Currently, interface files are stuctured as:
* Magic number, which signals that it should be a real `.hi` file
* Legacy empty field
* Interface format version
* Way descriptor
* `FastString` dictionary pointer
* `Name` symbol table pointer
* Actual interface file
* Symbol table
* Dictionary

## Additional Metadata Uses

https://gitlab.haskell.org/ghc/ghc/wikis/Core-interface-section motivates the case for including core in interface files, to make it available via `loadCore :: ModIface -> m (Maybe ModGuts)`, to be used by examples such as Liquid Haskell, resumable compilation, and plugins including Plutus.

In addition to their primary purpose for IDE-like uses, HIE files can we used for dead code elimination. Weeder (https://hackage.haskell.org/package/weeder-2.0.0) does this. By including the HIE data in the interface file instead of a separate file, other tools gain the opacity of a single file.

In the same way that plugins would like to access core data, it would also be useful to make it possible for them to write their own information, and access it later.

## Extensible Interface Files

We propose to extend interface files into a sectioned format that can store arbitrary
metadata for keys, to allow for a maximum flexibility. Sectioning is already used by certain `.o` file formats, for example ELF (https://en.wikipedia.org/wiki/Executable_and_Linkable_Format), and Mach-O (https://en.wikipedia.org/wiki/Mach-O#Mach-O_file_layout).

From the user's perspective, such an API would look like:
```haskell
type FieldName = String

readIfaceField   :: Binary a => FieldName      -> ModIface -> IO (Maybe a)
writeIfaceField  :: Binary a => FieldName -> a -> ModIface -> IO ModIface
deleteIfaceField ::             FieldName      -> ModIface ->    ModIface
```

By using GHC's internal `Binary` class, we can reuse instances for existing types in GHC. However, since `Name` and `FastString` are each serialised into a lookup table, fields that consist of these would require a more internal API.

```haskell
readIfaceFieldWith  :: FieldName -> (BinHandle -> IO a ) -> ModIface -> IO (Maybe a)
writeIfaceFieldWith :: FieldName -> (BinHandle -> IO ()) -> ModIface -> IO ModIface
```

This internal API presents a way for files that resemble the current `.hi`/`.HIE` file structure to simply write their format directly to a local `BinHandle`, and have this data be copied into the `.hi` file's handle during `ModIface` serialisation. Of course, the constrained API can be trivially written in terms of this internal API using `put_` and `get`.

Then, we can capture these fields in the `ModIface` as:

```haskell
data BinData = BinData Int (ForeignPtr Word8)
data ExtensibleFields = ExtensibleFields (Data.Map FieldName BinData)
```

Then the captured data can be simply copied to the end of the interface file. For external tools to access a field without fully deserialising the interface, it would be important to add an additional pointer ('fields pointer') in the header, for example after the symbol table pointer.

For a tool to jump quickly to its required field, there should then we a header for the additional data, for example formatted as:

* Number of additional fields - the fields pointer would point here
* Field 1 name
* Field 1 pointer
* Field 2 name
* Field 2 pointer
* ...
* Field 1 payload
* Field 2 payload

## Compatibility

The current format maintains header compatibility up to the version field, which is checked when deserialising, so these changes are within what different versions of GHC expect to see.

## Complications

Since interface files are written later in the build process, this doesn't properly enable us to store outputs of early stages in the compiler, without completing enough of the build. For example, this technique would be inappropriate for an IDE to capture a parsed AST, to use for syntax highlighting, so failed type checking would prevent the interface file, and therefore its additonal data, being written. Of course, the GHC API could be used instead in this example, and potentially for other early build output use cases.

`Name` and `FastString` have a special handling in interface files, in which they are written to the end of an interface file in a lookup table, and referenced within the serialised interface payload by their table indicies. There are currently minorly different implementations for how this is done with interfaces, HIE, and Haddock. Since additional fields that are serialised from the GHC pipeline will contain these two types, it's probably a good time to generalise this behaviour.

To solve this, we can define wrapper functions to `put`/`get` a `Binary` payload, including the pointers before it, and the tables after it. 

```haskell
putWithTable :: Binary a => BinHandle -> a -> IO ()
getWithTable :: Binary a => BinHandle      -> IO a
```

## Complete Interface File Format

* File header
  * [32 bits] Magic number, which signals that it should be a real `.hi` file
  * [32/64 bits] Legacy empty field
  * [String] Payload format version
  * [String] Way descriptor
  * [32 bits] File format version
  * [32 bits] Extensible fields header pointer
  * [32 bits] `FastString` dictionary pointer
  * [32 bits] `Name` symbol table pointer
* GHC interface payload (payload version dependent)
  * Actual interface data
  * Symbol table
  * Dictionary
* Extensible fields header
  * [64 bits] number of extensible fields
  * n extensible field header entries
    * [String] field name
    * [32 bits] field pointer
  * n extensible field data entries
    * [64 bits] size of the field in bytes
    * [x bytes] field data

* String format
  * [64 bit Int] number of list elements
  * [n * 32 bit] list elements

## Implementation

### Extensible `ModIface`

The starting step for implementation would include:
*  Adding `ExtensibleFields` to the `ModIface` record
*  Define the sectioned fields header format in the `Binary` instance of `ExtensibleFields`
*  `read`/`write`/`detele` functions for the fields
*  Generalised lookup table serialisation functions (`putWithTable`/`getWithTable`)

### Core Field

With the new lookup table serialisation functions, we can define the core field:
*  `IfaceSyn` versions of the parts of `ModGuts` that aren't currently used in the `ModIface`
*  Missing instances

### HIE Field

To write the HIE Field, we can reuse the existing HIE file functions by changing them to take a `BinHandle` as an argument.

### Refactoring Existing File Formats

`.hi` and `.hie` formats handle the `NameCache` in a fairly straightforward way, using an `IO` `NameCacheUpdater` in the case of interface files:
```haskell
newtype NameCacheUpdater
      = NCU { updateNameCache :: forall c. (NameCache -> (NameCache, c)) -> IO c }
```

 While the HIE file handling inlines this in its read signature:
```haskell
readHieFileWithVersion :: (HieHeader -> Bool) -> NameCache -> FilePath -> IO (Either HieHeader (HieFileResult, NameCache))
```

However, Haddock is more general in its monad:
```haskell
type NameCacheAccessor m = (m NameCache, NameCache -> m ())
```
and defines its own equivalent of `updateNameCache`:
```haskell
with_name_cache :: forall a.
                   ((forall b.
                             (NameCache -> IO (NameCache, b))
                             -> IO b)
                    -> m a)
                -> m a
```

Generalising these should be possible, but it may not be worth it to include Haddock's use case.

## Conclusion

Extensible interface files carry the advantage over custom additional files in that tooling doesn't need to know about the existence of the additional data, and sectioned object formats see common use elsewhere, with use cases such as:
*  Core:
   *  Resumable compilation
   *  Plutus (plugin)
   *  Liquid Haskell
*  HIE:
   *  IDEs
   *  Weeder
*  Custom data:
   *  Plugins
   *  IDEs
   *  Build tools
