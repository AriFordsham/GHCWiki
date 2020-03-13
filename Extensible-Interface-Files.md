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

In addition to their primary purpose for IDE-like uses, HIE files can we used for dead code elimination. A weeder re-implementation (https://github.com/ocharles/weeder) does this.

In the same way that plugins would like to access core data, it would also be useful to make it possible for them to write their own information, and access it later.

## Extensible Interface Files

We propose to extend interface files into a sectioned format that can store arbitrary
metadata for keys, to allow for a maximum flexibility.
(more technical details on how this API looks)

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

## Complications

Since interface files are written later in the build process, this doesn't properly enable us to store outputs of early stages in the compiler, without completing enough of the build. For example, this technique would be inappropriate for an IDE to capture a parsed AST, to use for syntax highlighting, so failed type checking would prevent the interface file, and therefore its additonal data, being written. Of course, the GHC API could be used instead in this example, and potentially for other early build output use cases.

`Name` and `FastString` have a special handling in interface files, in which they are written to the end of an interface file in a lookup table, and referenced within the serialised interface payload by their table indicies. There are currently minorly different implementations for how this is done with interfaces, HIE, and Haddock. Since additional fields that are serialised from the GHC pipeline will contain these two types, it's probably a good time to generalise this behaviour.

One possibility to solve this could be to define something like `data InterfaceData a`, and have the `Binary` instance for this handle the lookup tables, such that `a` is the payload - for example `InterfaceData ModIface`.

## Conclusion

Extensible interface files carry the advantage over custom additional files in that tooling doesn't need to know about the existence of the additional data, with use cases such as:
* Core:
..* Resumable compilation
..* Plutus (plugin)
..* Liquid Haskell
* HIE:
..* IDEs
..* Weeder
* Custom data:
..* Plugins
..* IDEs
..* Build tools
