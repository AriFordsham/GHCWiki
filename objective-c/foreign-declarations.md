# Haskell Objective-C FFI: Foreign Declarations


Objective-C is a proper superset of plain C.  Consequently, we want all the Haskell FFI foreign declarations for C unchanged when dealing with Objective-C.  They use the `ccall` calling convention as usual.  In addition, we introduce the new `objc` calling conventions for new declarations specific to Objective-C.

## Summary of foreign declarations


The following declarations specific to Objective-C:

- Import an ObjC class (may be defined in Haskell):

  ```wiki
  foreign class objc "<optional header> <optional implementation> <class name>" tycon
  ```

  The newly introduced type `tycon` can be marshalled to and from ObjC land; i.e., it is a *foreign type* in the sense of Section 3.2 of the Haskell FFI.
- Import of an ObjC selector (maybe synthesized from a property):

  ```wiki
  foreign import objc "<optional synthesize> <selector>" varid :: <type>
  ```

  there can be more than one foreign import declaration for one selector with different Haskell identifiers and types.
- Export of a method implementation:

  ```wiki
  foreign export objc "<optional header> +/-[<class> <selector>]" varid :: <type>
  ```