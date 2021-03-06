# Haskell Objective-C FFI: Foreign Declarations


Objective-C is a proper superset of plain C.  Consequently, we want all the Haskell FFI foreign declarations for C unchanged when dealing with Objective-C.  They use the `ccall` calling convention as usual.  In addition, we introduce the new `objc` calling conventions for new declarations specific to Objective-C.

## Summary of foreign declarations


With the current FFI we supply definitions to marshal foreign functions and basic types. Since this extension relates to classes, and since there's lots of OO languages out there, it would be nice to set the extension up in such a way that other OO FFI extensions look like it. Keeping this in mind, working with Obj-C classes in Haskell might look like:

### Import

- Import an ObjC class (may be defined in Haskell):

  ```wiki
  foreign import class objc "<optional header> <optional implementation> <class name>" tycon -- As a starting point, maybe derive the tycon from the class name?
  ```

  The newly introduced type `tycon` can be marshalled to and from ObjC land; i.e., it is a *foreign type* in the sense of Section 3.2 of the Haskell FFI.

- Import of an ObjC selector (maybe synthesized from a property):

  ```wiki
  foreign import objc "<optional synthesize> <selector>" varid :: <type>
  ```

  there can be more than one foreign import declaration for one selector with different Haskell identifiers and types. Note that, in the worst case we could just provide a function of type `String -> IO ObjCSel`, since Objective-C selectors are completely type-unsafe anyway.
- Import an ObjC constructor:

  ```wiki
  foreign import constructor objc "<constructor method name>" initializerName
  ```

  Note that for Objective-C we can get away without a separate constructor by calling methods on classes. However, it's a nice-to-have as part of bridging to OO languages. Ambitiously, perhaps we could also build in memory management for objects returned from these tagged constructors...

### Export

- Export a set of Haskell functions (pertaining to a certain datatype) as a class:

  ```wiki
  foreign export constructor objc tycon "<superclass> <class>" constructor :: ... -> IO tycon
  ```

  The Haskell-defined `tycon` would be type-constrained to have certain properties (yet to be specified).

- Export of a method implementation:

  ```wiki
  foreign export method objc "<optional header> +/-[<class> <selector>]" methodid :: <type>
  ```

  where

  ```wiki
  methodid :: ObjCPtr super -> tycon -> ... -> IO (tycon, b)
  ```

  This approach would provide opaque access to the object's Objective-C representation as an opaque supertype, giving Haskell method implementations an Objective-C feel.


In both of these cases, the underlying structure of the class' datatype - if possible - should be derived from the provided type constructor.
