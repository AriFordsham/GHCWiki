# Haskell Objective-C FFI: Declaring Objective-C Classes


One option might be something like

```wiki
foreign export "@class MyUIView : UIView" myUIView :: C'UIView
```

## Option 1: value definition


The class definition value `myUIView` contains the details of the class definition.  However, it is odd, because `C'UIView` should usually be the type of instances if of `UIView`.

## Option 2: incremental definition


In addition to the `foreign export` declaring the class, the class is populated with methods in further `foreign export` declarations.


We have the following problems: 

- The ObjC runtime uses two functions to initiate and finalise the declaration of a new class, namely `objc_allocateClassPair` and `objc_registerClassPair`.  All ivars must be added between these two calls (except for the 64-bit runtime, I guess, but the runtime ref doesn't say that).  Methods and protocols can still be added after registering.
- `objc_allocateClassPair` expects the superclass as a value of type `Class`.  The documentation does not say whether the superclass must already be registered (or whether it is sufficient to have it allocated at that point).


Problems arise because Haskell declarations are unordered, so we should make no assumption about the order of foreign declarations.  Moreover, we need to have foreign imported all class that we subclass (and which are not locally defined) – or we just call `objc_getClass`.  On the other hand, when you subclass an existing class, you almost certainly need to foreign import the superclass anyway, to implement the methods for the new class (so an implicit `objc_getClass` is probably not worth the effort).


What about ObjC classes that have been declared in other Haskell modules, do we have to foreign import them again or do we just import the Haskell representation?


We may have to compute the class DAG and make sure we generate class initialisation code that at module load time allocates, registers, and subclasses all Objective-C classes in the right order.
