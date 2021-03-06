# Haskell Objective-C FFI: Messaging


Here is an example of how we import selectors:

```wiki
foreign import objc "@selector locationInView:" s'locationInView
   :: Ptr CGPoint -> C'UITouch -> C'UIView -> IO ()
```


The first argument is the receiver object.  All following arguments are message arguments (one per colon, as usual).  A foreign import is just about issuing Objective-C messages.  We are not trying to model overloading at this point or anything similar.  So, if we, for example, want `s'locationInView` for a subclass, we need to define another function **with a different name.**


We might use the imported function as follows:

```wiki
-- ObjC: touchedPoint = [myTouchObject locationInView:currentView];
s'locationInView touchedPoint myTouchObject currentView
```


This will translate to something which essentially has the same effect as

```wiki
objc_msgSend_stret (touchedPoint, myTouchObject, sel_getUid("locationInView"), currentView);
```

*Implementation note:* The foreign import should arrange for a call to `sel_getUid` at program start or when an imported selector is used for the first time.  The result should be cached to speed up subsequent uses of the same selector in the same program.

*Usage note:* To wrap imported functions handling C structs in a nice way, we define a `Foreign.Storable` instance and a convenience wrapper as usual in the plain C FFI.

## Distinguishing the three messaging types


The ObjC runtime has three kinds of messaging functions: `objc_msgSend` (when the message returns an object), `objc_msgSend_stret` (when the message returns a structure), and `objc_msgSend_fpret` (when the structure returns a floating-point value).  During code generation for `foreign import`, we select the appropriate messaging function as follows:

- If the returned type is `()`, we use `objc_msgSend_stret`; the first argument to the function must be a `(Ptr t)` and the second must be the receiver object.
- If the returned type is `CFloat`, `CDouble`, or `CLDouble`, we use `objc_msgSend_fpret` (with the function pointer cast to the appropriate return type first); the first argument must be the receiver object.
- Otherwise, we use `objc_msgSend`; the first argument must be the receiver object.  We need to cast  `obc_msgSend`'s function pointer first.


The decision which function to use is unfortunately a bit more complicated - see [http://www.sealiesoftware.com/blog/archive/2008/10/30/objc_explain_objc_msgSend_stret.html](http://www.sealiesoftware.com/blog/archive/2008/10/30/objc_explain_objc_msgSend_stret.html) and [ http://www.sealiesoftware.com/blog/archive/2008/11/16/objc_explain_objc_msgSend_fpret.html](http://www.sealiesoftware.com/blog/archive/2008/11/16/objc_explain_objc_msgSend_fpret.html).

## Return values


Like in the C FFI, return values can be pure (no `IO`), but it is the programmer responsibility to ensure that these foreign imports are safe.

## Messaging super


The ObjC runtime has two extra functions, `objc_msgSendSuper` and `objc_msgSendSuper_stret`, to message the super object.


An obvious approach to providing calls to `super` might be:

- `foreign import` the class' `super`
- Superclass instances would be accommodated as part of the type of a method that is `foreign export`ed.


These functions need to get an `objc_super` struct as the first parameter, which the compiler is supposed to compute from the enclosing class definition.
