# Haskell Objective-C FFI: Memory Management


We need to be able to work with garbage-collected and memory-managed Objective-C code.  We also like to do as little memory-management in Haskell as possible, but at least when we implement classes in Haskell, we will have to deal with explicit memory-management to some degree.


BTW, Chicken objc egg tries to automate it all, but it's not clear whether that works in all cases.  They also don't seem to address ObjC 2.0 GC.  Also the ivar memory-management is one reason for their class proxies, which are awkward.
