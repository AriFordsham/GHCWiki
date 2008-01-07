# Unregisterised builds


Normally GHC will try to do a so-called registerised build, where it uses various architecture and OS specific knowledge to get more efficient code. However, on an architecture where this information has not been created, or has not been kept up-to-date, it is necessary to do an unregisterised build, which uses just plain old portable C.


To do an unregisterised build, add the following to your `mk/build.mk` file:

```wiki
GhcUnregisterised=YES                                                     
GhcWithNativeCodeGen=NO                                                   
GhcWithInterpreter=NO                                                     
SplitObjs=NO
```


GHC will automatically do an unregisterised build on platforms that it knows don't currently have registerised support.


Currently the native code generator requires a registerised build. GHCi seems close to working, but FFI in GHCi is not supported (see [\#631](https://gitlab.haskell.org//ghc/ghc/issues/631)). Object splitting only works when building registerised.
