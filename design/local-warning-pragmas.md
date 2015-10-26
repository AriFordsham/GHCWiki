**WORK IN PROGRESS**


THIS IS NOT READY FOR PUBLIC BIKESHEDDING YET

# Local Warning Pragmas

### What is it and why is it needed?


I want to implement possibility to suppress particular kinds of warnings for parts of a source file.


According to [\#602](https://gitlab.haskell.org//ghc/ghc/issues/602):
"One way to achieve this is to allow parts of a file to be delimited by pragmas specifying the warnings to be suppressed, and then filter out the warnings during compilation based on the source location attached to the warning."

## Use cases


For example you have a function in your source file that can perform some unsafe actions(unsafe I/O maybe). You think you are smarter than GHC, and you don't want to read such long warning's texts. So you wrap this piece of code in pragmas and compiler does not throw warnings and you are happy with that.

## Exempli gratia


I don't know conventions about naming pragmas, so let it be something like this.

```
{-# SUPPRESS_WARNINGS #-}foo::IORefIntfoo= unsafePerformIO (newIORef 10)
```


We are suppressing warnings for one particular function 


Or we can suppress warnings for some part of source code:

```
{-# SUPPRESS_WARNINGS #-}foo::IORefIntfoo= unsafePerformIO (newIORef 10)bar::IORefIntbar= unsafePerformIO (newIORef 11)baz::IORefIntbaz= unsafePerformIO (newIORef 12){-# UNSUPPRESS_WARNINGS #-}
```