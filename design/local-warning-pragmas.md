# Local Warning Pragmas

### What is it and why is it needed?


I want to implement possibility to suppress particular kinds of warnings for parts of a source file.


According to [\#602](https://gitlab.haskell.org//ghc/ghc/issues/602):
"One way to achieve this is to allow parts of a file to be delimited by pragmas specifying the warnings to be suppressed, and then filter out the warnings during compilation based on the source location attached to the warning."


Very natural thing is to suppress warnings, that can be thrown by some syntactic elements. To sum up, it would be a nice feature to have warning suppression for four things:

- Functions
- Instances
- Imports
- Type classes


Having source file delimeted by pragmas is not good idea as for me, because it will ruin code clarity and for me, for example, it would be too hard to read such source file. So i think that having a single pragma, attached to a function, instance import or typeclass will be a tool of power and precise. There is a nice example in [ Java](http://docs.oracle.com/javase/7/docs/api/java/lang/SuppressWarnings.html) of suppressing warnings for particular methods in classes

## Use cases


For example you have a function in your source file that can perform some unsafe actions(unsafe I/O maybe). You think you are smarter than GHC, and you don't want to read such long warning's texts. So you apply pragma to your function and compiler does not throw warnings and you are happy with that.

## Exempli gratia


I don't know conventions about naming pragmas, so let it be something like this.

```
{-# SUPPRESS #-}foo::IORefIntfoo= unsafePerformIO (newIORef 10)
```


We are suppressing warnings for one particular function 


Alternatively, we can do this in top-level, i mean in the file header we can write and compiler will suppress warnings, that foo will throw, see [ this](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/pragmas.html) about WARNING and DEPRECATED pragmas

```
{-# SUPPRESS foo  #-}--some code herefoo::IORefIntfoo= unsafePerformIO (newIORef 10)
```


Or as seen [ here](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/pragmas.html) for INLINE pragma one can write

```
foo::IORefIntfoo= unsafePerformIO (newIORef 10){-# SUPPRESS foo #-}
```