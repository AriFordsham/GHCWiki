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


It is not very easy to form a good use case for this but nevertheless. 

1. For example, if you depend on particular library function(if you want to support backward compatability) but in newer version of library this function marked as deprecated and so you get warnings about it. Here it may be useful to suppress them instead of rewriting the whole codebase. 
1. Recent monad of no return proposal suggests that having `Applicative` context sufficed for `Monad` assumes that `return` is already implemented as `pure`, so we don't need to duplicate code. However, Monad still has minimal complete definition `>>=` and `return`, so we can have warnings about incomplete minimal definition.

## Exempli gratia


I don't know conventions about naming pragmas, so let it be something like this.

```
moduleTestwhereimportold_lib(foo)...bar:: a -> b -> c 
{-# SUPPRESS foo #-}bar x y = foo y $ x
```


We are suppressing warnings for one particular function

```
{-# SUPPRESS return #-}instanceApplicative m =>Monad m where(>>=)=...
```


And here we are suppressing warnings about incomplete minimal 
