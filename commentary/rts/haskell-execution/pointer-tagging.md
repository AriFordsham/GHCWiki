# Pointer Tagging


Paper: [ Faster laziness using dynamic pointer tagging](http://www.haskell.org/~simonmar/papers/ptr-tagging.pdf)


In GHC we "tag" pointers to heap objects with information about the object they point to.  The tag goes in the low 2 bits of the pointer, which would normally be zero since heap objects are always [word](commentary/rts/word)-aligned (3 bits on a 64-bit platform).


The way the tag bits are used depends on the type of object pointed to:

- If the object is a constructor, the tag bits contain the *constructor tag*, if the number of
  constructors in the datatype is less than 4 (less than 8 on a 64-bit platform).  If the number of
  constructors in the datatype is more than 4 (resp 8), then the tag bits have the value 1.

- If the object is a function, the tag bits contain the *arity* of the function, if the arity fits
  in the tag bits.

- For a pointer to any other object, the tag bits are always zero.


Pointer tagging is optional: if all the tags were zero, everything would still work.  The presence of tag bits enables certain optimisations, however:

- In a case-expression, if the variable being scrutinised has non-zero tag bits, then we know
  that it points directly to a constructor and we can avoid *entering* it to evaluate it.
  Furthermore, for datatypes with only a few constructors, the tag bits will tell us *which*
  constructor it is, eliminating a further memory load to extract the constructor tag from the
  info table.

- In a [generic apply](commentary/rts/haskell-execution/function-calls#generic-apply), if the function being applied has a tag value that indicates it has exactly the
  right arity for the number of arguments being applied, we can jump directly to the function, instead of
  inspecting its info table first.


Pointer-tagging is a fairly significant optimisation: we measured 10-14% depending on platform.  A large proportion of this comes from eliminating the indirect jumps in a case expression, which are hard to predict by branch-prediction.  The paper has full results and analysis.


The [garbage collector](commentary/rts/storage/gc) maintains tag bits on the pointers it traverses; additionally when it eliminates an indirection it takes the tag bits from the pointer inside the indirection.

## Dealing with tags in the code


Every time we dereference a pointer to a heap object, we must first zero the tag bits.  In the RTS, this is done with the macro `UNTAG_CLOSURE()`; in `.cmm` code this is done with the `UNTAG()` macro.  Surprisingly few places needed untagging to be added.
