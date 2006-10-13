# The semi-tagging optimisation


Here I describe the design of the semi-tagging optimisation. Currently most of the text comes from [ http://hackage.haskell.org/trac/summer-of-code/ticket/48](http://hackage.haskell.org/trac/summer-of-code/ticket/48)


This page reflects my current understanding on the compiler and the RTS, so if there is something wrong, just yell!

## The starting point


Currently when evaluating an expression that is the scrutinee of a case:

```wiki
case x of { ... }
```


GHC jumps to the code for (i.e. "enters") the x closure, which returns when x is evaluated. Commonly, x is already evaluated, and the code for an evaluated constructor just (vector) returns immediately.

*Alexey: add some example HC code here*

## Testing before jumping


The simplest optimisation is this.  Instead of entering the closure, grab its info pointer, and follow the info pointer to get the tag.  Now test the tag; if it's evaluated, don't enter the closure.  


The benefit is that processors are typically faster at "test-and-jump to known location" than they are at "jump to this pointer".

*Alexey: add some example HC code here*

## Tagging the LSB of an evaluated closure

>
> The idea is to encode the fact that a pointer points to an evaluated object by setting the LSB of the pointer. If the case expression detects that the closure is evaluated, it can avoid the jump and return, which are expensive on modern processors (indirect jumps).

<table><tr><th></th>
<th> bits 31..2 </th>
<th> bits 1 0 
</th></tr>
<tr><th> unevaluated closure </th>
<th> ptr </th>
<th> 00 
</th></tr>
<tr><th> evaluated constructor closure </th>
<th> ptr </th>
<th> 01 
</th></tr></table>


This would require modifying

- the GC to set the LSB bit of constructor closure pointers,
- the GC and the RTS code to mask out the LSB pointer when dereferencing it,
- the code generation to test the LSB bit and case expressions and avoid the indirect jump.

## Using more than one bit


We can go a bit further than this, too. Since there are 2 spare bits (4 on a 64-bit machine), we can encode 4 (16) states. Taking 0 to mean "unevaluted", that leaves 3 (15) states to encode the values for the "tag" of the constructor. eg. an evaluated Bool would use 1 to indicate False and 2 to indicate True. An evaluated list cell would use 1 to indicate \[\] and 2 to indicate (:).

<table><tr><th></th>
<th> bits 31..2 </th>
<th> bits 1 0 
</th></tr>
<tr><th> unevaluated closure </th>
<th> ptr </th>
<th> 00 
</th></tr>
<tr><th> cons. no. 1    </th>
<th> ptr </th>
<th> 01 
</th></tr>
<tr><th> cons. no. 2    </th>
<th> ptr </th>
<th> 10 
</th></tr>
<tr><th> cons. no. 3    </th>
<th> ptr </th>
<th> 11 
</th></tr></table>


The nice thing about the current approach is that code size is small; implementing the test and jump will certainly add extra code to compiled case expressions. But the gains might be worth it. Complexity-wise this means masking out these bits when following any pointer to a heap object, which means carefully checking most of the runtime.


This would require modifying all of the above plus modifying

- the code generator so that it checks whether the number of constructors is smaller or equal than 3/15.

## Using a tag directly in the pointer


Constructors without children (such as `False` and `True`) only need their tag to be represented. Hence we can drop the pointer altogether as follows:

<table><tr><th></th>
<th> bits 31..2 </th>
<th> bits 1 0 
</th></tr>
<tr><th> unevaluated closure </th>
<th> ptr </th>
<th> 00 
</th></tr>
<tr><th> cons. w/no children </th>
<th> tag </th>
<th> 01 
</th></tr>
<tr><th> cons. w/children no. 1    </th>
<th> ptr </th>
<th> 10 
</th></tr>
<tr><th> cons. w/children no. 2    </th>
<th> ptr </th>
<th> 11 
</th></tr></table>


This still limits us to data types that have no more than two constructors with children. We can improve on this by noting that pointers will not point to low addresses. So we can make a simple test to distinguish between tags and pointers:

<table><tr><th></th>
<th> bits 31..2 </th>
<th> bits 1 0 </th>
<th></th></tr>
<tr><th> unevaluated closure </th>
<th> ptr </th>
<th> 00 </th>
<th></th></tr>
<tr><th> cons. w/no children </th>
<th> tag </th>
<th> 01 </th>
<th> tag \< no. of  constructors
</th></tr>
<tr><th> cons. w/ children </th>
<th> ptr </th>
<th> 01 </th>
<th> ptr \>= no. of constructors 
</th></tr>
<tr><th> cons. w/children no. 1    </th>
<th> ptr </th>
<th> 10 </th>
<th></th></tr>
<tr><th> cons. w/children no. 2    </th>
<th> ptr </th>
<th> 11 </th>
<th></th></tr></table>


Of course this assumes that we don't have data types with too many thousands of constructors.


It might be possible that the case code for the alternatives above is becoming too complex. We might settle for the following "simple" option:

<table><tr><th></th>
<th> bits 31..2 </th>
<th> bits 1 0 </th>
<th></th></tr>
<tr><th> unevaluated closure </th>
<th> ptr </th>
<th> 00 </th>
<th></th></tr>
<tr><th> cons. w/no children </th>
<th> tag </th>
<th> 01 </th>
<th> tag \< no. of  constructors
</th></tr>
<tr><th> cons. w/ children </th>
<th> ptr </th>
<th> 01 </th>
<th> ptr \>= no. of constructors 
</th></tr></table>