
On 5 May 2008, Isaac Dupree asked


>
>
> Is there documentation (e.g. on the GHC Commentary somewhere I can't
> find) an explanation of what C-- "kinds" are or how they're useful/used? 
>
>


Probably not.  GHC Cmm is a sort of pidgin version of C-- 2.0, and
true C-- kinds are explained in the [C-- specification, section 5.1](https://www.cs.tufts.edu/~nr/c--/code.html).


>
>
> When I was portabilizing that code area a while ago I had ignorantly 
> changed some of the uses of "kind" to "hint" for consistency (both names 
> had been being used for the same thing via type-synonym.) and because I 
> could guess how the code make sense if it was, informally, a hint about 
> what to do.
>
>


Hint was the word used originally, and several people (including
reviewers) objected to it on the grounds that the 'hints' are actually
mandatory to get the compiler to do what you want (e.g., pass
arguments in floating-point registers).  So we changed the name to
'kind'.


If you like dense, indigestible academic papers full of formalism,
there's [one I'm quite proud of](http://www.cs.tufts.edu/~nr/pubs/staged-abstract.html).
It explains in detail how kinds are useful for specifying and
implementing procedure calling conventions, which is the use to which
they are put within GHC. 


Norman Ramsey
