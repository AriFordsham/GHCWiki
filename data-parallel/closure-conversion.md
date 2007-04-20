DataParallel Up?

## Closure conversion as part of vectorisation

**TODO** Describe the treatment of higher-order functions and closure conversion here. The relevant paper is [ http://www.cse.unsw.edu.au/\~chak/papers/LCK06.html](http://www.cse.unsw.edu.au/~chak/papers/LCK06.html). The approach is described in more detail in [ http://opus.kobv.de/tuberlin/volltexte/2006/1286/](http://opus.kobv.de/tuberlin/volltexte/2006/1286/).

### Closure-converted types as indexed-types


One option for implementing closure-conversion is to represent closure-converted types as an indexed type whose type index is the original type and to combine that indexed type in a type class with methods for converting between closure-converted and vanilla terms.  The details are under [indexed closure conversion](data-parallel/closure-conversion/indexed).  There are two potential benefits for this approach: (1) we will probably have to do something similar for vectorisation anyway - see the requirements of vectorisation? - and (2) it seems that we need less bookkeeping (e.g., the name of a closure converted data type is just the indexed type with the original data type as its index).  However, there are problems, too; in particular, as we currently don't have class contexts and polytypes as type indexes.

---

**** OLD STUFF ****

### From the Skype discussion, 16 Mar 07


For each function `f::ty`, create a closure-converted function `fc::ty'`, where `ty'` is the closure-converted verion of `ty`.
Optimisation: make `fc=f`, if the code for `fc` will be identical to that of `f`.  


For each data type `T`, create a closure-converted data type `Tc`, whose constructors use `Clo` instead of `->`.  Optimisation: if `Tc` is identical to `T`, don't create a new data type.
