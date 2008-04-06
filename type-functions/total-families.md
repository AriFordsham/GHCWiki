## Total Families


To guarantee the termination and completeness of the solving of equality constraints, we need to impose rather draconian restrictions on the instances of type synonym families.  More specifically, to achieve both termination and completeness, we need the *Strong Termination Condition* and if we settle for termination alone (accepting to be incomplete for some rather exotic programs), we need the *Relaxed (Termination) Condition* as defined in [ Type Checking with Open Type Functions](http://www.cse.unsw.edu.au/~chak/papers/tc-tfs.pdf).  The *Strong Termination Condition* corresponds closely to the conditions imposed on functional dependencies.  The *Relaxed Condition* is somewhat more liberal, but still does not permit, for example, nested applications of type families in the right-hand side of a `type instance`.  For many practically attractive uses of type families, where the the system is actually terminating, this is still to restrictive.

### Two examples

```wiki
data Z; data S a;

-- meets the Strong Termination Condition
type family x :+ y
type instance Z   :+ y = y
type instance S x :+ y = S (x :+ y)

-- meets only the Relaxed Condition
type family x :* y
type instance Z     :* y = Z
type instance (S x) :* y = x :* y :+ y
```


The family `(:+)` meets the *Relaxed Condition*, but not the *Strong Termination Condition*.  However, `(:*)` meets not even the *Relaxed Condition*.  Nevertheless, we would expect that families defined by structural recursion should not compromise termination.  Another somewhat irritating aspect of this example is that we would like these definitions to be closed, but they are open in their present form.


We might want to define equality on types as

```wiki
data TFalse; data TTrue;

type family TypeEq s t
type instance TypeEq s s = TTrue
type instance TypeEq s t = FFalse  -- matches only if the previous instance does not
```


Unfortunately, the two instances are overlapping and there is no means by which we can disambiguate the overlap by using the same textual ordering as that which we are used to from value-level functions.
