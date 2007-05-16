## Class Families


Our translation of data families in combination with the desugaring of classes into data types suggest the idea of **indexed class families**, which turns out to be rather useful for generalising class APIs for commonly used data structures.

### An example


As a motivating example take the following problem from John Hughes' *Restricted Data Types*.  Suppose we want to implement a set API as a type class.  Then, we find that the signature

```wiki
insert :: Set s => a -> s a -> s a
```


is too general.  We need additional type constraints whose exact form *depends on the type constructor* we use to construct the sets; i.e., it varies on an instance by instance basis.  For lists, we just need `Eq`, but for sets as finite maps, we need `Ord`.


With indexed class families, we can define a set class as follows:

```wiki
class Set s where
  class C s a
  empty  :: s a
  insert :: C s a => a -> s a -> s a
```


Here, the **associated class**`C` of `Set` is indexed by the class parameter `s`.


In instances for sets as lists

```wiki
instance Set [] where
  class Eq a => C [] a
  empty = []
  insert x s | x `elem` s = s
             | otherwise  = x:s

instance Eq a => C [] a                   -- Tiresome instance
```


and sets as finite maps

```wiki
newtype MapSet a = MapSet (Data.Map.Map a ())
instance Set MapSet where
  class Ord a => C MapSet a
  empty = Data.Map.empty
  insert x s = Data.Map.insert x () s

instance Ord a => C MapSet a               -- Tiresome instance
```


we instantiate `C` differently for different type indexes.  


The class-family instances have no members in this case, but use existing classes as a superclass to supply `insert` with the equality and ordering methods, respectively.  As we want to use these superclasses for sets of any element type of which we have an instance of the superclasses, we need a catch-all instance  for each class instance (the "tiresome instances" avove).  That is somewhat ugly especially, as it requires the use of `-fallow-undecidable-instances`.  Furthermore, if the class has no signatures, there is no other useful instance we could possibly give.


SLPJ note: I wonder whether it is ever useful to have a class instance with signatures.  Suppose we only allowed the signature-free form?  That would simplify the explanation in many ways (e.g. no need to say whether class instances can themselves have assoicated types!), and loses no expressive power.  I don't think it loses much convenience either.

### The language extension


We define class families as

```wiki
class family C a1 .. an
```


and class-family instances as

```wiki
class instance ctxt => C t1 .. tn where { sigs }
```


where I'd propose to not allow functional dependencies to keep matters simpler.


Class instances of class-family instances take the normal form.  The only additional constraint is that the class parameters are type instances of the class-family instance types.  That is, if we have

```wiki
instance ctxt' => C s1 .. sn where { .. }
```


then we need to have that each `si` is a type instance of `ti` for this to be a class instance of the class-family instance `C t1 .. tn`.


As with data families, the class families can be associated with a class by declaring them in the class.  In this case, we omit the keywords `family` and `instance` in the family and instance declarations, respectively.  Moreover, all type indexes of an associated class need to be class parameters of the parent class.

**OPEN QUESTIONs:**

- Should an associated class be a (kind of) superclass of its parent.  At least, we may want to add it implicitly to the signature of each method.  Not sure about this, but Roman suggested it, too.
- Do we allow associated types and classes(?!?) in class-family instances?

### Type checking


Like with data families, there is little impact on type checking.  Methods of class-family instances have signatures whose class constraints are not just variables.  For example,

```wiki
class instance C Int a where 
  foo :: a -> a
```


gives us

```wiki
foo :: C Int a => a -> a
```


Otherwise, superclasses and class instance introduce the usual given constraints.  


However, to implement superclass constraints, we need to have a `ClassInstEnv` (similar to the `InstEnv` and `FamInstEnv` right now).  For a vanilla class, if we have `C t1 .. tn` in the constraint pool, we just can add all superclasses of `C` at the appropriate instance types.  However, if `C` is a class family, we need to check whether there is a class-family instance `C r1 .. rn` and a substitution `theta`, such that `theta (C r1 .. rn) == C t1 .. tn`; if so, we can add the superclasses of `C r1 .. rn` at the instance types suggested by `theta`.  This check for a class-family instance requires a function `lookupClassInstEnv` (similar to the current `lookupInstEnv` and `loookupFamInstEnv`).


Finally, we need to exclude overlap of class-family instances in the same way as for data-family instances.

### Desugaring


A class family declaration corresponds to a data family:

```wiki
class family C a1 .. an
    ||
    vv
data family C a1 .. an
```


A class-family instance corresponds to a data-family instance, which is the classes dictionary type.

```wiki
class instance forall b1..bn. ctxt => C t1 .. tn where { sigs }
    ||
    vv
data :R42C b1 .. bn = R42C { super :: ctxt, sigs }
coe $Co:R42C b1 .. bn :: C t1 .. tn ~ :R42C b1 .. bn
$WR42C :: ctxt -> sigs -> C t1 .. tn
  -- the datacon wrapper and the field selectors
  -- use the coercion $Co:R42C to move between the
  -- indexed dictionary type and the representation
  -- dictionary type (the current code in MkId for
  -- data families should do all this already)
```


Moreover, the class-family instance will have a representation as a `Class.Class` in GHC, where the `classTyCon` is `:R42C` (i.e., the instance tycon of the dictionary).  We might also want `Class.Class` to have a `classParent` field as we have this at them moment for instance `TyCon`s.


Finally, a class instance of a class-family instance is translated as usual:

```wiki
instance forall c1..cm. ctxt' => C s1 .. sn where { methods }
    ||
    vv
$dCs1sm :: ctxt' -> C s1 .. sn
$dCs1sm dicts = $WR42C <superdict> methods
```


Moreover, we will have a `InstEnv.Instance` representation of the instance where `is_class` is the name of the class family and `is_tys` is `s1` to `sn`.  This is as lookup in an `InstEnv.InstEnv` does not need to make a distinction between vanilla classes and class-family instances.

### Related work


Compare to **composite class signatures** and **submodules** of the *Modular Type Classes* paper.

### Comments


Roman objects that he really would like collection interfaces to use synonym families (rather than class families) - for example, 

```wiki
class Collection c where
  type Element c
instance Eq a => Collection (Set a) where
  type Element (Set a) = a
instance Ord a => Collection (OrdSet a) where
  type Element (OrdSet a) = a
instance Collection [a] where
  type Element [a] = a
```