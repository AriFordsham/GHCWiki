
On this page we describe the principles behind the implementation of the linear types extension as described at \[[LinearTypes](linear-types)\].


The current implementation progress can be seen on [ here](https://github.com/tweag/ghc/tree/linear-types)

## Principles


The main principle behind the implementation is to modify `FunTyCon` with an extra argument which indicates the \*multiplicity\* of an arrow. The data type for multiplicities is defined
in `compilerbasicTypes/Weight.hs` and is called `Rig`. There are two multiplicities, `One` which indicates that the function is linear and `Omega` which indicates that it is not.


Binders also have a weight attached to them. 


The rest of the implementation is essentially correctly propagating and calculating linearity information whenever a `FunTy` is created.

## DataCon

## Rebuilding expressions in the optimiser

## FunTyCon

## Core Lint

TODO - this is described somewhat in the minicore document but it is not finished. 

## Polymorphism


The principle of the polymorphism implementation is simple. We modified the function type constructor to take an extra type argument. So it's kind is now.

```wiki
 (->) :: forall (m :: Multiplicity) (rep1 :: RuntimeRep) (rep2 :: RuntimeRep). TYPE rep1 -> TYPE rep2 -> *
```


Then all arrows can be polymorphic in their multiplicity. 

## Data Constructors are polymorphic


A key part of the original proposal was the type of data constructors was linear. 

```wiki
(,) :: a ->. b ->. (a, b)
```


However, this was quickly found not to work properly. Below are two examples as described by Arnaud

### Example 1

```wiki
foo :: Identity (a -> b) -> a -> b
foo = unIndentity

foo (Identity Just)
```


The last line doesn’t type check because foo expects an Identity (a
-\> b) but Identity Just is inferred to have type Identity (a ⊸
b). The limited subtyping doesn’t handle this case. In a perfect
world, Identity Just would have been elaborated to Identity (\\x -\>
Just x) :: Identity (a -\> b), but at the time where the application
Identity Just was type-checked, it was not known that Just should
be cast to an unrestricted arrow type.

### Example 2


The second problem is much more obvious in retrospect: there are
typeclasses defined on (-\>), and they can fail to apply when using
linear function.

```wiki
import Control.Category

Just . Just -- fails
```


The latter fails because there are no Category instance of (⊸). In
the case of Category, we can write an instance, but it’s not
necessarily the case that an instance for (-\>) will have a
corresponding instance for (⊸). Anyway, even if we have a Category
instance for (⊸), it is not clear what expression will
typecheck: probably of the following will fail

```wiki
import Control.Category

(Just :: _ -> _) . Just
Just . (Just :: _ -> _)
```

## Polymorphic Constructors


Simon quickly suggested a solution to these problems. To make the type of linear data constructors polymorphic, when they are used as terms (their type stays linear when they are used in patterns).

```wiki
(,) :: forall (p :: Multiplicity) (q :: Multiplicity). a ->@{p} b  ->@{q} -> (a, b)
```


Currently simplified as having a single multiplicity variable for the sake of a simpler implementation

```wiki
(,) :: forall (p :: Multiplicity). a ->@{p} b  ->@{p} -> (a, b)
```


We never infer multiplicity polymorphic arrows (like levity polymorphism). Any type variables which get to the top level are default to `Omega`. Thus, in most cases the multiplicity argument is
defaulted to `Omega` or forced to be `Omega` by unification. 

### Implementation


The way this is implemented is that every data constructor is given a wrapper with NO exceptions. Even internally used data types have wrappers for the moment which makes the treatment quite uniform. The most significant challenge of this endeavour was giving build in data types wrappers as previously none of them had wrappers. This led to the refactoring of `mkDataConRep` and the
introduction of `mkDataConRepSimple` which is a pure, simpler version of `mkDataConRep`. 


Once everything has a wrapper, you have to be quite careful with the difference between `dataConWrapId` and `dataConWorkId`. There were a few places where this used to work
by accident as they were the same thing for builtin types.  


In particular, functions like `exprConApp_maybe` are fragile and I spent a while looking there. 


Other uses can be found by grepping for `omegaDataConTy`. There are probably places where they can be removed by using `dataConWorkId` rather than `dataConWrapId`. The desugaring of `ConLikeOut` uses `dataConWrapId` though so anything in source syntax should apply the extra argument.


As another note, be warned that the serialisation for inbuilt tuples is different from normal data constructors. I didn't know this until I printed out the uniques - ad397aa99be3aa1f46520953cb195b97e4cfaabf


Otherwise, the implementation followed much the same path as levity polymorphism. 

## Typechecking


The internal representation of a multiplicity is called `Rig`. 

```wiki
data Rig = Zero                                                                    
         | One                                                                     
         | Omega                                                                   
         | RigAdd Rig Rig                                                          
         | RigMul Rig Rig                                                          
         | RigTy Type                                                              
  deriving (Data) 
```


Each constructor represents how many times a variable is allowed to be used. There are precisely two places where we check how often variables are used.

1. In `TcEnv.tc_extend_local_env`, which is the function which brings local variables into scope. Upon exiting a binder, we call `tcSubWeight` (via `check_binder`) to ensure that the variable usage is compatible with the declared multiplicity (if no multiplicity was declared, a fresh existential multiplicity variable is created instead).

  - `tcSubWeight` emits constraints of the form `π ⩽ ρ`, represented as `<TODO: GHC representation here>`.
1. In `tc_sub_type_ds`, In the `FunTy` case, we unify the arrow multiplicity which can lead to the unification of multiplicity variables.

  - `tc_sub_type_ds` emits constraints of the form `π = ρ`, represented as `<TODO: GHC representation here>`.


There are two useful functions in this dance. In order to use the normal unification machinery, we 

## Solving constraints


Constraint solving is not completely designed yet. The current implementation follows very simple rules, to get the implementation off the ground. Basically both equality and inequality constraints are treated as syntactic equality unification (as opposed, in particular, to unification up to laws of multiplicities as in the proposal). There are few other rules (described below) which are necessary to type even simple linear programs:

### The 1 \<= p rule


Given the current domain, it is true that `1` is the smallest element. As such, we assume `1` is smaller than everything which allows more functions to type check. 


This is implemented by making sure to call `subweight` on the weights before passing them to the normal unifier which knows nothing special about multiplicities. This can be seen at both
`tc_extend_local_env` and `tc_sub_type_ds`. At the moment we also get much better error messages by doing this short circuiting.

### Complex constraints


Multiplication and addition are approximated.

- A constraint of the form `p1 * p2 <= q` is solved as `p1 <= q` and `p2 <= q`. 
- A constraint of the form `p1 + p2 <= q` is solved as `Omega <= q`

### Specialisation


Unsolved multiplicity variables are specialised to ω by the function `<TODO: function name here, and possibly mechanism>`.
