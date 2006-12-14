# Type Checking with Indexed Type Synonyms

## Background


GHC has now FC as its typed intermediate language.
In a next step, we wish to add type functions to
GHC's source language.  Type functions in combination
with type annotations and GADTs allow us to type check
some interesting programs.

```wiki
data Zero
data Succ n
data List a n where
  Nil  :: List a Zero
  Cons :: a -> List a m -> List a (Succ m)

type family Add :: * -> * -> *
type instance Add Zero     y = y
type instance Add (Succ x) y = Succ (Add x y)

append :: List a l -> List a m -> List a (Add l m)
append Nil xs = xs
append (Cons x xs) ys = Cons x (append xs ys)
```


However, type checking with type functions is challenging.

## The challenge


Consider the axioms

```wiki
forall a. S [a] = [S a]   (R1)
T Int = Int               (R2)
```


S and T are type functions of kind \*-\>\*
For convenience, I drop the \`redundant' forall a. on R1's lhs.


Suppose some type annotations/pattern matchings give rise
to the local assumptions

```wiki
T [Int] = S [Int]        (R3)
T Int = S Int            (R4)
```


and under these assumptions we need to verify

```wiki
T [Int] = [Int]
```


Logically, we can express the above as follows:

```wiki
(forall a. S [a] = [S a]) /\       -- axioms
(T Int = Int)

 |=

(T [Int] = S [Int]) /\             -- local assumptions
(T Int = S Int)

 implies

(T [Int] = [Int])                  -- (local) property
```


That is, any model (in the first-order sense) which is
a model of the axioms and local assumptions is also
a model of the property.


NOTE: There are further axioms such as reflexitivity of = etc.
We'll leave them our for simplicitiy.


The all important question:
How can we algorithmically check the above statement?
Roughly, we perform the following two steps.

1. Generate the appropriate implication constraint out of the program text.  That's easy cause GHC supports now implication constraints. (There are some potential subtleties, see GENERATEIMP below).
1. Solve the implication constraint by applying axioms and local assumptions until the (local) property is verified. That's the hard part.


NOTE: 


We assume that (implication) constraints consist of  equality constraints only. In general, we'll also find type class constraints. We ignore such constraints  for the moment.


In the following, we assume that symbols t refer to types and symbols C refer to conjunctions of equality constraints and Ax refers to an axiom set.


We'll restrict ourselves to simple implication constraints of the form `   C implies t1=t2 `
In general, implication constraints may be nested, e.g
` C1 implies (C2 implies C3) ` and may contain conjunctions
of implications, e.g. `C1 implies (F1 /\ F2)` where F1 and F2 are arbitrary implication constraints. Implication constraints may be universally quantified, e.g. 
` forall a (S a = T a implies ...) `
These universal quantifiers arise from universal type annotations, e.g.  ` f :: S a = T a => ....`, and
pattern matchings over data types with abstract components, e.g. data Foo where
` K :: S a = T a => a -> Foo`
We can operationally deal with universally quantified variables by skolemizing them (and we must ensure that skolemized/universal variables do not escape).


End of NOTE

## Various refinements of an approach to solve the challenge

- [A first (naive) attempt](type-functions-syn-tc/naive)
- [A second attempt](type-functions-syn-tc/second)
- [Type equations in GHC](type-functions-syn-tc/ghc)

## A first (naive) attempt


To solve  `(C implies t1=t2)` with respect to Ax

1. We interpret Ax /\\ C as a rewrite system (from left to right).
1. We exhaustively apply rewrite rules on t1 and t2, written t1 --\>\* t1' and t2 --\>\* t2' and check that t1' and t2' are syntactically equivalent.


Immediately, we find a problem with this solving strategy.
Consider our running example.


Rewrite rules

```wiki
(forall a. S [a] = [S a]) /\      (R1) -- axioms
(T Int = Int)                     (R2)

 /\

(T [Int] = S [Int]) /\            (R3) -- local assumptions
(T Int = S Int)                   (R4)
```


applied to `(T [Int] = [Int])`


yields

```wiki
T [Int] -->* [S Int]       (R3,R1)

[Int] -->* [Int]
```


Hence, our (naive) solver fails, but
clearly the (local) property  (T \[Int\] = \[Int\])
holds.


The trouble here is that

- the axiom system Ax is confluent, but
- if we include the local assumptions C, the combined system Ax /\\ C is non-confluent (interpreted as a rewrite system)


Possible solutions:


Enforce syntactic conditions such that Ax /\\ C is confluent.
It's pretty straightforward to enforce that Ax and
constraints appearing in type annotations and data types
are confluent. The tricky point is that if we combine
these constraints they may become non-confluent.
For example, imagine

```wiki
Ax : T Int = Int

   a= T Int      -- from f :: a=T Int => ...

      implies 

        (a = S Int -- from a GADT pattern

            implies ...)
```


The point is that only during type checking we may
encounter that Ax /\\ C is non-confluent!
So, we clearly need a better type checking method.

## A second attempt


To solve  `(C implies t1=t2)` with respect to Ax

1. First:

  1. We interpret Ax /\\ C as a rewrite system (from left to right)   and 
  1. perform completion until the rewrite system is confluent.
1. We exhaustively apply rewrite rules on t1 and t2, written t1 --\>\* t1' and t2 --\>\* t2' and check that t1' and t2' are syntactically equivalent.


Step (1.1) is new and crucial. For confluent rewrite systems the
checking step (2) will work fine (we also need termination of course).
But how do we now that completion will succeed?
The important condition is to guarantee that Ax is confluent (and
terminating) then completion will be successful (i.e. terminated
and produce a confluent rewrite system).


Let's take a look at our running example.

```wiki
(forall a. S [a] = [S a]) /\      (R1) -- axioms
(T Int = Int)                     (R2)

 /\

(T [Int] = S [Int]) /\            (R3) -- local assumptions
(T Int = S Int)                   (R4)
```


The axioms are clearly confluent
but there's a critical pair between (R2,R4).


Completion yields

```wiki
(S Int = Int)                     (R5)
```


Now, we can verify that (T \[Int\] = \[Int\])


by executing

```wiki
T [Int] -->* [Int]       (R3,R1,R5)

[Int] -->* [Int]
```


The completion method in more detail.

### There are two kinds of critical pairs

- Axiom vs local assumption, see (R2,R4) above
- Local assumption vs local assumption. For example,

  ```wiki
    T Int = S Int  /\ 
    T Int = R Int
  ```

  Completion yields

  ```wiki
    S Int = R Int
    R Int = S Int
  ```


NOTE: Axiom vs axiom impossible cause axiom set is confluent


Towards establishing a connection between completion and CHR derivation steps 


NOTE: 


There's a straightforward translation from type functions to constraints. For each n-ary function symbol T, we introduce a n+1-ary constraint symbol T. Thus, we can represent 
`T Int = Int`  as
` T Int a /\ a=Int`
For example, `T Int = S Int` is represented by 
`T Int a /\ S Int b /\ a=b`


We can verify that the completion method success by showing that each critical pair arises in the \`corresponding' CHR derivation (this derivation terminates if the axiom system is confluent and terminating, hence, we'll only encounter a finite number of critical pairs, hence, completion terminates).


Recall the critical pair (axioms vs local assumption) from above

```wiki
T Int = Int     -- axiom
T Int = S Int  -- local assumption
```


In the CHR world, we'll represent both as

```wiki
T Int a <==> a=Int         -- axiom turned into CHR

T Int b /\ S Int c /\ b=c  -- local assumption turned into CHR
                           -- constraints
```


In the CHR world, we find that

```wiki
    T Int b /\ S Int c /\ b=c
--> b=Int /\ S Int c /\ b=c      -- apply CHR
<--> b=Int /\ c=Int /\ S Int Int -- equivalence transformation
                                 -- apply mgu
```


directly corresponds to 

```wiki
S Int = Int
```


generated by our completion method


Recall the critical pair (local assumption vs local assumption)

```wiki
  T Int = S Int  /\ 
  T Int = R Int
```


represented in the CHR world as

```wiki
 T Int a /\ S Int b /\ a=b /\
 T Int c /\ R Int d /\ c=d
```


In the CHR world, we find that

```wiki
    T Int a /\ S Int b /\ a=b /\
    T Int c /\ R Int d /\ c=d
-->T Int a /\ S Int b /\ a=b /\  
   c=a /\ R Int d /\ c=d

      -- apply FD rule
      -- T a b /\ T a c ==> b=c

<--> T Int a /\ S Int a /\ R Int a /\
     a=b, c=a, d=a
```


directly corresponds to

```wiki
S Int = R Int
R Int = S Int
```


The general cases are as follows.

#### Axiom vs local assumption case

```wiki
forall as. (T t1 ... tn = s)  -- axiom

T t1' ... tn' = s'            -- local assumption
```


where exist phi, dom(phi)=as such that phi(ti) = ti' for i=1,...,n


completion yields

```wiki
    s' = phi(s)
    phi(s) = s'       
```


NOTE: We may need both orientation see above example.
We assume that symbol t refers to types NOT containing type functions and s refers to types which may contain type functions (can be lifted, more below)


Explaining completion in terms of CHRs.
Above translates to

```wiki
T t1 ... tn b <==> C

T t1' ... tn' b' /\ C'
```


where  s is translated to (C \| b) and s' is translated to (C \| b')


(see above where each type function type is represented by
a variable under some CHR constraints)


The type functions

```wiki
    s' = phi(s)
    phi(s) = s'       
```


resulting from completion 'appear' in the CHR derivation
(i.e. the operational effect is the same)

```wiki
     T t1' ... tn' b' /\ C'    -- apply CHR
--> b=b', phi(C) /\ C'
```

#### Local assumption vs local assumption

```wiki
T t1 ... tn = s1
T t1 ....tn = sn
```


completion yields

```wiki
  s1 = s2
  s2 = s1
```


In the CHR world, above represented by

```wiki
T t1 ... tn a /\ C1
T t1 ....tn b /\ C2
```


where s1 translated to (C1 \| a)

>
> s2 translated to (C1 \| n)


Then, 

```wiki
    T t1 ... tn a /\ C1 /\
    T t1 ....tn b /\ C2

--> FD rule step

    T t1 ... tn a /\ C1 /\
    a=b /\ [a/b] C2
```


Again, the operational effect of the type function generated
is also present in the CHR derivation


Lifting the restriction that t refers to types NOT containing
type functions (we only lift this restriction for
local assumptions).


Consider

```wiki
forall a. T [a] = [T a]      -- axiom

T [S Int] = s                -- local assumption
```


We can normalize

```wiki
T [S Int] = s
```


to

```wiki
T [b] = s
S Int = b
```


Method from above applies then.


NOTE: Regarding generation of implication constraints.
GENERATEIMP


The literate implication constraints generated out of the
program text may look as follows

```wiki
a=T Int implies ( a= S Int implies ...)
```


The above can be simplified to

```wiki
(a=T Int /\ a = S Int) implies ...
```


Before we proceed with the completion method, we first
need to apply some closure rules (eg. transitivity, left, right etc)
Hence, from the above we generatet

```wiki
   a=T Int /\ a = S Int /\ 
   T Int = a /\ S Int = a /\       -- symmetry
   T Int = S Int /\ S Int = T Int  -- transitivity
```


We omit the trival (reflexive) equations

```wiki
T Int = T Int /\ S Int = S Int 
```

---

## Special forms of type equations


We impose some syntactic restrictions on type instances and normalise equations arising during type checking to ensure that type checking remains (a) deterministic, (b) a simple rewrite model is sufficient to reduce type function applications, and (c) it hopefully remains decidable.

### Type variables


We have three kinds of type variables:

- *Schema variables*: These are type variables in the left-hand side of type equations that maybe instantiated during applying a type equation during type rewriting.  For example, in `type instance F [a] = a`, `a` is a schema variable.
- *Rigid variables*: These are variables that may not be instantiated (they represent variables in signatures and existentials during matching).
- *Wobbly variables*: Variables that may be instantiated during unification.

### Normal type equations

*Normal type equations*`s = t` obey the following constraints:

- *Constructor based*: The left-hand side `s` must have for form `F s1 .. sn` where `F` is a type family and the `si` are formed from data type constructors, schema variables, and rigid variables only (i.e., they may not contain type family constructors or wobbly variables).
- *Non-overlapping*: For any other axiom or local assumption `s' = t'`, there may not be any substitution *theta*, such that (*theta*`s`) equals (*theta*`s'`).
- *Left linear*: No schema variable appears more than once in `s`.
- *Decreasing*: The number of data type constructor and variables symbols occurring in the arguments of a type family occuring in `t` must be strictly less than the number of data type constructor and variable symbols in `s`.


Examples of normal type equations:

```wiki
data C
type instance Id a = a
type instance F [a] = a
type instance F (C (C a)) = F (C a)
type instance F (C (C a)) = F (C (Id a))
type instance F (C (C a)) = C (F (C a))
type instance F (C (C a)) = (F (C a), F (C a))
```


Examples of type equations that are *not* normal:

```wiki
type instance F [a] = F (Maybe a)            -- Not decreasing
type instance G a a = a                      -- Not left linear
type instance F (G a) = a                    -- Not constructor-based
type instance F (C (C a)) = F (C (Id (C a))) -- Not decreasing
```


Note that `forall a. (G a a = a) => a -> a` is fine, as `a` is a rigid variables, not a schema variable.


We require that all type family instances are normal.  Moreover, all equalities arising as local assumptions need to be such that they can be normalised (see below).  NB: With `-fundecidable-indexed-types`, we can drop left linearity and decreasingness.

### Semi-normal type equations


If an equation `s = t` does not contain any schema variables and is normal, except that it's left-hand side `F s1 .. sn` contains one or more type family constructors in the `si`, we call it *semi-normal*.

### Normalisation of equalities


Normalisation of an equality `s = t` of arbitrary type terms `s` and `t` (not containing schema variables) leads to a (possibly empty) set of normal equations, or to a type error.  We proceed as follows:

1. Reduce `s` and `t` to NF, giving us `s'` and `t'`.
1. If `s'` and `t'` are identical, we succeed (with no new rule).
1. If `s'` or `t'` is a rigid variable, we fail.  (Reason: cannot instantiate rigid variables.)
1. If `s'` or `t'` is a wobbly type variables, instantiate it with the other type (after occurs check).
1. If `s'` = `C s1 .. sn` and `t'` = `C t1 .. tn`, then yield the union of the equations obtained by normalising all `ti = si`.
1. If `s'` = `C1 ...` and `t' = C2 ...`, where `C1` and `C2` are different data type constructors, we fail.  (Reason: unfication failure.)
1. Now, at least one of `s'` and `t'` has the form `F r1 .. rn`, where F is a type family:

  - If `s' = t'` is normal, yield it.
  - If `t' = s'` is normal, yield it.
  - If `s' = t'` is semi-normal, yield it.
  - If `t' = s'` is semi-normal, yield it.
  - Otherwise, fail.  (Reason: a wobbly type variable, lack of left linearity, or non-decreasingness prevents us from obtaining a normal equation.  If it is a wobbly type variable, the user can help by adding a type annotation; otherwise, we cannot handle the program without (maybe) losing decidability.)


Rejection of local assumptions that after normalisation are either not left linear or not decreasing may lead to incompleteness.  However, this should only happen for programs that are invalid or combine GADTs and type functions in ellaborate ways.

---

## Maintaining type equations


The set of *given* equalities (i.e., those that we use as rewrite rules to normalise type terms) comprises two subsets:

- *Axioms*: The equations derived from type family instances.  They are the only equations that may contain schema variables, and they are normal for well-formed programs.
- *Local assumptions:* The equations arising from equalities in signatures and from GADT pattern matching after normalisation.


The set of axioms stays the same throughout type checking a module, whereas the set of local assumptions grows while descending into expressions and shrinks when ascending out of these expressions again.  We have two different sorts of local assumptions:

- *Normal assumptions*: These are not altered anymore once they have been added to the set of local assumptions, until the moment when they are removed again.
- *Semi-normal assumptions:* These are only added tentatively.  They are reconsidered whenever a new rule is added to the local assumptions, because a new rule may lead to further normalisation of semi-normal assumptions.  If a semi-normal assumption is further normalised, the original assumption is removed and the further normalised one added (which can again trigger subsequent normalisation).  NB: Strictly speaking, we can leave the original (semi-normal) equation in the set together with its further normalised version.
