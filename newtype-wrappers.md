
#### Page Contents


1. 
1. 
      [Newtype wrappers](#Newtypewrappers)
      

  1. 
  1. 
            [The problem](#Theproblem)
          
  1. 
  1. 
            [Goal](#Goal)
          
  1. 
  1. 
            [Use cases](#Usecases)
          
  1. 
  1. 
            [The implementation](#Theimplementation)
          
  1. 
  1. 
            [Discussion of alternatives](#Discussionofalternatives)
            

    1. 
    1. 
                  [Proposal 1](#Proposal1)
                
    1. 


          
  1. 
  1. 
            [Proposal 2](#Proposal2)
          
  1. 
  1. 
            [Proposal 3](#Proposal3)
          
  1. 


    
1. 




# Newtype wrappers


This page proposes **newtype wrappers**, a new feature for Haskell
indended to make newtypes more flexible and useful.  It tackles head-on 
the problem underlying #7542 and #2110.


Email thread [here](http://www.haskell.org/pipermail/glasgow-haskell-users/2013-January/023455.html).

## The problem


Suppose we have

```wiki
newtype Age = MkAge Int
```


Then if `n :: Int`, we can convert `n` to an `Age` thus: `MkAge n :: Age`.
Moreover, this conversion is a type conversion only, and involves no runtime
instructions whatsoever.  This cost model -- that newtypes are free -- is important
to Haskell programmers, and encourages them to use newtypes freely to express
type distinctions without introducing runtime overhead.


Alas, the newtype cost model breaks down when we involve other data structures.
Suppose we have these declarations

```wiki
data T a   = TLeaf a     | TNode (Tree a) (Tree a)
data S m a = SLeaf (m a) | SNode (S m a) (S m a)
```


and we have these variables in scope

```wiki
x1 :: [Int]
x2 :: Char -> Int
x3 :: T Int
x4 :: S IO Int
```


Can we convert these into the corresponding forms where the `Int` is replaced by `Age`?
Alas, not easily, and certainly not without overhead.  

- For `x1` we can write `map MkAge x1 :: [Age]`. But this does not follow the newtype cost model: there will be runtime overhead from executing the `map` at runtime, and sharing will be lost too.  Could GHC optimise the `map` somehow?  This is hard; apart from anything else, how would GHC know that `map` was special?  And it gets worse.

- For `x2` we'd have to eta-expand: `(\y -> MkAge (x2 y)) :: Char -> Age`.  But this isn't good either, because eta exapansion isn't semantically valid (if `x2` was bottom, `seq` could distinguish the two).  See #7542 for a real life example.

- For `x3`, we'd have to map over `T`, thus `mapT MkAge x3`.  But what if `mapT` didn't exist?  We'd have to make it. And not all data types have maps. `S` is a harder one: you could only map over S-values if `m` was a functor.  There's a lot of discussion about this on #2110.

## Goal


In summary: The programmer expects zero-cost conversions between a newtype *N* and the type *T* it is based on. We want to allow the programmer to have zero-cost conversions between *C N* and *C T*. Requirements:

- It should be sound, i.e. have an implementation in Core with existing coercions, without new coercions or `unsafeCoerce`.
- It should respect module boundaries, i.e. not allow the user to create a function which he could not write using plain Haskell (and non-zero cost).
- (desirable:) It should be exportable and composable, i.e. the module of *N* should be able to specify that a conversion between *N* and *T* is allowed (or not), and the module of *C* should be able to specify if a coercion can be lifted (or not), and if both allow it, the user should be able to combine that even if the modules of *N* and *T* are independent.
- (desirable:) It should be possible to use such a coercion in one module in internal code, but not export it.
- (desirable:) It should be possible to add REWRITE rules that turn, for example, `map N` into the zero-cost conversion from *C T* to *C N*.

## Use cases


To clarify these requirements, here some benchmarks; feel free to expand if you have further needs.

- Allow the user to define a function of type `C N -> C T` with a zero-cost model.
- Allow the author of `Data.Set` prevent a coercion `Set N -> Set T` (as it would break internal invariants).
- Allow the author of a escaping-ensuring `newtype HTML = HTML String` to use the coercion `[String] -> [HTML]` internally, but prevent the user of the library from doing that.

## The implementation


Core already had provided all the necessary feature; the question was just how to offer it on the Haskell level. The implementation comes in form of a `coerce :: Coercible a b -> a -> b` and a type class `Coercible` that relates two types if they have the same representation, i.e. can be related by a coercion of role Representational (see [Roles](roles)). See the haddock documentation for `coercible` for user-level documentation and [Note \[Coercible Instances](https://ghc.haskell.org/ghc/ghc/blob/master/compiler/typecheck/TcInteract.lhs#L2013)\] for information on the implementation.


The implementation fulfills the first goal, the second partly (`C N -> C T` is allowed even without `C`'s data constructors in scope; if `C` should be abstract the role of its argument needs to be `Nominal`). Due to the ad-hoc nature of the `Coercible` instances, the second and third goal are not achieve. No work towards the fifths goal has been done.

## Discussion of alternatives

### Proposal 1


Top level declarations of coercions as functions (e.g. `newtype wrap w1 :: [Int] -> [Age]` and `newtype wrap w2 :: (Char -> Int) -> (Char -> Age)`).


This is close to the actual implementation, but requires giving names to the coercion and introduces new syntax.

## Proposal 2


(This is basically the current implementation, advocated by Roman Cheplyaka.)

## Proposal 3


Given a module providing

```wiki
data NT a b -- abstract
coerce :: NT a b -> a -> b
refl   :: NT a a
sym    :: NT a b -> NT b a
trans  :: NT a b -> NT b c -> NT a c
```


and additional code to derive stuff like

```wiki
deriving nt :: NT N T
deriving listNT :: NT a b -> NT [a] [b]
```


One problem with this approach is that if the user can use arbitrary Haskell to mange the `NT` values, he can create bottoms. Also, additional syntax is required. It was [argued](http://www.haskell.org/pipermail/ghc-devs/2013-July/001667.html) that the benefits over the type-class apporoach (Approach 2) do not warrant the extra syntactical complexity.
