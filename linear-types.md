
This page describes the motivation and a design for integration of linear types in GHC.

# The problem


The following scenarios can benefit greatly from adding first-class support for linear types in the language itself:

- Implementing latency sensitive real-time services and analytics jobs. In this use case a major issue is GC pauses, which can happen at arbitrary points for fairly long time periods. The problem is exacerbated as the size of the working set increases. The goal here is to partially or completely eliminate garbage collection by controlling aliasing and making memory management explicit (yet safe!).
- Intensive computations on large streams of input data, or on vectors. In this scenario a core issue is that the results of fusion via rewrite rules are hard to predict and hard to control by the programmer. This makes the allocation profile of a computation kernel hard to reason about, and performance problems hard to weed out. We want to ensure *predictable* fusion that is statically known to be *resource-invariant*: that is to say that is guaranteed to not change the resource usage profile.
- Modeling protocols between independent actors on one or more nodes. The point here is to statically mediate interactions between actors to ensure that these interactions happen according to an explicit protocol specification.


Crucially, these use cases all require fine-grained modeling of resource usage. Linear types provide the means for the programmer to inform the compiler about how resources are used at runtime, and conversely provide the means for the compiler to statically enforce strong invariants on resource usage (memory, channels, buffers, etc).

# Overview


We propose here plain linear types, directly inspired from linear logic. In particular we do not propose affine types, nor uniqueness types.

# Syntax


We propose to extend the language with

- weighted type annotations and
- a linear arrow

## Weighted type annotations


We propose to add the ability to annotate bindings with a weight. This weight corresponds to the number of times that a value exists at runtime (and, by linearity, the number of times that it must be used).

```
let x ::1Int
    x =1234in...
```


The weights can be either 1 or ω. (See semantics below)


Weights may be specified anywhere, including in record syntax. For example, the following declaration is valid:

```
dataTensorPair a b =TensorPair{
    first  ::1 a,
    second ::1 b
  }
```

## Linear arrow


As customary, we note the linear arrow `⊸`, and propose the ASCII encoding `-o` for it (stealing syntax). This new type operator would have the same fixity as the regular function arrow `(->)`.


example:

```
f::A⊸A
```


We propose to reuse the usual syntax for abstraction, patterns and application. Thus a linear identity function may be written as follows.

```
f=\(x ::1A)-> x
```


When they are not explicit, the compiler shall make a reasonable effort to infer weights. If the compiler should fail to infer a weight, ω will be picked. Thus one may equivalently write the following for the linear identity:

```
f::A⊸Af x = x
```

# Semantics


The idea is to track, in the context, the number of times that a variable is provided (and to be used). Linear variables (of weight 1) must be used exactly once; unrestricted variables (of weight ω) can be used any number of times, including none at all.


When performing a function call, the context used to check the argument is scaled by the weight of the function type. This concept can be expressed by the following rules:

```wiki
 Γ ⊢ f :ρ S ­> T     Δ ⊢ s :ω S
–––––––––––––––––––––––––––––––––– unrestricted application
          Γ+Δ ⊢ f s  :ρ  T

 Γ ⊢ f :ρ S ⊸ T     Δ ⊢ s :ρ S
–––––––––––––––––––––––––––––––––– linear application
          Γ+Δ ⊢ f s  :ρ  T

       Γ, x :ω S  ⊢   t :ρ T
------------------------------------- unrestricted abstraction
     Γ ⊢ λx. t  :ρ S → T

       Γ, x :1 S  ⊢   t :ρ T
------------------------------------- linear abstraction
     Γ ⊢ λx. t  :ρ  S ⊸ T
```


(The above two rules can be generalized by using a single arrow type with weights, say `Π(x :π A). B`)


This rule allows code which is linearly typed to 'scale' automatically, so that it can be used in unrestricted contexts.


For example, one may want to give the more precise type to the map function, using linearity, as follows:

```
map::(a ⊸ b)→[a]⊸[b]map f []=[]map f (x:xs)= f x : map f xs
```


Yet, given `y ::ω [a]`, we have `map f y ::ω [b]`. This means that we do not have to define several versions of `map` to take advantage of the new feature. In general, taking advantage of linearity does entail combinatorial explosion of the code.

### Subtyping


The expression `map (\x -> f x) y` always checks, but `map f y` checks only with the appropriate subtyping rule for arrows:

```wiki
   π' ⊆ π     A' ⊆ A      B ⊆ B'
----------------------------------------
   Π(x :π A). B  ⊆  Π(x :π' A'). B'

 and  ω ⊆ 1, ω ⊆ ω, 1 ⊆ 1
```

## Data types


In a way similar to application, pattern matching multiplies the weights.


Assume `x ::π D` and

```
dataD=C(y ::ρ B)case x ofC y ->_
```


In _ we have `x ::(π×ρ) B`


Let us see how this plays out for lists. Say we define

```
dataList a whereNil::List a
  Cons:: a ⊸List a ⊸List a
```


assume `xs ::π List a`. Then:

```
case xs ofCons x ys ->_
```


In the hole we have `x ::π a` and `ys ::π List a`


So, if `π=1` then we have the linear behavior; if `π=ω` then we have the usual unrestricted behavior. So, linear data types also scale in unrestricted contexts. The above definition for lists could completely replace the standard definition.

### Records


Handling projection functions is a bit subtle. Recall:

```
dataTensorPair a b =TensorPair{
    first  ::1 a,
    second ::1 b
  }
```


Given `p ::1 TensorPair a b`, we cannot have `first p ::1 a`, because then we would lose the second component. It is however fine to introduce `first :: Tensor a b -> a`. A similar issue occurs with records update, with the same solution.


We remark that having `x ::ω TensorPair a b` is semantically equivalent to having `x :: (a,b)`; thus it would make sense to let linearity be the default for weights in data types.

## Lazy pattern matching


Lazy pattern matching on a linear fields is disallowed. For example if we write

```
letTensorPair x y = t in...
```


Then we must have `(t ::ω TensorPair A B`, and not `(t ::1 TensorPair A B`. The reason for this limitation is that, essentially, accessing `x` and `y` correspond to using projection functions.

## Runtime semantics


In this proposal, we tackle only the type-checking aspects of linear types. Taking advantage of linearity in order to improve the performance of programs is left to future work, either as changes in the compiler on in libraries.

# Evaluating alternatives

## Why not monads?

### Scoped resource allocation


The standard solution for managing resources in Haskell is to use monads. In this subsection we compare the lightweight regions monad with linear types. This example is inspired by [Kiselyov and Chan](linear-types#regions).


Our representative example below concerns three files: `file1` and `file2` are known files; the first line of `file2` is the name of `file3`. The remaining lines of `file2` and the lines of `file1` are interleaved into `file3` (until one of the files runs out of lines, in the fashion of `zip`). Finally "Done" is printed in `file3`. We'll compare two implementations of this example: one with monadic regions and one with first-class linear types.


Note that one application of managing resource lifespans is doing away with a garbage collector entirely, since memory resources can be managed explicitly and safely manually. While this is not desirable in general, for latency sensitive applications moving some objects out of the purview of the GC (thus reducing the size of its working set) is key to getting predictable latencies.

#### Monadic regions


The following code is adapted from [Kiselyov and Chan](linear-types#regions). It uses the library developed in the paper. It uses a system of monadic regions to be able to close `file2` as soon as possible (*i.e.* when the sub-region introduced by `newRgn` ends).

```
do
  h1 <- newSHandle file1 ReadMode
  h3 <- newRgn $do
    h2 <- liftSIO (newSHandle file2 ReadMode)
    file3 <- shGetLine h2
    h3 <- liftSIO (newSHandle fname WriteMode)
    till (liftM2 (||)(shIsEOF h2)(shIsEOF h1))(shGetLine h2 >>= shPutStrLn h3 >>
       shGetLine h1 >>= shPutStrLn h3)
    return h3
  shPutStrLn h3 "Done"
```

#### Linear types


With linear types we can give a functional interface to streams. (Note that in the example below we keep the output as an old-style file, only the input streams are changed). In essence, linear types allow writing in the style of lazy effects, but without resource leaks due to late garbage collection.


Some of the types in this example API include `IO` in order to mix well with the IO-monad-based outputing. The type family {{{\\a::\* -\>
Context ⊸ IO (Stream ⊗ Context)}}} is, implicitly, the application of a linear state monad transformer with the `IO` monad (strictly speaking, linear state is not a monad transformer because it requires a stronger types for the underlying monad *e.g.*: `return :: a ⊸ m a`). It is important that while `open` and `close` are bound to `IO`, `read` is not and gives a pure interface to file traversal.

```
typeStreamread::Stream⊸Maybe(Text⊗Stream)open::Filename->Context⊸IO(Stream⊗Context)close::Stream⊸Context⊸IO(()⊗Context)instanceMonad(\a::*->Context⊸IO(a ⊗Context))
```

```
do
  s1 <- open file1
  s2 <- open file2
  letJust(s2,file3)= read s2
  lift $ withFile file3 WriteMode$\h3 ->do(s1,s2)<- interleave s2 s1 h3
    close s2
    close s1
    hPutStrLn h3 "Done"where interleave ::Stream⊸Stream⊸IO(Stream⊗Stream)
      interleave (read ->Just(s1,l1))(read ->Just(s2,l2))=do
        hPutStrLn h3 l1
        hPutStrLn h3 l2
        interleave s1 s2
      interleave s1@(read ->Nothing) s2 = return (s1,s2)
      interleave s1 s2@(read ->Nothing)= return (s1,s2)
```

#### Linear types, take 2


If instead of a simple type we have a functorial interface for streams supporting a `zip` function (note that this makes the `close` function more elaborate as it needs to close several physical files, see also [the work of Bernardy and Svenningsson](linear-types#linear-types-for-protocols-and-resources)), the example can be further simplified:

```
do
  s1 <- open file1
  s2 <- open file2
  letJust(s2',file3)= read s2
  let zipped = zip s2' s1
  lift $ withFile file3 WriteMode$\h3 ->do
    zipped <- mapM (\(l1,l2)-> hPutStrLn h3 l1 >> hPutStrLn h3 l2) zipped
    close zipped
    hPutStrLn h3 "Done"
```

#### Comparison


Disadvantages of the region-monadic approach include:

1. Monadic regions only work given a very rigid memory allocation discipline: one can control exactly when a resource gets freed, by introducing subregions. But all subregions must be well-nested, confining to a stack-like resource management discipline (you have to dispose of the last opened subregion first before closing the others). With linear types, we are free to deallocate whenever the programmer sees fit. The type system will point out if we forget to do so. It will furthermore statically rule out any possibility of use-after-free bugs (just like with memory regions).
1. One needs to work "within" the monad at all times. This means that in essence we work in an imperative language embedded in a functional one.
1. There is a risk of proliferation of resource-managing libraries for different kind of resources. Composing monads coming from different such libraries can prove cumbersome.


While regions can work adequately for management of coarse-grain ("large") resources, they still fall short outside of this use-case:

1. Managing the allocation of many small resources (such as run-of-the-mill memory allocations) is cumbersome with regions, due to extra encoding. Thus, regions cannot be seriously considered as a way to avoid GC in Haskell, except for large, seldom allocated memory regions.

## Why not encoded linear types?


Language extensions can very often be described as an embedded language in Haskell. This technique has also been proposed for linear types [Polakow](linear-types#encoding-of-linear-types). However, the embedding must be a deep one. This has the following unfortunate consequences:

1. Even a lightweight encoding requires at least one syntax symbol for each application and abstraction
1. Error messages are typically rather inscrutable
1. Crucially, encodings are an all-or-nothing proposition: existing Haskell libraries have to be re-implemented in the encoding to get the full benefit, rather than reused wholesale. This observation jeopardizes modularity: composing two packages would only be possible if they share the same encoding.


See for instance the following example given by [Polakow](linear-types#encoding-of-linear-types):

```
defn$
 llam $\f -> llam $\x -> llam $\y ->(f ^ x ^ y)&amp;(f ^ x)-- <interactive>:3:16: Could not deduce (MrgL ('Box : 'Box : 'Box : i1) 'False-- ('Elm ('S ('S v1)) : 'Box : 'Box : i1) 'False ('Box : 'Box : 'Box : i1))
```


It is very small, yet, the error message is already unhelpful. Beyond error messages, the syntactic overhead of the encoding as well as the fact that it does not mix well with existing Haskell code makes for rather cumbersome code, as the following comparison illustrates:

```
-- In Polakow's encodingbind::(s⊸(a⊗s))->(a⊸s⊸(b⊗s))->(s⊸(b⊗s))bind x f = defn $ llam $(s ->(llam $\as' -> letstar as' (\a s' -> f ^ a ^ s'))^( x ^ s ))-- With this proposalbind::(s⊸(a⊗s))->(a⊸s⊸(b⊗s))->(s⊸(b⊗s))bind x f s =let(a,s')= x s in f a s'
```

## Why not just stick to rewrite rules for fusion?


One notorious difficulty with rule-based fusion in practice is that it's very hard for the programmer to be certain that, given some expression `E` of an arbitrary composition of producers and reducers, whether intermediate allocations in `E` will be fused away or not. Rules opportunistically fire when they can, but for many applications, *not* firing is no different than a semantic bug: producing the right result too late due to extra allocations and therefore poor performance can be just as bad as producing none at all or an incorrect one.


Streaming frameworks often implement basic combinators as inlinable functions, so that fusion simply comes as a byproduct of inlining. But if you inline a producer naively, one can end up duplicating work, so it's not always safe to inline without blowing up resource usage. With linear types, one gets the guarantee that, when composing a producer with a consumer as in, inlining is completely safe. This is achieved by giving a consumer a linear type, as in:

```
f::B-o Cg::A->Bf. g
```


Here, the type of `f` guarantees that there will be only one use site for the output of `g`. The programmer who implements `f . g` gets a strong static guarantee that inlining is well-behaved.

# References

## Type system


The type-system suggested here is similar to that proposed by McBride in a draft paper available here: [ https://personal.cis.strath.ac.uk/conor.mcbride/pub/Rig.pdf](https://personal.cis.strath.ac.uk/conor.mcbride/pub/Rig.pdf)

## Regions

- Lightweight Monadic Regions, Oleg Kiselyov and Chung-chieh Shan [ http://okmij.org/ftp/Computation/resource-aware-prog/region-io.pdf](http://okmij.org/ftp/Computation/resource-aware-prog/region-io.pdf)

## Encoding of linear types

- Embedding a Full Linear Lambda Calculus in Haskell. Jeff Polakow. [ http://functorial.com/Embedding-a-Full-Linear-Lambda-Calculus-in-Haskell/linearlam.pdf](http://functorial.com/Embedding-a-Full-Linear-Lambda-Calculus-in-Haskell/linearlam.pdf)

## Motivation

- Lightweight Linear Types in System F◦, Karl Mazurak Jianzhou Zhao Steve Zdancewic. (The intro provides a survey of applications of linear types.)

### Linear types for memory management

- Yves Lafont. The linear abstract machine. Theoretical Computer Science, 59:157–180, 1988. Some corrections in volume 62 (1988), pp. 327–328.
- David Walker. Advanced Topics in Types and Programming Languages, chapter Substructural Type Systems. MIT Press, 2005

### Linear types for fusion

- Composable Efficient Array Computations Using Linear Types, Jean-Philippe Bernardy Víctor López Juan Josef Svenningsson [ http://www.cse.chalmers.se/\~josefs/publications/vectorcomp.pdf](http://www.cse.chalmers.se/~josefs/publications/vectorcomp.pdf)

### Linear types for protocols and resources

- Manuel Fahndrich, Mark Aiken, Chris Hawblitzel, Orion Hodson, ¨ Galen Hunt, James R. Larus, and Steven Levi. Language support for fast and reliable message-based communication in singularity os. SIGOPS Oper. Syst. Rev., 40(4):177–190, 2006.
- Kaku Takeuchi, Kohei Honda, and Makoto Kubo. An interaction-based language and its typing system. In Proceedings of PARLE’94, pages 398–413. Springer-Verlag, 1994. Lecture Notes in Computer Science number 817.
- On the Duality of Streams How Can Linear Types Help to Solve the Lazy IO Problem? Jean-Philippe Bernardy Josef Svenningsson [ https://jyp.github.io/pdf/Organ.pdf](https://jyp.github.io/pdf/Organ.pdf)