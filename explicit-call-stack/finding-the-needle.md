# Finding the needle


This page documents the "finding the needle" idea for locating errors. It's a proposed lightweight way to selectively transform Haskell programs to include error location information. See the [ExplicitCallStack](explicit-call-stack) page for other approaches)


The basic idea is described below, but these links are actually more up to date:

- [Finding the needle](http://research.microsoft.com/~simonpj/papers/stack-trace/DebugTraces.pdf), a paper describing Tristan Allwood's intern project to implement explicit call stacks in GHC (May 2009).
- [ExplicitCallStack/StackTraceExperience](explicit-call-stack/stack-trace-experience): Some tests done on real programs with various tools for displaying stack traces. 
- [ExplicitCallStack/CorePassImplementation](explicit-call-stack/core-pass-implementation): MSR Internship Oct-Dec 2008 that provided a prototype implementation for this.

## The basic idea


(This stuff is still relevant, but pre-dates the paper, so read with care.)

1.  GHC's 'assert' magically injects the current file location.  But Template Haskell already allows you do to this rather nicely:

  ```wiki
  	...(f $currentLocation)...
  ```

  where

  ```wiki
    currentLocation :: Q Exp
    currentLocation = do { loc <- qLocation
                         ; return [| loc |] }
  ```

  This doesn't quite work today because `loc` has type `Language.Haskell.TH.Syntax.Loc`, a record of location information, and that isn't an instance of `Lift` (yet).  But the idea is basically fine: TH gives you access to the current source location.

1.  But that doesn't help with 'head'.  We want to pass head's *call site* to head. That's what jhc does when you give 'head' the a magic [SRCLOC_ANNOTATE pragma](http://repetae.net/computer/jhc/jhc.shtml):

  - every call to `head` gets replaced with `head_check $currentLocation`
  - in jhc, you get to write `head_check` yourself, with type

    ```wiki
    		head_check :: String -> [a] -> a
    ```

  It'd be nicer if you didn't have to write `head_check` yourself, but instead the compiler wrote it.

1.  But what about the caller of the function that calls head?  Obviously we'd like to pass that on too!

  ```wiki
  	foo :: [Int] -> Int
  	{-# SRCLOC_ANNOTATE foo #-}
  	foo xs = head (filter odd xs)
  ===>
  	foo_check :: String -> [Int] -> Int
  	foo_check s xs = head_check ("line 5 in Bar.hs" ++ s) xs
  ```

  Now in effect, we build up a call stack.  Now we *really* want the compiler to write `foo_check`.

1.  In fact, it's very similar to the "cost-centre stack" that GHC builds for profiling, except that it's explicit rather than implicit.  (Which is good.   Of course the stack should be a proper data type, not a String.)

>
>
> However, unlike GHC's profiling stuff, it is *selective*.  You can choose to annotate just one function, or 10, or all.  If call an annotated function from an unannotated one, you get only the information that it was called from the unannotated one:
>
>
> ```wiki
> 	foo :: [Int] -> Int   -- No SRCLOC_ANNOTATE
> 	foo xs = head (filter odd xs)
> ===>
> 	foo:: [Int] -> Int
> 	foo xs = head_check ("line 5 in Bar.hs") xs
> ```
>
>
> This selectiveness makes it much less heavyweight than GHC's currrent "recompile everything" story.
>
>

1. The dynamic hpc tracer will allow reverse time-travel, from an exception to the call site, by keeping a small queue of recently ticked locations. This will make it easier to find out what called the error calling function (head, !, !!, etc.), but will require a hpc-trace compiled prelude if we want to find places in the prelude that called the error. (A standard prelude would find the prelude function that was called that called the error inducing function).

1. The stack could thread entire ghci debugger 'frames' instead of just Strings. Such frames would have this aspect:

  ```wiki
       type Stack = [DebuggerFrame]
       data DebuggerFrame = DF {   ids     :: (Ptr [Id]) 
                                , locals :: [Locals] 
                                , srcloc :: SrcLoc }
       data Locals = forall a. Locals a
  ```

  The debugger can be extended to recognize bindings carrying this explicit stack and provide call stack traces in vanilla breakpoints. It would be possible then to fire the debugger in head_check by simply:

  ```wiki
      head_check :: Stack -> [a] -> a
      head_check stack [] = breakpoint (error "prelude: head")
  ```

  or make this the default behaviour for error, and ensure that this transformation always applies to it, so there will be a stack around for breakpoint:

  ```wiki
       error0 :: String -> a    -- behaves as the current error
                 
       error msg = breakpoint (error0 msg)

       head_check stack [] = error "prelude:head" 
  ```

  How realistic this is, I have no idea... but sounds good.

1. This is similar to numbers 2-4 above, but rather than having an implicit stack built up we annotate all the functions whose position we don't care about, and we are told the position of the most recent function which doesn't have such an annotation. Suppose `location` is a magic variable of a datatype `Location`, which might include source position, source span, lexical address (by which I mean, for `foo = let x = location in ...`, something like `["Main", "foo", "x"]`), and anything else that might be useful. Then

  ```wiki
  undefinedFunction = error ("Undefined here: " ++ showLocation location)
  ```

  would desugar to

  ```wiki
  undefinedFunction = error ("Undefined here: " ++ showLocation (Loc 3 ...)
  ```

  However, for `assert`, we want the location of the assertion, not the assertion function. So we annotate `assert` with a pragma to indicate that it is the location of the caller that we care about, so

  ```wiki
  {-# INVISIBLE_LOCATION assert #-}
  assert True  _ x = x
  assert False s _ = error ("Assert failed at " ++
                            showLocation location ++ ": " ++ s)

  {-# INVISIBLE_LOCATION fooAssert #-}
  fooAssert b s x = assert b ("Foo: " ++ s) x

  fun = fooAssert myBool "what myBool tests" myResult
  ```

  would desugar to

  ```wiki
  {-# INVISIBLE_LOCATION assert #-}
  assert _ True  _ x = x
  assert l False s _ = error ("Assert failed at " ++ 
                              showLocation l ++ ": " ++ s)

  {-# INVISIBLE_LOCATION fooAssert #-}
  fooAssert l b s x = assert l b ("Foo: " ++ s) x

  fun = fooAssert (Loc 10 ...) myBool "what myBool tests" myResult
  ```

  i.e. wherever you see `assert` or `fooAssert` you apply your location as the first argument, unless you are yourself at an invisible location in which case you just pass along your first argument.

## Open questions



Lots of open questions


- It would be great to use the exact same stack value for profiling.  Not so easy...for example, time profiling uses sampling based on timer interrupts that expect to find the current cost centre stack in a particular register.  But a big pay-off; instead of having magic rules in GHC to handle SCC annotations, we could throw the full might of the Simplifier at it.


  


- CAFs are a nightmare.  Here's a nasty case:

  ```wiki
    foo :: Int -> Int -> Int
    foo = \x. if fac x > 111 then \y. stuff else \y. other-stuff

    bad :: Int -> Int
    bad = foo 77
  ```

  How would you like to transform this?

## An example to consider


Suppose we are to transform the following code. The line numbers in comments are useful later when we consider what hat-stack does.

```wiki
   main = print d

   d :: Int
   d = e []                                      {- line 4 -}

   e :: [Int] -> Int
   e = f 10

   f :: Int -> [Int] -> Int
   f = \x -> case fac x < 10 of
                True  -> \_ -> 3
                False -> hd

   hd :: [a] -> a
   hd = \x -> case x of                          {- line 15 -}
                 [] -> error "hd: empty list"    {- line 16 -}
                 (x:_) -> x

   fac :: Int -> Int
   fac = \n -> case n == 0 of
                  True -> 1
                  False -> n * fac (n - 1)

```


People probably don't write code like this very much, but nonetheless, it does expose some of the issues we must deal with.


Here is a basic term-reduction for the program:

```wiki
   main => print d
        => print (e [])
        => print ((f 10) [])
        => print ((case fac 10 < 10 of { True -> \_ -> 3; False -> hd }) [])
        => .. some reductions of fac, and then select the False branch of the case ...
        => print (hd [])
        => print (error "hd: empty list")
        => <uncaught exception>
```


By the time that `hd` is called on the empty list, the only thing remaining on the dynamic evaluation stack is `print`.


The question is what stack would you *like* to see in this case?


One option is this:

```wiki
   main -> d -> e -> f -> hd
```


Another option is this:

```wiki
   main -> d -> hd
```


The difference between these two stacks is how we determine *when* `hd` is called. Is it in the context where `hd` is first mentioned by name (in the body of `f`), or is it when `hd` becomes fully saturated (in the body of `d`)? Both contexts seem reasonable. Does it really matter which one is chosen? At the moment I can't say for sure.



There are more possibilities, for instance, we could treat CAFs as roots of the stack, thus dropping `main` and `d` from the first of the options above:

```wiki
   e -> f -> hd
```


or by dropping main from the second of the options above:

```wiki
   d -> hd
```


(which is, as we shall see, incidentally what hat-stack does (so it is not unreasonable)).

## What does Hat do?


As a starting point it is useful to see what Hat does, in particular hat-stack, which is the tool for generating stack traces. The example of "`hd []`" is exactly the kind of problem that hat-stack is designed to tackle.


Here is the stack trace generated for the example program. You can see the relevant line numbers in comments in the source code above. I've added comments to the hat output on the RHS to emphasise what the entries mean, when it is not immediately clear.

```wiki
   Program terminated with error:
           hd: empty list
   Virtual stack trace:
   (unknown)       {?}
   (H.hs:16)       error "hd: empty..."
   (H.hs:15)       (\..) [] | case []        {- case analysis of [] in hd -}
   (H.hs:4)        (\..) []                  {- (body of) hd applied to [] in the body of d -}
   (unknown)       d
```


More-or-less this stack resembles:

```wiki
   d -> hd 
```


Curiously, if we change the definition of `hd` to a function binding instead of a pattern binding we get this stack trace instead:

```wiki
   Program terminated with error:
           hd: empty list
   Virtual stack trace:
   (unknown)       {?}
   (I.hs:16)       error "hd: empty..."
   (I.hs:15)       hd [] | case []
   (I.hs:4)        hd []
   (unknown)       d
```


Which is a little clearer, but still represents:

```wiki
   d -> hd
```


So hat-stack considers the context in which a function is saturated to be the place where it is called. To make this even more apparent we can eta-expand `e`:

```wiki
   e :: [Int] -> Int
   e x = f 10 x
```


Now we get this stack trace:

```wiki
   Program terminated with error:
           hd: empty list
   Virtual stack trace:
   (unknown)       {?}
   (J.hs:16)       error "hd: empty..."
   (J.hs:15)       hd [] | case []
   (J.hs:7)        hd []
   (J.hs:4)        e []
   (unknown)       d
```


Which is:

```wiki
   d -> e -> hd
```


This is because `hd` now becomes saturated in the body of `e`.

## Transformation rules


The key issues are:

- **Higher-order calls**. Where does a partially applied function receive its stack trace from? Possible options include:

  1. The lexical call site (corresponding to where the function is mentioned in the source code).
  1. The context in which the function receives a particular argument, for instance the one where it is saturated.  

- **CAFs**. The problem with CAFs is that, at least for expensive ones, we want to preserve the sharing of their evaluation. Therefore we cannot simply extend them into functions which take a stack trace as an argument - this would cause the CAF to be recomputed at each place where it is called. The simplest solution is to make CAFs the roots of call stacks, but it seems like there will be situations where it would be useful to know more about the context in which a CAF was evaluated.

- **Recursion**. Obviously the call stack will grow in size proportional to the depth of recursion. This could lead to prohibitive space usage, thus it is desirable that the size of the stack be kept within reasonable bounds. We will probably need some way to dynamically prune the stack.

- **Extent**. It is desirable to have a scheme where only some functions in the program are transformed for stack tracing, whilst others remain in their original form. We want to avoid the all-or-nothing situation, where the whole program has to be recompiled before tracing can be done. For example, with the current state of profiling in GHC, the whole program has to be recompiled, and special profiling libraries must be linked against. This is a nuisance which reduces the usability of the system. Similar problems occur with other debugging tools, such as Hat and buddha, and this really hampers their acceptance by programmers.

### An abstract syntax


For the purpose of exploring the rules we need an abstract syntax. Below is one for a simple core functional language:

```wiki
   Decls(D)        -->   x :: T   |   x = E   |   data f a1 .. an = K1 .. Km

   Constructors(K) -->   k T1 .. Tn

   Types(T)        -->   f   |   a   |   T1 T2

   Expressions(E)  -->   x   |   k   |   E1 E2   |   let D1 .. Dn in E   |   case E of A1 .. An   |   \y1 .. yn -> E

   Alts(A)         -->   p -> E

   Pats(P)         -->   x   |   k P1 .. Pn
```

### Stack representation


For simplicity we assume:

```wiki
   type Stack = [String]
```


which is just a list of function names.

### Notation


Double square brackets denote the transformation function, which has two arguments. The first argument is denoted inside the brackets and it is the syntactic object that we are transforming. The second argument is a stack value.
For instance:

```wiki
   [[E ]]_k
```


means transform expression E with k as the current stack value. When we wish to ignore the current stack value (because it is not meaningful in a certain context, such as the top-level declarations) we write:

```wiki
   [[E ]]_?
```

### Transformation option 1


This is probably the simplest transformation style possible. Stack traces are passed to (let bound) functions at their lexical call sites, which correspond to the places where the function is mentioned in the source code. Pattern bindings are treated as roots of stacks, so only function bindings receive stack arguments. In this transformation we can get away with simply passing one stack argument for each function, regardless of how many regular arguments it has. In contrast, other transformation styles
might pass one stack argument for every regular argument of the function.

```wiki
Declarations (top level):

   [[x :: T ]]_?                     ==>   x :: Trace -> T     , x is function bound, and transformed for tracing
   
   [[x :: T ]]_?                     ==>   x :: T              , x does not match the above rule

   [[x = \y1 .. yn -> E ]]_?         ==>   x = \t y1 .. yn -> [[E ]]_("x":t)       , x is transformed for tracing

   [[x = \y1 .. yn -> E ]]_?         ==>   x = \y1 .. yn -> [[E ]]_["x"]           , x is not transformed for tracing

   [[x = E ]]_?                      ==>   x = [[E ]]_["x"]

   [[data f a1 .. an = K1 .. Km ]]_? ==>   data f a1 .. an = K1 .. Km

Declarations (local):

   [[x = E ]]_t                      ==>   x = [[E ]]_("x":t) 

   (all other local decls are the same as top level rules)

Expressions:

   [[x ]]_t                          ==>   x t    , x is function bound, and transformed for tracing

   [[x ]]_t                          ==>   x      , x does not match the above rule

   [[k ]]_t                          ==>   k

   [[E1 E2 ]]_t                      ==>   [[E1 ]]_t [[E2 ]]_t

   [[let D1 .. Dn in E ]]_t          ==>   let [[D1 ]]_t .. [[Dn ]]_t in  [[E ]]_t

   [[case E of A1 .. An ]]_t         ==>   case [[E ]]_t of [[A1 ]]_t .. [[An ]]_t

   [[\y1 .. yn -> E ]]_t             ==>   \y1 .. yn -> [[E ]]_t

Alternatives:

   [[p -> E ]]_t                     ==>   p -> [[E ]]_t 
```


In the above rules a variable is function bound if it is defined in the form "`x = \y1 .. y n -> E`". In other words it
is a syntactic property of the way the variable is defined, rather than a type property. In contrast, a variable is pattern bound if it is defined in the form "`x = E`", where `E` is *not* a lambda abstraction. Using a syntactic rule can lead to some potentially confusing results, as we shall see below. Note, however, that tools like Hat and Buddha do use a syntactic rule like this (you can see from the hat example above that eta expanding a definition can change the stack trace). So there is at least some precedent for using a syntactic rule. However, it must be said that the choices made by Hat and buddha are driven by the desire to avoid having to type-check the program first. Inside GHC this is not a concern, and it may be prudent to take advantage of type information.


An advantage of this transformation style is that it handles combinations of transformed and untransformed functions easily. When variable expressions are transformed we simply check to see if the variable corresponds to a transformed function. If it does, we pass it the current stack value as an argument, otherwise we don't.


If you apply this transformation to the example above you get:

```wiki
   main = print d

   d :: Int
   d = e []

   e :: [Int] -> Int
   e = f ["e"] 10

   f :: Trace -> Int -> [Int] -> Int
   f = \t x -> case fac ("f":t) x < 10 of
                  True  -> \_ -> 3
                  False -> hd ("f":t)

   hd :: Trace -> [a] -> a
   hd = \t x -> case x of
                   [] -> error ("hd: empty list " ++ show t)      {- simple hack so we can see the stack -}
                   (x:_) -> x

   fac :: Trace -> Int -> Int                             
   fac = \t n -> case n == 0 of
                    True -> 1
                    False -> n * fac ("fac":t) (n - 1)            {- no stack pruning, though there ought to be -}
```


When you run this program you get:

```wiki
   Program error: hd: empty list ["f","e"]
```


Which is essentially:

```wiki
   e -> f -> hd
```

*Note*: this is different to the stack trace given by hat-stack above.


A problem with this transformation style is that it is sensitive to the position of lambdas in the body of a declaration. For example, it transforms these two functions differently, even though they are semantically equivalent:

```wiki
   f1 = let x = EXP in (\y -> head (foo x))

   f2 = \y -> head (foo (let x = EXP in x))
```


Here is the output of the two different transformations:

```wiki
   f1 = let x = EXP in (\y -> head ["f1"] (foo ["f1"] x))

   f2 = \t y -> head ("f2":t) (foo ("f2":t) (let x = EXP in x))
```


Notice that in the first case the stack passed to `head` and `foo` is simply `["f1"]`, but in the second case it is `"f2":t`. One *might* expect the same stack trace to be generated for each declaration. This might be particularly annoying/confusing in cases where people use a point free style of programming. Consider these two definitions:

```wiki
   g1 = foo . bar . goo

   g2 = \x -> foo (bar (goo x))
```


These are largely the same definition, just written in different style. One might expect them to result in the same stack trace. But the above transformation rule treats them differently, because the `g1` is a pattern binding, whereas `g2` is a function binding.

## Issue with type classes and instances


One problem with a selective transformation scheme is how to transform overloaded functions. Suppose we have the following type class:

```wiki
   class Wibble t where
      wibbit :: t -> t -> (t, t)
      wabbit :: t -> Bool
```


And we have these instances:

```wiki
   instance Wibble Int where
      wibbit x y = ...
      wabbit x = ...

   instance Wibble MyType where
      wibbit x y = ...
      wabbit x = ...
```


We might like to trace the wibbit function, but only for the `MyType` instance, and not the existing instances. The problem is that we only want the `MyType` version to receive a stack argument, whereas we don't want to give a stack argument to the other versions of wibbit. Because wibbit is overloaded we run into trouble, since all instances of wibbit must have the same type scheme (if one instance needs a stack argument, then they all need a stack argument).
