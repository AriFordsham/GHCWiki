
As part of his final year work at Cambridge, Max Bolingbroke is working on implementing the "Comprehensive Comprehensions" described in a paper [ available here](http://research.microsoft.com/~simonpj/papers/list-comp/index.htm) in GHC. This page will contain notes on the implementation as it unfolds.

## Bracketing Syntax


Due to the generality added to comprehensions by the paper, it now makes sense to allow bracketing of qualifiers. An example from the paper is:

```wiki
xs = [1,2]
ys = [3,4]
zs = [5,6]

p1 = 
  [ (x,y,z)
  | ( x <- xs
    | y <- ys )
  , z <- zs ]

p2 = 
  [ (x,y,z)
  | x <- xs
  | ( y <- ys
    , z <- zs ) ]
```


This results in:

```wiki
p1 = [(1,3,5), (1,3,6), (2,4,5), (2,4,6)]
p2 = [(1,3,5), (2,3,6)]
```


Unfortunately, there is a practical problem with using brackets in this way: doing so causes a reduce/reduce conflict in the grammar. Consider this expression:

```wiki
[foo | (i, e) <- ies]
```


When the parser reaches the bracket after "e" it is valid to either reduce "(i, e)" to a pair of qualifiers (i.e. i and e are treated as guard expressions), OR to reduce it to the tuple expression (i, e) which will be later converted to a pattern. There are a number of alternative ways we could solve this:

- Disallow bracketing of qualifiers altogether!

  - This keeps the concrete syntax simple and should cover all common use cases
  - It does reduce the composability of the qualifier syntax rather drastically however
- Keep bracketing in this manner but use type information to resolve the ambiguity

  - I will need to change the parser to consider qualifiers as expressions so that we can parse without any reduce/reduce conflicts
  - We can then always use type information to determine which reading is correct, because guards are always boolean, and so can be distinguished from tuples as required
  - Might have negative implications on the readability of some error messages :(
  - If the parser finds it hard to understand this syntax, you can argue that any human reader would too and hence we should look for something less ambiguous
- Introduce new syntax to allow this idiom to be expressed unambiguously. Some examples of what we could use are below:

```wiki
-- 1) A new keyword
[ foo | x <- e,
        nest { y <- ys,
               z <- zs },
        x > y + 3 ] 

-- 2) Trying to suggest pulling things out of a sublist without having to mention binders
[ foo | x <- e,
        <- [ .. | y <- ys,
                  z <- zs ],
        x > y + 3 ]

-- 3) New kind of brackets
[ foo | x <- e,
        (| y <- ys,
           z <- zs |),
        x < y + 3 ]

-- 4) Variation on 2), slightly more concise
[ foo | x <- e,
        <- [ y <- ys,
             z <- zs ],
        x > y + 3 ]

-- 5) Another variation on 2), moving the ".." into the pattern rather than the comprehension body
[ foo | x <- e,
        .. <- [ y <- ys,
                z <- zs ],
        x > y + 3 ]
```

## Ordering Syntax

TODO

## Extending To Arbitrary Monads


On the [ paper talk page](http://haskell.org/haskellwiki/Simonpj/Talk:ListComp), Michael Adams has outlined how the new idioms could be extended to arbitrary monads. It looks very nice theoretically, but before we consider actually implementing this we need to know if anyone has a use case for the syntax. To demonstrate the kind of thing that this would make possible, consider the following example from Michael:

```wiki
do a <- ma
   ...
   b <- mb
   c <- mc
   sort by (b, c) using foo
   d <- md
   ...
   return (a, b, c, d)
```


It would de-sugar to:

```wiki
((do a <- ma
   ...
   b <- mb
   c <- mc
   return ((b, c), (a, b, c))
) `foo` fst) >>= \result ->
do let (a, _, _) = result
       (_, b, _) = result
       (_, _, c) = result
   d <- md
   ...
   return (a, b, c, d)
```


Where we have:

```wiki
foo :: forall a. (a -> t) -> m a -> m a
```