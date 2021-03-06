## Vectorisation: The lifted case example


This example is taken from [http://www.cse.unsw.edu.au/\~chak/papers/LCK06.html](http://www.cse.unsw.edu.au/~chak/papers/LCK06.html).

## Flattening sums


We represent arrays of type

```wiki
data Either a b = Left a | Right b
```


essentially as

```wiki
instance (ArrayElem a, ArrayElem b) => ArrayElem (Either a b) where
  type [:Either a b:] = ([:Bool:], [:a:], [:b:])
```


For instance, `[:Left 5, Right 4, Left 2:]` will be represented by `([:True, False, True:], [:5,2:], [:4:])`. The boolean array (the *selector*) indicates whether the corresponding element is a `Left` or a `Right` one; the actual data is stored in two separate arrays.

## Case distinction (take 1)


For case distinction on `Either`, we have

```wiki
either :: (a -> c) -> (b -> c) -> Either a b -> c
either f g (Left  x) = f x
either f g (Right y) = g y
```


The interesting question, of course, is how this works in a data-parallel context, i.e., what semantics something like `mapP (either f g) xs` has. For the moment, we assume that `[:a -> b:] = [:a:] -> [:b:]`.

### Sometimes it works …


Let us first consider

```wiki
inc :: Int -> Int
inc n = n+1

good :: Either Int Int -> Int
good x = either inc id x
```


The term `mapP good [:Left 5, Right 4, Left 2:]` translates to `good^ (EitherArr [:True, False, True:] [:5,2:] [:4:])`; to see how this is evaluated, we have to derive the lifted version of `good^`. This is easy (we ignore the tupling of functions here):

```wiki
good^ :: [:Either Int Int:] -> [:Int:]
good^ xs = either^ inc^ id^ xs
```


We need the lifted versions of `either`, `inc` and `id`. The latter is trivial:

```wiki
id :: a -> a
id x = x

id^ :: [:a:] -> [:a:]
id^ xs = xs
```


We will omit the definition of `inc^` - it just increments every element in an array. The definition of `either^` is easily derived from its type:

```wiki
either^ :: ([:a -> c:])     -> ([:b -> c:])     -> [:Either a b:]           -> [:c:]
        :: ([:a:] -> [:c:]) -> ([:b:] -> [:c:]) -> ([:Bool:], [:a:], [:b:]) -> [:c:]   -- assuming [:a -> b:] = [:a:] -> [:b:]
either^ f g (bs, xs, ys) = combineP bs (f xs) (g ys)
```


Here, we apply `f` and `g` to the respective data arrays and the combine the results according to the selector. For instance, we have

```wiki
combineP [:True, False, True:] [:6,3:] [:4:] = [:6,4,3:]
```


and therefore

```wiki
  mapP good [:Left 5, Right 4, Left 2:]
= good^ ([:True, False, True:], [:5,2:], [:4:])
= either^ inc^ id^ ([:True, False, True:], [:5,2:], [:4:])
= combineP [:True, False, True:] (inc^ [:5,2:]) (id^ [:4:])
= combineP [:True, False, True:] [:6,3:] [:4:]
= [:6,4,3:]
```


This is precisely the expected result.

### ... but sometimes it doesn't


Now, let us consider a slightly more complex function:

```wiki
bad :: (Either Int Int, Int) -> Int
bad x = either ((+) (snd x)) id (fst x)
```


Again, we derive the lifted version of `bad`:

```wiki
bad^ :: [:(Either Int Int, Int):] -> [:Int:]
bad^ xs = either^ ((+^) (snd^ x)) id^ (fst^ x)
```


So, how is `mapP bad [:(Left 5, 1), (Right 4, 3), (Left 2, 7):]` evaluated? We have

```wiki
  mapP bad [:(Left 5, 1), (Right 4, 3), (Left 2, 7):]
= bad^ [:(Left 5, 1), (Right 4, 3), (Left 2, 7):]
= either^ ((+^) [:1,3,7:]) id^ [:Left 5, Right 4, Left 2:]
= either^ ((+^) [:1,3,7:]) id^ ([:True,False,True:], [:5,2:], [:4:])
= combineP [:True,False,True:] ((+^) [:1,3,7:] [:5,2:]) (id^ [:4:])
```


This program diverges when trying to evaluate `(+^) [:1,3,7:] [:5,2:]` since `(+^)` only works with arrays of the same length. Note that it is not enough to just throw away the last element of the first array like Haskell's `zipWith` would do; we really want to evaluate `(+^) [:1,7:] [:5,2:]`, i.e. throw away the 3.

## Case distinction (take 2)


So why doesn't it work? Simply because our definition of `either^` is wrong. In `either^ ((+^) [:1,3,7:]) id^ ([:True,False,True:], [:5,2:], [:4:])` two function arguments each contain one computation for *each* element of the `Either` array; it is the job of `either^` (and of case distinction in general) to select those which really should be applied. In particular, `either^` should filter the function arguments according to the selector. Without assuming `[:a -> b:] = [:a:] -> [:b:]`, it has the type

```wiki
either^ :: [:a -> c:] -> [:b -> c:] -> [:Either a b:] -> [:c:]
```


The real type is actually `[:a -> c:] -> [:(b -> c) -> Either a b -> c:]` but this is not important at the moment. So, `either^` has three array parameters; and since it is a lifted function, these arrays *must have the same length*. The correct definition is

```wiki
either^ fs gs (bs, xs, ys) = let fs' = packP bs fs
                                 gs' = packP (not^ bs) gs
                                 xs' = fs' $^ xs
                                 ys' = gs' $^ ys
                             in
                             combineP bs xs' ys'
```


Here, `$^` denotes the elementwise application of an array of functions to an array of arguments; `packP` filters an array according to a selector.


Let us see how this works out for our example. Now, the type of `(+^)` is

```wiki
(+^) :: [:Int:] -> [:Int -> Int:]
```


and we will denote the result of `f^ [:x1, ..., xn:]` by `[:f x1, ..., f xn:]`. Moreover, `bad^` should really be defined as

```wiki
bad^ :: [:(Either Int Int, Int):] -> [:Int:]
bad^ xs = either^ ((+^) (snd^ x)) (replicateP (lengthP xs) id) (fst^ x)
```


Note that where we previously lifted `id` to `id^`, we now *replicate* it to get a function array. Then, we have

```wiki
  mapP bad [:(Left 5, 1), (Right 4, 3), (Left 2, 7):]
= bad^ [:(Left 5, 1), (Right 4, 3), (Left 2, 7):]
= either^ ((+^) [:1,3,7:]) (replicateP 3 id) [:Left 5, Right 4, Left 2:]
= either^ [:(+) 1, (+) 3, (+) 7:] [:id, id, id:] ([:True,False,True:], [:5,2:], [:4:])
= let fs' = packP [:True,False,True:] [:(+) 1, (+) 3, (+) 7:]
      gs' = packP [:False,True,False:] [:id,id,id:]
      xs' = fs' $^ [:5,2:]
      ys' = gs' $^ [:4:]
  in
  combineP [:True,False,True:] xs' ys'
= let fs' = [:(+) 1, (+) 7:]
      gs' = [:id:]
      xs' = fs' $^ [:5,2:]
      ys' = gs' $^ [:4:]
  in
  combineP [:True,False,True:] xs' ys'
= let xs' = [:(+) 1, (+) 7:] $^ [:5,2:]
      ys' = [:id:] $^ [:4:]
  in
  combineP [:True,False,True:] xs' ys'
= combineP [:True,False,True:] [:6,9:] [:4:]
= [:6,4,9:]
```


This gives us the correct result.

## Arrays of functions


The crucial mistake in our first approach was to assume that arrays of functions can be represented by functions over arrays. This does not work because the latter do not (and cannot) support array operations. Unfortunately, it is not enough to simply forbid `[:a -> b:]` as the transformation introduces such types and sometimes needs to manipulate these arrays (as in `packP` above - `either^` is just an example of how we would translate `case` expressions).


So why can't we just use boxed arrays of functions? First, this kills performance. The main reason, however, is that this would break the semantics of the data-parallel model. The flattening transformation ensures that in the parallel case, all processes essentially execute the same code at the same time. This gives us very strong semantic guarantees; in particular, we can use collective operations for communication and synchronisation. This would not be the case if we could run unlifted code in a lifted context, which is necessarily the case with boxed function arrays. Explicit closures seem to be the only solution to this problem.
