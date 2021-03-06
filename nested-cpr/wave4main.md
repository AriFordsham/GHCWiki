## wave4main


Baseline: \[0e2fd3/ghc\], Tested: nested-cpr (without nesting inside sum-types, without join-point detection).


Found a 11% increase in allocation, around `9000000` bytes.



The most obvious change in ticky-ticky-number are:


- `FUNCTION ENTRIES` and `ENTERS` increasing by \~100000
- `RETURNS` doubling from 140745 to 280795
-  `ALLOC_FUN_ctr` and `ALLOC_FUN_gds` almost doubling, by \~18000 resp. 9000000


So we are allocating more function closures. First guess: Join point property destroyed somewhere.


The ticky output shows a `$wgo{v s60k} (main:Main)` appearing that was not there before, with `140016` enters and `23522688` allocations. This appears in `$wtabulate`, and indeed corresponds to a `go1` that is a join-point before. So what is happening? We are changing

```wiki
go1 [Occ=LoopBreaker]                                      
  :: GHC.Prim.Int#                                         
     -> GHC.Prim.State# s                                  
     -> (# GHC.Prim.State# s, GHC.Arr.Array GHC.Types.Int x #)
```


to

```wiki
$wgo [Occ=LoopBreaker]          
  :: GHC.Prim.Int#
     -> GHC.Prim.State# s
     -> (# GHC.Prim.State# s,   
           GHC.Prim.Int#,       
           GHC.Prim.Int#,       
           GHC.Prim.Int#,       
           GHC.Prim.Array# x #) 
```

`go1` is recursive, but tail-recursive, so the worker and wrapper indeed cancel for the recursive call. But where it is being used, we simply apply the `Array` constructor to the second component. So nothing is gained, but a join-point is lost.


My attempt below to detect join points does not help: The CPR information for `go1` is the same as for `let go1 = rhs in body`, as `body` is just `go1 ww ipv`.


The problem is that this is being passed as an argument to a function (in this case `runSTRep`), and there is not much that can be done about this at this point.

## Summary with simple expressions


Original code:

```wiki
f a x = case a of
  True -> case foo of b -> foo $
    let go 0 = (1,(2,3))
        go n = go (n-1)
    in go b
  False -> undefined
```


which after CPR transformation yields, where `$go` is no longer a join-point for the argument to `snd`.

```wiki
f a x = case a of
  True -> case foo of b -> foo $
    let $wgo 0 = (# 1, 2, 3 #)
        $wgo n = go (n-1)
    in case $wgo b of (# a, b, c #) -> (a, (b,c))
  False -> undefined
```