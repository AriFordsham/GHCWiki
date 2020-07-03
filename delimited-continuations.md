# Adding delimited continuations primops to Haskell


This page contains information on the Delimited continuation primops Proposal.


## Discussion

The Delimited continuation primops Proposal is being discussed [on the ghc proposal \#313](https://github.com/ghc-proposals/ghc-proposals/pull/313).


## Source material

Some motivating examples in the video: [Alexis King, Effects for Less,  ZuriHac 2020](https://www.youtube.com/watch?v=0jI-AlWEwYI)


## Implementation

The implementation in progress can be found at [https://gitlab.haskell.org/lexi.lambda/ghc/-/tree/first-class-continuations](https://gitlab.haskell.org/lexi.lambda/ghc/-/commits/first-class-continuations) .


## How delimited continuations work

See [Alexis King's mail](https://mail.haskell.org/pipermail/ghc-devs/2020-July/019016.html).

Delimited continuations allow capturing slices
of the call stack and restoring them later. For example, the program

```haskell
    do y <- prompt $ do x <- control0 $ \k -> k (pure 10)
                        pure (x + 5)
       print y
```

will print 15. To understand what’s happening operationally, we can
imagine an abstract call stack made up of continuation frames:

```
    ┌──────────┐
    │  ● + 5   │    redex: control0 $ \k -> k (pure 10)
    ├──────────┤
    │ prompt ● │
    ├──────────┤
    │ print ●  │
    ├──────────┤
    │   ...    │
    ├──────────┤
```

Here, each ● represents the “hole” where the evaluated result of the
redex will be returned. `control0` moves all the frames between the
top of the stack and the first `prompt` into the heap and returns a
reference to them, so after a single reduction step, we have

```
    ┌──────────┐
    │ print ●  │    redex: k1 (pure 10)
    ├──────────┤    heap:  k1 = ┌──────────┐
    │   ...    │                │  ● + 5   │
    ├──────────┤                └──────────┘
```

When a continuation is applied, its stored stack frames are copied
back onto the top of the current stack, and the argument becomes the
new redex:

```
    ┌──────────┐
    │  ● + 5   │    redex: pure 10
    ├──────────┤
    │ print ●  │
    ├──────────┤
    │   ...    │
    ├──────────┤
```

Now it should hopefully be clear how we end up printing 15.
