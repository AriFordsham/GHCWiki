# Concurrent programming in GHC


This page contains notes and information about how to write concurrent programs in GHC.


Please feel free to add stuff here (login **guest**, password **guest**).

## Starting points

- **Basic concurrency: forkIO and MVars**.  Read [ Tackling the awkward squad: monadic input/output, concurrency, exceptions, and foreign-language calls in Haskell](http://research.microsoft.com/Users/simonpj/papers/marktoberdorf/marktoberdorf.ps.gz).
  The [original paper about Concurrent Haskell](http://www.haskell.org/ghc/docs/papers/concurrent-haskell.ps.gz) contains quite a few examples about how to write concurrent programs.  A larger example is 

[ Writing High-Performance Server Applications in Haskell, Case Study: A Haskell Web Server](http://www.haskell.org/~simonmar/papers/web-server.ps.gz)

- **Software Transactional Memory** (STM) is a new way to coordinate concurrent threads. STM will be in GHC 6.6, and is described in the paper [ Composable memory transactions](http://research.microsoft.com/~simonpj/papers/stm/index.htm).  The paper [ Lock-free data structures using Software Transactional Memory in Haskell](http://research.microsoft.com/~simonpj/papers/stm/lock-free.htm) gives further examples of concurrent programming using STM.

- **Foreign function interface**.  If you are calling foreign functions in a concurrent program, you need to know about *bound threads*.  They are described in a Haskell workshop paper, [ Extending the Haskell Foreign Function Interface with Concurrency](http://research.microsoft.com/~simonpj/Papers/conc-ffi/index.htm).

## Using concurrency in GHC

- You get access to concurrency operations by importing the library [Control.Concurrent](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Concurrent.html).

- The GHC manual gives a few useful flags that control scheduling (not usually necessary) [RTS options](http://www.haskell.org/ghc/docs/latest/html/users_guide/sec-using-parallel.html#parallel-rts-opts).
