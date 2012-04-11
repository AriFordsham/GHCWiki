## Cloud Haskell


Cloud Haskell is a distributed computing framework for Haskell, implemented in Haskell. It's a tool for writing applications that coordinate their work on a cluster of commodity computers or virtual machines. This is useful for providing highly reliable, redundant, long-running services, as well as for building compute-intensive applications that can benefit from lots of hardware. It has two interfaces:

- The *process layer* (aka [ErlangInHaskell](erlang-in-haskell)): an interface based on message-passing between distributed processes.
- The *task layer* (aka SkywritingInHaskell): a fault-tolerant data-centric interface.


Here are some resources relevant to this project:

- [ A paper on the process layer](http://www.cl.cam.ac.uk/~jee36/remote.pdf)
- [ Haddock documentation](http://www.cl.cam.ac.uk/~jee36/remote/)
- [ The source repository](http://github.com/jepst/CloudHaskell)
- [ Haskell Wiki page](http://www.haskell.org/haskellwiki/GHC/CloudAndHPCHaskell)


This is currently being worked on by [ me](http://www.cl.cam.ac.uk/~jee36/jee36.html). Feel free to drop me a line.
