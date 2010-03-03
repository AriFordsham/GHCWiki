# Setting up a nightly build


The GHC buildbot builds GHC on various platforms in various different ways each night, runs the test suite and performance benchmarks, and mails the results to the `cvs-ghc@haskell.org` mailing list.  We're always keen to add more build slaves to the setup, especially if you have a platform that doesn't already have a build slave, so if you'd like to join the fun, please let us know at cvs-ghc@….  If a platform is represented in the nightly builds, it's more likely we'll be able to identify and fix problems specific to that platform quickly.

## To create a new build slave


The code for the builder lives in this darcs repo: [ http://darcs.haskell.org/builder/](http://darcs.haskell.org/builder/)


Once you have built it, pick a username (something fairly unique to you) and password, and send them to igloo@…. Then initialise the client with:

```wiki
./client init username password darcs.haskell.org
```


where `username` and `password` are your username and password, and put a copy of [ http://darcs.haskell.org/ghcBuilder/cert/root.pem](http://darcs.haskell.org/ghcBuilder/cert/root.pem) in `certs/`.


You can now run the client with:

```wiki
./client
```


or

```wiki
./client -v
```


We recommend running in screen for now, as the client doesn't daemonise itself yet.
