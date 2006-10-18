## Submitting patches


To submit patches to the developers, please use `darcs send`.  You don't need any special permission to do this.

## Committing changes


If you have commit permission (pretty easy to get, just demonstrate your competence by sending us a patch or two first), then you can use `darcs push` to commit changes directly to the main repository.

```wiki
  $ darcs push <account>@darcs.haskell.org:/home/darcs/ghc
```


(change `ghc` to the name of the repository if you're pushing changes from one of the sub-repositories, like `testsuite`, or a package such as `base`.  Note: `darcs push` requires that SSH is working and can log in to your account on `darcs.haskell.org`.


Do not forget to `darcs record` your changes first!


Please test changes before committing: you can run a cut-down version of the full test suite like this:

```wiki
  $ cd testsuite/tests/ghc-regress
  $ make fast
```


You need to have `testsuite` checked out, of course.  Running `make fast` should only take a few minutes.
