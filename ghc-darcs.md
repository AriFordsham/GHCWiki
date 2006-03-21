### Gettig a GHC source tree


The first thing to do is install [ darcs](http://darcs.net/).


A source tree consists of the GHC repository, with a set of packages in the libraries directory.  We supply a script to automate the checking out of packages, `darcs-all`.  Checking out a tree goes like this:

```wiki
  $ darcs get --partial http://darcs.haskell.org/ghc
  $ cd ghc
  $ chmod +x ./darcs-all
  $ ./darcs-all get
```


If `darcs-all` fails with

```wiki
  $ ./darcs-all
  ./darcs-all: 12: Syntax error: "(" unexpected
```


you need to invoke the script with `bash` explicitly, eg. `bash darcs-all get`.

**NOTE**: you really want `--partial` when grabbing GHC.  There are some 13000 patches in the repository, which take a long time to download without `--partial`.  The `darcs-all` script automatically adds `--partial` for the packages.


Optionally, you might want to grab the testsuite and benchmark suite too, which should also be sub-directories of ghc:

```wiki
  $ darcs get --partial http://darcs.haskell.org/testsuite
  $ darcs get --partial http://darcs.haskell.org/nofib
```

## List of repositories


These darcs repositories are related to GHC:

<table><tr><th>[ http://darcs.haskell.org/ghc](http://darcs.haskell.org/ghc)</th>
<th>The main GHC repository
</th></tr>
<tr><th>[ http://darcs.haskell.org/testsuite](http://darcs.haskell.org/testsuite)</th>
<th>The test suite
</th></tr>
<tr><th>[ http://darcs.haskell.org/nofib](http://darcs.haskell.org/nofib)</th>
<th>The benchmark suite
</th></tr></table>


And the following repositories contain packages that are used to populate the libraries directory of a darcs checkout:

<table><tr><th>[ http://darcs.haskell.org/packages/Cabal](http://darcs.haskell.org/packages/Cabal)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/ALUT](http://darcs.haskell.org/packages/ALUT)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/GLUT](http://darcs.haskell.org/packages/GLUT)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/HGL](http://darcs.haskell.org/packages/HGL)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/HUnit](http://darcs.haskell.org/packages/HUnit)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/HaXml](http://darcs.haskell.org/packages/HaXml)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/Japi](http://darcs.haskell.org/packages/Japi)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/ObjectIO](http://darcs.haskell.org/packages/ObjectIO)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/OpenAL](http://darcs.haskell.org/packages/OpenAL)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/OpenGL](http://darcs.haskell.org/packages/OpenGL)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/QuickCheck](http://darcs.haskell.org/packages/QuickCheck)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/Win32](http://darcs.haskell.org/packages/Win32)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/X11](http://darcs.haskell.org/packages/X11)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/arrows](http://darcs.haskell.org/packages/arrows)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/base](http://darcs.haskell.org/packages/base)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/fgl](http://darcs.haskell.org/packages/fgl)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/haskell-src](http://darcs.haskell.org/packages/haskell-src)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/haskell98](http://darcs.haskell.org/packages/haskell98)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/monads](http://darcs.haskell.org/packages/monads)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/mtl](http://darcs.haskell.org/packages/mtl)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/network](http://darcs.haskell.org/packages/network)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/parsec](http://darcs.haskell.org/packages/parsec)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/readline](http://darcs.haskell.org/packages/readline)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/stm](http://darcs.haskell.org/packages/stm)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/template-haskell](http://darcs.haskell.org/packages/template-haskell)</th></tr>
<tr><th>[ http://darcs.haskell.org/packages/unix](http://darcs.haskell.org/packages/unix)</th></tr></table>


There are no branches currently; we will use CVS for the 6.4 branch until its end of life, and use darcs for future branches.

### Pulling new patches


To update your tree from the master repositories, the quickest way is to use the `darcs-all` script:

```wiki
  $ ./darcs-all pull -a
```

### Submitting patches


To submit patches to the developers, please use `darcs send`.  You don't need any special permission to do this.

### Committing changes


If you have commit permission (pretty easy to get, just demonstrate your competence by sending us a patch or two first), then you can use `darcs push` to commit changes directly to the main repository.

```wiki
  $ darcs push <account>@darcs.haskell.org:/home/darcs/ghc
```


(change `ghc` to the name of the repository if you're pushing changes from one of the sub-repositories, like `testsuite`, or a package such as `base`.  Note: `darcs push` requires that SSH is working and can log in to your account on `darcs.haskell.org`.


Do not forget to `darcs record` your changes first!


Please test changes before committing: you can run a cut-down version of the full test suite like this:

```wiki
  $ cd testsuite
  $ make boot
  $ make fast
```


You need to have `testsuite` checked out, of course.  Running `make fast` should only take a few minutes.
