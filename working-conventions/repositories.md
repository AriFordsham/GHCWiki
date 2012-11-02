
This is a proposal for a new policy for how we handle
repositories in a GHC tree. It is still in the discussion
phase, and has not been implemented yet.

# GHC libraries and other packages


As well as the GHC repository itself, there are a number libraries
and other packages that make up a GHC tree. This page describes the
process by which those libraries can be altered.

## Packages for which we use the upstream repository directly


In the case of

- utils/hsc2hs
- utils/haddock
- libraries/array
- libraries/base
- libraries/deepseq
- libraries/directory
- libraries/filepath
- libraries/ghc-prim
- libraries/haskell98
- libraries/haskell2010
- libraries/hoopl
- libraries/hpc
- libraries/integer-gmp
- libraries/integer-simple
- libraries/old-locale
- libraries/old-time
- libraries/process
- libraries/template-haskell
- libraries/unix
- libraries/dph
- libraries/deepseq
- libraries/parallel
- libraries/stm


we use the main repository directly. Therefore changes can be pushed
directly, as with GHC patches.


When making a change to a library, you must also update the version
number if appropriate. Version number in the repositories should be
maintained such that, if the library were to be release as-is, then
they would have the correct version number. For example, if the last
release of a library was 1.2.0.3 and you remove a functino from it
then, as per the
[ Package versioning policy](http://www.haskell.org/haskellwiki/Package_versioning_policy),
the version number should be bumped to 1.3.0.0. If it is already
1.3.0.0 or higher then no further change is necessary. In order to
make this easier, the version line in the `.cabal` file should be
followed by a comment such as

```wiki
-- GHC 7.6.1 released with 1.2.0.3
```

## Packages for which there is a separate upstream repository


In the case of

- libraries/binary
- libraries/bytestring
- libraries/Cabal
- libraries/containers
- libraries/haskeline
- libraries/pretty
- libraries/terminfo
- libraries/transformers
- libraries/xhtml
- libraries/Win32
- libraries/primitive
- libraries/vector


there is a separate upstream repository.


The process for updating these is a little more complicated, motivated
by the following objectives:

1. Any changes needed by GHC should be made not only in our repository,
  but also in the upstream repository.

1. Being used in a GHC tree should not make life harder for the
  upstream maintainer.


Note that these two objectives are to some extent in conflict: If a
change in GHC or one of its libraries requires a change in a library
with an upstream repo then, in order to satisfy objective 1, the
maintainer would need to apply the patch, but if doing so is currently
inconvenient for them then this would fail objective 2. This policy
therefore tries to find the best compromise, without being too onerous
for any party.


For these repositories, we use a "git submodule" rather than a normal
repository. Using submodules means that the repository doesn't need to
follow a linear path through the git history, but can instead jump
around, for example from a release commit on one branch to the next
release commit on a different branch.

### From the GHC developer's point of view


If you are not modifying these packages then you don't need to do
anything special: A regular `./sync-all pull` will update the submodules
as normal. However, you may find it useful to run

```wiki
git config --global diff.ignoreSubmodules dirty
```


or each time you run `git status` or `git diff`, git will check for
changes not only in the GHC repository, but also in all the submodules.
(you must have `git >= 1.7.3` for this to work).


If you need to modify one of these libraries, then ordinarily you should
first send the modifications upstream. Ideally upstream will apply the
patches and make a release (the easiest way to acomplish objective 1 is
for changes to be applied upstream *first*, so that they can't be
forgotten about after being applied to GHC's repo). You can then update
GHC's submodule by running

```wiki
cd libraries/foo
git reset --hard some_commit_id
cd ../..
git commit -a
./sync-all push
```


There are some scenarios where you may need to modify GHC's repository
without the upstream repository already having the change that you need:

- The maintainer may tell you that they are too busy to deal with the
  package at the moment, or not be responding at all. In this case, it
  may be necessary to make changes only to GHC's repositories in the
  short term, and for the changes to be merged upstream later.

- In a GHC stable branch, we may be using an old version of a library
  that we need to make a change to, but upstream may only be interested
  in working on the latest version rather than also maintaining old
  release branches. In that case, we would only make the change in the
  GHC respository.


In order to make the change in this case, you

```wiki
cd libraries/foo
git commit -a
cd ../..
git commit -a
./sync-all push
```

### From the upstream maintainer's point of view


Upstream maintainers don't need to do anything special. You can continue
to use any version control system and whatever branching policy works best
for you. However, there are two issues to be aware of:

- For libraries that are shipped with GHC, we need to have releases of
  libraries that can build with that GHC. There may be no suitable
  existing release (most commonly due to trivial things such as library
  dependencies needing to be changed, but sometimes due to real changes
  in other libraries or the compiler), in which case we will request
  that you make a suitable release or, if it is not convenient for you
  to do so, we can make one on your behalf (in which case it will
  normally have only the minimal changes necessary since the previous
  release).

- Sometimes we may need to make changes to old versions of libraries,
  as we try to avoid making interface changes within GHC stable
  branches and upstream development may have moved on since a GHC
  stable branch was created. When this happens it is up to you whether
  the changes are sent upstream as normal (and maintained in an upstream
  branch), or whether they are left only in the GHC repository. Note that
  if they are made only the GHC repository then we will probably need to
  make a release from the GHC repository, as per the previous point.
