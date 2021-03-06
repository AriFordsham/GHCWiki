# Git Repository Reorganization (#8545)


The content on this page is still work in progress

## Current state



As of [db19c665ec5055c2193b2174519866045aeff09a](/trac/ghc/changeset/db19c665ec5055c2193b2174519866045aeff09a/ghc) all repos except for `ghc-tarballs` have been turned into proper submodules in in the `master` branch. GHC 7.10 will be the first release having proper submodules in place.



~~The following repositories are currently submodules: `binary.git` `bytestring.git` `Cabal.git` `containers.git` `haskeline.git` `pretty.git` `primitive.git` `random.git` `terminfo.git` `time.git` `transformers.git` `vector.git` `Win32.git` `xhtml.git`~~



As a crude statistics, the number of `git push` operations performed to the master branches of non-git-submodules between 2013-08-10 and 2013-11-23:

```wiki
      2 packages/haskell2010
      2 packages/old-locale
      3 hsc2hs
      3 packages/haskell98
      3 packages/hoopl
      3 packages/hpc
      4 packages/old-time
      5 packages/deepseq
      5 packages/dph
      5 packages/integer-simple
      6 packages/parallel
      6 packages/stm
      7 nofib
      7 packages/filepath
      8 packages/array
      9 haddock
      9 packages/directory
     11 packages/ghc-prim
     11 packages/integer-gmp
     13 packages/template-haskell
     14 packages/process
     21 packages/unix
     59 packages/base
    176 testsuite
    347 ghc
```


Moreover, the following packages are tightly coupled to specific GHC versions (and are not supposed be `cabal install`able)

- `base`
- `ghc-prim`
- `integer-gmp`/`integer-simple`
- `template-haskell`

## Some misc. notes about submodules


See also some older notes: [DarcsConversion\#Theperspectiveonsubmodules](darcs-conversion#the-perspective-on-submodules)

- based on the current access patterns, a few repositories are modified often (specifically,  ghc.git and testsuite.git are often updated in a inter-dependent way); most repositories aren't updated for several weeks; therefore some of the issues outlined with the usability of submodules might have less weight here.

- Tooling is required, as discussed in [this ghc-devs@ posting](http://permalink.gmane.org/gmane.comp.lang.haskell.ghc.devel/2718) .

### Simple migration plan

#### The plan


Fold some high-frequency repositories directly into `ghc.git`:

- `testsuite.git` (commits to `testsuite.git` are tightly coupled to associated commits to `ghc.git` most of the time, and occur often)
- `base.git` (tightly coupled to GHC internals, 3rd most often updated repo, often coupled with `testsuite.git` tweaks)
- not so clear but potential candidates: `ghc-prim.git`, `integer-gmp.git`, `integer-simple.git`, `template-haskell.git`


Reasons for folding some repos into `ghc.git`:

- Avoid overhead/noise of frequent git-submodule ref update commits to `ghc.git`
- Simplify history (i.e. interlocked changes to `ghc.git` & `testsuite.git` are one single atomic commit/patch)


Reasons against folding all repos into `ghc.git`:

- `ghc.git` repo (history) size
- sparse/partial checkouts (e.g. via `cabal get -s`) require to fetch all of `ghc.git`'s history


 



The remaining add-on repositories shall be converted into Git submodules.


#### The consequences


- `ghc.git`'s commit id by itself effectively provides a sufficient source-tree fingerprint; therefore, this renders the `fingerprint.py` superfluous

- `git bisect` works (almost) out of the box now

- buildbots don't need to fetch all repos *every time* to check for new commits; it's enough to check `ghc.git`, and only if a new commit in `ghc.git` exists, run `git submodule update`
