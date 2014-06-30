
This page is a WIP and will subsume [Repositories/Upstream](repositories/upstream)

# GHC Repositories


This page lists the active repositories relating to GHC. For instructions on actually getting a GHC source tree, see [Getting The Sources](building/getting-the-sources).


For read-only browsing, you can use the:

- [Trac source browser](/trac/ghc/browser/)
- [ Gitweb browser @ git.haskell.org](http://git.haskell.org/)
- [ GitHub GHC mirror](http://github.com/ghc/ghc)


For info on the active branches of the main GHC repo, see

- [ActiveBranches](active-branches)


GHC's repos use git; see [Git Working Conventions](working-conventions/git).

## GHC Repositories


The GHC source code tracks many related sub-repositories, which are needed for external dependencies during the build, or tools that are included in the build. Not every sub-repository is maintained by us; in fact, the large majority are *not* maintained by GHC HQ.


As a result of this, in HEAD, essentially every single upstream repository we track is tracked with a **git submodule**. These submodules are mirrored for us, and we send patches we need to the upstream maintainer.


But what happens if *you* need to get a submodule updated? It's quite simple...

### Sending patches upstream

- Send a patch upstream. Get it merged.

- Once that is done, just update the submodule:

```wiki
cd libraries/foo
git reset --hard some_commit_id
cd ../..
git commit -asm "Update foo submodule"
./sync-all push
```

>
> where `some_commit_id` should be the commit ID of the change you pushed. For example, if you pushed three changes (where 'Commit [\#3](https://gitlab.haskell.org//ghc/ghc/issues/3)' is the latest HEAD):

```wiki
026d0f64723729823ef29991218c7a15d8d37264 - Commit #1
235da0f3e38c2b4c489a82778a5fd84a895cb693 - Commit #2
0667f01823552caf97c4d6e8b876b1d9db00a172 - Commit #3
```

>
> then `some_commit_id = 0667f01823552caf97c4d6e8b876b1d9db00a172`

- Done!

### Submodule reference validation


There is a problem with submodules, in that it would be possible to commit a submodule change to `ghc.git`, which points to an invalid commit. For example, perhaps you make a change to `haddock` and `ghc`. You might accidentally push to `ghc` before pushing to Haddock, or worse, you might forget.


For this reason, the GHC repository has commit hooks that validate submodule references. Any time you update a submodule, you must:

- Have already pushed the new changes upstream, in a way `git` can find them.

- You **must** include the word '**submodule**' in your commit message.


If either of these assumptions are violated, your push will fail.

## Full repository breakdown


A GHC source tree is made of a collection of repositories. Here is a list of the repositories that GHC uses.  The columns have the following meaning

- **Location in tree**: where in the source tree this repository sits.

- **Upstream repo?**: if "yes", this library is maintained by someone else, 
  and its master repo is somewhere else.  See [Repositories/Upstream](repositories/upstream).

- **Reqd to build?**: is "no" if this library is not required to build GHC. We have a few of these because we use them for tests and suchlike.

- **Installed?**: is "no" if the library is not installed in a GHC installation, and "extra" if it is only installed if `InstallExtraPackages` is `YES`. All others are installed with GHC. See the [libraries page](commentary/libraries) for more info.

- **GHC repo**: in every case there is a repo on `http://git.haskell.org/`, which contains the bits we use for building GHC every night. For libraries with upstream repos, this is just a lagging mirror of the master (see [Repositories/Upstream](repositories/upstream)).  The read-only HTTP URL for the repo is `http://git.haskell.org/<table-entry>`.  To get a read/write URL, replace HTTP prefix `http://git.haskell.org` with the SSH prefix `ssh://git@git.haskell.org`. 

<table><tr><th>**Location in tree**</th>
<th>**Upstream repo?**</th>
<th>**Reqd to build?**</th>
<th>**Installed?**</th>
<th>**GHC repo http://git.haskell.org/...**</th></tr>
<tr><th>. (ghc itself)</th>
<th></th>
<th></th>
<th></th>
<th>ghc.git</th></tr>
<tr><th>ghc-tarballs</th>
<th></th>
<th></th>
<th> N/A </th>
<th>ghc-tarballs.git</th></tr>
<tr><th>utils/hsc2hs</th>
<th></th>
<th></th>
<th></th>
<th>hsc2hs.git</th></tr>
<tr><th>utils/haddock</th>
<th> yes </th>
<th></th>
<th></th>
<th>haddock.git</th></tr>
<tr><th>nofib</th>
<th></th>
<th></th>
<th> N/A </th>
<th>nofib.git</th></tr>
<tr><th>libraries/array</th>
<th> yes </th>
<th></th>
<th></th>
<th>packages/array.git</th></tr>
<tr><th>libraries/binary</th>
<th> yes </th>
<th></th>
<th></th>
<th>packages/binary.git</th></tr>
<tr><th>libraries/bytestring</th>
<th> yes </th>
<th></th>
<th></th>
<th>packages/bytestring.git</th></tr>
<tr><th>libraries/Cabal</th>
<th> yes </th>
<th></th>
<th></th>
<th>packages/Cabal.git</th></tr>
<tr><th>libraries/containers</th>
<th> yes </th>
<th></th>
<th></th>
<th>packages/containers.git</th></tr>
<tr><th>libraries/deepseq</th>
<th> yes </th>
<th></th>
<th></th>
<th>packages/deepseq.git</th></tr>
<tr><th>libraries/directory</th>
<th> yes </th>
<th></th>
<th></th>
<th>packages/directory.git</th></tr>
<tr><th>libraries/extensible-exceptions</th>
<th></th>
<th></th>
<th></th>
<th>packages/extensible-exceptions.git</th></tr>
<tr><th>libraries/filepath</th>
<th> yes </th>
<th></th>
<th></th>
<th>packages/filepath.git</th></tr>
<tr><th>libraries/haskeline</th>
<th> yes </th>
<th></th>
<th> no  </th>
<th>packages/haskeline.git</th></tr>
<tr><th>libraries/haskell98</th>
<th> yes </th>
<th></th>
<th></th>
<th>packages/haskell98.git</th></tr>
<tr><th>libraries/haskell2010</th>
<th> yes </th>
<th></th>
<th></th>
<th>packages/haskell2010.git</th></tr>
<tr><th>libraries/hoopl</th>
<th> yes </th>
<th></th>
<th></th>
<th>packages/hoopl.git</th></tr>
<tr><th>libraries/hpc</th>
<th> yes </th>
<th></th>
<th></th>
<th>packages/hpc.git</th></tr>
<tr><th>libraries/old-locale</th>
<th> yes </th>
<th></th>
<th></th>
<th>packages/old-locale.git</th></tr>
<tr><th>libraries/old-time</th>
<th> yes </th>
<th></th>
<th></th>
<th>packages/old-time.git</th></tr>
<tr><th>libraries/pretty</th>
<th> yes </th>
<th></th>
<th></th>
<th>packages/pretty.git</th></tr>
<tr><th>libraries/process</th>
<th> yes </th>
<th></th>
<th></th>
<th>packages/process.git</th></tr>
<tr><th>libraries/terminfo</th>
<th> yes </th>
<th></th>
<th> no  </th>
<th>packages/terminfo.git</th></tr>
<tr><th>libraries/time</th>
<th> yes </th>
<th></th>
<th></th>
<th>packages/time.git</th></tr>
<tr><th>libraries/transformers</th>
<th> yes </th>
<th></th>
<th></th>
<th>packages/transformers.git</th></tr>
<tr><th>libraries/unix</th>
<th> yes </th>
<th></th>
<th></th>
<th>packages/unix.git</th></tr>
<tr><th>libraries/utf8-string</th>
<th> yes </th>
<th></th>
<th></th>
<th>packages/utf8-string.git</th></tr>
<tr><th>libraries/Win32</th>
<th> yes </th>
<th></th>
<th></th>
<th>packages/Win32.git</th></tr>
<tr><th>libraries/xhtml</th>
<th> yes </th>
<th></th>
<th> no  </th>
<th>packages/xhtml.git</th></tr>
<tr><th>libraries/random</th>
<th> yes </th>
<th> no  </th>
<th> no  </th>
<th>packages/random.git</th></tr>
<tr><th>libraries/primitive</th>
<th> yes </th>
<th> no  </th>
<th> no  </th>
<th>packages/primitive.git</th></tr>
<tr><th>libraries/vector</th>
<th> yes </th>
<th> no  </th>
<th> no  </th>
<th>packages/vector.git</th></tr>
<tr><th>libraries/dph</th>
<th></th>
<th> no  </th>
<th> no  </th>
<th>packages/dph.git</th></tr>
<tr><th>libraries/parallel</th>
<th> yes </th>
<th> no  </th>
<th> no  </th>
<th>packages/parallel.git</th></tr>
<tr><th>libraries/stm</th>
<th> yes </th>
<th> no  </th>
<th> no  </th>
<th>packages/stm.git</th></tr></table>

### The 'packages' file


The master list of repositories is in the file [packages](/trac/ghc/browser/ghc/packages), and this is where the `sync-all` script finds out about which repositories make up the complete tree.  It duplicates the information in the above table; indeed, it is really the authoritative version (so complain if the table and file differ!).


The "`tag`" in the master table in [packages](/trac/ghc/browser/ghc/packages) has the following significance:

- **"`-`"**: [boot libraries](commentary/libraries), necessary to build GHC
- **"`testsuite`"**: GHC's [regression tests](building/running-tests), not necessary for a build, but is necessary if you're working on GHC
- **"`nofib`"**: GHC's [nofib benchmark suite](building/running-no-fib)
- **"`dph`"**: packages for [Data Parallel Haskell](data-parallel), which is not shipped with GHC but we test all changes to GHC against these repositories so they are usually included in a checked-out source tree.
- **"`extra`"**: extra packages you might want to include in a build (the `parallel` package, for example), but aren't necessary to get a working GHC.


See the [Commentary/Libraries](commentary/libraries) page for more information about GHC's libraries.

## Mirroring new packages to GitHub


Currently, all our repositories are being mirrored to GitHub by GitHub themselves. If you wish to add/remove a repository you need to email GitHub support at support@… and ask them to do it. Currently there is no way to administer this ourselves.
