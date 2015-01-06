# GHC Repositories


This page lists the active repositories relating to GHC. These are Git repositories, so you should learn [about Git](working-conventions/git) first. For instructions on actually getting a GHC source tree, see [Getting The Sources](building/getting-the-sources). For information on using these repositories (via submodules), see [the Submodules page](working-conventions/git/submodules).

## Repository listing


The GHC source code uses many related sub-repositories, which are needed for external dependencies during the build, or tools that are included in the build. Every single upstream repository we track is tracked with a **git submodule**, so that for any particular GHC commit, Git's submodule mechanism makes it possible to check out exactly the right version of each sub-repository.


Some submodules are maintained by GHC HQ, and some by their parties.  You can find out by looking at the `.cabal` file.


Here is the setup in more detail:

- **Upstream repo**.

  - Each submodule has an **upstream**, or master, repository.
  - Patches to the submodule must be pushed to the upstream repo.
  - The authoritative info for the upstream URL is in the file `packages` in the root directory of the main GHC repo.
  - If the `packages` file gives an upstream URL of "-", authoritative info is in the file `.gitmodules`.

- **Mirror repo**.

  - If the upstream repo is not at `git.haskell.org`, then we maintain a **mirror** repo at `git.haskell.org`.
  - Pulling and cloning happens from the mirror repo, so that we can build GHC without relying on lots of other machines being up.
  - The mirror should be updated from the upstream repo at least every minute or so.
  - The authoritative info for the mirror URL is in the file `.gitmodules` in the root directory of the main GHC repo.

- **Upstream GHC branch**.  As GHC's HEAD moves on, between releases, there is often a need to update a library in sync.  Each library has a named branch, the **upstream GHC branch** to which patches can be pushed.  (See table below.)  If the library author is actively developing the library, he or she will typically do so on a different branch from the upstream GHC branch.  The typical sequence is

  - `cd libraries/parallel`
  - `git checkout master`, or whatever the "upstream GHC branch" is called.
  - Make modifications to the library
  - `git push`: push the patch (may need to ask the maintainer to do this)
  - Wait for the mirror
  - `cd ../..`: back into the GHC directory
  - `git add libraries/parallel`: record the new library commit in the main GHC repo.
  - `git push`, with a commit message mentioning the word "submodule"
    More details in [the Submodules page](working-conventions/git/submodules)


Here are the submodules we use, and where their upstreams point:

<table><tr><th>**Location in tree**</th>
<th>**Upstream repo**</th>
<th>**Upstream GHC branch**</th>
<th>**Installed\[1\]**</th>
<th>**Req'd to build\[2\]**</th></tr>
<tr><th>utils/hsc2hs</th>
<th>https://git.haskell.org/hsc2hs.git</th>
<th>master</th>
<th>Yes</th>
<th>Yes</th></tr>
<tr><th>utils/haddock</th>
<th>https://github.com/haskell/haddock</th>
<th>ghc-head</th>
<th>Yes</th>
<th>No</th></tr>
<tr><th>nofib</th>
<th>https://git.haskell.org/nofib.git</th>
<th>master</th>
<th>N/A</th>
<th>N/A</th></tr>
<tr><th>libraries/array</th>
<th>https://git.haskell.org/packages/array.git</th>
<th>master</th>
<th>Yes</th>
<th>Yes</th></tr>
<tr><th>libraries/binary</th>
<th>https://github.com/kolmodin/binary</th>
<th>master</th>
<th>Yes</th>
<th>Yes</th></tr>
<tr><th>libraries/bytestring</th>
<th>https://github.com/haskell/bytestring</th>
<th>master</th>
<th>Yes</th>
<th>Yes</th></tr>
<tr><th>libraries/Cabal</th>
<th>https://github.com/haskell/Cabal</th>
<th>master</th>
<th>Yes</th>
<th>Yes</th></tr>
<tr><th>libraries/containers</th>
<th>https://github.com/haskell/containers</th>
<th>master</th>
<th>Yes</th>
<th>Yes</th></tr>
<tr><th>libraries/deepseq</th>
<th>https://git.haskell.org/packages/deepseq.git</th>
<th>master</th>
<th>No</th>
<th>No</th></tr>
<tr><th>libraries/directory</th>
<th>https://github.com/haskell/directory</th>
<th>master</th>
<th>Yes</th>
<th>Yes</th></tr>
<tr><th>libraries/filepath</th>
<th>https://github.com/haskell/filepath</th>
<th>master</th>
<th>Yes</th>
<th>Yes</th></tr>
<tr><th>libraries/haskeline</th>
<th>https://github.com/judah/haskeline</th>
<th>master</th>
<th>Yes</th>
<th>Yes</th></tr>
<tr><th>libraries/haskell98</th>
<th>https://git.haskell.org/packages/haskell98.git</th>
<th>master</th>
<th>Yes</th>
<th>Yes</th></tr>
<tr><th>libraries/haskell2010</th>
<th>https://git.haskell.org/packages/haskell2010.git</th>
<th>master</th>
<th>Yes</th>
<th>Yes</th></tr>
<tr><th>libraries/hoopl</th>
<th>https://git.haskell.org/packages/hoopl.git</th>
<th>master</th>
<th>Yes</th>
<th>Yes</th></tr>
<tr><th>libraries/hpc</th>
<th>https://git.haskell.org/packages/hpc.git</th>
<th>master</th>
<th>Yes</th>
<th>Yes</th></tr>
<tr><th>libraries/old-locale</th>
<th>https://git.haskell.org/packages/old-locale.git</th>
<th>master</th>
<th>Yes</th>
<th>Yes</th></tr>
<tr><th>libraries/old-time</th>
<th>https://git.haskell.org/packages/old-time.git</th>
<th>master</th>
<th>Yes</th>
<th>Yes</th></tr>
<tr><th>libraries/process</th>
<th>https://github.com/process</th>
<th>master</th>
<th>Yes</th>
<th>Yes</th></tr>
<tr><th>libraries/terminfo</th>
<th>https://github.com/judah/terminfo</th>
<th>master</th>
<th>Yes</th>
<th>Yes</th></tr>
<tr><th>libraries/time</th>
<th>https://github.com/haskell/time</th>
<th>master</th>
<th>Yes</th>
<th>Yes</th></tr>
<tr><th>libraries/unix</th>
<th>https://github.com/haskell/unix</th>
<th>master</th>
<th>Yes</th>
<th>Yes</th></tr>
<tr><th>libraries/Win32</th>
<th>https://github.com/haskell/win32</th>
<th>master</th>
<th>Yes</th>
<th>Yes</th></tr>
<tr><th>libraries/xhtml</th>
<th>https://github.com/haskell/xhtml</th>
<th>master</th>
<th>Yes</th>
<th>Yes</th></tr>
<tr><th>libraries/random</th>
<th>https://github.com/haskell/random</th>
<th>master</th>
<th>No</th>
<th>No</th></tr>
<tr><th>libraries/primitive</th>
<th>https://github.com/haskell/primitive</th>
<th>master</th>
<th>No</th>
<th>No</th></tr>
<tr><th>libraries/vector</th>
<th>https://github.com/haskell/vector</th>
<th>master</th>
<th>No</th>
<th>No</th></tr>
<tr><th>libraries/dph</th>
<th>https://git.haskell.org/packages/dph.git</th>
<th>master</th>
<th>No</th>
<th>No</th></tr>
<tr><th>libraries/parallel</th>
<th>https://github.com/haskell/parallel</th>
<th>master</th>
<th>No</th>
<th>No</th></tr>
<tr><th>libraries/stm</th>
<th>https://git.haskell.org/packages/stm.git</th>
<th>master</th>
<th>No</th>
<th>No</th></tr></table>

- \[1\] These libraries are not installed in the resulting compiler when you do `make install`

- \[2\] These libraries are not required to build the compiler, but may be used for tests or other libraries. Right now, most of these are based on whether you build DPH. At the moment, DPH is turned off. To build these libraries, set `BUILD_DPH=YES` in `mk/build.mk`. You can skip haddock by setting `HADDOCK_DOCS=NO` in `mk/build.mk`. TODO Explain how to skip `deepseq`, since it seems to only be used for tests.


The table above is maintained manually and can sometimes get out of sync. If in doubt, the primary data source is  the [ packages](http://git.haskell.org/ghc.git/blob_plain/HEAD:/packages) file in the top-level `ghc.git` repo folder.
