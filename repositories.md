# GHC Repositories


This page lists the active repositories relating to GHC. These are Git repositories, so you should learn [about Git](working-conventions/git) first. For instructions on actually getting a GHC source tree, see [Getting The Sources](building/getting-the-sources). For information on using these repositories (via submodules), see [the Submodules page](working-conventions/git/submodules).

## Repository listing


The GHC source code tracks many related sub-repositories, which are needed for external dependencies during the build, or tools that are included in the build. Not every sub-repository is maintained by us; in fact, the large majority are *not* maintained by GHC HQ.


As a result of this, in HEAD, essentially every single upstream repository we track is tracked with a **git submodule**. These submodules are mirrored for us, and we send patches we need to the upstream maintainer. Here are the submodules we use, and where their upstreams point:

<table><tr><th>**Location in tree**</th>
<th>**Upstream repo**</th>
<th>**Upstream GHC branch**</th>
<th>**Installed**</th>
<th>**Req'd to build**</th></tr>
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
<th>https://github.com/haskell/binary</th>
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
<th>Yes</th>
<th>Yes</th></tr>
<tr><th>libraries/directory</th>
<th>https://git.haskell.org/packages/directory.git</th>
<th>master</th>
<th>Yes</th>
<th>Yes</th></tr>
<tr><th>libraries/filepath</th>
<th>https://git.haskell.org/packages/filepath.git</th>
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
<th>https://git.haskell.org/packages/process.git</th>
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
<th>https://git.haskell.org/packages/Win32.git</th>
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
<th>https://git.haskell.org/packages/parallel.git</th>
<th>master</th>
<th>No</th>
<th>No</th></tr>
<tr><th>libraries/stm</th>
<th>https://git.haskell.org/packages/stm.git</th>
<th>master</th>
<th>No</th>
<th>No</th></tr></table>