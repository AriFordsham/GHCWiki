# Licensing


GHC's codebase is mostly distributed under a 3-clause BSD license, which you can find in the file [LICENSE](https://gitlab.haskell.org/ghc/ghc/blob/master/LICENSE):

```wiki
The Glasgow Haskell Compiler License

Copyright 2002, The University Court of the University of Glasgow. 
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice,
this list of conditions and the following disclaimer.
 
- Redistributions in binary form must reproduce the above copyright notice,
this list of conditions and the following disclaimer in the documentation
and/or other materials provided with the distribution.
 
- Neither name of the University nor the names of its contributors may be
used to endorse or promote products derived from this software without
specific prior written permission. 

THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY COURT OF THE UNIVERSITY OF
GLASGOW AND THE CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
UNIVERSITY COURT OF THE UNIVERSITY OF GLASGOW OR THE CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
DAMAGE.
```


Exceptions to this are:

- `gmp/gmp-*.tar.gz`, which is LGPL

- All the libraries, each of which has its own license.  To find out the license for a library, check
  the `license` field of its `.cabal` file.  The libraries we ship with GHC are also under a
  2- or 3-clause BSD license.

## Licensing contributions


If you make a contribution to GHC or to the libraries, we will assume that you are supplying your contribution under the same license as the existing code.


If you want to copy code from another project, please check that the licenses in question allow this, and make it clear which code has been copied and from where, both in the source file itself and in the patch description.

## Copyright


Most source files have a copyright notice at the top.  If you contribute significantly to an existing file, or create a new file, please add your own copyright to the top of the file.  If you copy code from one file to another, please try to maintain the copyright attributions correctly (this may involve digging through the revision history).  
