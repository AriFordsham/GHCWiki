# Cross Compiling GHC


As of this moment (GHC 6.12) GHC does not support cross-compilation.  There are reasons that we would like it to:

- [ TakeoffGW](http://takeoffgw.sourceforge.net/) is a distribution of Unix tools for Windows, built by cross-compiling on a Linux machine.  They would like to be able to build and distribute GHC this way.  It might be useful for us to be able to cross-compile a Windows GHC from Linux too.

- We could build a 64-bit GHC on OS X, by cross-compiling using the 32-bit version.

- We could port to Win64 ([\#1884](https://gitlab.haskell.org//ghc/ghc/issues/1884)) by cross-compiling using a 32-bit Windows GHC.

- Other porting tasks might be easier, given a suitable cross-compilation toolchain.


In general, we have:

<table><tr><th></th>
<th>Overall build</th>
<th>Stage 1</th>
<th>Compiler RTS</th>
<th>Stage 2</th>
<th>Code RTS
</th></tr>
<tr><th>Build platform </th>
<th>Build        </th>
<th>Build  </th>
<th>(Build)     </th>
<th>Build  </th>
<th>(Build) 
</th></tr>
<tr><th>Host platform  </th>
<th>Host         </th>
<th>Build  </th>
<th>Host        </th>
<th>Host   </th>
<th>Target  
</th></tr>
<tr><th>Target platform</th>
<th>Target       </th>
<th>Host   </th>
<th></th>
<th>Target </th>
<th></th></tr></table>


In the special case where we are using cross compilation to bootstrap a new platform, we have Host=Target:

<table><tr><th></th>
<th>Overall build</th>
<th>Stage 1</th>
<th>RTS    </th>
<th>Stage 2
</th></tr>
<tr><th>Build platform </th>
<th>Build        </th>
<th>Build  </th>
<th>(Build)</th>
<th>Build  
</th></tr>
<tr><th>Host platform  </th>
<th>Target       </th>
<th>Build  </th>
<th>Target </th>
<th>Target 
</th></tr>
<tr><th>Target platform</th>
<th>Target       </th>
<th>Target </th>
<th></th>
<th>Target 
</th></tr></table>


In the special case where we are building a cross-compiler running on our current platform, we have Host=Build:

<table><tr><th></th>
<th>Overall build</th>
<th>Stage 1</th>
<th>Compiler RTS</th>
<th>Stage 2</th>
<th>Code RTS
</th></tr>
<tr><th>Build platform </th>
<th>Build        </th>
<th>Build  </th>
<th>(Build)     </th>
<th>Build  </th>
<th>(Build) 
</th></tr>
<tr><th>Host platform  </th>
<th>Build        </th>
<th>Build  </th>
<th>Build       </th>
<th>Build  </th>
<th>Target  
</th></tr>
<tr><th>Target platform</th>
<th>Target       </th>
<th>Build  </th>
<th></th>
<th>Target </th>
<th></th></tr></table>

## Plan


Here is how it should work:

```wiki
$ ./configure --build=<here> --host=<there> --target=<there>
```


note that we're cross-compiling from the *build* machine to the *host* machine.  The *target* machine is the same as the *host*: the GHC that we're trying to create will generate binaries for *host*.


No doubt we'll also need to specify some additional configuration parameters to tell the build system where to find our cross-compilation tools.  Perhaps something like

```wiki
--with-host-cc=...
--with-host-as=...
--with-host-ld=...
--with-host-ar=...
--with-host-strip=...
```

- stage 1: runs on `build`, compiles for `host`
- stage 2: runs on `host`, compiles for `host`

## Things that probably need fixing

- The configure script doesn't let you specify different `build`, `host`, and `target` right now
- The build systme has no distinction between the gcc used to compile from build-\>build and build-\>host.
- We can't build anything with stage2 when cross-compiling, e.g. Haddock and DPH must be disabled.
