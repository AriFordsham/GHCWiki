# Platforms


The following table describes to what extent GHC currently supports
various platforms.  To find out who is responsible for each platform, see [GHC Code Owners](code-owners). 


For information about what distributions GHC is part of, see the [ distribution packages](http://haskell.org/ghc/distribution_packages) page.


Please be aware that this page is highly unmaintained and may list platforms as tier 2 that didn't see any updates for years.

## Tier 1 platforms


Tier 1 platforms are our top priority.  We only release GHC when they all work.
Although there are not many Tier 1 platforms, they cover a very large fraction of our users.


Criteria for Tier 1 platforms:

- An active buildbot client, capable of doing full builds and uploading distributions.
- An active sponsor, willing to investigate and fix platform-specific bugs, and 
  to work with us during the release process

<table><tr><th>**Architecture**</th>
<th>**OS**</th>
<th>**Build name**</th>
<th>**Reg'd**</th>
<th>**GHCi**</th>
<th>**NCG**</th>
<th>**f.i.**</th>
<th>**Dyn libs**</th>
<th>**Sponsor**</th>
<th>**WikiPage**</th></tr>
<tr><th> x86          </th>
<th> Windows (MinGW) </th>
<th> i386-unknown-mingw32    </th>
<th> Yes          </th>
<th> Yes  </th>
<th> Yes                   </th>
<th> Yes          </th>
<th> Yes(\*W)            </th>
<th></th>
<th>[WindowsGhc](windows-ghc)</th></tr>
<tr><th> x86          </th>
<th> Linux           </th>
<th> i386-unknown-{linux,gnu} </th>
<th> Yes          </th>
<th> Yes  </th>
<th> Yes                   </th>
<th> Yes          </th>
<th> Yes               </th>
<th></th>
<th></th></tr>
<tr><th> x86-64       </th>
<th> Linux           </th>
<th> x86_64-unknown-linux    </th>
<th> Yes          </th>
<th> Yes  </th>
<th> Yes                   </th>
<th> Yes          </th>
<th> Yes               </th>
<th></th>
<th></th></tr>
<tr><th> x86          </th>
<th> MacOS X         </th>
<th> i386-apple-darwin       </th>
<th> Yes          </th>
<th> Yes  </th>
<th> Yes                   </th>
<th> Yes          </th>
<th> No                </th>
<th></th>
<th>[Attic/X86OSXGhc](attic/x86-osx-ghc)</th></tr>
<tr><th> x86-64       </th>
<th> MacOS X         </th>
<th> x86_64-apple-darwin     </th>
<th> Yes          </th>
<th> Yes  </th>
<th> Yes    </th>
<th> Yes          </th>
<th> Yes               </th>
<th></th>
<th></th></tr>
<tr><th> x86          </th>
<th> FreeBSD         </th>
<th> i386-portbld-freebsd    </th>
<th> Yes          </th>
<th> Yes  </th>
<th> Yes    </th>
<th> Yes          </th>
<th> Yes                </th>
<th> Gabor Pali </th>
<th>[FreeBSDGhc](free-bsd-ghc)</th></tr>
<tr><th> x86-64       </th>
<th> FreeBSD         </th>
<th> amd64-portbld-freebsd   </th>
<th> Yes          </th>
<th> Yes  </th>
<th> Yes    </th>
<th> Yes          </th>
<th> Yes                </th>
<th> Gabor Pali </th>
<th>[FreeBSDGhc](free-bsd-ghc)</th></tr></table>

**\*W** Win32 DLL support bitrotten

## Tier 2 platforms


Tier 2 platforms work (to varying degrees), but we rely on community support for
developing, testing, and building distributions.  We may release GHC
with some Tier 2 platforms not working.  


Platform-specific bugs on Tier 2 platforms are marked "low priority" (unless there's
a strong reason not to do so), not because they are unimportant to the users of that
platform, but to express the fact that they aren't going to hold up the release.


We'd like to promote as many
Tier 2 platforms as possible to Tier 1, as soon as they meet the Tier 1 criteria.

<table><tr><th>**Architecture**</th>
<th>**OS**</th>
<th>**Build name**</th>
<th>**Reg'd**</th>
<th>**GHCi**</th>
<th>**NCG**</th>
<th>**f.i.**</th>
<th>**Dyn libs**</th>
<th>**WikiPage**</th></tr>
<tr><th> x86          </th>
<th> OpenBSD         </th>
<th> i386-unknown-openbsd    </th>
<th> Yes          </th>
<th> Yes  </th>
<th> Yes    </th>
<th> Yes          </th>
<th> No                </th>
<th></th></tr>
<tr><th> x86          </th>
<th> Solaris         </th>
<th> i386-unknown-solaris2   </th>
<th> Yes          </th>
<th> Yes  </th>
<th> Yes    </th>
<th> Yes          </th>
<th> Yes (\*S4)         </th>
<th></th></tr>
<tr><th> x86-64       </th>
<th> OpenBSD         </th>
<th> amd64-unknown-openbsd   </th>
<th> Yes          </th>
<th> Yes  </th>
<th> Yes    </th>
<th> Yes          </th>
<th> No                </th>
<th></th></tr>
<tr><th> PowerPC      </th>
<th> Linux           </th>
<th> powerpc-unknown-linux   </th>
<th> Yes          </th>
<th> Yes  </th>
<th> Yes    </th>
<th> Yes          </th>
<th> No                </th>
<th></th></tr>
<tr><th> PowerPC      </th>
<th> MacOS X         </th>
<th> powerpc-apple-darwin    </th>
<th> Yes          </th>
<th> Yes  </th>
<th> Yes    </th>
<th> Yes          </th>
<th> Yes               </th>
<th></th></tr>
<tr><th> PowerPC64    </th>
<th> Linux           </th>
<th> powerpc64-unknown-linux </th>
<th> No           </th>
<th> No   </th>
<th> No     </th>
<th> Yes          </th>
<th> No                </th>
<th></th></tr>
<tr><th> Sparc        </th>
<th> Solaris         </th>
<th> sparc-sun-solaris2      </th>
<th> No (\*S3)     </th>
<th> Yes  </th>
<th> Yes    </th>
<th> Yes          </th>
<th> No                </th>
<th>[Building/Solaris](building/solaris)</th></tr>
<tr><th> Sparc        </th>
<th> Linux           </th>
<th> sparc-unknown-linux     </th>
<th> Yes(\*S2)     </th>
<th> Yes  </th>
<th> Yes    </th>
<th> Yes          </th>
<th> No                </th>
<th></th></tr>
<tr><th> IA-64        </th>
<th> Linux           </th>
<th> ia64-unknown-linux      </th>
<th> No           </th>
<th> No   </th>
<th> No     </th>
<th> No           </th>
<th> No                </th>
<th>[Building/IA64Linux](building/i-a64-linux)</th></tr>
<tr><th> Alpha        </th>
<th> Linux           </th>
<th> alpha-unknown-linux     </th>
<th> No           </th>
<th> No   </th>
<th> No     </th>
<th> Yes          </th>
<th> No                </th>
<th></th></tr>
<tr><th> HPPA         </th>
<th> Linux           </th>
<th> hppa-unknown-linux      </th>
<th> No           </th>
<th> No   </th>
<th> No     </th>
<th> No           </th>
<th> No                </th>
<th></th></tr>
<tr><th> S/390        </th>
<th> Linux           </th>
<th> s390-ibm-linux          </th>
<th> No           </th>
<th> No   </th>
<th> No     </th>
<th> No           </th>
<th> No                </th>
<th></th></tr>
<tr><th> m68k         </th>
<th> Linux           </th>
<th> m68k-unknown-linux      </th>
<th> No           </th>
<th> No   </th>
<th> No     </th>
<th> No           </th>
<th> No                </th>
<th></th></tr>
<tr><th> mips         </th>
<th> Linux           </th>
<th> mips-unknown-linux      </th>
<th> No           </th>
<th> No   </th>
<th> No     </th>
<th> No           </th>
<th> No                </th>
<th></th></tr>
<tr><th> mipsel       </th>
<th> Linux           </th>
<th> mipsel-unknown-linux    </th>
<th> No           </th>
<th> No   </th>
<th> No     </th>
<th> No           </th>
<th> No                </th>
<th></th></tr>
<tr><th> ARM          </th>
<th> Linux           </th>
<th> arm-unknown-linux       </th>
<th> Yes          </th>
<th> Yes  </th>
<th> No     </th>
<th> No           </th>
<th> Yes               </th>
<th></th></tr>
<tr><th> ARM          </th>
<th> Debian armel    </th>
<th> arm-linux-gnueabi       </th>
<th> Yes          </th>
<th> No   </th>
<th> No     </th>
<th> No           </th>
<th> No                      </th>
<th>[Building/ARMLinuxGnuEABI](building/arm-linux-gnu-eabi)</th></tr>
<tr><th> ARM          </th>
<th> iOS             </th>
<th> arm-apple-darwin10      </th>
<th> Yes          </th>
<th> No   </th>
<th> Yes    </th>
<th> No           </th>
<th> No                      </th>
<th>[Building/CrossCompiling/iOS](building/cross-compiling/i-os)</th></tr></table>

**\*S1** Goetz Isenmann [ reports](http://www.haskell.org/pipermail/glasgow-haskell-users/2009-November/017961.html) that GHCi is currently unable to load compiled code that refers to errno.
**\*S2** but see [\#591](https://gitlab.haskell.org//ghc/ghc/issues/591)
**\*S3** registerised in the past up to GHC 7.0.4 release, then unregisterised due to removal of registerised -fvia-C way (mangler)
**\*S4** shared libraries are supported on Solaris 11 version 11/11 and higher


In most cases, binaries for the tier 2 platforms can be downloaded from the [Distribution Packages](http://www.haskell.org/ghc/distribution_packages) page, e.g. you can get binaries for most of the Linux platforms from Debian. In some cases, for example the Solaris platforms, you'll need to go to the [download page](http://www.haskell.org/ghc/download) of a particular release to get a bindist.

## Tier 3 platforms


Tier 3 platforms worked in the past, but probably do not work now.

<table><tr><th>**Architecture**</th>
<th>**OS**</th>
<th>**Build name**</th>
<th>**Reg'd**</th>
<th>**GHCi**</th>
<th>**NCG**</th>
<th>**f.i.**</th>
<th>**Dyn libs**</th>
<th>**WikiPage**</th></tr>
<tr><th> Mips64       </th>
<th> Irix            </th>
<th> mips-unknown-linux      </th>
<th> No           </th>
<th> No   </th>
<th> No     </th>
<th> No           </th>
<th> No                </th>
<th></th></tr>
<tr><th> x86          </th>
<th> Windows (Cygwin) </th>
<th> i386-unknown-cygwin32 </th>
<th> Yes          </th>
<th> No  </th>
<th> Yes      </th>
<th> Yes          </th>
<th> No(\*2)            
</th>
<th></th></tr>
<tr><th> Alpha        </th>
<th> Dec OSF          </th>
<th> alpha-dec-osf3        </th>
<th> No           </th>
<th> No  </th>
<th> No       </th>
<th> Yes          </th>
<th> No               
</th>
<th></th></tr>
<tr><th> ARM          </th>
<th> Maemo (Linux)   </th>
<th> arm-unknown-linux-gnu   </th>
<th> Yes          </th>
<th> No   </th>
<th> No     </th>
<th> No           </th>
<th> No                      </th>
<th>[ArmLinuxGhc](arm-linux-ghc)</th></tr>
<tr><th> x86          </th>
<th> NetBSD          </th>
<th> i386-unknown-netbsd     </th>
<th> Yes          </th>
<th> Yes  </th>
<th> Yes    </th>
<th> Yes          </th>
<th> No                </th>
<th></th></tr>
<tr><th> x86          </th>
<th> DragonFlyBSD    </th>
<th> i386-unknown-dflybsd    </th>
<th> Yes          </th>
<th> No(\*S1) </th>
<th> Yes    </th>
<th> Yes          </th>
<th> No                </th>
<th></th></tr>
<tr><th> x86-64       </th>
<th> NetBSD          </th>
<th> amd64-unknown-netbsd    </th>
<th> Yes          </th>
<th> Yes  </th>
<th> Yes    </th>
<th> Yes          </th>
<th> No                </th>
<th></th></tr>
<tr><th> PowerPC      </th>
<th> AIX             </th>
<th> powerpc-ibm-aix         </th>
<th> Yes          </th>
<th></th>
<th> Yes    </th>
<th> Yes          </th>
<th> No                </th>
<th></th></tr></table>

## Definitions

**Reg'd (Registerised)**

>
> A catch-all term for a number of optimisations, which collectively
> require the *mangler* (a Perl script that post-processes the
> assembly output from gcc).  Unregisterised builds require only a
> working C compiler and are hence far more portable.  The
> registerised optimisations include:
> direct tail calls (as opposed to using the "mini-interpreter"),
> info-tables adjacent to entry code, and virtual machine registers mapped
> to real machine registers.

**GHCi**

>
> The interactive environment, including dynamic linking of object
> code and dynamic generation of FFI calls.

**NCG**

>
> Native code generator: GHC can generate assembly code directly for this platform, bypassing gcc.

**f.i. wrapper**

>
> Support for `foreign import "wrapper"` (`ghc/rts/Adjustor.c`).

**Dynamic libraries**

>
> Support for generating dynamically-linked sharable libraries from
> Haskell code.
