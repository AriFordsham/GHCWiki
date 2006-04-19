# Platforms


The following table describes to what extent GHC currently supports
various platforms.  Definitions:

**Registerised**

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
> Native code generator: GHC can generate assemply code directly for this platform, bypassing gcc.

**f.i. wrapper**

>
> Support for `foreign import "wrapper"` (`ghc/rts/Adjustor.c`).

**Dynamic libraries**

>
> Support for generating dynamically-linked sharable libraries from
> Haskell code.

<table><tr><th>**Architecture**</th>
<th>**OS**</th>
<th>**Registerised**</th>
<th>**GHCi**</th>
<th>**NCG**</th>
<th>**f.i. wrapper**</th>
<th>**Dynamic libraries**</th></tr>
<tr><th> x86          </th>
<th> Windows  </th>
<th> Yes          </th>
<th> Yes  </th>
<th> Yes                   </th>
<th> Yes          </th>
<th> No(\*2)            
</th></tr>
<tr><th> x86          </th>
<th> Linux    </th>
<th> Yes          </th>
<th> Yes  </th>
<th> Yes                   </th>
<th> Yes          </th>
<th> No                
</th></tr>
<tr><th> x86          </th>
<th> FreeBSD  </th>
<th> Yes          </th>
<th> Yes  </th>
<th> Yes                   </th>
<th> Yes          </th>
<th> No                
</th></tr>
<tr><th> x86          </th>
<th> OpenBSD  </th>
<th> Yes          </th>
<th> Yes  </th>
<th> Yes                   </th>
<th> Yes          </th>
<th> No                
</th></tr>
<tr><th> x86          </th>
<th> NetBSD   </th>
<th> Yes          </th>
<th> Yes  </th>
<th> Yes                   </th>
<th> Yes          </th>
<th> No                
</th></tr>
<tr><th> x86          </th>
<th> MacOS X  </th>
<th> Yes          </th>
<th> Yes  </th>
<th> Yes                   </th>
<th> Yes          </th>
<th> No                
</th></tr>
<tr><th> PowerPC      </th>
<th> AIX      </th>
<th> Yes          </th>
<th></th>
<th> Yes                   </th>
<th> Yes          </th>
<th> No                
</th></tr>
<tr><th> PowerPC      </th>
<th> Linux    </th>
<th> Yes          </th>
<th> Yes  </th>
<th> Yes                   </th>
<th> Yes          </th>
<th> No                
</th></tr>
<tr><th> PowerPC      </th>
<th> MacOS X  </th>
<th> Yes          </th>
<th> Yes  </th>
<th> Yes                   </th>
<th> Yes          </th>
<th> Yes               
</th></tr>
<tr><th> PowerPC64    </th>
<th> Linux    </th>
<th> Yes          </th>
<th></th>
<th> No                    </th>
<th> Yes          </th>
<th> No                
</th></tr>
<tr><th> Sparc        </th>
<th> Solaris  </th>
<th> Yes          </th>
<th> Yes  </th>
<th> No(\*1)                </th>
<th> Yes          </th>
<th> No                
</th></tr>
<tr><th> Sparc        </th>
<th> Linux    </th>
<th> Yes          </th>
<th> Yes  </th>
<th> No(\*1)                </th>
<th> Yes          </th>
<th> No                
</th></tr>
<tr><th> x86_64       </th>
<th> Linux    </th>
<th> Yes          </th>
<th> Yes  </th>
<th> Yes                   </th>
<th> Yes          </th>
<th> No                
</th></tr>
<tr><th> x86_64       </th>
<th> OpenBSD  </th>
<th> Yes          </th>
<th> Yes  </th>
<th> Yes                   </th>
<th> Yes          </th>
<th> No                
</th></tr>
<tr><th> Mips64       </th>
<th> Irix     </th>
<th> No           </th>
<th> No   </th>
<th> No                    </th>
<th> No           </th>
<th> No                
</th></tr>
<tr><th> IA-64        </th>
<th> Linux    </th>
<th> Yes          </th>
<th> Yes  </th>
<th> No                    </th>
<th> Yes          </th>
<th> No                
</th></tr>
<tr><th> Alpha        </th>
<th> Dec OSF  </th>
<th> No           </th>
<th> No   </th>
<th> No                    </th>
<th> Yes          </th>
<th> No                
</th></tr>
<tr><th> Alpha        </th>
<th> Linux    </th>
<th> No           </th>
<th> No   </th>
<th> No                    </th>
<th> Yes          </th>
<th> No                
</th></tr>
<tr><th> HPPA         </th>
<th> Linux    </th>
<th> No           </th>
<th> No   </th>
<th> No                    </th>
<th> No           </th>
<th> No                
</th></tr>
<tr><th> S/390        </th>
<th> Linux    </th>
<th> No           </th>
<th> No   </th>
<th> No                    </th>
<th> No           </th>
<th> No                
</th></tr>
<tr><th> m68k         </th>
<th> Linux    </th>
<th> No           </th>
<th> No   </th>
<th> No                    </th>
<th> No           </th>
<th> No                
</th></tr></table>

**\*1** Sparc NCG bitrotted, but still in the tree
**\*2** Win32 DLL support bitrotted