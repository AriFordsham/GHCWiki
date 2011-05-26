# Base Package Safety


This page presents a module breakdown of the safety of the Base package.

- **Green**: Made safe with no modifications
- **Blue**: Made trustworthy with no modifications
- **Yellow**: Split out some unsafe functions to Module.Unsafe, made Module trustworthy
- **Red**: Left unsafe


Most blue squares are blue because they import GHC.Base which is currently unsafe. Other also import unsafePerformIO operations.


For splitting modules that contain both Safe and Unsafe Symbols, I've moved the entire definition to a new module called say GHC.Arr.Imp. Then added two new module, GHC.Arr.Safe, GHC.Arr.Unsafe. Then changed GHC.Arr to import the Safe and Unsafe modules and either just export the Safe API or export both Safe and Unsafe depending on a CPP flag. This allows us to choose at compile time if we want the base package to be safe by default or not. I could have used a simpler approach like having the entire module defined in GHC.Arr.Unsafe and not have a Imp module but I preferred the Safe and Unsafe modules having disjoint API's rather than Safe being a subset.

## Base Package

<table><tr><th>**Top Level**</th>
<th>**Control**</th>
<th>**Data**</th>
<th>**Debug**</th>
<th>**Foreign**</th>
<th>**System**</th>
<th>**Text**</th>
<th>**Unsafe**</th></tr>
<tr><th> Foreign </th>
<th> Applicative </th>
<th> Bits         </th>
<th> Trace </th>
<th> C           </th>
<th> CPUTime      </th>
<th> Printf          </th>
<th> Coerce </th></tr>
<tr><th> Numeric   </th>
<th> Arrow        </th>
<th> Bool         </th>
<th></th>
<th> Concurren   </th>
<th> Enviornment </th>
<th> Read             </th>
<th></th></tr>
<tr><th> Prelude   </th>
<th> Category     </th>
<th> Char         </th>
<th></th>
<th> ForeignPtr </th>
<th> Exit        </th>
<th> Show            </th>
<th></th></tr>
<tr><th></th>
<th> Concurrent   </th>
<th> Complex      </th>
<th></th>
<th> Marshal     </th>
<th> IO           </th>
<th>**Text.ParserCombinators**</th>
<th></th></tr>
<tr><th></th>
<th> Exception    </th>
<th> Data          </th>
<th></th>
<th> Ptr          </th>
<th> Info        </th>
<th> ReadP            </th>
<th></th></tr>
<tr><th></th>
<th> Monad        </th>
<th> Dynamic       </th>
<th></th>
<th> StablePtr   </th>
<th> Mem         </th>
<th> ReadPrec         </th>
<th></th></tr>
<tr><th></th>
<th> OldException </th>
<th> Either       </th>
<th></th>
<th> Storable     </th>
<th> Timeout      </th>
<th>**Text.Read**</th>
<th></th></tr>
<tr><th></th>
<th>**Control.Concurrent**</th>
<th> Eq           </th>
<th></th>
<th>**Foregin.C**</th>
<th>**System.Console**</th>
<th> Lex              </th>
<th></th></tr>
<tr><th></th>
<th> Chan         </th>
<th> Fixed        </th>
<th></th>
<th> Error        </th>
<th> GetOpt      </th>
<th>**Text.Show**</th>
<th></th></tr>
<tr><th></th>
<th> MVar         </th>
<th> Foldable     </th>
<th></th>
<th> String       </th>
<th>**System.IO**</th>
<th> Functions       </th>
<th></th></tr>
<tr><th></th>
<th> QSem         </th>
<th> Function    </th>
<th></th>
<th> Types        </th>
<th> Error        </th>
<th></th>
<th></th></tr>
<tr><th></th>
<th> QSemN        </th>
<th> Functor      </th>
<th></th>
<th>**Foreign.Marshal**</th>
<th>**System.Mem**</th>
<th></th>
<th></th></tr>
<tr><th></th>
<th> SampleVar    </th>
<th> HashTable    </th>
<th></th>
<th> Alloc        </th>
<th> StableName   </th>
<th></th>
<th></th></tr>
<tr><th></th>
<th>**Control.Exception**</th>
<th> IORef        </th>
<th></th>
<th> Array        </th>
<th> Weak        </th>
<th></th>
<th></th></tr>
<tr><th></th>
<th> Base         </th>
<th> Int          </th>
<th></th>
<th> Error        </th>
<th>**System.Posix**</th>
<th></th>
<th></th></tr>
<tr><th></th>
<th>**Control.Monad**</th>
<th> Ix           </th>
<th></th>
<th> Pool         </th>
<th> Internals    </th>
<th></th>
<th></th></tr>
<tr><th></th>
<th> Fix         </th>
<th> List         </th>
<th></th>
<th> Utils        </th>
<th> Types        </th>
<th></th>
<th></th></tr>
<tr><th></th>
<th> Group        </th>
<th> Maybe        </th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th></th>
<th> Instances   </th>
<th> Monoid       </th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th></th>
<th> ST         </th>
<th> Ord          </th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th></th>
<th> Zip         </th>
<th> Ratio       </th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th></th>
<th>**Control.Monad.ST**</th>
<th> STRef       </th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th></th>
<th> Lazy       </th>
<th> String       </th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th></th>
<th> Strict     </th>
<th> Traversable </th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th></th>
<th></th>
<th> Tuple        </th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th></th>
<th></th>
<th> Typeable      </th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th></th>
<th></th>
<th> Unique       </th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th></th>
<th></th>
<th> Version      </th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th></th>
<th></th>
<th> Word         </th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th></th>
<th></th>
<th>**Data.STRef**</th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th></th>
<th></th>
<th> Lazy        </th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr>
<tr><th></th>
<th></th>
<th> Strict      </th>
<th></th>
<th></th>
<th></th>
<th></th>
<th></th></tr></table>

## GHC


Below is the breakdown for just the GHC modules in base:

<table><tr><th>**GHC**</th>
<th>**GHC.Conc**</th>
<th>**GHC.Float**</th>
<th>**GHC.IO**</th></tr>
<tr><th>Arr          </th>
<th> IO      </th>
<th> ConversionUtils </th>
<th> Buffer              </th></tr>
<tr><th>Base            </th>
<th> Signal    </th>
<th> RealFracMethods </th>
<th> BufferedIO          </th></tr>
<tr><th>Classes        </th>
<th> Sync    </th>
<th></th>
<th> Device              </th></tr>
<tr><th>Conc         </th>
<th> Windows </th>
<th></th>
<th> Encoding            </th></tr>
<tr><th>ConsoleHandler </th>
<th></th>
<th></th>
<th> Exception           </th></tr>
<tr><th>Constants      </th>
<th></th>
<th></th>
<th> FD                  </th></tr>
<tr><th>Desugar        </th>
<th></th>
<th></th>
<th> Handle              </th></tr>
<tr><th>Enum           </th>
<th></th>
<th></th>
<th> IOMode              </th></tr>
<tr><th>Environment    </th>
<th></th>
<th></th>
<th>**GHC.IO.Encoding**</th></tr>
<tr><th>Err            </th>
<th></th>
<th></th>
<th> CodePage            </th></tr>
<tr><th>Event          </th>
<th></th>
<th></th>
<th> Failure             </th></tr>
<tr><th>Exception      </th>
<th></th>
<th></th>
<th> Iconv               </th></tr>
<tr><th>Exts            </th>
<th></th>
<th></th>
<th> Latin1              </th></tr>
<tr><th>Float          </th>
<th></th>
<th></th>
<th> Types               </th></tr>
<tr><th>Foreign        </th>
<th></th>
<th></th>
<th> UTF16               </th></tr>
<tr><th>ForeignPtr   </th>
<th></th>
<th></th>
<th> UTF32               </th></tr>
<tr><th>Handle         </th>
<th></th>
<th></th>
<th> UTF8                </th></tr>
<tr><th>IO           </th>
<th></th>
<th></th>
<th>**GHC.IO.Encoding.CodePage**</th></tr>
<tr><th>IOArray      </th>
<th></th>
<th></th>
<th> Table                </th></tr>
<tr><th>IOBase          </th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>IORef        </th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>Int            </th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>List           </th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>MVar         </th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>Num            </th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>PArr           </th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>Pack            </th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>Ptr          </th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>Read           </th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>Real           </th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>ST           </th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>STRef        </th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>Show           </th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>Stable       </th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>Storable       </th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>TopHandler     </th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>Unicode        </th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>Weak\*           </th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>Windows        </th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>Word           </th>
<th></th>
<th></th>
<th></th></tr></table>


\*I tried to split Weak into Unsafe and Safe modules and have GHC.Weak just expose the Safe api (i.e this would make it a yellow box like the others). However I wasn't able to figure out how to move the definition of Weak. Many of the GHC modules are wired in and require changes to compiler/prelude/PreNames. For all other modules I was able to update their builtin location fine but for Weak I continually got links errors when trying to build libRts.a if I tried to move the definition of GHC.Weak around.

## Notes


These are notes on specific modules and why they are the colour they are... ect.

**GHC.Base** and **GHC.Prim:**
Leaving unsafe. Had a go at making safe versions but gets
pretty ugly and complex quickly. See 
[Base Module](safe-haskell/base-package#base-module) for
a more detailed discussion.

**GHC.Conc: **
Is it safe to expose ThreadId's constructors?


For the moment I've hidden both

**GHC.Conc.IO** and **GHC.Conc.IO.Windows:**
Made safe version that doesn't contain the asyncReadBA, asyncWriteBA functions.
Perhaps these can be left in and GHC.Conc.IO just made trustworthy since their
result is in the IO monad but they take a 'MutableByteArray\# RealWorld' as a second
parameter.

**GHC.Event:**
Made trustworthy... Not sure of this though

**GHC.Exts:**
Left unsafe and didn't make safe / unsafe split
Mostly seems fine, only worry is access to Ptr constructor.
Also re-exports GHC.Prim

**GHC.Ptr:**
made safe/unsafe split
Exposes Ptr constructor
Cast operations of funptr to ptr seem dangerous as well, removed from safe version.

**GHC.ForeignPtr:**
Made ForeignPtr type abstract
Has an '!unsafeForeignPtrToPtr' function also excluded
The whole module seems a little dangerous. (e.g castForeignPtr) As long as pointers can
only be dereferenced in the IO monad we should be OK though.


(Foreign.ForeignPtr - as above)
(Foreign.Ptr         - as above)

**GHC.IOBase:**
keeping unsafe and no safe version as depreciated module.

**GHC.IORef:**
Made safe version due to access to IORef constructor

**GHC.Pack:**
keeping unsafe and no safe version.
unpackCString\# Among others seem quite unsafe.

**GHC.Weak:**
\*Made a Safe version but I had to leave GHC.Weak alone. When I tried to move GHC.Weak to GHC.Weak.Imp I would constantly get link errors when linking the libRts library. I changed the values in compiler/prelude/PrelNames.hs for GHC.Weak but this didn't seem to work. So there is GHC.Weak.Safe and GHC.Weak.Unsafe but no GHC.Weak.Imp and GHC.Weak has to be unsafe.

**GHC.Word:**
Left unmodified and made trustworthy
'uncheckedShiftRL64' is a little scary sounding but seems fine.

**Data.Data** and **Data.Dynamic** and **Data.Typeable**'
Left unsafe due to whole [Typeable](safe-haskell/base-package#) issue.

**Debug.Trace:**
Was left unsafe. It can leak information to the console without detection.

## Base Module


The root of the base package and so of Haskell is GHC.Base and GHC.Prim. These both contain a lot of code and a lot of it is unsafe. Some of it obviously other less so. For example:

- **Addr\#** and **Array\#** types are basically C style pointers, so no bounds checks. Can access arbitary memory with them, buffer overflows... ect
- **divInt :: Int -\> Int -\> Int** seems perfectly safe but division by zero throws an uncatchable exception that crashes the program. (Is this intentional or a bug?)


It is also quite difficult to split this up since 1) GHC.Prim is defined inside of GHC not in any module text file, 2) GHC.Base is defined in a text file but extended by GHC (so GHC.Base exports Bool but Bool isn't defined in the actual GHC.Base text file).                                                    


This is potentially another argument for symbol level safety, it would make handling Base and Prim easier.


This does mean a lot of stuff is trustworthy though since
they import Base. I'd be happy to deal with the complexity
of making Safe versions but it seemed like the ongoing
maintenance work wouldn't be worth the benefits.


The best solution might be to leave Base and Prim alone and make Base.Safe and Prim.Safe that are both extended on demmand. (e.g we just add safe symbols to them as needed to get modules that use Base and Prim in a safe way to work in -XSafe). A fine grained total split of Base and Prim is doable but seems like it might be a maintenance problem.

## Data.Typeable


I feel we could enable all of this except make Typeable
abstract so that instances can't be defined. (Could also
still allow deriving of these instances). My understanding
is that all of this dynamic stuff works fine as long as
the typeOf method basically doesn't lie and pretend two
types are the same. The original SYB paper on Typeable from
memory basically said this and said that allowing programmers
to define their own instances of typeOf was really an implementation
artifact and that it should be left up to the compiler.
