# Welcome to the GHC Developer Wiki


This is the home for GHC developers. If you're only interested in using GHC,
then you probably want the [ GHC home page](http://www.haskell.org/ghc). If you are an aspiring GHC developer,
and just want to get started, read the [Newcomers page](newcomers) (which is always in the
sidebar under "Joining in").

## Please help improve the GHC developer's wiki


Please help us improve the information on the GHC developer's wiki. You can easily do this by editing the wiki directly. Just [ register](https://ghc.haskell.org/trac/ghc/register) an account, and then edit away. Alternatively, [ log in](https://ghc.haskell.org/trac/ghc/login) as user **guest** with password **guest** (but we'd prefer you to create an account, because it enables us to contact you if necessary). The [ Help/Guide](https://ghc.haskell.org/trac/ghc/wiki/TracGuide) link at the top of every page gives a good description of the markup language and how to use Trac in general.

## Status updates

- Forthcoming planned releases:

  - 8.0.1 (0 (Ticket query: status: open, priority: high, priority: highest, milestone: 8.0.1, max: 0, order: id) open tickets with priority high or highest) 

- Total open tickets: 3204 (Ticket query: status: !closed, max: 0, order: id) (

  <table><tr><th>[\#149](https://gitlab.haskell.org//ghc/ghc/issues/149)</th>
  <td>missed CSE opportunity</td></tr>
  <tr><th>[\#344](https://gitlab.haskell.org//ghc/ghc/issues/344)</th>
  <td>arrow notation: incorrect scope of existential dictionaries</td></tr>
  <tr><th>[\#345](https://gitlab.haskell.org//ghc/ghc/issues/345)</th>
  <td>GADT - fundep interaction</td></tr>
  <tr><th>[\#367](https://gitlab.haskell.org//ghc/ghc/issues/367)</th>
  <td>Infinite loops can hang Concurrent Haskell</td></tr>
  <tr><th>[\#418](https://gitlab.haskell.org//ghc/ghc/issues/418)</th>
  <td>throwTo to a thread inside 'block'</td></tr>
  <tr><th>[\#552](https://gitlab.haskell.org//ghc/ghc/issues/552)</th>
  <td>GHCi  :m  doesn't restore default decl</td></tr>
  <tr><th>[\#589](https://gitlab.haskell.org//ghc/ghc/issues/589)</th>
  <td>Various poor type error messages</td></tr>
  <tr><th>[\#781](https://gitlab.haskell.org//ghc/ghc/issues/781)</th>
  <td>GHCi on x86_64, cannot link to static data in shared libs</td></tr>
  <tr><th>[\#806](https://gitlab.haskell.org//ghc/ghc/issues/806)</th>
  <td>hGetBufNonBlocking doesn't work on Windows</td></tr>
  <tr><th>[\#816](https://gitlab.haskell.org//ghc/ghc/issues/816)</th>
  <td>Weird fundep behavior (with -fallow-undecidable-instances)</td></tr>
  <tr><th>[\#917](https://gitlab.haskell.org//ghc/ghc/issues/917)</th>
  <td>-O introduces space leak</td></tr>
  <tr><th>[\#926](https://gitlab.haskell.org//ghc/ghc/issues/926)</th>
  <td>infinite loop in ShutdownIOManager()</td></tr>
  <tr><th>[\#947](https://gitlab.haskell.org//ghc/ghc/issues/947)</th>
  <td>ghc -O space leak: CSE between different CAFs</td></tr>
  <tr><th>[\#1012](https://gitlab.haskell.org//ghc/ghc/issues/1012)</th>
  <td>ghc panic with mutually recursive modules and template haskell</td></tr>
  <tr><th>[\#1057](https://gitlab.haskell.org//ghc/ghc/issues/1057)</th>
  <td>Implicit parameters on breakpoints</td></tr>
  <tr><th>[\#1087](https://gitlab.haskell.org//ghc/ghc/issues/1087)</th>
  <td>bang patterns with infix ops</td></tr>
  <tr><th>[\#1147](https://gitlab.haskell.org//ghc/ghc/issues/1147)</th>
  <td>Quadratic behaviour in the compacting GC</td></tr>
  <tr><th>[\#1158](https://gitlab.haskell.org//ghc/ghc/issues/1158)</th>
  <td>Problem with GADTs and explicit type signatures</td></tr>
  <tr><th>[\#1168](https://gitlab.haskell.org//ghc/ghc/issues/1168)</th>
  <td>Optimisation sometimes decreases sharing in IO code</td></tr>
  <tr><th>[\#1171](https://gitlab.haskell.org//ghc/ghc/issues/1171)</th>
  <td>GHC doesn't respect the imprecise exceptions semantics</td></tr>
  <tr><th>[\#1216](https://gitlab.haskell.org//ghc/ghc/issues/1216)</th>
  <td>Missed opportunity for let-no-esape</td></tr>
  <tr><th>[\#1290](https://gitlab.haskell.org//ghc/ghc/issues/1290)</th>
  <td>ghc runs preprocessor too much</td></tr>
  <tr><th>[\#1308](https://gitlab.haskell.org//ghc/ghc/issues/1308)</th>
  <td>Type signature in warning is wrong</td></tr>
  <tr><th>[\#1330](https://gitlab.haskell.org//ghc/ghc/issues/1330)</th>
  <td>Impredicativity bug: Church2 test gives a rather confusing error with the HEAD</td></tr>
  <tr><th>[\#1400](https://gitlab.haskell.org//ghc/ghc/issues/1400)</th>
  <td>:set +r doesn't work for interpreted modules</td></tr>
  <tr><th>[\#1466](https://gitlab.haskell.org//ghc/ghc/issues/1466)</th>
  <td>Stack check for AP_STACK</td></tr>
  <tr><th>[\#1487](https://gitlab.haskell.org//ghc/ghc/issues/1487)</th>
  <td>unix package: test needed for getLoginName</td></tr>
  <tr><th>[\#1498](https://gitlab.haskell.org//ghc/ghc/issues/1498)</th>
  <td>Optimisation: eliminate unnecessary heap check in recursive function</td></tr>
  <tr><th>[\#1526](https://gitlab.haskell.org//ghc/ghc/issues/1526)</th>
  <td>-fobject-code doesn't apply to expressions typed at the prompt</td></tr>
  <tr><th>[\#1530](https://gitlab.haskell.org//ghc/ghc/issues/1530)</th>
  <td>debugging :steps inside TH spliced code need to be bypassed</td></tr>
  <tr><th>[\#1544](https://gitlab.haskell.org//ghc/ghc/issues/1544)</th>
  <td>Derived Read instances for recursive datatypes with infix constructors are too inefficient</td></tr>
  <tr><th>[\#1612](https://gitlab.haskell.org//ghc/ghc/issues/1612)</th>
  <td>GHC_PACKAGE_PATH and $topdir bug</td></tr>
  <tr><th>[\#1614](https://gitlab.haskell.org//ghc/ghc/issues/1614)</th>
  <td>Type checker does not use functional dependency to avoid ambiguity</td></tr>
  <tr><th>[\#1620](https://gitlab.haskell.org//ghc/ghc/issues/1620)</th>
  <td>ModBreaks.modBreaks_array not initialised</td></tr>
  <tr><th>[\#1687](https://gitlab.haskell.org//ghc/ghc/issues/1687)</th>
  <td>A faster (\^)-function.</td></tr>
  <tr><th>[\#1693](https://gitlab.haskell.org//ghc/ghc/issues/1693)</th>
  <td>Make distclean (still) doesn't</td></tr>
  <tr><th>[\#1727](https://gitlab.haskell.org//ghc/ghc/issues/1727)</th>
  <td>Precedence and associativity rules ignored when mixing infix type and data constructors in a single expression</td></tr>
  <tr><th>[\#1831](https://gitlab.haskell.org//ghc/ghc/issues/1831)</th>
  <td>reify never provides the declaration of variables</td></tr>
  <tr><th>[\#1851](https://gitlab.haskell.org//ghc/ghc/issues/1851)</th>
  <td>"make install-strip" should work</td></tr>
  <tr><th>[\#1853](https://gitlab.haskell.org//ghc/ghc/issues/1853)</th>
  <td>hpc mix files for Main modules overwrite each other</td></tr>
  <tr><th>[\#1928](https://gitlab.haskell.org//ghc/ghc/issues/1928)</th>
  <td>Confusing type error message</td></tr>
  <tr><th>[\#2028](https://gitlab.haskell.org//ghc/ghc/issues/2028)</th>
  <td>STM slightly conservative on write-only transactions</td></tr>
  <tr><th>[\#2031](https://gitlab.haskell.org//ghc/ghc/issues/2031)</th>
  <td>relocation overflow</td></tr>
  <tr><th>[\#2057](https://gitlab.haskell.org//ghc/ghc/issues/2057)</th>
  <td>inconsistent .hi file error gets ignored</td></tr>
  <tr><th>[\#2132](https://gitlab.haskell.org//ghc/ghc/issues/2132)</th>
  <td>Optimise nested comparisons</td></tr>
  <tr><th>[\#2140](https://gitlab.haskell.org//ghc/ghc/issues/2140)</th>
  <td>cpuTimePrecision is wrong</td></tr>
  <tr><th>[\#2147](https://gitlab.haskell.org//ghc/ghc/issues/2147)</th>
  <td>unhelpful error message for a misplaced DEPRECATED pragma</td></tr>
  <tr><th>[\#2189](https://gitlab.haskell.org//ghc/ghc/issues/2189)</th>
  <td>hSetBuffering stdin NoBuffering doesn't work on Windows</td></tr>
  <tr><th>[\#2224](https://gitlab.haskell.org//ghc/ghc/issues/2224)</th>
  <td>-fhpc inteferes/prevents rewrite rules from firing</td></tr>
  <tr><th>[\#2255](https://gitlab.haskell.org//ghc/ghc/issues/2255)</th>
  <td>Improve SpecConstr for free variables</td></tr>
  <tr><th>[\#2273](https://gitlab.haskell.org//ghc/ghc/issues/2273)</th>
  <td>inlining defeats seq</td></tr>
  <tr><th>[\#2289](https://gitlab.haskell.org//ghc/ghc/issues/2289)</th>
  <td>Needless reboxing of values when returning from a tight loop</td></tr>
  <tr><th>[\#2346](https://gitlab.haskell.org//ghc/ghc/issues/2346)</th>
  <td>Compilation of large source files requires a lot of RAM</td></tr>
  <tr><th>[\#2356](https://gitlab.haskell.org//ghc/ghc/issues/2356)</th>
  <td>GHC accepts multiple instances for the same type in different modules</td></tr>
  <tr><th>[\#2370](https://gitlab.haskell.org//ghc/ghc/issues/2370)</th>
  <td>num009 fails on OS X 10.5?</td></tr>
  <tr><th>[\#2374](https://gitlab.haskell.org//ghc/ghc/issues/2374)</th>
  <td>MutableByteArray\# is slower than Addr\#</td></tr>
  <tr><th>[\#2387](https://gitlab.haskell.org//ghc/ghc/issues/2387)</th>
  <td>Optimizer misses unboxing opportunity</td></tr>
  <tr><th>[\#2401](https://gitlab.haskell.org//ghc/ghc/issues/2401)</th>
  <td>aborting an STM transaction should throw an exception</td></tr>
  <tr><th>[\#2408](https://gitlab.haskell.org//ghc/ghc/issues/2408)</th>
  <td>threadWaitRead on mingw32 threaded causes internal error</td></tr>
  <tr><th>[\#2439](https://gitlab.haskell.org//ghc/ghc/issues/2439)</th>
  <td>Missed optimisation with dictionaries and loops</td></tr>
  <tr><th>[\#2496](https://gitlab.haskell.org//ghc/ghc/issues/2496)</th>
  <td>Invalid Eq/Ord instances in Data.Version</td></tr>
  <tr><th>[\#2607](https://gitlab.haskell.org//ghc/ghc/issues/2607)</th>
  <td>Inlining defeats selector thunk optimisation</td></tr>
  <tr><th>[\#2625](https://gitlab.haskell.org//ghc/ghc/issues/2625)</th>
  <td>Unexpected -ddump-simpl output for derived Ord instance and UNPACKed fields</td></tr>
  <tr><th>[\#2642](https://gitlab.haskell.org//ghc/ghc/issues/2642)</th>
  <td>Improve SpecConstr for join points</td></tr>
  <tr><th>[\#2731](https://gitlab.haskell.org//ghc/ghc/issues/2731)</th>
  <td>Avoid unnecessary evaluation when unpacking constructors</td></tr>
  <tr><th>[\#2776](https://gitlab.haskell.org//ghc/ghc/issues/2776)</th>
  <td>Document -pgmL (Use cmd as the literate pre-processor)</td></tr>
  <tr><th>[\#2783](https://gitlab.haskell.org//ghc/ghc/issues/2783)</th>
  <td>RTS -K/-M options not honored</td></tr>
  <tr><th>[\#2786](https://gitlab.haskell.org//ghc/ghc/issues/2786)</th>
  <td>Blackhole loops are not detected and reported in GHCi</td></tr>
  <tr><th>[\#2926](https://gitlab.haskell.org//ghc/ghc/issues/2926)</th>
  <td>Foreign exported function returns wrong type</td></tr>
  <tr><th>[\#2933](https://gitlab.haskell.org//ghc/ghc/issues/2933)</th>
  <td>LDFLAGS ignored by build system</td></tr>
  <tr><th>[\#3034](https://gitlab.haskell.org//ghc/ghc/issues/3034)</th>
  <td>divInt\# floated into a position which leads to low arity</td></tr>
  <tr><th>[\#3048](https://gitlab.haskell.org//ghc/ghc/issues/3048)</th>
  <td>Heap size suggestion gets ignored when -G1 flag is passed</td></tr>
  <tr><th>[\#3055](https://gitlab.haskell.org//ghc/ghc/issues/3055)</th>
  <td>Int / Word / IntN / WordN are unequally optimized</td></tr>
  <tr><th>[\#3061](https://gitlab.haskell.org//ghc/ghc/issues/3061)</th>
  <td>GHC's GC default heap growth strategy is not as good as other runtimes</td></tr>
  <tr><th>[\#3070](https://gitlab.haskell.org//ghc/ghc/issues/3070)</th>
  <td>floor(0/0) should not be defined</td></tr>
  <tr><th>[\#3073](https://gitlab.haskell.org//ghc/ghc/issues/3073)</th>
  <td>Avoid reconstructing dictionaries in recursive instance methods</td></tr>
  <tr><th>[\#3081](https://gitlab.haskell.org//ghc/ghc/issues/3081)</th>
  <td>Double output after Ctrl+C on Windows</td></tr>
  <tr><th>[\#3094](https://gitlab.haskell.org//ghc/ghc/issues/3094)</th>
  <td>Some GHC.\* module should export word size and heap object header size</td></tr>
  <tr><th>[\#3107](https://gitlab.haskell.org//ghc/ghc/issues/3107)</th>
  <td>Over-eager GC when blocked on a signal in the non-threaded runtime</td></tr>
  <tr><th>[\#3134](https://gitlab.haskell.org//ghc/ghc/issues/3134)</th>
  <td>encodeFloat . decodeFloat</td></tr>
  <tr><th>[\#3138](https://gitlab.haskell.org//ghc/ghc/issues/3138)</th>
  <td>Returning a known constructor: GHC generates terrible code for cmonad</td></tr>
  <tr><th>[\#3184](https://gitlab.haskell.org//ghc/ghc/issues/3184)</th>
  <td>package.conf.d should be under /var, not /usr</td></tr>
  <tr><th>[\#3231](https://gitlab.haskell.org//ghc/ghc/issues/3231)</th>
  <td>Permission denied error with runProcess/openFile</td></tr>
  <tr><th>[\#3238](https://gitlab.haskell.org//ghc/ghc/issues/3238)</th>
  <td>CInt FFI exports do not use C int in _stub.h header file</td></tr>
  <tr><th>[\#3351](https://gitlab.haskell.org//ghc/ghc/issues/3351)</th>
  <td>Generated ghc man page missing xrefs</td></tr>
  <tr><th>[\#3353](https://gitlab.haskell.org//ghc/ghc/issues/3353)</th>
  <td>Add CLDouble support</td></tr>
  <tr><th>[\#3373](https://gitlab.haskell.org//ghc/ghc/issues/3373)</th>
  <td>GHC API is not thread safe</td></tr>
  <tr><th>[\#3458](https://gitlab.haskell.org//ghc/ghc/issues/3458)</th>
  <td>Allocation where none should happen</td></tr>
  <tr><th>[\#3549](https://gitlab.haskell.org//ghc/ghc/issues/3549)</th>
  <td>unlit does not follow H98 spec</td></tr>
  <tr><th>[\#3571](https://gitlab.haskell.org//ghc/ghc/issues/3571)</th>
  <td>Bizzarely bloated binaries</td></tr>
  <tr><th>[\#3588](https://gitlab.haskell.org//ghc/ghc/issues/3588)</th>
  <td>ghc -M should emit dependencies on CPP headers</td></tr>
  <tr><th>[\#3606](https://gitlab.haskell.org//ghc/ghc/issues/3606)</th>
  <td>The Ord instance for unboxed arrays is very inefficient</td></tr>
  <tr><th>[\#3628](https://gitlab.haskell.org//ghc/ghc/issues/3628)</th>
  <td>exceptions reported to stderr when they propagate past forkIO</td></tr>
  <tr><th>[\#3676](https://gitlab.haskell.org//ghc/ghc/issues/3676)</th>
  <td>realToFrac doesn't sanely convert between floating types</td></tr>
  <tr><th>[\#3711](https://gitlab.haskell.org//ghc/ghc/issues/3711)</th>
  <td>Bad error reporting when calling a function in a module which depends on a DLL on Windows</td></tr>
  <tr><th>[\#3765](https://gitlab.haskell.org//ghc/ghc/issues/3765)</th>
  <td>Rules should "look through" case binders too</td></tr>
  <tr><th>[\#3766](https://gitlab.haskell.org//ghc/ghc/issues/3766)</th>
  <td>Parsing of lambdas is not consistent with Haskell'98 report.</td></tr>
  <tr><th>[\#3767](https://gitlab.haskell.org//ghc/ghc/issues/3767)</th>
  <td>SpecConstr for join points</td></tr>
  <tr><th>[\#3781](https://gitlab.haskell.org//ghc/ghc/issues/3781)</th>
  <td>Improve inlining for local functions</td></tr>
  <tr><th>[\#3782](https://gitlab.haskell.org//ghc/ghc/issues/3782)</th>
  <td>Data Parallel "Impossible happened" compiler error</td></tr>
  <tr><th>[\#3831](https://gitlab.haskell.org//ghc/ghc/issues/3831)</th>
  <td>SpecConstr should exploit cases where there is exactly one call pattern</td></tr>
  <tr><th>[\#3872](https://gitlab.haskell.org//ghc/ghc/issues/3872)</th>
  <td>New way to make the simplifier diverge</td></tr>
  <tr><th>[\#3881](https://gitlab.haskell.org//ghc/ghc/issues/3881)</th>
  <td>section parse errors, e.g. ( let x=1 in x + )</td></tr>
  <tr><th>[\#3903](https://gitlab.haskell.org//ghc/ghc/issues/3903)</th>
  <td>DPH bad sliceP causes RTS panic "allocGroup: requested zero blocks"</td></tr>
  <tr><th>[\#3937](https://gitlab.haskell.org//ghc/ghc/issues/3937)</th>
  <td>Cannot killThread in listen/accept on Windows threaded runtime</td></tr>
  <tr><th>[\#3960](https://gitlab.haskell.org//ghc/ghc/issues/3960)</th>
  <td>ghc panic when attempting to compile DPH code</td></tr>
  <tr><th>[\#3995](https://gitlab.haskell.org//ghc/ghc/issues/3995)</th>
  <td>Comment delimiters ignored inside compiler pragma</td></tr>
  <tr><th>[\#3998](https://gitlab.haskell.org//ghc/ghc/issues/3998)</th>
  <td>strace breaks System.Process(?)</td></tr>
  <tr><th>[\#4005](https://gitlab.haskell.org//ghc/ghc/issues/4005)</th>
  <td>Bad behaviour in the generational GC with paraffins -O2</td></tr>
  <tr><th>[\#4012](https://gitlab.haskell.org//ghc/ghc/issues/4012)</th>
  <td>Compilation results are not deterministic</td></tr>
  <tr><th>[\#4022](https://gitlab.haskell.org//ghc/ghc/issues/4022)</th>
  <td>GHC Bindist is Broken on FreeBSD/amd64</td></tr>
  <tr><th>[\#4043](https://gitlab.haskell.org//ghc/ghc/issues/4043)</th>
  <td>Parsing of guards, and type signatures</td></tr>
  <tr><th>[\#4048](https://gitlab.haskell.org//ghc/ghc/issues/4048)</th>
  <td>ghc-pkg should check for existence of extra-libraries</td></tr>
  <tr><th>[\#4049](https://gitlab.haskell.org//ghc/ghc/issues/4049)</th>
  <td>Support for ABI versioning of C libraries</td></tr>
  <tr><th>[\#4081](https://gitlab.haskell.org//ghc/ghc/issues/4081)</th>
  <td>Strict constructor fields inspected in loop</td></tr>
  <tr><th>[\#4101](https://gitlab.haskell.org//ghc/ghc/issues/4101)</th>
  <td>Primitive constant unfolding</td></tr>
  <tr><th>[\#4105](https://gitlab.haskell.org//ghc/ghc/issues/4105)</th>
  <td>ffi005 fails on OS X</td></tr>
  <tr><th>[\#4140](https://gitlab.haskell.org//ghc/ghc/issues/4140)</th>
  <td>dynHelloWorld(dyn) fails in an unreg build</td></tr>
  <tr><th>[\#4144](https://gitlab.haskell.org//ghc/ghc/issues/4144)</th>
  <td>Exception: ToDo: hGetBuf - when using custom handle infrastructure</td></tr>
  <tr><th>[\#4150](https://gitlab.haskell.org//ghc/ghc/issues/4150)</th>
  <td>CPP+QuasiQuotes confuses compilation errors' line numbers</td></tr>
  <tr><th>[\#4162](https://gitlab.haskell.org//ghc/ghc/issues/4162)</th>
  <td>GHC API messes up signal handlers</td></tr>
  <tr><th>[\#4176](https://gitlab.haskell.org//ghc/ghc/issues/4176)</th>
  <td>reject unary minus in infix left hand side function bindings that resolve differently as expressions</td></tr>
  <tr><th>[\#4288](https://gitlab.haskell.org//ghc/ghc/issues/4288)</th>
  <td>Poor -fspec-constr-count=n warning messages</td></tr>
  <tr><th>[\#4296](https://gitlab.haskell.org//ghc/ghc/issues/4296)</th>
  <td>The dreaded SkolemOccurs problem</td></tr>
  <tr><th>[\#4301](https://gitlab.haskell.org//ghc/ghc/issues/4301)</th>
  <td>Optimisations give bad core for foldl' (flip seq) ()</td></tr>
  <tr><th>[\#4308](https://gitlab.haskell.org//ghc/ghc/issues/4308)</th>
  <td>LLVM compiles Updates.cmm badly</td></tr>
  <tr><th>[\#4372](https://gitlab.haskell.org//ghc/ghc/issues/4372)</th>
  <td>Accept expressions in left-hand side of quasiquotations</td></tr>
  <tr><th>[\#4413](https://gitlab.haskell.org//ghc/ghc/issues/4413)</th>
  <td>(\^\^) is not correct for Double and Float</td></tr>
  <tr><th>[\#4451](https://gitlab.haskell.org//ghc/ghc/issues/4451)</th>
  <td>Re-linking avoidance is too aggressive</td></tr>
  <tr><th>[\#4471](https://gitlab.haskell.org//ghc/ghc/issues/4471)</th>
  <td>Incorrect Unicode output on Windows Console</td></tr>
  <tr><th>[\#4505](https://gitlab.haskell.org//ghc/ghc/issues/4505)</th>
  <td>Segmentation fault on long input (list of pairs)</td></tr>
  <tr><th>[\#4824](https://gitlab.haskell.org//ghc/ghc/issues/4824)</th>
  <td>Windows: Dynamic linking doesn't work out-of-the-box</td></tr>
  <tr><th>[\#4831](https://gitlab.haskell.org//ghc/ghc/issues/4831)</th>
  <td>Too many specialisations in SpecConstr</td></tr>
  <tr><th>[\#4833](https://gitlab.haskell.org//ghc/ghc/issues/4833)</th>
  <td>Finding the right loop breaker</td></tr>
  <tr><th>[\#4836](https://gitlab.haskell.org//ghc/ghc/issues/4836)</th>
  <td>literate markdown not handled correctly by unlit</td></tr>
  <tr><th>[\#4899](https://gitlab.haskell.org//ghc/ghc/issues/4899)</th>
  <td>Non-standard compile plus Template Haskell produces spurious "unknown symbol" linker error</td></tr>
  <tr><th>[\#4942](https://gitlab.haskell.org//ghc/ghc/issues/4942)</th>
  <td>GHC.ConsoleHandler does not call back application when Close button is pressed</td></tr>
  <tr><th>[\#4945](https://gitlab.haskell.org//ghc/ghc/issues/4945)</th>
  <td>Another SpecConstr infelicity</td></tr>
  <tr><th>[\#5041](https://gitlab.haskell.org//ghc/ghc/issues/5041)</th>
  <td>Incorrect Read deriving for MagicHash constructors</td></tr>
  <tr><th>[\#5051](https://gitlab.haskell.org//ghc/ghc/issues/5051)</th>
  <td>Typechecker behaviour change</td></tr>
  <tr><th>[\#5071](https://gitlab.haskell.org//ghc/ghc/issues/5071)</th>
  <td>GHCi crashes on large alloca/allocaBytes requests</td></tr>
  <tr><th>[\#5142](https://gitlab.haskell.org//ghc/ghc/issues/5142)</th>
  <td>stub header files don't work with the MS C compiler</td></tr>
  <tr><th>[\#5188](https://gitlab.haskell.org//ghc/ghc/issues/5188)</th>
  <td>Runtime error when allocating lots of memory</td></tr>
  <tr><th>[\#5224](https://gitlab.haskell.org//ghc/ghc/issues/5224)</th>
  <td>Improve consistency checking for family instances</td></tr>
  <tr><th>[\#5262](https://gitlab.haskell.org//ghc/ghc/issues/5262)</th>
  <td>Compiling with -O makes some expressions too lazy and causes space leaks</td></tr>
  <tr><th>[\#5267](https://gitlab.haskell.org//ghc/ghc/issues/5267)</th>
  <td>Missing type checks for arrow command combinators</td></tr>
  <tr><th>[\#5291](https://gitlab.haskell.org//ghc/ghc/issues/5291)</th>
  <td>GhcDynamic build fails on Windows: can't find DLLs</td></tr>
  <tr><th>[\#5298](https://gitlab.haskell.org//ghc/ghc/issues/5298)</th>
  <td>Inlined functions aren't fully specialised</td></tr>
  <tr><th>[\#5302](https://gitlab.haskell.org//ghc/ghc/issues/5302)</th>
  <td>Unused arguments in join points</td></tr>
  <tr><th>[\#5305](https://gitlab.haskell.org//ghc/ghc/issues/5305)</th>
  <td>crash after writing around 40 gigabytes to stdout</td></tr>
  <tr><th>[\#5320](https://gitlab.haskell.org//ghc/ghc/issues/5320)</th>
  <td>check_overlap panic (7.1 regression)</td></tr>
  <tr><th>[\#5326](https://gitlab.haskell.org//ghc/ghc/issues/5326)</th>
  <td>Polymorphic instances aren't automatically specialised</td></tr>
  <tr><th>[\#5340](https://gitlab.haskell.org//ghc/ghc/issues/5340)</th>
  <td>wrong warning on incomplete case analysis in conjunction with empty data declarations</td></tr>
  <tr><th>[\#5355](https://gitlab.haskell.org//ghc/ghc/issues/5355)</th>
  <td>Link plugins against existing libHSghc</td></tr>
  <tr><th>[\#5369](https://gitlab.haskell.org//ghc/ghc/issues/5369)</th>
  <td>Reinstate VECTORISE pragmas with expressions as right-hand sides</td></tr>
  <tr><th>[\#5378](https://gitlab.haskell.org//ghc/ghc/issues/5378)</th>
  <td>unreg compiler: warning: conflicting types for built-in function ‘memcpy’</td></tr>
  <tr><th>[\#5400](https://gitlab.haskell.org//ghc/ghc/issues/5400)</th>
  <td>GHC loops on compiling with optimizations</td></tr>
  <tr><th>[\#5444](https://gitlab.haskell.org//ghc/ghc/issues/5444)</th>
  <td>Slow 64-bit primops on 32 bit system</td></tr>
  <tr><th>[\#5448](https://gitlab.haskell.org//ghc/ghc/issues/5448)</th>
  <td>GHC stuck in infinite loop compiling with optimizations</td></tr>
  <tr><th>[\#5463](https://gitlab.haskell.org//ghc/ghc/issues/5463)</th>
  <td>SPECIALISE pragmas generated from Template Haskell are ignored</td></tr>
  <tr><th>[\#5466](https://gitlab.haskell.org//ghc/ghc/issues/5466)</th>
  <td>Documentation for Chan could be better</td></tr>
  <tr><th>[\#5470](https://gitlab.haskell.org//ghc/ghc/issues/5470)</th>
  <td>The DPH library needs to support PData and PRepr instances for more than 15-tuples</td></tr>
  <tr><th>[\#5495](https://gitlab.haskell.org//ghc/ghc/issues/5495)</th>
  <td>simple program fails with -shared on mac</td></tr>
  <tr><th>[\#5620](https://gitlab.haskell.org//ghc/ghc/issues/5620)</th>
  <td>Dynamic linking and threading does not work on Windows</td></tr>
  <tr><th>[\#5641](https://gitlab.haskell.org//ghc/ghc/issues/5641)</th>
  <td>The -L flag should not exist</td></tr>
  <tr><th>[\#5642](https://gitlab.haskell.org//ghc/ghc/issues/5642)</th>
  <td>Deriving Generic of a big type takes a long time and lots of space</td></tr>
  <tr><th>[\#5645](https://gitlab.haskell.org//ghc/ghc/issues/5645)</th>
  <td>Sharing across functions causing space leak</td></tr>
  <tr><th>[\#5646](https://gitlab.haskell.org//ghc/ghc/issues/5646)</th>
  <td>Initialise tuples using pragmas</td></tr>
  <tr><th>[\#5702](https://gitlab.haskell.org//ghc/ghc/issues/5702)</th>
  <td>Can't vectorise pattern matching on numeric literals</td></tr>
  <tr><th>[\#5722](https://gitlab.haskell.org//ghc/ghc/issues/5722)</th>
  <td>GHC inlines class method forever</td></tr>
  <tr><th>[\#5761](https://gitlab.haskell.org//ghc/ghc/issues/5761)</th>
  <td>Getting stdout and stderr as a single handle from createProcess does not work on Windows</td></tr>
  <tr><th>[\#5775](https://gitlab.haskell.org//ghc/ghc/issues/5775)</th>
  <td>Inconsistency in demand analysis</td></tr>
  <tr><th>[\#5777](https://gitlab.haskell.org//ghc/ghc/issues/5777)</th>
  <td>core lint error with arrow notation and GADTs</td></tr>
  <tr><th>[\#5780](https://gitlab.haskell.org//ghc/ghc/issues/5780)</th>
  <td>-faggressive-primops change caused a failure in perf/compiler/parsing001</td></tr>
  <tr><th>[\#5797](https://gitlab.haskell.org//ghc/ghc/issues/5797)</th>
  <td>readRawBufferPtr cannot be interrupted by exception on Windows with -threaded</td></tr>
  <tr><th>[\#5807](https://gitlab.haskell.org//ghc/ghc/issues/5807)</th>
  <td>DPH library functions don't work without -fvectorise.</td></tr>
  <tr><th>[\#5840](https://gitlab.haskell.org//ghc/ghc/issues/5840)</th>
  <td>Extend the supported environment sizes of vectorised closures</td></tr>
  <tr><th>[\#5902](https://gitlab.haskell.org//ghc/ghc/issues/5902)</th>
  <td>Cannot tell from an exception handler whether the exception was asynchronous</td></tr>
  <tr><th>[\#5928](https://gitlab.haskell.org//ghc/ghc/issues/5928)</th>
  <td>INLINABLE fails to specialize in presence of simple wrapper</td></tr>
  <tr><th>[\#5954](https://gitlab.haskell.org//ghc/ghc/issues/5954)</th>
  <td>Performance regression 7.0 -\> 7.2 (still in 7.4)</td></tr>
  <tr><th>[\#5959](https://gitlab.haskell.org//ghc/ghc/issues/5959)</th>
  <td>Top level splice in Template Haskell has over-ambitious lexical scope?</td></tr>
  <tr><th>[\#5974](https://gitlab.haskell.org//ghc/ghc/issues/5974)</th>
  <td>Casts, rules, and parametricity</td></tr>
  <tr><th>[\#6004](https://gitlab.haskell.org//ghc/ghc/issues/6004)</th>
  <td>dph-lifted-vseg package doesn't provide Data.Array.Parallel.Prelude.Float module</td></tr>
  <tr><th>[\#6034](https://gitlab.haskell.org//ghc/ghc/issues/6034)</th>
  <td>Parse error when using ' with promoted kinds</td></tr>
  <tr><th>[\#6040](https://gitlab.haskell.org//ghc/ghc/issues/6040)</th>
  <td>Adding a type signature changes heap allocation into stack allocation without changing the actual type</td></tr>
  <tr><th>[\#6047](https://gitlab.haskell.org//ghc/ghc/issues/6047)</th>
  <td>GHC retains unnecessary binding</td></tr>
  <tr><th>[\#6065](https://gitlab.haskell.org//ghc/ghc/issues/6065)</th>
  <td>Suggested type signature causes a type error (even though it appears correct)</td></tr>
  <tr><th>[\#6070](https://gitlab.haskell.org//ghc/ghc/issues/6070)</th>
  <td>Fun with the demand analyser</td></tr>
  <tr><th>[\#6087](https://gitlab.haskell.org//ghc/ghc/issues/6087)</th>
  <td>Join points need strictness analysis</td></tr>
  <tr><th>[\#6092](https://gitlab.haskell.org//ghc/ghc/issues/6092)</th>
  <td>Liberate case not happening</td></tr>
  <tr><th>[\#6101](https://gitlab.haskell.org//ghc/ghc/issues/6101)</th>
  <td>Show instance for integer-simple is not lazy enough</td></tr>
  <tr><th>[\#6107](https://gitlab.haskell.org//ghc/ghc/issues/6107)</th>
  <td>GHCi runtime linker cannot link with duplicate common symbols</td></tr>
  <tr><th>[\#6113](https://gitlab.haskell.org//ghc/ghc/issues/6113)</th>
  <td>Profiling with -p not written if killed with SIGTERM</td></tr>
  <tr><th>[\#6132](https://gitlab.haskell.org//ghc/ghc/issues/6132)</th>
  <td>Can't use both shebang line and \#ifdef declarations in the same file.</td></tr>
  <tr><th>[\#7026](https://gitlab.haskell.org//ghc/ghc/issues/7026)</th>
  <td>Impredicative implicit parameters</td></tr>
  <tr><th>[\#7044](https://gitlab.haskell.org//ghc/ghc/issues/7044)</th>
  <td>reject reading rationals with exponent notation</td></tr>
  <tr><th>[\#7045](https://gitlab.haskell.org//ghc/ghc/issues/7045)</th>
  <td>The \`Read\` instance of \`Rational\` does not support decimal notation</td></tr>
  <tr><th>[\#7057](https://gitlab.haskell.org//ghc/ghc/issues/7057)</th>
  <td>Simplifier infinite loop regression</td></tr>
  <tr><th>[\#7063](https://gitlab.haskell.org//ghc/ghc/issues/7063)</th>
  <td>Register allocators can't handle non-uniform register sets</td></tr>
  <tr><th>[\#7066](https://gitlab.haskell.org//ghc/ghc/issues/7066)</th>
  <td>isInstance does not work for compound types</td></tr>
  <tr><th>[\#7078](https://gitlab.haskell.org//ghc/ghc/issues/7078)</th>
  <td>Panic using mixing list with parallel arrays incorrectly</td></tr>
  <tr><th>[\#7080](https://gitlab.haskell.org//ghc/ghc/issues/7080)</th>
  <td>Make RULES and SPECIALISE more consistent</td></tr>
  <tr><th>[\#7098](https://gitlab.haskell.org//ghc/ghc/issues/7098)</th>
  <td>GHC 7.4.1 reports an internal error and core dumps while using DPH</td></tr>
  <tr><th>[\#7102](https://gitlab.haskell.org//ghc/ghc/issues/7102)</th>
  <td>Type family instance overlap accepted in ghci</td></tr>
  <tr><th>[\#7109](https://gitlab.haskell.org//ghc/ghc/issues/7109)</th>
  <td>Inlining depends on datatype size, even with INLINE pragmas</td></tr>
  <tr><th>[\#7114](https://gitlab.haskell.org//ghc/ghc/issues/7114)</th>
  <td>Cannot recover (good) inlining behaviour from 7.0.2 in 7.4.1</td></tr>
  <tr><th>[\#7141](https://gitlab.haskell.org//ghc/ghc/issues/7141)</th>
  <td>Inlining the single method of a class can shadow rules</td></tr>
  <tr><th>[\#7161](https://gitlab.haskell.org//ghc/ghc/issues/7161)</th>
  <td>hSetNewlineMode and hSetEncoding can be performed on closed and semi-closed handles</td></tr>
  <tr><th>[\#7190](https://gitlab.haskell.org//ghc/ghc/issues/7190)</th>
  <td>GHC's -fprof-auto does not work with LINE pragmas</td></tr>
  <tr><th>[\#7206](https://gitlab.haskell.org//ghc/ghc/issues/7206)</th>
  <td>Implement cheap build</td></tr>
  <tr><th>[\#7240](https://gitlab.haskell.org//ghc/ghc/issues/7240)</th>
  <td>Stack trace truncated too much with indirect recursion</td></tr>
  <tr><th>[\#7245](https://gitlab.haskell.org//ghc/ghc/issues/7245)</th>
  <td>INLINEing top-level patterns causes ghc to emit 'arity missing' traces</td></tr>
  <tr><th>[\#7246](https://gitlab.haskell.org//ghc/ghc/issues/7246)</th>
  <td>Callstack depends on way (prof, profasm, profthreaded</td></tr>
  <tr><th>[\#7258](https://gitlab.haskell.org//ghc/ghc/issues/7258)</th>
  <td>Compiling DynFlags is jolly slow</td></tr>
  <tr><th>[\#7259](https://gitlab.haskell.org//ghc/ghc/issues/7259)</th>
  <td>Eta expansion of products in System FC</td></tr>
  <tr><th>[\#7273](https://gitlab.haskell.org//ghc/ghc/issues/7273)</th>
  <td>Binary size increase in nofib/grep between 7.6.1 and HEAD</td></tr>
  <tr><th>[\#7277](https://gitlab.haskell.org//ghc/ghc/issues/7277)</th>
  <td>Recompilation check fails for TH unless functions are inlined</td></tr>
  <tr><th>[\#7287](https://gitlab.haskell.org//ghc/ghc/issues/7287)</th>
  <td>Primops in RULES generate warnings</td></tr>
  <tr><th>[\#7296](https://gitlab.haskell.org//ghc/ghc/issues/7296)</th>
  <td>ghc-7 assumes incoherent instances without requiring language \`IncoherentInstances\`</td></tr>
  <tr><th>[\#7297](https://gitlab.haskell.org//ghc/ghc/issues/7297)</th>
  <td>LLVM incorrectly hoisting loads</td></tr>
  <tr><th>[\#7298](https://gitlab.haskell.org//ghc/ghc/issues/7298)</th>
  <td>GHCi is setting stdin/stdout to NoBuffering in runghc when DYNAMIC_GHC_PROGRAMS=YES</td></tr>
  <tr><th>[\#7307](https://gitlab.haskell.org//ghc/ghc/issues/7307)</th>
  <td>Share top-level code for strings</td></tr>
  <tr><th>[\#7309](https://gitlab.haskell.org//ghc/ghc/issues/7309)</th>
  <td>The Ix instance for (,) leaks space in range</td></tr>
  <tr><th>[\#7336](https://gitlab.haskell.org//ghc/ghc/issues/7336)</th>
  <td>Defined but not used is not detected for data types with instances</td></tr>
  <tr><th>[\#7353](https://gitlab.haskell.org//ghc/ghc/issues/7353)</th>
  <td>Make system IO interruptible on Windows</td></tr>
  <tr><th>[\#7367](https://gitlab.haskell.org//ghc/ghc/issues/7367)</th>
  <td>float-out causes extra allocation</td></tr>
  <tr><th>[\#7373](https://gitlab.haskell.org//ghc/ghc/issues/7373)</th>
  <td>When building GHC: Failed to load interface for \`GHC.Fingerprint'</td></tr>
  <tr><th>[\#7374](https://gitlab.haskell.org//ghc/ghc/issues/7374)</th>
  <td>rule not firing</td></tr>
  <tr><th>[\#7378](https://gitlab.haskell.org//ghc/ghc/issues/7378)</th>
  <td>Identical alts/bad divInt\# code</td></tr>
  <tr><th>[\#7380](https://gitlab.haskell.org//ghc/ghc/issues/7380)</th>
  <td>Panic: mkNoTick: Breakpoint loading modules with -O2 via API</td></tr>
  <tr><th>[\#7388](https://gitlab.haskell.org//ghc/ghc/issues/7388)</th>
  <td>CAPI doesn't work with ghci</td></tr>
  <tr><th>[\#7398](https://gitlab.haskell.org//ghc/ghc/issues/7398)</th>
  <td>RULES don't apply to a newtype constructor</td></tr>
  <tr><th>[\#7411](https://gitlab.haskell.org//ghc/ghc/issues/7411)</th>
  <td>Exceptions are optimized away in certain situations</td></tr>
  <tr><th>[\#7428](https://gitlab.haskell.org//ghc/ghc/issues/7428)</th>
  <td>GHC compile times are seriously non-linear in program size</td></tr>
  <tr><th>[\#7430](https://gitlab.haskell.org//ghc/ghc/issues/7430)</th>
  <td>GHC API reports CPP errors in confusing ways</td></tr>
  <tr><th>[\#7443](https://gitlab.haskell.org//ghc/ghc/issues/7443)</th>
  <td>Generated C code under -prof -fprof-auto -fprof-cafs very slow to compile</td></tr>
  <tr><th>[\#7450](https://gitlab.haskell.org//ghc/ghc/issues/7450)</th>
  <td>Regression in optimisation time of functions with many patterns (6.12 to 7.4)?</td></tr>
  <tr><th>[\#7503](https://gitlab.haskell.org//ghc/ghc/issues/7503)</th>
  <td>Bug with PolyKinds, type synonyms & GADTs</td></tr>
  <tr><th>[\#7511](https://gitlab.haskell.org//ghc/ghc/issues/7511)</th>
  <td>Room for GHC runtime improvement \>\~5%, inlining related</td></tr>
  <tr><th>[\#7535](https://gitlab.haskell.org//ghc/ghc/issues/7535)</th>
  <td>Using -with-rtsopts=-N should fail unless -threaded is also specified</td></tr>
  <tr><th>[\#7539](https://gitlab.haskell.org//ghc/ghc/issues/7539)</th>
  <td>Hard ghc api crash when calling runStmt on code which has not been compiled</td></tr>
  <tr><th>[\#7542](https://gitlab.haskell.org//ghc/ghc/issues/7542)</th>
  <td>GHC doesn't optimize (strict) composition with id</td></tr>
  <tr><th>[\#7593](https://gitlab.haskell.org//ghc/ghc/issues/7593)</th>
  <td>Unable to print exceptions of unicode identifiers</td></tr>
  <tr><th>[\#7596](https://gitlab.haskell.org//ghc/ghc/issues/7596)</th>
  <td>Opportunity to improve CSE</td></tr>
  <tr><th>[\#7602](https://gitlab.haskell.org//ghc/ghc/issues/7602)</th>
  <td>Threaded RTS performing badly on recent OS X (10.8?)</td></tr>
  <tr><th>[\#7610](https://gitlab.haskell.org//ghc/ghc/issues/7610)</th>
  <td>Cross compilation support for LLVM backend</td></tr>
  <tr><th>[\#7621](https://gitlab.haskell.org//ghc/ghc/issues/7621)</th>
  <td>Cross-build for QNX ARM smashes stack when using FunPtr wrappers</td></tr>
  <tr><th>[\#7624](https://gitlab.haskell.org//ghc/ghc/issues/7624)</th>
  <td>Handling ImplicitParams in Instance Declaration</td></tr>
  <tr><th>[\#7634](https://gitlab.haskell.org//ghc/ghc/issues/7634)</th>
  <td>MD5 collision could lead to SafeHaskell violation</td></tr>
  <tr><th>[\#7644](https://gitlab.haskell.org//ghc/ghc/issues/7644)</th>
  <td>Hackage docs for base library contain broken links</td></tr>
  <tr><th>[\#7665](https://gitlab.haskell.org//ghc/ghc/issues/7665)</th>
  <td>dynamicToo001 fails on Windows</td></tr>
  <tr><th>[\#7668](https://gitlab.haskell.org//ghc/ghc/issues/7668)</th>
  <td>Location in -fdefer-type-errors</td></tr>
  <tr><th>[\#7670](https://gitlab.haskell.org//ghc/ghc/issues/7670)</th>
  <td>StablePtrs should be organized by generation for efficient minor collections</td></tr>
  <tr><th>[\#7679](https://gitlab.haskell.org//ghc/ghc/issues/7679)</th>
  <td>Regression in -fregs-graph performance</td></tr>
  <tr><th>[\#7779](https://gitlab.haskell.org//ghc/ghc/issues/7779)</th>
  <td>building GHC overwrites the installed package database if GHC_PACKAGE_PATH is set</td></tr>
  <tr><th>[\#7789](https://gitlab.haskell.org//ghc/ghc/issues/7789)</th>
  <td>GHCI core dumps when used with VTY</td></tr>
  <tr><th>[\#7803](https://gitlab.haskell.org//ghc/ghc/issues/7803)</th>
  <td>Superclass methods are left unspecialized</td></tr>
  <tr><th>[\#7828](https://gitlab.haskell.org//ghc/ghc/issues/7828)</th>
  <td>RebindableSyntax and Arrow</td></tr>
  <tr><th>[\#7831](https://gitlab.haskell.org//ghc/ghc/issues/7831)</th>
  <td>Bad fragmentation when allocating many large objects</td></tr>
  <tr><th>[\#7842](https://gitlab.haskell.org//ghc/ghc/issues/7842)</th>
  <td>Incorrect checking of let-bindings in recursive do</td></tr>
  <tr><th>[\#7849](https://gitlab.haskell.org//ghc/ghc/issues/7849)</th>
  <td>Error on pattern matching of an existential whose context includes a type function</td></tr>
  <tr><th>[\#7897](https://gitlab.haskell.org//ghc/ghc/issues/7897)</th>
  <td>MakeTypeRep fingerprints be proper, robust fingerprints</td></tr>
  <tr><th>[\#7930](https://gitlab.haskell.org//ghc/ghc/issues/7930)</th>
  <td>Nested STM Invariants are lost</td></tr>
  <tr><th>[\#7966](https://gitlab.haskell.org//ghc/ghc/issues/7966)</th>
  <td>'make distclean' does not work in nofib</td></tr>
  <tr><th>[\#7983](https://gitlab.haskell.org//ghc/ghc/issues/7983)</th>
  <td>Bug in hsc2hs --cross-safe</td></tr>
  <tr><th>[\#7985](https://gitlab.haskell.org//ghc/ghc/issues/7985)</th>
  <td>Allow openFile on unknown file type</td></tr>
  <tr><th>[\#7988](https://gitlab.haskell.org//ghc/ghc/issues/7988)</th>
  <td>Big integers crashing integer-simple on qnxnto-arm with unregisterised build</td></tr>
  <tr><th>[\#7997](https://gitlab.haskell.org//ghc/ghc/issues/7997)</th>
  <td>waitForProcess  and getProcessExitCode are unsafe against asynchronous exceptions</td></tr>
  <tr><th>[\#8014](https://gitlab.haskell.org//ghc/ghc/issues/8014)</th>
  <td>Assertion failure when using multithreading in debug mode.</td></tr>
  <tr><th>[\#8023](https://gitlab.haskell.org//ghc/ghc/issues/8023)</th>
  <td>dph-examples binaries don't use all CPUs</td></tr>
  <tr><th>[\#8032](https://gitlab.haskell.org//ghc/ghc/issues/8032)</th>
  <td>Worker-wrapper transform and NOINLINE trigger bad reboxing behavior</td></tr>
  <tr><th>[\#8036](https://gitlab.haskell.org//ghc/ghc/issues/8036)</th>
  <td>Demand analyser is unpacking too deeply</td></tr>
  <tr><th>[\#8040](https://gitlab.haskell.org//ghc/ghc/issues/8040)</th>
  <td>installed include/HsVersions.h  wants to \#include "../includes/ghcautoconf.h"</td></tr>
  <tr><th>[\#8042](https://gitlab.haskell.org//ghc/ghc/issues/8042)</th>
  <td>\`:load \*\` and \`:add \*\` misbehave in presence of \`-fobject-code\`</td></tr>
  <tr><th>[\#8048](https://gitlab.haskell.org//ghc/ghc/issues/8048)</th>
  <td>Register spilling produces ineffecient/highly contending code</td></tr>
  <tr><th>[\#8095](https://gitlab.haskell.org//ghc/ghc/issues/8095)</th>
  <td>TypeFamilies painfully slow</td></tr>
  <tr><th>[\#8118](https://gitlab.haskell.org//ghc/ghc/issues/8118)</th>
  <td>\<stdout\>: commitAndReleaseBuffer: invalid argument (invalid character)</td></tr>
  <tr><th>[\#8123](https://gitlab.haskell.org//ghc/ghc/issues/8123)</th>
  <td>GHCi warns about -eventlog even though it's sometimes necessary</td></tr>
  <tr><th>[\#8127](https://gitlab.haskell.org//ghc/ghc/issues/8127)</th>
  <td>iOS patch no 19: Linking</td></tr>
  <tr><th>[\#8144](https://gitlab.haskell.org//ghc/ghc/issues/8144)</th>
  <td>Interface hashes include time stamp of dependent files (UsageFile mtime)</td></tr>
  <tr><th>[\#8147](https://gitlab.haskell.org//ghc/ghc/issues/8147)</th>
  <td>Exponential behavior in instance resolution on fixpoint-of-sum</td></tr>
  <tr><th>[\#8151](https://gitlab.haskell.org//ghc/ghc/issues/8151)</th>
  <td>ghc-7.4.2 on OpenIndiana (Solaris) createSubprocess fails</td></tr>
  <tr><th>[\#8159](https://gitlab.haskell.org//ghc/ghc/issues/8159)</th>
  <td>Uses of Binary decode should have a proper error message</td></tr>
  <tr><th>[\#8173](https://gitlab.haskell.org//ghc/ghc/issues/8173)</th>
  <td>GHC uses nub</td></tr>
  <tr><th>[\#8177](https://gitlab.haskell.org//ghc/ghc/issues/8177)</th>
  <td>Roles for type families</td></tr>
  <tr><th>[\#8195](https://gitlab.haskell.org//ghc/ghc/issues/8195)</th>
  <td>Different floating point results with -msse2 on 32bit Linux</td></tr>
  <tr><th>[\#8198](https://gitlab.haskell.org//ghc/ghc/issues/8198)</th>
  <td>One-shot mode is buggy w.r.t. hs-boot files</td></tr>
  <tr><th>[\#8211](https://gitlab.haskell.org//ghc/ghc/issues/8211)</th>
  <td>ghc -c recompiles TH every time while --make doesn't</td></tr>
  <tr><th>[\#8228](https://gitlab.haskell.org//ghc/ghc/issues/8228)</th>
  <td>GHC built under Windows does not generate dyn_hi files</td></tr>
  <tr><th>[\#8265](https://gitlab.haskell.org//ghc/ghc/issues/8265)</th>
  <td>getTokenStream fails for source using cpp</td></tr>
  <tr><th>[\#8279](https://gitlab.haskell.org//ghc/ghc/issues/8279)</th>
  <td>bad alignment in code gen  yields substantial perf issue</td></tr>
  <tr><th>[\#8281](https://gitlab.haskell.org//ghc/ghc/issues/8281)</th>
  <td>The impossible happened: primRepToFFIType</td></tr>
  <tr><th>[\#8285](https://gitlab.haskell.org//ghc/ghc/issues/8285)</th>
  <td>unexpected behavior with encodeFloat on large inputs</td></tr>
  <tr><th>[\#8293](https://gitlab.haskell.org//ghc/ghc/issues/8293)</th>
  <td>user001 spuriously fails if getGroupEntryForID correctly fails</td></tr>
  <tr><th>[\#8303](https://gitlab.haskell.org//ghc/ghc/issues/8303)</th>
  <td>defer StackOverflow exceptions (rather than dropping them) when exceptions are masked</td></tr>
  <tr><th>[\#8316](https://gitlab.haskell.org//ghc/ghc/issues/8316)</th>
  <td>GHCi debugger panics when trying force a certain variable</td></tr>
  <tr><th>[\#8318](https://gitlab.haskell.org//ghc/ghc/issues/8318)</th>
  <td>GHC does not infer type of \`tagToEnum\#\` expression</td></tr>
  <tr><th>[\#8319](https://gitlab.haskell.org//ghc/ghc/issues/8319)</th>
  <td>Simplifier ticks exhausted (need -fsimpl-tick-factor=955)</td></tr>
  <tr><th>[\#8327](https://gitlab.haskell.org//ghc/ghc/issues/8327)</th>
  <td>Cmm sinking does not eliminate dead code in loops</td></tr>
  <tr><th>[\#8336](https://gitlab.haskell.org//ghc/ghc/issues/8336)</th>
  <td>Sinking pass could optimize some assignments better</td></tr>
  <tr><th>[\#8338](https://gitlab.haskell.org//ghc/ghc/issues/8338)</th>
  <td>Incoherent instances without -XIncoherentInstances</td></tr>
  <tr><th>[\#8346](https://gitlab.haskell.org//ghc/ghc/issues/8346)</th>
  <td>Rank 1 type signature still requires RankNTypes</td></tr>
  <tr><th>[\#8362](https://gitlab.haskell.org//ghc/ghc/issues/8362)</th>
  <td>Filesystem related tests failed on solaris (SmartOS)</td></tr>
  <tr><th>[\#8363](https://gitlab.haskell.org//ghc/ghc/issues/8363)</th>
  <td>Order matters for unused import warnings when reexporting identifiers</td></tr>
  <tr><th>[\#8378](https://gitlab.haskell.org//ghc/ghc/issues/8378)</th>
  <td>Cross-compiling from x86_64-unknown-linux-gnu to x86_64-sun-solaris2 with mkGmpConstants workaround fails to build objects for integer-gmp</td></tr>
  <tr><th>[\#8388](https://gitlab.haskell.org//ghc/ghc/issues/8388)</th>
  <td>forall on non-\* types</td></tr>
  <tr><th>[\#8399](https://gitlab.haskell.org//ghc/ghc/issues/8399)</th>
  <td>Of Bird tacks and non-blank blank lines</td></tr>
  <tr><th>[\#8420](https://gitlab.haskell.org//ghc/ghc/issues/8420)</th>
  <td>Spurious dynamic library dependencies</td></tr>
  <tr><th>[\#8422](https://gitlab.haskell.org//ghc/ghc/issues/8422)</th>
  <td>type nats solver is too weak!</td></tr>
  <tr><th>[\#8426](https://gitlab.haskell.org//ghc/ghc/issues/8426)</th>
  <td>one-shot compilation + TH doesn't see instances that is seen in batch mode</td></tr>
  <tr><th>[\#8447](https://gitlab.haskell.org//ghc/ghc/issues/8447)</th>
  <td>A combination of type-level comparison and subtraction does not work for 0</td></tr>
  <tr><th>[\#8457](https://gitlab.haskell.org//ghc/ghc/issues/8457)</th>
  <td>-ffull-laziness does more harm than good</td></tr>
  <tr><th>[\#8484](https://gitlab.haskell.org//ghc/ghc/issues/8484)</th>
  <td>Compile-time panic</td></tr>
  <tr><th>[\#8487](https://gitlab.haskell.org//ghc/ghc/issues/8487)</th>
  <td>Debugger confuses variables</td></tr>
  <tr><th>[\#8510](https://gitlab.haskell.org//ghc/ghc/issues/8510)</th>
  <td>Clear up what extensions are needed at a Template Haskell splice site</td></tr>
  <tr><th>[\#8520](https://gitlab.haskell.org//ghc/ghc/issues/8520)</th>
  <td>ghc.exe: internal error: loadArchive: error whilst reading \`C'</td></tr>
  <tr><th>[\#8523](https://gitlab.haskell.org//ghc/ghc/issues/8523)</th>
  <td>blowup in space/time for type checking and object size for high arity tuples</td></tr>
  <tr><th>[\#8524](https://gitlab.haskell.org//ghc/ghc/issues/8524)</th>
  <td>GHC is inconsistent with the Haskell Report on which Unicode characters are allowed in string and character literals</td></tr>
  <tr><th>[\#8527](https://gitlab.haskell.org//ghc/ghc/issues/8527)</th>
  <td>The ordering of -I directives should be consistent with the ordering of -package directives</td></tr>
  <tr><th>[\#8556](https://gitlab.haskell.org//ghc/ghc/issues/8556)</th>
  <td>Invalid constructor names are accepted in data declarations</td></tr>
  <tr><th>[\#8572](https://gitlab.haskell.org//ghc/ghc/issues/8572)</th>
  <td>Building an empty module with profiling requires profiling libraries for integer-gmp</td></tr>
  <tr><th>[\#8573](https://gitlab.haskell.org//ghc/ghc/issues/8573)</th>
  <td>"evacuate: strange closure type 0" when creating large array</td></tr>
  <tr><th>[\#8589](https://gitlab.haskell.org//ghc/ghc/issues/8589)</th>
  <td>Bad choice of loop breaker with INLINABLE/INLINE</td></tr>
  <tr><th>[\#8591](https://gitlab.haskell.org//ghc/ghc/issues/8591)</th>
  <td>Concurrent executions of ghc-pkg can cause inconstant package.cache files</td></tr>
  <tr><th>[\#8604](https://gitlab.haskell.org//ghc/ghc/issues/8604)</th>
  <td>Some stack/vmem limits (ulimit) combinations causing GHC to fail</td></tr>
  <tr><th>[\#8611](https://gitlab.haskell.org//ghc/ghc/issues/8611)</th>
  <td>nofib’s cacheprof’s allocations nondeterminisitic</td></tr>
  <tr><th>[\#8623](https://gitlab.haskell.org//ghc/ghc/issues/8623)</th>
  <td>Strange slowness when using async library with FFI callbacks</td></tr>
  <tr><th>[\#8627](https://gitlab.haskell.org//ghc/ghc/issues/8627)</th>
  <td>mallocForeignPtrBytes documentation unobvious regarding memory alignment</td></tr>
  <tr><th>[\#8635](https://gitlab.haskell.org//ghc/ghc/issues/8635)</th>
  <td>GHC optimisation flag ignored when importing a local module with derived type classes</td></tr>
  <tr><th>[\#8648](https://gitlab.haskell.org//ghc/ghc/issues/8648)</th>
  <td>Initialization of C statics broken in threaded runtime</td></tr>
  <tr><th>[\#8657](https://gitlab.haskell.org//ghc/ghc/issues/8657)</th>
  <td>-fregs-graph still has a limit on spill slots</td></tr>
  <tr><th>[\#8662](https://gitlab.haskell.org//ghc/ghc/issues/8662)</th>
  <td>GHC does not inline cheap inner loop when used in two places</td></tr>
  <tr><th>[\#8666](https://gitlab.haskell.org//ghc/ghc/issues/8666)</th>
  <td>integer-gmp fails to compile on Debian Squeeze</td></tr>
  <tr><th>[\#8668](https://gitlab.haskell.org//ghc/ghc/issues/8668)</th>
  <td>SPECIALIZE silently fails to apply</td></tr>
  <tr><th>[\#8671](https://gitlab.haskell.org//ghc/ghc/issues/8671)</th>
  <td>Rebindable syntax creates bogus warning</td></tr>
  <tr><th>[\#8684](https://gitlab.haskell.org//ghc/ghc/issues/8684)</th>
  <td>hWaitForInput cannot be interrupted by async exceptions on unix</td></tr>
  <tr><th>[\#8694](https://gitlab.haskell.org//ghc/ghc/issues/8694)</th>
  <td>ghc -M doesn't handle addDependentFile or \#included files</td></tr>
  <tr><th>[\#8713](https://gitlab.haskell.org//ghc/ghc/issues/8713)</th>
  <td>Avoid libraries if unneeded (librt, libdl, libpthread)</td></tr>
  <tr><th>[\#8721](https://gitlab.haskell.org//ghc/ghc/issues/8721)</th>
  <td>Testsuite not reporting errors for DYN way on OS X</td></tr>
  <tr><th>[\#8730](https://gitlab.haskell.org//ghc/ghc/issues/8730)</th>
  <td>Invalid Unicode Codepoints in Char</td></tr>
  <tr><th>[\#8731](https://gitlab.haskell.org//ghc/ghc/issues/8731)</th>
  <td>long compilation time for module with large data type and partial record selectors</td></tr>
  <tr><th>[\#8732](https://gitlab.haskell.org//ghc/ghc/issues/8732)</th>
  <td>Global big object heap allocator lock causes contention</td></tr>
  <tr><th>[\#8733](https://gitlab.haskell.org//ghc/ghc/issues/8733)</th>
  <td>I/O manager causes unnecessary syscalls in send/recv loops</td></tr>
  <tr><th>[\#8774](https://gitlab.haskell.org//ghc/ghc/issues/8774)</th>
  <td>Transitivity of Auto-Specialization</td></tr>
  <tr><th>[\#8784](https://gitlab.haskell.org//ghc/ghc/issues/8784)</th>
  <td>Makefile missing a dependency</td></tr>
  <tr><th>[\#8808](https://gitlab.haskell.org//ghc/ghc/issues/8808)</th>
  <td>ImpredicativeTypes type checking fails depending on syntax of arguments</td></tr>
  <tr><th>[\#8814](https://gitlab.haskell.org//ghc/ghc/issues/8814)</th>
  <td>7.8 optimizes attoparsec improperly</td></tr>
  <tr><th>[\#8827](https://gitlab.haskell.org//ghc/ghc/issues/8827)</th>
  <td>Inferring Safe mode with GeneralizedNewtypeDeriving is wrong</td></tr>
  <tr><th>[\#8847](https://gitlab.haskell.org//ghc/ghc/issues/8847)</th>
  <td>Int64 \^ Int64 broken by optimization on SPARC</td></tr>
  <tr><th>[\#8862](https://gitlab.haskell.org//ghc/ghc/issues/8862)</th>
  <td>forkProcess does not play well with heap or time profiling</td></tr>
  <tr><th>[\#8871](https://gitlab.haskell.org//ghc/ghc/issues/8871)</th>
  <td>No-op assignment I64\[BaseReg + 784\] = I64\[BaseReg + 784\]; is generated into optimized Cmm</td></tr>
  <tr><th>[\#8872](https://gitlab.haskell.org//ghc/ghc/issues/8872)</th>
  <td>hsc cast size warnings on 32-bit Linux</td></tr>
  <tr><th>[\#8887](https://gitlab.haskell.org//ghc/ghc/issues/8887)</th>
  <td>Double double assignment in optimized Cmm on SPARC</td></tr>
  <tr><th>[\#8905](https://gitlab.haskell.org//ghc/ghc/issues/8905)</th>
  <td>Function arguments are always spilled/reloaded if scrutinee is already in WHNF</td></tr>
  <tr><th>[\#8922](https://gitlab.haskell.org//ghc/ghc/issues/8922)</th>
  <td>GHC unnecessarily sign/zero-extends C call arguments</td></tr>
  <tr><th>[\#8925](https://gitlab.haskell.org//ghc/ghc/issues/8925)</th>
  <td>:print and :sprint sometimes fully evaluates strings</td></tr>
  <tr><th>[\#8948](https://gitlab.haskell.org//ghc/ghc/issues/8948)</th>
  <td>Profiling report resolution too low</td></tr>
  <tr><th>[\#8949](https://gitlab.haskell.org//ghc/ghc/issues/8949)</th>
  <td>switch -msse2 to be on by default</td></tr>
  <tr><th>[\#8956](https://gitlab.haskell.org//ghc/ghc/issues/8956)</th>
  <td>Parser error shadowed by "module ‘main:Main’ is defined in multiple files" error</td></tr>
  <tr><th>[\#8971](https://gitlab.haskell.org//ghc/ghc/issues/8971)</th>
  <td>Native Code Generator for 8.0.1 is not as optimized as 7.6.3...</td></tr>
  <tr><th>[\#8981](https://gitlab.haskell.org//ghc/ghc/issues/8981)</th>
  <td>ghc-pkg complains about missing haddock interface files</td></tr>
  <tr><th>[\#8982](https://gitlab.haskell.org//ghc/ghc/issues/8982)</th>
  <td>Cost center heap profile restricted by biography of GHC segfaults</td></tr>
  <tr><th>[\#8995](https://gitlab.haskell.org//ghc/ghc/issues/8995)</th>
  <td>When generalising, use levels rather than global tyvars</td></tr>
  <tr><th>[\#9020](https://gitlab.haskell.org//ghc/ghc/issues/9020)</th>
  <td>Massive blowup of code size on trivial program</td></tr>
  <tr><th>[\#9041](https://gitlab.haskell.org//ghc/ghc/issues/9041)</th>
  <td>NCG generates slow loop code</td></tr>
  <tr><th>[\#9059](https://gitlab.haskell.org//ghc/ghc/issues/9059)</th>
  <td>Excessive space usage while generating code for fractional literals with big exponents</td></tr>
  <tr><th>[\#9074](https://gitlab.haskell.org//ghc/ghc/issues/9074)</th>
  <td>GHC 7.8.2's ghci does not track missing symbols when loading non-Haskell object files</td></tr>
  <tr><th>[\#9076](https://gitlab.haskell.org//ghc/ghc/issues/9076)</th>
  <td>GHC.Exts docs don't contain all primops</td></tr>
  <tr><th>[\#9079](https://gitlab.haskell.org//ghc/ghc/issues/9079)</th>
  <td>Foreign.C.Types in haskell2010</td></tr>
  <tr><th>[\#9088](https://gitlab.haskell.org//ghc/ghc/issues/9088)</th>
  <td>Per-thread Haskell thread list/numbering (remove global lock from thread allocation)</td></tr>
  <tr><th>[\#9121](https://gitlab.haskell.org//ghc/ghc/issues/9121)</th>
  <td>Presence of dyn_o files not checked upon recompilation</td></tr>
  <tr><th>[\#9135](https://gitlab.haskell.org//ghc/ghc/issues/9135)</th>
  <td>readProcessWithExitCode leaks when the program does not exist</td></tr>
  <tr><th>[\#9173](https://gitlab.haskell.org//ghc/ghc/issues/9173)</th>
  <td>Improve type mismatch error messages</td></tr>
  <tr><th>[\#9176](https://gitlab.haskell.org//ghc/ghc/issues/9176)</th>
  <td>GHC not generating dyn_hi files</td></tr>
  <tr><th>[\#9198](https://gitlab.haskell.org//ghc/ghc/issues/9198)</th>
  <td>large performance regression in type checker speed in 7.8</td></tr>
  <tr><th>[\#9210](https://gitlab.haskell.org//ghc/ghc/issues/9210)</th>
  <td>"overlapping instances" through FunctionalDependencies</td></tr>
  <tr><th>[\#9219](https://gitlab.haskell.org//ghc/ghc/issues/9219)</th>
  <td>Parallel build proceeds despite errors</td></tr>
  <tr><th>[\#9221](https://gitlab.haskell.org//ghc/ghc/issues/9221)</th>
  <td>(super!) linear slowdown of parallel builds on 40 core machine</td></tr>
  <tr><th>[\#9223](https://gitlab.haskell.org//ghc/ghc/issues/9223)</th>
  <td>Type equality makes type variable untouchable</td></tr>
  <tr><th>[\#9232](https://gitlab.haskell.org//ghc/ghc/issues/9232)</th>
  <td>Stats file has wrong numbers</td></tr>
  <tr><th>[\#9235](https://gitlab.haskell.org//ghc/ghc/issues/9235)</th>
  <td>Simplifier ticks exhausted on recursive class method</td></tr>
  <tr><th>[\#9237](https://gitlab.haskell.org//ghc/ghc/issues/9237)</th>
  <td>GHC not recognizing INPUT(-llibrary) in linker scripts</td></tr>
  <tr><th>[\#9246](https://gitlab.haskell.org//ghc/ghc/issues/9246)</th>
  <td>GHC generates poor code for repeated uses of min/max</td></tr>
  <tr><th>[\#9248](https://gitlab.haskell.org//ghc/ghc/issues/9248)</th>
  <td>Document -XHaskell98 and -XHaskell2010 in flag reference</td></tr>
  <tr><th>[\#9249](https://gitlab.haskell.org//ghc/ghc/issues/9249)</th>
  <td>Link to "latest" user's guide</td></tr>
  <tr><th>[\#9267](https://gitlab.haskell.org//ghc/ghc/issues/9267)</th>
  <td>Lack of type information in GHC error messages when the liberage coverage condition is unsatisfied</td></tr>
  <tr><th>[\#9277](https://gitlab.haskell.org//ghc/ghc/issues/9277)</th>
  <td>GHCi panic: Loading temp shared object failed: Symbol not found</td></tr>
  <tr><th>[\#9278](https://gitlab.haskell.org//ghc/ghc/issues/9278)</th>
  <td>GHCi crash: selector _ for message _ does not match selector known to Objective C runtime</td></tr>
  <tr><th>[\#9279](https://gitlab.haskell.org//ghc/ghc/issues/9279)</th>
  <td>Local wrapper function remains in final program; result = extra closure allocation</td></tr>
  <tr><th>[\#9280](https://gitlab.haskell.org//ghc/ghc/issues/9280)</th>
  <td>GHCi crash: illegal text-relocation to _ in _ from _ in _ for architecture x86_64; relocation R_X86_64_PC32 against undefined symbol _ can not be used when making a shared object</td></tr>
  <tr><th>[\#9283](https://gitlab.haskell.org//ghc/ghc/issues/9283)</th>
  <td>bad autoconf variable names</td></tr>
  <tr><th>[\#9292](https://gitlab.haskell.org//ghc/ghc/issues/9292)</th>
  <td>Race condition when multiple threads wait for a process</td></tr>
  <tr><th>[\#9307](https://gitlab.haskell.org//ghc/ghc/issues/9307)</th>
  <td>LLVM vs NCG: floating point numbers close to zero have different sign</td></tr>
  <tr><th>[\#9315](https://gitlab.haskell.org//ghc/ghc/issues/9315)</th>
  <td>Weird change in allocation numbers of T9203</td></tr>
  <tr><th>[\#9320](https://gitlab.haskell.org//ghc/ghc/issues/9320)</th>
  <td>Inlining regression/strangeness in 7.8</td></tr>
  <tr><th>[\#9347](https://gitlab.haskell.org//ghc/ghc/issues/9347)</th>
  <td>forkProcess does not acquire global handle locks</td></tr>
  <tr><th>[\#9349](https://gitlab.haskell.org//ghc/ghc/issues/9349)</th>
  <td>excessive inlining due to state hack</td></tr>
  <tr><th>[\#9353](https://gitlab.haskell.org//ghc/ghc/issues/9353)</th>
  <td>prefetch primops are not currently useful</td></tr>
  <tr><th>[\#9358](https://gitlab.haskell.org//ghc/ghc/issues/9358)</th>
  <td>Improve flag description in the user guide</td></tr>
  <tr><th>[\#9364](https://gitlab.haskell.org//ghc/ghc/issues/9364)</th>
  <td>GHCi (or haskeline?) confused by non-single-width characters</td></tr>
  <tr><th>[\#9370](https://gitlab.haskell.org//ghc/ghc/issues/9370)</th>
  <td>unfolding info as seen when building a module depends on flags in a previously-compiled module</td></tr>
  <tr><th>[\#9386](https://gitlab.haskell.org//ghc/ghc/issues/9386)</th>
  <td>GHCi cannot load .so in ./</td></tr>
  <tr><th>[\#9388](https://gitlab.haskell.org//ghc/ghc/issues/9388)</th>
  <td>Narrow the scope of the notorious "state hack"</td></tr>
  <tr><th>[\#9406](https://gitlab.haskell.org//ghc/ghc/issues/9406)</th>
  <td>unexpected failure for T7837(profasm)</td></tr>
  <tr><th>[\#9418](https://gitlab.haskell.org//ghc/ghc/issues/9418)</th>
  <td>Warnings about "INLINE binder is (non-rule) loop breaker"</td></tr>
  <tr><th>[\#9421](https://gitlab.haskell.org//ghc/ghc/issues/9421)</th>
  <td>Problems and workarounds when installing and using a 32bit GHC on 64bit Linux machine</td></tr>
  <tr><th>[\#9432](https://gitlab.haskell.org//ghc/ghc/issues/9432)</th>
  <td>IncoherentInstances are too restricted</td></tr>
  <tr><th>[\#9434](https://gitlab.haskell.org//ghc/ghc/issues/9434)</th>
  <td>GHC.List.reverse does not fuse</td></tr>
  <tr><th>[\#9445](https://gitlab.haskell.org//ghc/ghc/issues/9445)</th>
  <td>GHC Panic: Tick Exhausted with high factor</td></tr>
  <tr><th>[\#9450](https://gitlab.haskell.org//ghc/ghc/issues/9450)</th>
  <td>GHC instantiates Data instances before checking hs-boot files</td></tr>
  <tr><th>[\#9456](https://gitlab.haskell.org//ghc/ghc/issues/9456)</th>
  <td>Weird behavior with polymorphic function involving existential quantification and GADTs</td></tr>
  <tr><th>[\#9457](https://gitlab.haskell.org//ghc/ghc/issues/9457)</th>
  <td>hsc2hs breaks with \`--cflag=-Werror\` in cross-compilation mode</td></tr>
  <tr><th>[\#9468](https://gitlab.haskell.org//ghc/ghc/issues/9468)</th>
  <td>Internal error: resurrectThreads: thread blocked in a strange way: 10</td></tr>
  <tr><th>[\#9470](https://gitlab.haskell.org//ghc/ghc/issues/9470)</th>
  <td>forkProcess breaks in multiple ways with GHC 7.6</td></tr>
  <tr><th>[\#9481](https://gitlab.haskell.org//ghc/ghc/issues/9481)</th>
  <td>Linker does not correctly resolve symbols in previously loaded objects</td></tr>
  <tr><th>[\#9503](https://gitlab.haskell.org//ghc/ghc/issues/9503)</th>
  <td>Cross compiling with mingw uses wrong gcc</td></tr>
  <tr><th>[\#9512](https://gitlab.haskell.org//ghc/ghc/issues/9512)</th>
  <td>T9329 fails test on unregisterised i386, amd64</td></tr>
  <tr><th>[\#9539](https://gitlab.haskell.org//ghc/ghc/issues/9539)</th>
  <td>TQueue can lead to thread starvation</td></tr>
  <tr><th>[\#9547](https://gitlab.haskell.org//ghc/ghc/issues/9547)</th>
  <td>Empty constraint tuples are mis-kinded</td></tr>
  <tr><th>[\#9557](https://gitlab.haskell.org//ghc/ghc/issues/9557)</th>
  <td>Deriving instances is slow</td></tr>
  <tr><th>[\#9562](https://gitlab.haskell.org//ghc/ghc/issues/9562)</th>
  <td>Type families + hs-boot files = unsafeCoerce</td></tr>
  <tr><th>[\#9570](https://gitlab.haskell.org//ghc/ghc/issues/9570)</th>
  <td>cryptarithm1 (normal) has bimodal runtime</td></tr>
  <tr><th>[\#9573](https://gitlab.haskell.org//ghc/ghc/issues/9573)</th>
  <td>Add warning for invalid digits in integer literals</td></tr>
  <tr><th>[\#9587](https://gitlab.haskell.org//ghc/ghc/issues/9587)</th>
  <td>Type checking with type functions introduces many type variables, which remain ambiguous. The code no longer type checks.</td></tr>
  <tr><th>[\#9599](https://gitlab.haskell.org//ghc/ghc/issues/9599)</th>
  <td>app runs 10 times faster when compiled with profilling information than without it</td></tr>
  <tr><th>[\#9607](https://gitlab.haskell.org//ghc/ghc/issues/9607)</th>
  <td>Programs that require AllowAmbiguousTypes in 7.8</td></tr>
  <tr><th>[\#9614](https://gitlab.haskell.org//ghc/ghc/issues/9614)</th>
  <td>ghc --print-(gcc\|ld)-linker-flags broken</td></tr>
  <tr><th>[\#9627](https://gitlab.haskell.org//ghc/ghc/issues/9627)</th>
  <td>Type error with functional dependencies</td></tr>
  <tr><th>[\#9631](https://gitlab.haskell.org//ghc/ghc/issues/9631)</th>
  <td>Comment in GHC.Base about GHC.Prim does not appear to be correct</td></tr>
  <tr><th>[\#9636](https://gitlab.haskell.org//ghc/ghc/issues/9636)</th>
  <td>Function with type error accepted</td></tr>
  <tr><th>[\#9643](https://gitlab.haskell.org//ghc/ghc/issues/9643)</th>
  <td>ghci must be restarted to use break point more than once?</td></tr>
  <tr><th>[\#9646](https://gitlab.haskell.org//ghc/ghc/issues/9646)</th>
  <td>Simplifer non-determinism leading to 8 fold difference in run time performance</td></tr>
  <tr><th>[\#9655](https://gitlab.haskell.org//ghc/ghc/issues/9655)</th>
  <td>Do not UNPACK strict fields that are very wide</td></tr>
  <tr><th>[\#9660](https://gitlab.haskell.org//ghc/ghc/issues/9660)</th>
  <td>unnecessary indirect jump when returning a case scrutinee</td></tr>
  <tr><th>[\#9666](https://gitlab.haskell.org//ghc/ghc/issues/9666)</th>
  <td>runtime crashing with +RTS -w -h</td></tr>
  <tr><th>[\#9669](https://gitlab.haskell.org//ghc/ghc/issues/9669)</th>
  <td>Long compile time/high memory usage for modules with many deriving clauses</td></tr>
  <tr><th>[\#9672](https://gitlab.haskell.org//ghc/ghc/issues/9672)</th>
  <td>Error message too long (full case statement printed)</td></tr>
  <tr><th>[\#9675](https://gitlab.haskell.org//ghc/ghc/issues/9675)</th>
  <td>Unreasonable memory usage on large data structures</td></tr>
  <tr><th>[\#9686](https://gitlab.haskell.org//ghc/ghc/issues/9686)</th>
  <td>Link option detection does not work for incremental builds on Windows</td></tr>
  <tr><th>[\#9693](https://gitlab.haskell.org//ghc/ghc/issues/9693)</th>
  <td>Reloading GHCi with Template Haskell names can panic GHC</td></tr>
  <tr><th>[\#9701](https://gitlab.haskell.org//ghc/ghc/issues/9701)</th>
  <td>GADTs not specialized properly</td></tr>
  <tr><th>[\#9704](https://gitlab.haskell.org//ghc/ghc/issues/9704)</th>
  <td>GHC fails with "Loading temp shared object failed"</td></tr>
  <tr><th>[\#9708](https://gitlab.haskell.org//ghc/ghc/issues/9708)</th>
  <td>Type inference non-determinism due to improvement</td></tr>
  <tr><th>[\#9709](https://gitlab.haskell.org//ghc/ghc/issues/9709)</th>
  <td>Make restarts itself sometimes, and that's OK</td></tr>
  <tr><th>[\#9717](https://gitlab.haskell.org//ghc/ghc/issues/9717)</th>
  <td>More lazy orphan module loading</td></tr>
  <tr><th>[\#9729](https://gitlab.haskell.org//ghc/ghc/issues/9729)</th>
  <td>GHCi accepts invalid programs when recompiling</td></tr>
  <tr><th>[\#9730](https://gitlab.haskell.org//ghc/ghc/issues/9730)</th>
  <td>Polymorphism and type classes</td></tr>
  <tr><th>[\#9737](https://gitlab.haskell.org//ghc/ghc/issues/9737)</th>
  <td>List ANN in pragmas chapter of user manual</td></tr>
  <tr><th>[\#9755](https://gitlab.haskell.org//ghc/ghc/issues/9755)</th>
  <td>Unhelpful error message when -XScopedTypeVariables is omitted</td></tr>
  <tr><th>[\#9765](https://gitlab.haskell.org//ghc/ghc/issues/9765)</th>
  <td>Strange behavior of GC under ghci</td></tr>
  <tr><th>[\#9775](https://gitlab.haskell.org//ghc/ghc/issues/9775)</th>
  <td>"Failed to remove" errors during Windows build from hsc2hs</td></tr>
  <tr><th>[\#9780](https://gitlab.haskell.org//ghc/ghc/issues/9780)</th>
  <td>dep_orphs in Dependencies redundantly records type family orphans</td></tr>
  <tr><th>[\#9792](https://gitlab.haskell.org//ghc/ghc/issues/9792)</th>
  <td>map/coerce rule does not fire until the coercion is known</td></tr>
  <tr><th>[\#9798](https://gitlab.haskell.org//ghc/ghc/issues/9798)</th>
  <td>Frustrating behaviour of the INLINE pragma</td></tr>
  <tr><th>[\#9806](https://gitlab.haskell.org//ghc/ghc/issues/9806)</th>
  <td>malloc and mallocArray ignore Storable alignment requirements</td></tr>
  <tr><th>[\#9809](https://gitlab.haskell.org//ghc/ghc/issues/9809)</th>
  <td>Overwhelming the TimerManager</td></tr>
  <tr><th>[\#9811](https://gitlab.haskell.org//ghc/ghc/issues/9811)</th>
  <td>constant folding with infinities is wrong</td></tr>
  <tr><th>[\#9825](https://gitlab.haskell.org//ghc/ghc/issues/9825)</th>
  <td>ghc "panic! (the 'impossible' happened)" building vimus on NixOS</td></tr>
  <tr><th>[\#9841](https://gitlab.haskell.org//ghc/ghc/issues/9841)</th>
  <td>Touching a file that uses TH triggers TH recompilation flood</td></tr>
  <tr><th>[\#9854](https://gitlab.haskell.org//ghc/ghc/issues/9854)</th>
  <td>Literal overflow check is too aggressive</td></tr>
  <tr><th>[\#9916](https://gitlab.haskell.org//ghc/ghc/issues/9916)</th>
  <td>ghc -e ":foo bar" exit status is inconsistent</td></tr>
  <tr><th>[\#9918](https://gitlab.haskell.org//ghc/ghc/issues/9918)</th>
  <td>GHC chooses an instance between two overlapping, but cannot resolve a clause within the similar closed type family</td></tr>
  <tr><th>[\#9932](https://gitlab.haskell.org//ghc/ghc/issues/9932)</th>
  <td>GHC fails to build when cross compiling for mingw with the message "Threads not supported"</td></tr>
  <tr><th>[\#9936](https://gitlab.haskell.org//ghc/ghc/issues/9936)</th>
  <td>Data.Fixed truncates 5.17 to 5.16</td></tr>
  <tr><th>[\#9940](https://gitlab.haskell.org//ghc/ghc/issues/9940)</th>
  <td>GHCi crashes when I hold down control-c for a short time, and then hold down any key that would normally result in typing.</td></tr>
  <tr><th>[\#9944](https://gitlab.haskell.org//ghc/ghc/issues/9944)</th>
  <td>Performance issue re: simple loop</td></tr>
  <tr><th>[\#9979](https://gitlab.haskell.org//ghc/ghc/issues/9979)</th>
  <td>Performance regression GHC 7.8.4 to GHC HEAD</td></tr>
  <tr><th>[\#9980](https://gitlab.haskell.org//ghc/ghc/issues/9980)</th>
  <td>TcS monad is too heavy</td></tr>
  <tr><th>[\#9981](https://gitlab.haskell.org//ghc/ghc/issues/9981)</th>
  <td>Potential typechecker regression in GHC 7.10.1RC</td></tr>
  <tr><th>[\#9982](https://gitlab.haskell.org//ghc/ghc/issues/9982)</th>
  <td>cross building integer-gmp is running target program on build host</td></tr>
  <tr><th>[\#9983](https://gitlab.haskell.org//ghc/ghc/issues/9983)</th>
  <td>configure script invokes ghc with LDFLAGS during cross-builds</td></tr>
  <tr><th>[\#9985](https://gitlab.haskell.org//ghc/ghc/issues/9985)</th>
  <td>GHC panic with ViewPatterns and GADTs in a proc pattern</td></tr>
  <tr><th>[\#9989](https://gitlab.haskell.org//ghc/ghc/issues/9989)</th>
  <td>GHCI is slow for precompiled code</td></tr>
  <tr><th>[\#9992](https://gitlab.haskell.org//ghc/ghc/issues/9992)</th>
  <td>Constructor specialization requires eta expansion</td></tr>
  <tr><th>[\#10005](https://gitlab.haskell.org//ghc/ghc/issues/10005)</th>
  <td>Operations on string literals won't be inlined</td></tr>
  <tr><th>[\#10010](https://gitlab.haskell.org//ghc/ghc/issues/10010)</th>
  <td>LLVM/optimized code for sqrt incorrect for negative values</td></tr>
  <tr><th>[\#10012](https://gitlab.haskell.org//ghc/ghc/issues/10012)</th>
  <td>Cheap-to-compute values aren't pushed into case branches inducing unnecessary register pressure</td></tr>
  <tr><th>[\#10022](https://gitlab.haskell.org//ghc/ghc/issues/10022)</th>
  <td>Clean up GHC.RTS.Flags</td></tr>
  <tr><th>[\#10027](https://gitlab.haskell.org//ghc/ghc/issues/10027)</th>
  <td>Importing constructor of associated data type fails</td></tr>
  <tr><th>[\#10032](https://gitlab.haskell.org//ghc/ghc/issues/10032)</th>
  <td>binary distributions not working --with-system-libffi</td></tr>
  <tr><th>[\#10037](https://gitlab.haskell.org//ghc/ghc/issues/10037)</th>
  <td>Several profiling tests give different results optimised vs. unoptimised</td></tr>
  <tr><th>[\#10044](https://gitlab.haskell.org//ghc/ghc/issues/10044)</th>
  <td>Wrong line number reported with CPP and line beginning with \#</td></tr>
  <tr><th>[\#10046](https://gitlab.haskell.org//ghc/ghc/issues/10046)</th>
  <td>Linker script patch in rts/Linker.c doesn't work for (non-C or non-en..) locales</td></tr>
  <tr><th>[\#10056](https://gitlab.haskell.org//ghc/ghc/issues/10056)</th>
  <td>Inconsistent precedence of \~</td></tr>
  <tr><th>[\#10062](https://gitlab.haskell.org//ghc/ghc/issues/10062)</th>
  <td>Codegen on sequential FFI calls is not very good</td></tr>
  <tr><th>[\#10065](https://gitlab.haskell.org//ghc/ghc/issues/10065)</th>
  <td>Definition of fix lacks commentary</td></tr>
  <tr><th>[\#10069](https://gitlab.haskell.org//ghc/ghc/issues/10069)</th>
  <td>CPR related performance issue</td></tr>
  <tr><th>[\#10077](https://gitlab.haskell.org//ghc/ghc/issues/10077)</th>
  <td>Providing type checker plugin on command line results in false cyclic import error</td></tr>
  <tr><th>[\#10097](https://gitlab.haskell.org//ghc/ghc/issues/10097)</th>
  <td>GHC 7.11 errors on dictionary casting tricks</td></tr>
  <tr><th>[\#10106](https://gitlab.haskell.org//ghc/ghc/issues/10106)</th>
  <td>GHC doesn't warn on typos in language pragmas</td></tr>
  <tr><th>[\#10111](https://gitlab.haskell.org//ghc/ghc/issues/10111)</th>
  <td>hp2ps silently discards samples</td></tr>
  <tr><th>[\#10114](https://gitlab.haskell.org//ghc/ghc/issues/10114)</th>
  <td>Kind mismatches with AnyK in rank-2 types</td></tr>
  <tr><th>[\#10117](https://gitlab.haskell.org//ghc/ghc/issues/10117)</th>
  <td>Change the scheme for reporting redundant imports</td></tr>
  <tr><th>[\#10120](https://gitlab.haskell.org//ghc/ghc/issues/10120)</th>
  <td>Unnecessary code duplication from case analysis</td></tr>
  <tr><th>[\#10124](https://gitlab.haskell.org//ghc/ghc/issues/10124)</th>
  <td>Simple case analyses generate too many branches</td></tr>
  <tr><th>[\#10141](https://gitlab.haskell.org//ghc/ghc/issues/10141)</th>
  <td>CUSK mysteries</td></tr>
  <tr><th>[\#10144](https://gitlab.haskell.org//ghc/ghc/issues/10144)</th>
  <td>Variant of runGhc which accepts prelude for setting up DynFlags</td></tr>
  <tr><th>[\#10160](https://gitlab.haskell.org//ghc/ghc/issues/10160)</th>
  <td>GHCi :sprint has odd/unhelpful behavior for values defined within the REPL</td></tr>
  <tr><th>[\#10161](https://gitlab.haskell.org//ghc/ghc/issues/10161)</th>
  <td>GHC does not relink if we link against a new library with old timestamp</td></tr>
  <tr><th>[\#10169](https://gitlab.haskell.org//ghc/ghc/issues/10169)</th>
  <td>bracket not running the final action on termination through SIGTERM</td></tr>
  <tr><th>[\#10171](https://gitlab.haskell.org//ghc/ghc/issues/10171)</th>
  <td>runghc has problem when the argument list is too big</td></tr>
  <tr><th>[\#10173](https://gitlab.haskell.org//ghc/ghc/issues/10173)</th>
  <td>Panic: Irrefutable pattern failed for pattern Data.Maybe.Just</td></tr>
  <tr><th>[\#10174](https://gitlab.haskell.org//ghc/ghc/issues/10174)</th>
  <td>AArch64 : ghc-stage2 segfaults compiling libraries/parallel</td></tr>
  <tr><th>[\#10178](https://gitlab.haskell.org//ghc/ghc/issues/10178)</th>
  <td>hs-boot/hsig ambiguity empty data declaration or type with no constructors</td></tr>
  <tr><th>[\#10179](https://gitlab.haskell.org//ghc/ghc/issues/10179)</th>
  <td>Kinds missing from types in ghci</td></tr>
  <tr><th>[\#10183](https://gitlab.haskell.org//ghc/ghc/issues/10183)</th>
  <td>Warning for redundant constraints: interaction with pattern matching</td></tr>
  <tr><th>[\#10184](https://gitlab.haskell.org//ghc/ghc/issues/10184)</th>
  <td>Coercible solver incomplete with recursive newtypes</td></tr>
  <tr><th>[\#10185](https://gitlab.haskell.org//ghc/ghc/issues/10185)</th>
  <td>Coercible solver incomplete with non-variable transitivity</td></tr>
  <tr><th>[\#10187](https://gitlab.haskell.org//ghc/ghc/issues/10187)</th>
  <td>Panic: RegAlloc.Liveness.computeLivenss; SCCs aren't in reverse dependent order; bad blockId c8xHd</td></tr>
  <tr><th>[\#10189](https://gitlab.haskell.org//ghc/ghc/issues/10189)</th>
  <td>explicit promotions of prefix data constructors can't be parsed naturally</td></tr>
  <tr><th>[\#10199](https://gitlab.haskell.org//ghc/ghc/issues/10199)</th>
  <td>Sending SIGINT to a program that uses forkOS may crash with various errors</td></tr>
  <tr><th>[\#10227](https://gitlab.haskell.org//ghc/ghc/issues/10227)</th>
  <td>Type checker cannot deduce type</td></tr>
  <tr><th>[\#10228](https://gitlab.haskell.org//ghc/ghc/issues/10228)</th>
  <td>Increased memory usage with GHC 7.10.1</td></tr>
  <tr><th>[\#10229](https://gitlab.haskell.org//ghc/ghc/issues/10229)</th>
  <td>setThreadAffinity assumes a certain CPU virtual core layout</td></tr>
  <tr><th>[\#10249](https://gitlab.haskell.org//ghc/ghc/issues/10249)</th>
  <td>GHCi leaky abstraction: error message mentions \`ghciStepIO\`</td></tr>
  <tr><th>[\#10270](https://gitlab.haskell.org//ghc/ghc/issues/10270)</th>
  <td>inconsistent semantics of type class instance visibility outside recursive modules</td></tr>
  <tr><th>[\#10271](https://gitlab.haskell.org//ghc/ghc/issues/10271)</th>
  <td>Typed Template Haskell splice difficulty when resolving overloading</td></tr>
  <tr><th>[\#10295](https://gitlab.haskell.org//ghc/ghc/issues/10295)</th>
  <td>Putting SCC in heavily inlined code results in "error: redefinition of global"</td></tr>
  <tr><th>[\#10311](https://gitlab.haskell.org//ghc/ghc/issues/10311)</th>
  <td>package name returned from tyConPackage is garbled</td></tr>
  <tr><th>[\#10328](https://gitlab.haskell.org//ghc/ghc/issues/10328)</th>
  <td>Control.Monad exports lead to weird Haddocks</td></tr>
  <tr><th>[\#10330](https://gitlab.haskell.org//ghc/ghc/issues/10330)</th>
  <td>Better Template Haskell error message locations</td></tr>
  <tr><th>[\#10332](https://gitlab.haskell.org//ghc/ghc/issues/10332)</th>
  <td>AArch64 : divbyzero test fails</td></tr>
  <tr><th>[\#10333](https://gitlab.haskell.org//ghc/ghc/issues/10333)</th>
  <td>hs-boot modification doesn't induce recompilation</td></tr>
  <tr><th>[\#10334](https://gitlab.haskell.org//ghc/ghc/issues/10334)</th>
  <td>__ctzdi2/si2/__clzdi2/si2 unknown symbols in ghc-prim on non-shared libs platform</td></tr>
  <tr><th>[\#10337](https://gitlab.haskell.org//ghc/ghc/issues/10337)</th>
  <td>One-shot module loops have hard to understand messages</td></tr>
  <tr><th>[\#10338](https://gitlab.haskell.org//ghc/ghc/issues/10338)</th>
  <td>GHC Forgets Constraints</td></tr>
  <tr><th>[\#10341](https://gitlab.haskell.org//ghc/ghc/issues/10341)</th>
  <td>hs-boot files can have bogus declarations if they're not exported</td></tr>
  <tr><th>[\#10346](https://gitlab.haskell.org//ghc/ghc/issues/10346)</th>
  <td>Cross-module SpecConstr</td></tr>
  <tr><th>[\#10347](https://gitlab.haskell.org//ghc/ghc/issues/10347)</th>
  <td>Spurious "unused constructor" warning with Coercible</td></tr>
  <tr><th>[\#10353](https://gitlab.haskell.org//ghc/ghc/issues/10353)</th>
  <td>Haddock for Data.List should list instances</td></tr>
  <tr><th>[\#10367](https://gitlab.haskell.org//ghc/ghc/issues/10367)</th>
  <td>"ghc: panic! (the 'impossible' happened)"</td></tr>
  <tr><th>[\#10371](https://gitlab.haskell.org//ghc/ghc/issues/10371)</th>
  <td>GHC fails to inline and specialize a function</td></tr>
  <tr><th>[\#10373](https://gitlab.haskell.org//ghc/ghc/issues/10373)</th>
  <td>Haiku: ghc-stage1 compiler crashes at exit</td></tr>
  <tr><th>[\#10374](https://gitlab.haskell.org//ghc/ghc/issues/10374)</th>
  <td>Can't build GHC with a dynamic only GHC installation</td></tr>
  <tr><th>[\#10378](https://gitlab.haskell.org//ghc/ghc/issues/10378)</th>
  <td>min/max for Double/Float instances are incorrect</td></tr>
  <tr><th>[\#10381](https://gitlab.haskell.org//ghc/ghc/issues/10381)</th>
  <td>Type-checking failure with RankNTypes and RebindableSyntax</td></tr>
  <tr><th>[\#10385](https://gitlab.haskell.org//ghc/ghc/issues/10385)</th>
  <td>Annotation restriction is not respected while generating Annotation via TH</td></tr>
  <tr><th>[\#10387](https://gitlab.haskell.org//ghc/ghc/issues/10387)</th>
  <td>toRational should error out on NaN and Infinity values for Double/Floats</td></tr>
  <tr><th>[\#10401](https://gitlab.haskell.org//ghc/ghc/issues/10401)</th>
  <td>state hack-related regression</td></tr>
  <tr><th>[\#10411](https://gitlab.haskell.org//ghc/ghc/issues/10411)</th>
  <td>Neighbour let-bindings are not reported as relevant</td></tr>
  <tr><th>[\#10417](https://gitlab.haskell.org//ghc/ghc/issues/10417)</th>
  <td>Rule matching not "seeing through" floating and type lambda (and maybe cast)</td></tr>
  <tr><th>[\#10418](https://gitlab.haskell.org//ghc/ghc/issues/10418)</th>
  <td>Incorrect RULE warning on constructor, and inablilty to {-\# INLINE \[0\] \#-} constrcutor</td></tr>
  <tr><th>[\#10421](https://gitlab.haskell.org//ghc/ghc/issues/10421)</th>
  <td>exponential blowup in inlining (without INLINE pragmas)</td></tr>
  <tr><th>[\#10424](https://gitlab.haskell.org//ghc/ghc/issues/10424)</th>
  <td>Build path leaks into ABI hashes</td></tr>
  <tr><th>[\#10434](https://gitlab.haskell.org//ghc/ghc/issues/10434)</th>
  <td>SPECIALISE instance does not specialize as far as SPECIALISE for type signatures</td></tr>
  <tr><th>[\#10436](https://gitlab.haskell.org//ghc/ghc/issues/10436)</th>
  <td>Excessive numbers of packages loaded for TH</td></tr>
  <tr><th>[\#10437](https://gitlab.haskell.org//ghc/ghc/issues/10437)</th>
  <td>RunHaskell error in HSExtra on Win64</td></tr>
  <tr><th>[\#10440](https://gitlab.haskell.org//ghc/ghc/issues/10440)</th>
  <td>Float out just gets floated in</td></tr>
  <tr><th>[\#10445](https://gitlab.haskell.org//ghc/ghc/issues/10445)</th>
  <td>Wrong stack space size when using -Ksize</td></tr>
  <tr><th>[\#10456](https://gitlab.haskell.org//ghc/ghc/issues/10456)</th>
  <td>Wrong CPP during cross-compilation</td></tr>
  <tr><th>[\#10469](https://gitlab.haskell.org//ghc/ghc/issues/10469)</th>
  <td>ghc crash on arm with -j2: internal error: scavenge: unimplemented/strange closure type</td></tr>
  <tr><th>[\#10470](https://gitlab.haskell.org//ghc/ghc/issues/10470)</th>
  <td>Allocating StablePtrs leads to GC slowdown even after they're freed</td></tr>
  <tr><th>[\#10477](https://gitlab.haskell.org//ghc/ghc/issues/10477)</th>
  <td>Tab-completing in a directory with Unicode heiroglyph crashes ghci</td></tr>
  <tr><th>[\#10482](https://gitlab.haskell.org//ghc/ghc/issues/10482)</th>
  <td>Not enough unboxing happens on data-family function argument</td></tr>
  <tr><th>[\#10484](https://gitlab.haskell.org//ghc/ghc/issues/10484)</th>
  <td>hPutBuf crashes when trying to write a large string to stdout (resource exhausted)</td></tr>
  <tr><th>[\#10504](https://gitlab.haskell.org//ghc/ghc/issues/10504)</th>
  <td>GHC panics with dsImpSpecs on SPECIALISE pragma with -fhpc enabled</td></tr>
  <tr><th>[\#10506](https://gitlab.haskell.org//ghc/ghc/issues/10506)</th>
  <td>SourceNotes are not applied to all identifiers</td></tr>
  <tr><th>[\#10509](https://gitlab.haskell.org//ghc/ghc/issues/10509)</th>
  <td>UnicodeSyntax documentation lists wrong symbols</td></tr>
  <tr><th>[\#10526](https://gitlab.haskell.org//ghc/ghc/issues/10526)</th>
  <td>Overlapping instances, incoherence, and optimisation</td></tr>
  <tr><th>[\#10531](https://gitlab.haskell.org//ghc/ghc/issues/10531)</th>
  <td>modules that can be linked successfully when compiled with optimizations, fail to link with: multiple definition of \`__stginit_ZCMain'</td></tr>
  <tr><th>[\#10542](https://gitlab.haskell.org//ghc/ghc/issues/10542)</th>
  <td>Incorrect Unicode input on Windows Console</td></tr>
  <tr><th>[\#10554](https://gitlab.haskell.org//ghc/ghc/issues/10554)</th>
  <td>Replacing existing attachment with the same name doesn't work</td></tr>
  <tr><th>[\#10555](https://gitlab.haskell.org//ghc/ghc/issues/10555)</th>
  <td>RULE left-hand side too complicated to desugar</td></tr>
  <tr><th>[\#10560](https://gitlab.haskell.org//ghc/ghc/issues/10560)</th>
  <td>-f and -O options interact in non-obvious, order dependent ways</td></tr>
  <tr><th>[\#10572](https://gitlab.haskell.org//ghc/ghc/issues/10572)</th>
  <td>Type signatures are not implicitly quantified over TH type variables</td></tr>
  <tr><th>[\#10576](https://gitlab.haskell.org//ghc/ghc/issues/10576)</th>
  <td>REPL returns list of all imported names when operator completion requested</td></tr>
  <tr><th>[\#10582](https://gitlab.haskell.org//ghc/ghc/issues/10582)</th>
  <td>Tiny bug in lexer around lexing banana brackets</td></tr>
  <tr><th>[\#10583](https://gitlab.haskell.org//ghc/ghc/issues/10583)</th>
  <td>Chaos in Lexeme.hs</td></tr>
  <tr><th>[\#10584](https://gitlab.haskell.org//ghc/ghc/issues/10584)</th>
  <td>Installation of SFML failed</td></tr>
  <tr><th>[\#10587](https://gitlab.haskell.org//ghc/ghc/issues/10587)</th>
  <td>Suspending and unsuspending ghci kills and spawns threads</td></tr>
  <tr><th>[\#10595](https://gitlab.haskell.org//ghc/ghc/issues/10595)</th>
  <td>BuiltinRules override other rules in some cases.</td></tr>
  <tr><th>[\#10599](https://gitlab.haskell.org//ghc/ghc/issues/10599)</th>
  <td>Template Haskell doesn't allow \`newName "type"\`</td></tr>
  <tr><th>[\#10616](https://gitlab.haskell.org//ghc/ghc/issues/10616)</th>
  <td>Panic in ghci debugger with PolyKinds and PhantomTypes</td></tr>
  <tr><th>[\#10617](https://gitlab.haskell.org//ghc/ghc/issues/10617)</th>
  <td>Panic in GHCi debugger with GADTs, PolyKinds and Phantom types</td></tr>
  <tr><th>[\#10626](https://gitlab.haskell.org//ghc/ghc/issues/10626)</th>
  <td>Missed opportunity for SpecConstr</td></tr>
  <tr><th>[\#10631](https://gitlab.haskell.org//ghc/ghc/issues/10631)</th>
  <td>Report of GHC Panic loading temp shared object</td></tr>
  <tr><th>[\#10643](https://gitlab.haskell.org//ghc/ghc/issues/10643)</th>
  <td>GHC cannot import submodules when run from subfolder</td></tr>
  <tr><th>[\#10648](https://gitlab.haskell.org//ghc/ghc/issues/10648)</th>
  <td>Some 64-vector SIMD primitives are absolutely useless</td></tr>
  <tr><th>[\#10671](https://gitlab.haskell.org//ghc/ghc/issues/10671)</th>
  <td>inplace/bin/ghc-stage1 doesn't respect --with-ld override</td></tr>
  <tr><th>[\#10675](https://gitlab.haskell.org//ghc/ghc/issues/10675)</th>
  <td>GHC does not check the functional dependency consistency condition correctly</td></tr>
  <tr><th>[\#10686](https://gitlab.haskell.org//ghc/ghc/issues/10686)</th>
  <td>Process stops responding to sigINT</td></tr>
  <tr><th>[\#10695](https://gitlab.haskell.org//ghc/ghc/issues/10695)</th>
  <td>Trac errors when creating a ticket with a Blocking: field</td></tr>
  <tr><th>[\#10698](https://gitlab.haskell.org//ghc/ghc/issues/10698)</th>
  <td>Forall'd variable ‘$rcobox’ is not bound in RULE lhs</td></tr>
  <tr><th>[\#10701](https://gitlab.haskell.org//ghc/ghc/issues/10701)</th>
  <td>-fth-dec-file uses qualified names from hidden modules</td></tr>
  <tr><th>[\#10702](https://gitlab.haskell.org//ghc/ghc/issues/10702)</th>
  <td>-fth-dec-file uses qualified names in binding positions</td></tr>
  <tr><th>[\#10707](https://gitlab.haskell.org//ghc/ghc/issues/10707)</th>
  <td>-fth-dec-file outputs invalid case clauses</td></tr>
  <tr><th>[\#10709](https://gitlab.haskell.org//ghc/ghc/issues/10709)</th>
  <td>Using ($) allows sneaky impredicativity on its left</td></tr>
  <tr><th>[\#10730](https://gitlab.haskell.org//ghc/ghc/issues/10730)</th>
  <td>Spectral norm allocations increased 17% between 7.6 and 7.8</td></tr>
  <tr><th>[\#10732](https://gitlab.haskell.org//ghc/ghc/issues/10732)</th>
  <td>Legal Bang Patterns cannot parse</td></tr>
  <tr><th>[\#10749](https://gitlab.haskell.org//ghc/ghc/issues/10749)</th>
  <td>Boot file instances should imply superclasses</td></tr>
  <tr><th>[\#10761](https://gitlab.haskell.org//ghc/ghc/issues/10761)</th>
  <td>GHC panic when compiling vimus: failed to map segment from shared object</td></tr>
  <tr><th>[\#10768](https://gitlab.haskell.org//ghc/ghc/issues/10768)</th>
  <td>Location information of LHsModule is incorrect</td></tr>
  <tr><th>[\#10770](https://gitlab.haskell.org//ghc/ghc/issues/10770)</th>
  <td>Typeable solver has strange effects</td></tr>
  <tr><th>[\#10778](https://gitlab.haskell.org//ghc/ghc/issues/10778)</th>
  <td>GHC doesn't infer all constrains</td></tr>
  <tr><th>[\#10779](https://gitlab.haskell.org//ghc/ghc/issues/10779)</th>
  <td>.so files in 64-bit Debian builds are 4% larger than they have to be</td></tr>
  <tr><th>[\#10783](https://gitlab.haskell.org//ghc/ghc/issues/10783)</th>
  <td>Partial type signatures should work in pattern synonym signatures</td></tr>
  <tr><th>[\#10792](https://gitlab.haskell.org//ghc/ghc/issues/10792)</th>
  <td>Bounded Enums \[minBound..maxBound\] produces runtime error</td></tr>
  <tr><th>[\#10799](https://gitlab.haskell.org//ghc/ghc/issues/10799)</th>
  <td>"I found a duplicate definition for symbol: __x86.get_pc_thunk.bx" in package network</td></tr>
  <tr><th>[\#10808](https://gitlab.haskell.org//ghc/ghc/issues/10808)</th>
  <td>Odd interaction between record update and type families</td></tr>
  <tr><th>[\#10818](https://gitlab.haskell.org//ghc/ghc/issues/10818)</th>
  <td>GHC 7.10.2 takes much longer to compile some packages</td></tr>
  <tr><th>[\#10822](https://gitlab.haskell.org//ghc/ghc/issues/10822)</th>
  <td>GHC inconsistently handles \\\\?\\ for long paths on Windows</td></tr>
  <tr><th>[\#10853](https://gitlab.haskell.org//ghc/ghc/issues/10853)</th>
  <td>Refine addTopDecls</td></tr>
  <tr><th>[\#10856](https://gitlab.haskell.org//ghc/ghc/issues/10856)</th>
  <td>Record update doesn't emit new constraints</td></tr>
  <tr><th>[\#10861](https://gitlab.haskell.org//ghc/ghc/issues/10861)</th>
  <td>\`configure -C\` yields different results on second run</td></tr>
  <tr><th>[\#10875](https://gitlab.haskell.org//ghc/ghc/issues/10875)</th>
  <td>Unexpected defaulting of partial type signatures and inconsistent behaviour when -fdefer-typed-holes is set.</td></tr>
  <tr><th>[\#10878](https://gitlab.haskell.org//ghc/ghc/issues/10878)</th>
  <td>Near doubling of generated code size for compiler/cmm/PprC.hs with commit 5d57087e31</td></tr>
  <tr><th>[\#10920](https://gitlab.haskell.org//ghc/ghc/issues/10920)</th>
  <td>ghci can't load local Prelude module</td></tr>
  <tr><th>[\#10922](https://gitlab.haskell.org//ghc/ghc/issues/10922)</th>
  <td>String inlining is inconsistent</td></tr>
  <tr><th>[\#10923](https://gitlab.haskell.org//ghc/ghc/issues/10923)</th>
  <td>GHC should recompile if flags change</td></tr>
  <tr><th>[\#10927](https://gitlab.haskell.org//ghc/ghc/issues/10927)</th>
  <td>IndexError: pop from empty list</td></tr>
  <tr><th>[\#10937](https://gitlab.haskell.org//ghc/ghc/issues/10937)</th>
  <td>"ghc -no-link --make A.hs -o foo" does something silly</td></tr>
  <tr><th>[\#10944](https://gitlab.haskell.org//ghc/ghc/issues/10944)</th>
  <td>powModInteger slower than computing pow and mod separately</td></tr>
  <tr><th>[\#10946](https://gitlab.haskell.org//ghc/ghc/issues/10946)</th>
  <td>Typed hole inside typed Template Haskell bracket causes panic</td></tr>
  <tr><th>[\#10951](https://gitlab.haskell.org//ghc/ghc/issues/10951)</th>
  <td>HPC program has poor error reporting / strange CLI in general</td></tr>
  <tr><th>[\#10952](https://gitlab.haskell.org//ghc/ghc/issues/10952)</th>
  <td>Use IPids instead of package keys in HPC tix files</td></tr>
  <tr><th>[\#10957](https://gitlab.haskell.org//ghc/ghc/issues/10957)</th>
  <td>getExecutablePath adds " (deleted)" suffix if executable was deleted under linux</td></tr>
  <tr><th>[\#10958](https://gitlab.haskell.org//ghc/ghc/issues/10958)</th>
  <td>"Annotating pure code for parallelism" docs based on old par/pseq primitives</td></tr>
  <tr><th>[\#10966](https://gitlab.haskell.org//ghc/ghc/issues/10966)</th>
  <td>dirtiness checking isn't keeping track of which source file contained Main</td></tr>
  <tr><th>[\#10975](https://gitlab.haskell.org//ghc/ghc/issues/10975)</th>
  <td>At program exit, finalizer runs while foreign function is running</td></tr>
  <tr><th>[\#10980](https://gitlab.haskell.org//ghc/ghc/issues/10980)</th>
  <td>Deriving Read instance from datatype with N fields leads to N\^2 code size growth</td></tr>
  <tr><th>[\#10987](https://gitlab.haskell.org//ghc/ghc/issues/10987)</th>
  <td>-i option requires named module</td></tr>
  <tr><th>[\#10992](https://gitlab.haskell.org//ghc/ghc/issues/10992)</th>
  <td>Performance regression due to lack of inlining of \`foldl\` and \`foldl'\`.</td></tr>
  <tr><th>[\#10993](https://gitlab.haskell.org//ghc/ghc/issues/10993)</th>
  <td>Bad error message reported when -XBinaryLiterals is not enabled</td></tr>
  <tr><th>[\#10995](https://gitlab.haskell.org//ghc/ghc/issues/10995)</th>
  <td>Existentials in newtypes</td></tr>
  <tr><th>[\#10996](https://gitlab.haskell.org//ghc/ghc/issues/10996)</th>
  <td>family is treated as keyword in types even without TypeFamilies enabled</td></tr>
  <tr><th>[\#10998](https://gitlab.haskell.org//ghc/ghc/issues/10998)</th>
  <td>Parser should suggest -XMagicHash</td></tr>
  <tr><th>[\#11005](https://gitlab.haskell.org//ghc/ghc/issues/11005)</th>
  <td>GHC's build system can't deal with ghc install path with multiple spaces in it</td></tr>
  <tr><th>[\#11008](https://gitlab.haskell.org//ghc/ghc/issues/11008)</th>
  <td>Difficulties around inferring exotic contexts</td></tr>
  <tr><th>[\#11009](https://gitlab.haskell.org//ghc/ghc/issues/11009)</th>
  <td>Errors reading stdin on Windows</td></tr>
  <tr><th>[\#11013](https://gitlab.haskell.org//ghc/ghc/issues/11013)</th>
  <td>GHC sometimes forgets to test for hs-boot consistency</td></tr>
  <tr><th>[\#11023](https://gitlab.haskell.org//ghc/ghc/issues/11023)</th>
  <td>ghci and ghc-pkg disagree about what's exposed</td></tr>
  <tr><th>[\#11029](https://gitlab.haskell.org//ghc/ghc/issues/11029)</th>
  <td>Performance loss due to eta expansion</td></tr>
  <tr><th>[\#11045](https://gitlab.haskell.org//ghc/ghc/issues/11045)</th>
  <td>Loading temp shared object failed - undefined symbol</td></tr>
  <tr><th>[\#11050](https://gitlab.haskell.org//ghc/ghc/issues/11050)</th>
  <td>\[bug\] ModOrigin: hidden module redefined</td></tr>
  <tr><th>[\#11058](https://gitlab.haskell.org//ghc/ghc/issues/11058)</th>
  <td>selected processor does not support ARM mode</td></tr>
  <tr><th>[\#11068](https://gitlab.haskell.org//ghc/ghc/issues/11068)</th>
  <td>Make Generic/Generic1 methods inlinable</td></tr>
  <tr><th>[\#11069](https://gitlab.haskell.org//ghc/ghc/issues/11069)</th>
  <td>:cd in GHCi unloads modules</td></tr>
  <tr><th>[\#11070](https://gitlab.haskell.org//ghc/ghc/issues/11070)</th>
  <td>Type-level arithmetic of sized-types has weaker inference power than in 7.8</td></tr>
  <tr><th>[\#11075](https://gitlab.haskell.org//ghc/ghc/issues/11075)</th>
  <td>Confusing parallel spark behaviour with safe FFI calls</td></tr>
  <tr><th>[\#11084](https://gitlab.haskell.org//ghc/ghc/issues/11084)</th>
  <td>Some type families don't reduce with :kind!</td></tr>
  <tr><th>[\#11092](https://gitlab.haskell.org//ghc/ghc/issues/11092)</th>
  <td>ApiAnnotations : make annotation for shebang</td></tr>
  <tr><th>[\#11099](https://gitlab.haskell.org//ghc/ghc/issues/11099)</th>
  <td>Incorrect warning about redundant constraints</td></tr>
  <tr><th>[\#11101](https://gitlab.haskell.org//ghc/ghc/issues/11101)</th>
  <td>Expand Template Haskell type splices before quantification</td></tr>
  <tr><th>[\#11106](https://gitlab.haskell.org//ghc/ghc/issues/11106)</th>
  <td>Turning on optimizer changes behavior in 7.10.3</td></tr>
  <tr><th>[\#11107](https://gitlab.haskell.org//ghc/ghc/issues/11107)</th>
  <td>Can't use type wildcard infix</td></tr>
  <tr><th>[\#11110](https://gitlab.haskell.org//ghc/ghc/issues/11110)</th>
  <td>GHCi documentation says ":show packages" gives a list of packages currently loaded</td></tr>
  <tr><th>[\#11113](https://gitlab.haskell.org//ghc/ghc/issues/11113)</th>
  <td>Type family If is too strict</td></tr>
  <tr><th>[\#11117](https://gitlab.haskell.org//ghc/ghc/issues/11117)</th>
  <td>mdo blocks in error messages are shown modified</td></tr>
  <tr><th>[\#11124](https://gitlab.haskell.org//ghc/ghc/issues/11124)</th>
  <td>GHC does not shadow -package-name/-this-package-key</td></tr>
  <tr><th>[\#11126](https://gitlab.haskell.org//ghc/ghc/issues/11126)</th>
  <td>Entered absent arg in a Repa program</td></tr>
  <tr><th>[\#11131](https://gitlab.haskell.org//ghc/ghc/issues/11131)</th>
  <td>Eta reduction/expansion loop</td></tr>
  <tr><th>[\#11141](https://gitlab.haskell.org//ghc/ghc/issues/11141)</th>
  <td>Better error message when instance signature is incorrect</td></tr>
  <tr><th>[\#11146](https://gitlab.haskell.org//ghc/ghc/issues/11146)</th>
  <td>Manual eta expansion leads to orders of magnitude less allocations</td></tr>
  <tr><th>[\#11151](https://gitlab.haskell.org//ghc/ghc/issues/11151)</th>
  <td>T3064 regresses with wildcard refactor</td></tr>
  <tr><th>[\#11180](https://gitlab.haskell.org//ghc/ghc/issues/11180)</th>
  <td>A program writing to a read-only stdout should not succeed</td></tr>
  <tr><th>[\#11181](https://gitlab.haskell.org//ghc/ghc/issues/11181)</th>
  <td>Program hangs forever in sched_yield() / yield() unless -N is limited</td></tr>
  <tr><th>[\#11195](https://gitlab.haskell.org//ghc/ghc/issues/11195)</th>
  <td>New pattern-match check can be non-performant</td></tr>
  <tr><th>[\#11196](https://gitlab.haskell.org//ghc/ghc/issues/11196)</th>
  <td>TypeInType performance regressions</td></tr>
  <tr><th>[\#11197](https://gitlab.haskell.org//ghc/ghc/issues/11197)</th>
  <td>Overeager deferred type errors</td></tr>
  <tr><th>[\#11198](https://gitlab.haskell.org//ghc/ghc/issues/11198)</th>
  <td>TypeInType error message regressions</td></tr>
  <tr><th>[\#11201](https://gitlab.haskell.org//ghc/ghc/issues/11201)</th>
  <td>ghc --make on Haskell and non-Haskell inputs can silently clobber input</td></tr>
  <tr><th>[\#11204](https://gitlab.haskell.org//ghc/ghc/issues/11204)</th>
  <td>Recompilation checking stochastically broken on Darwin</td></tr>
  <tr><th>[\#11207](https://gitlab.haskell.org//ghc/ghc/issues/11207)</th>
  <td>GHC cannot infer injectivity on type family operating on GHC.TypeLits' Nat, but can for equivalent type family operating on user-defined Nat kind</td></tr>
  <tr><th>[\#11212](https://gitlab.haskell.org//ghc/ghc/issues/11212)</th>
  <td>Should be more liberal parsing pattern synonyms with view patterns</td></tr>
  <tr><th>[\#11214](https://gitlab.haskell.org//ghc/ghc/issues/11214)</th>
  <td>Remove JavaScriptFFI from --supported-extensions list</td></tr>
  <tr><th>[\#11215](https://gitlab.haskell.org//ghc/ghc/issues/11215)</th>
  <td>Line endings in quasiquotations are not normalised</td></tr>
  <tr><th>[\#11226](https://gitlab.haskell.org//ghc/ghc/issues/11226)</th>
  <td>Performance regression (involving sum, map, enumFromThenTo)</td></tr>
  <tr><th>[\#11228](https://gitlab.haskell.org//ghc/ghc/issues/11228)</th>
  <td>Interaction between ORF and record pattern synonyms needs to be resolved.</td></tr>
  <tr><th>[\#11247](https://gitlab.haskell.org//ghc/ghc/issues/11247)</th>
  <td>Weird error from running runghc on an invalid input filename</td></tr>
  <tr><th>[\#11251](https://gitlab.haskell.org//ghc/ghc/issues/11251)</th>
  <td>isInstance does not work on Typeable with base-4.8 anymore</td></tr>
  <tr><th>[\#11253](https://gitlab.haskell.org//ghc/ghc/issues/11253)</th>
  <td>Duplicate warnings for pattern guards and relevant features (e.g. View Patterns)</td></tr>
  <tr><th>[\#11259](https://gitlab.haskell.org//ghc/ghc/issues/11259)</th>
  <td>Use system runtime linker in GHCi on PowerPC 64 bit</td></tr>
  <tr><th>[\#11260](https://gitlab.haskell.org//ghc/ghc/issues/11260)</th>
  <td>Re-compilation driver/recomp11 test fails</td></tr>
  <tr><th>[\#11261](https://gitlab.haskell.org//ghc/ghc/issues/11261)</th>
  <td>Implement DWARF debugging on powerpc64</td></tr>
  <tr><th>[\#11263](https://gitlab.haskell.org//ghc/ghc/issues/11263)</th>
  <td>"Simplifier ticks exhausted" that resolves with fsimpl-tick-factor=200</td></tr>
  <tr><th>[\#11271](https://gitlab.haskell.org//ghc/ghc/issues/11271)</th>
  <td>Costly let binding gets duplicated in IO action value</td></tr>
  <tr><th>[\#11288](https://gitlab.haskell.org//ghc/ghc/issues/11288)</th>
  <td>Thread blocked indefinitely in a Mvar operation</td></tr>
  <tr><th>[\#11293](https://gitlab.haskell.org//ghc/ghc/issues/11293)</th>
  <td>Compiler plugins don't work with profiling</td></tr>
  <tr><th>[\#11301](https://gitlab.haskell.org//ghc/ghc/issues/11301)</th>
  <td>Using GHC's parser and rendering the results is unreasonably difficult</td></tr>
  <tr><th>[\#11306](https://gitlab.haskell.org//ghc/ghc/issues/11306)</th>
  <td>Do not generate warning in \`do\` when result is of type \`Void\`.</td></tr>
  <tr><th>[\#11307](https://gitlab.haskell.org//ghc/ghc/issues/11307)</th>
  <td>Regresssion: parsing type operators</td></tr>
  <tr><th>[\#11310](https://gitlab.haskell.org//ghc/ghc/issues/11310)</th>
  <td>Surprising accepted constructor for GADT instance of data family</td></tr>
  <tr><th>[\#11312](https://gitlab.haskell.org//ghc/ghc/issues/11312)</th>
  <td>GHC inlining primitive string literals can affect program output</td></tr>
  <tr><th>[\#11317](https://gitlab.haskell.org//ghc/ghc/issues/11317)</th>
  <td>Test prog003 fails with segfault on Windows (GHCi)</td></tr>
  <tr><th>[\#11323](https://gitlab.haskell.org//ghc/ghc/issues/11323)</th>
  <td>powerpc64: recomp015 fails with redundant linking</td></tr>
  <tr><th>[\#11327](https://gitlab.haskell.org//ghc/ghc/issues/11327)</th>
  <td>GHC doesn't create dyn_o-boot files</td></tr>
  <tr><th>[\#11346](https://gitlab.haskell.org//ghc/ghc/issues/11346)</th>
  <td>GHCi can crash when tabbing a filename</td></tr>
  <tr><th>[\#11360](https://gitlab.haskell.org//ghc/ghc/issues/11360)</th>
  <td>Test "termination" doesn't pass with reversed uniques</td></tr>
  <tr><th>[\#11368](https://gitlab.haskell.org//ghc/ghc/issues/11368)</th>
  <td>Pattern synonym name is mangled when patterns are non-exhaustive</td></tr>
  <tr><th>[\#11369](https://gitlab.haskell.org//ghc/ghc/issues/11369)</th>
  <td>Suppress redundant-constraint warnings in case of empty classes</td></tr>
  <tr><th>[\#11371](https://gitlab.haskell.org//ghc/ghc/issues/11371)</th>
  <td>Bogus in-scope set in substitutions</td></tr>
  <tr><th>[\#11380](https://gitlab.haskell.org//ghc/ghc/issues/11380)</th>
  <td>Compiling a 10.000 line file exhausts memory</td></tr>
  <tr><th>[\#11384](https://gitlab.haskell.org//ghc/ghc/issues/11384)</th>
  <td>Error says to fix incorrect return type</td></tr>
  <tr><th>[\#11406](https://gitlab.haskell.org//ghc/ghc/issues/11406)</th>
  <td>RTS gets stuck in scheduleDetectDeadlock()</td></tr>
  <tr><th>[\#11424](https://gitlab.haskell.org//ghc/ghc/issues/11424)</th>
  <td>"Occurs check" not considered when reducing closed type families</td></tr>
  <tr><th>[\#11427](https://gitlab.haskell.org//ghc/ghc/issues/11427)</th>
  <td>superclasses aren't considered because context is no smaller than the instance head</td></tr>
  <tr><th>[\#11435](https://gitlab.haskell.org//ghc/ghc/issues/11435)</th>
  <td>Evidence from TC Plugin triggers core-lint warning</td></tr>
  <tr><th>[\#11436](https://gitlab.haskell.org//ghc/ghc/issues/11436)</th>
  <td>ValueError: invalid literal for int() with base 10: '\#11238'</td></tr>
  <tr><th>[\#11437](https://gitlab.haskell.org//ghc/ghc/issues/11437)</th>
  <td>Don't put static (version-based) feature gates in compilerInfo</td></tr>
  <tr><th>[\#11449](https://gitlab.haskell.org//ghc/ghc/issues/11449)</th>
  <td>Treat '_' consistently in type declarations</td></tr>
  <tr><th>[\#11475](https://gitlab.haskell.org//ghc/ghc/issues/11475)</th>
  <td>Lint should check for inexhaustive alternatives</td></tr>
  <tr><th>[\#11476](https://gitlab.haskell.org//ghc/ghc/issues/11476)</th>
  <td>gcc leaves undeleted temporary files when invoked with a response file</td></tr>
  <tr><th>[\#11490](https://gitlab.haskell.org//ghc/ghc/issues/11490)</th>
  <td>check architecture before attempting to load object files</td></tr>
  <tr><th>[\#11495](https://gitlab.haskell.org//ghc/ghc/issues/11495)</th>
  <td>TH_spliceE5_prof is failing with release candidate 8.0.1</td></tr>
  <tr><th>[\#11499](https://gitlab.haskell.org//ghc/ghc/issues/11499)</th>
  <td>Loading temp shared object failed in GHCi</td></tr>
  <tr><th>[\#11503](https://gitlab.haskell.org//ghc/ghc/issues/11503)</th>
  <td>TypeError woes (incl. pattern match checker)</td></tr>
  <tr><th>[\#11505](https://gitlab.haskell.org//ghc/ghc/issues/11505)</th>
  <td>Boot file problem</td></tr>
  <tr><th>[\#11506](https://gitlab.haskell.org//ghc/ghc/issues/11506)</th>
  <td>uType_defer can defer too long</td></tr>
  <tr><th>[\#11511](https://gitlab.haskell.org//ghc/ghc/issues/11511)</th>
  <td>Type family producing infinite type accepted as injective</td></tr>
  <tr><th>[\#11514](https://gitlab.haskell.org//ghc/ghc/issues/11514)</th>
  <td>Impredicativity is still sneaking in</td></tr>
  <tr><th>[\#11517](https://gitlab.haskell.org//ghc/ghc/issues/11517)</th>
  <td>ghc -haddock fails to parse doc comments in closed type families</td></tr>
  <tr><th>[\#11522](https://gitlab.haskell.org//ghc/ghc/issues/11522)</th>
  <td>maxStkSize can overflow</td></tr>
  <tr><th>[\#11523](https://gitlab.haskell.org//ghc/ghc/issues/11523)</th>
  <td>Infinite Loop when mixing UndecidableSuperClasses and the class/instance constraint synonym trick.</td></tr>
  <tr><th>[\#11526](https://gitlab.haskell.org//ghc/ghc/issues/11526)</th>
  <td>unsafeLookupStaticPtr should not live in IO</td></tr>
  <tr><th>[\#11527](https://gitlab.haskell.org//ghc/ghc/issues/11527)</th>
  <td>Pattern match translation suboptimal</td></tr>
  <tr><th>[\#11529](https://gitlab.haskell.org//ghc/ghc/issues/11529)</th>
  <td>Show instance of Char should print literals for non-ascii printable charcters</td></tr>
  <tr><th>[\#11536](https://gitlab.haskell.org//ghc/ghc/issues/11536)</th>
  <td>Multitude of different error messages when installed package is missing a module</td></tr>
  <tr><th>[\#11538](https://gitlab.haskell.org//ghc/ghc/issues/11538)</th>
  <td>Wrong constants in LL code for big endian targets</td></tr>
  <tr><th>[\#11540](https://gitlab.haskell.org//ghc/ghc/issues/11540)</th>
  <td>ghc accepts non-standard type without language extension</td></tr>
  <tr><th>[\#11542](https://gitlab.haskell.org//ghc/ghc/issues/11542)</th>
  <td>Profiling call count frequently 0 when it shouldn't be</td></tr>
  <tr><th>[\#11545](https://gitlab.haskell.org//ghc/ghc/issues/11545)</th>
  <td>Strictness signature blowup</td></tr>
  <tr><th>[\#11546](https://gitlab.haskell.org//ghc/ghc/issues/11546)</th>
  <td>Compiler warning: cast from pointer to integer of different size</td></tr>
  <tr><th>[\#11553](https://gitlab.haskell.org//ghc/ghc/issues/11553)</th>
  <td>\`round (± ∞ :: Double)\` not infinite</td></tr>
  <tr><th>[\#11556](https://gitlab.haskell.org//ghc/ghc/issues/11556)</th>
  <td>GHC recompiles unchanged hs-boot files</td></tr>
  <tr><th>[\#11559](https://gitlab.haskell.org//ghc/ghc/issues/11559)</th>
  <td>Building a cross-compiler for MIPS target on Mac OS X host fails</td></tr>
  <tr><th>[\#11566](https://gitlab.haskell.org//ghc/ghc/issues/11566)</th>
  <td>I don't need madvise MADV_DONTNEED</td></tr>
  <tr><th>[\#11571](https://gitlab.haskell.org//ghc/ghc/issues/11571)</th>
  <td>Need more intelligent conditionalization of libgcc rts symbols for x32</td></tr>
  <tr><th>[\#11577](https://gitlab.haskell.org//ghc/ghc/issues/11577)</th>
  <td>GHCi accepts invalid programs when recompiling</td></tr>
  <tr><th>[\#11578](https://gitlab.haskell.org//ghc/ghc/issues/11578)</th>
  <td>Fix GHC on AArch64/Arm64</td></tr>
  <tr><th>[\#11584](https://gitlab.haskell.org//ghc/ghc/issues/11584)</th>
  <td>\[Template Haskell\] Language.Haskell.TH.Syntax.hs contains misleading comment</td></tr>
  <tr><th>[\#11587](https://gitlab.haskell.org//ghc/ghc/issues/11587)</th>
  <td>Place shared objects in LIBDIR</td></tr>
  <tr><th>[\#11596](https://gitlab.haskell.org//ghc/ghc/issues/11596)</th>
  <td>ghci gets confused if a file is deleted</td></tr>
  <tr><th>[\#11599](https://gitlab.haskell.org//ghc/ghc/issues/11599)</th>
  <td>Why is UndecidableInstances required for an obviously terminating type family?</td></tr>
  <tr><th>[\#11602](https://gitlab.haskell.org//ghc/ghc/issues/11602)</th>
  <td>Exponential behaviour in typeKind, unifyTys etc</td></tr>
  <tr><th>[\#11604](https://gitlab.haskell.org//ghc/ghc/issues/11604)</th>
  <td>Build system fails after submodule update</td></tr>
  <tr><th>[\#11605](https://gitlab.haskell.org//ghc/ghc/issues/11605)</th>
  <td>GHC accepts overlapping instances without pragma</td></tr>
  <tr><th>[\#11621](https://gitlab.haskell.org//ghc/ghc/issues/11621)</th>
  <td>GHC doesn't see () as a Constraint in type family</td></tr>
  <tr><th>[\#11628](https://gitlab.haskell.org//ghc/ghc/issues/11628)</th>
  <td>Unexpected results with Read/Show</td></tr>
  <tr><th>[\#11630](https://gitlab.haskell.org//ghc/ghc/issues/11630)</th>
  <td>More precise LANGUAGE pragma when forall is used</td></tr>
  <tr><th>[\#11634](https://gitlab.haskell.org//ghc/ghc/issues/11634)</th>
  <td>Bang patterns bind... unexpectedly. Deserves loud warning</td></tr>
  <tr><th>[\#11650](https://gitlab.haskell.org//ghc/ghc/issues/11650)</th>
  <td>Documentation does not mention that default definitions for Alternative(some, many) can easily blow up</td></tr>
  <tr><th>[\#11655](https://gitlab.haskell.org//ghc/ghc/issues/11655)</th>
  <td>Ambiguous types in pattern synonym not determined by functional dependencies</td></tr>
  <tr><th>[\#11668](https://gitlab.haskell.org//ghc/ghc/issues/11668)</th>
  <td>SPEC has a runtime cost if constructor specialization isn't performed</td></tr>
  <tr><th>[\#11677](https://gitlab.haskell.org//ghc/ghc/issues/11677)</th>
  <td>Dramatic de-optimization with "-O", "-O1", "-O2" options</td></tr>
  <tr><th>[\#11695](https://gitlab.haskell.org//ghc/ghc/issues/11695)</th>
  <td>On GHCi prompt the arrow (movement) keys create strange character sequences</td></tr>
  <tr><th>[\#11704](https://gitlab.haskell.org//ghc/ghc/issues/11704)</th>
  <td>Word and Int literals not correctly truncated when cross compiling 64 -\> 32 bit</td></tr>
  <tr><th>[\#11715](https://gitlab.haskell.org//ghc/ghc/issues/11715)</th>
  <td>Constraint vs \*</td></tr>
  <tr><th>[\#11771](https://gitlab.haskell.org//ghc/ghc/issues/11771)</th>
  <td>ghc.exe: \`panic'! (the 'impossible' happened); thread blocked indefinitely in an MVar operation</td></tr>
  <tr><th>[\#11773](https://gitlab.haskell.org//ghc/ghc/issues/11773)</th>
  <td>linux/powepc : ghc-stage1 segfaults when building unregisterised</td></tr>
  <tr><th>[\#11774](https://gitlab.haskell.org//ghc/ghc/issues/11774)</th>
  <td>Regression on GHC 8 branch (vs 7.10.3) when using the GHC API to parse code that uses TH</td></tr>
  <tr><th>[\#11780](https://gitlab.haskell.org//ghc/ghc/issues/11780)</th>
  <td>GHC stage-2 build fails with "relocation R_X86_64_PC32 against \`exitStaticPtrTable' can not be used when making a shared object"</td></tr>
  <tr><th>[\#11812](https://gitlab.haskell.org//ghc/ghc/issues/11812)</th>
  <td>Template Haskell can induce non-unique Uniques</td></tr>
  <tr><th>[\#11822](https://gitlab.haskell.org//ghc/ghc/issues/11822)</th>
  <td>Pattern match checker exceeded (2000000) iterations</td></tr>
  <tr><th>[\#11827](https://gitlab.haskell.org//ghc/ghc/issues/11827)</th>
  <td>InteractiveEval error handling gets a boot ModSummary instead of normal ModSummary</td></tr>
  <tr><th>[\#11829](https://gitlab.haskell.org//ghc/ghc/issues/11829)</th>
  <td>C++ does not catch exceptions when used with Haskell-main and linked by ghc</td></tr>
  <tr><th>[\#11831](https://gitlab.haskell.org//ghc/ghc/issues/11831)</th>
  <td>Illegal Instruction when running byte operations in ghci</td></tr>
  <tr><th>[\#11836](https://gitlab.haskell.org//ghc/ghc/issues/11836)</th>
  <td>Hello World Bug - silent stdout errors</td></tr>
  <tr><th>[\#11944](https://gitlab.haskell.org//ghc/ghc/issues/11944)</th>
  <td>Simplifier ticks exhausted   When trying UnfoldingDone ip_X7RI</td></tr>
  <tr><th>[\#11955](https://gitlab.haskell.org//ghc/ghc/issues/11955)</th>
  <td>Haddock documentation for pattern synonyms printed with explicit forall quantifiers</td></tr>
  <tr><th>[\#11957](https://gitlab.haskell.org//ghc/ghc/issues/11957)</th>
  <td>DataKinds: lifting constructors whose identifier is a single character</td></tr>
  <tr><th>[\#11959](https://gitlab.haskell.org//ghc/ghc/issues/11959)</th>
  <td>Importing doubly exported pattern synonym and associated pattern synonym panics</td></tr>
  <tr><th>[\#11982](https://gitlab.haskell.org//ghc/ghc/issues/11982)</th>
  <td>Typechecking fails for parallel monad comprehensions with polymorphic let (GHC 7.10.3 through 8.6.3)</td></tr>
  <tr><th>[\#11994](https://gitlab.haskell.org//ghc/ghc/issues/11994)</th>
  <td>ghci not applying defaulting when showing type</td></tr>
  <tr><th>[\#11998](https://gitlab.haskell.org//ghc/ghc/issues/11998)</th>
  <td>Symbol not found: __hpc_tickboxes_DataziHeterogeneousEnvironment_hpc</td></tr>
  <tr><th>[\#11999](https://gitlab.haskell.org//ghc/ghc/issues/11999)</th>
  <td>expressing injectivity on functional dependencies gives orphan instances warnings</td></tr>
  <tr><th>[\#12002](https://gitlab.haskell.org//ghc/ghc/issues/12002)</th>
  <td>Pragmas after a module declaration are ignored without warning.</td></tr>
  <tr><th>[\#12006](https://gitlab.haskell.org//ghc/ghc/issues/12006)</th>
  <td>Can't infer constraint of pattern synonyms</td></tr>
  <tr><th>[\#12018](https://gitlab.haskell.org//ghc/ghc/issues/12018)</th>
  <td>Equality constraint not available in pattern type signature (GADTs/ScopedTypeVariables)</td></tr>
  <tr><th>[\#12019](https://gitlab.haskell.org//ghc/ghc/issues/12019)</th>
  <td>Profiling option -hb is not thread safe</td></tr>
  <tr><th>[\#12021](https://gitlab.haskell.org//ghc/ghc/issues/12021)</th>
  <td>Type variable escapes its scope</td></tr>
  <tr><th>[\#12028](https://gitlab.haskell.org//ghc/ghc/issues/12028)</th>
  <td>Large let bindings are 6x slower (since 6.12.x to 7.10.x)</td></tr>
  <tr><th>[\#12032](https://gitlab.haskell.org//ghc/ghc/issues/12032)</th>
  <td>Performance regression with large numbers of equation-style decls</td></tr>
  <tr><th>[\#12034](https://gitlab.haskell.org//ghc/ghc/issues/12034)</th>
  <td>Template Haskell + hs-boot = Not in scope during type checking, but it passed the renamer</td></tr>
  <tr><th>[\#12038](https://gitlab.haskell.org//ghc/ghc/issues/12038)</th>
  <td>Shutdown interacts badly with requestSync()</td></tr>
  <tr><th>[\#12046](https://gitlab.haskell.org//ghc/ghc/issues/12046)</th>
  <td>AllowAmbiguousTypes doesn't work with UndecidableSuperClasses</td></tr>
  <tr><th>[\#12060](https://gitlab.haskell.org//ghc/ghc/issues/12060)</th>
  <td>GHC panic depending on what a Haskell module is named</td></tr>
  <tr><th>[\#12063](https://gitlab.haskell.org//ghc/ghc/issues/12063)</th>
  <td>Knot-tying failure when type-synonym refers to non-existent data</td></tr>
  <tr><th>[\#12074](https://gitlab.haskell.org//ghc/ghc/issues/12074)</th>
  <td>RULE too complicated to desugar</td></tr>
  <tr><th>[\#12078](https://gitlab.haskell.org//ghc/ghc/issues/12078)</th>
  <td>ghc-boot-th package reveals issue with build system's treatment of transitive dependencies</td></tr>
  <tr><th>[\#12079](https://gitlab.haskell.org//ghc/ghc/issues/12079)</th>
  <td>segmentation fault in both ghci and compiled program involves gtk library</td></tr>
  <tr><th>[\#12088](https://gitlab.haskell.org//ghc/ghc/issues/12088)</th>
  <td>Type/data family instances in kind checking</td></tr>
  <tr><th>[\#12100](https://gitlab.haskell.org//ghc/ghc/issues/12100)</th>
  <td>GHC 8.0.1 build segmentation fault in haddock</td></tr>
  <tr><th>[\#12102](https://gitlab.haskell.org//ghc/ghc/issues/12102)</th>
  <td>“Constraints in kinds” illegal family application in instance (+ documentation issues?)</td></tr>
  <tr><th>[\#12113](https://gitlab.haskell.org//ghc/ghc/issues/12113)</th>
  <td>ghc-8.0.1-rc4: unification false positive?</td></tr>
  <tr><th>[\#12117](https://gitlab.haskell.org//ghc/ghc/issues/12117)</th>
  <td>Thread by forkIO freezes (while read :: Int if error appears) when compiled with -threaded option</td></tr>
  <tr><th>[\#12120](https://gitlab.haskell.org//ghc/ghc/issues/12120)</th>
  <td>GHC accepts invalid Haskell: \`class Eq (a Int) =\> C a where\`</td></tr>
  <tr><th>[\#12121](https://gitlab.haskell.org//ghc/ghc/issues/12121)</th>
  <td>FlexibleContexts is under specified</td></tr>
  <tr><th>[\#12122](https://gitlab.haskell.org//ghc/ghc/issues/12122)</th>
  <td>User's guide (master): all links to libraries are broken</td></tr>
  <tr><th>[\#12126](https://gitlab.haskell.org//ghc/ghc/issues/12126)</th>
  <td>Bad error messages for SPECIALIZE pragmas</td></tr>
  <tr><th>[\#12131](https://gitlab.haskell.org//ghc/ghc/issues/12131)</th>
  <td>Can't solve constraints with UndecidableSuperClasses but can infer kind (+ undesired order of kinds)</td></tr>
  <tr><th>[\#12135](https://gitlab.haskell.org//ghc/ghc/issues/12135)</th>
  <td>Failure to recompile when \#include file is created earlier on include path</td></tr>
  <tr><th>[\#12142](https://gitlab.haskell.org//ghc/ghc/issues/12142)</th>
  <td>-Wredundant-constraints warns about constraints introduced via type synonyms.</td></tr>
  <tr><th>[\#12149](https://gitlab.haskell.org//ghc/ghc/issues/12149)</th>
  <td>Support bit-fields</td></tr>
  <tr><th>[\#12161](https://gitlab.haskell.org//ghc/ghc/issues/12161)</th>
  <td>Panic when literal is coerced into function</td></tr>
  <tr><th>[\#12168](https://gitlab.haskell.org//ghc/ghc/issues/12168)</th>
  <td>panic! (the 'impossible' happened) with gi-gtk 3.0.4</td></tr>
  <tr><th>[\#12169](https://gitlab.haskell.org//ghc/ghc/issues/12169)</th>
  <td>libraries/base/dist-install/build/HSbase-4.9.0.0.o: unknown symbol \`stat'</td></tr>
  <tr><th>[\#12173](https://gitlab.haskell.org//ghc/ghc/issues/12173)</th>
  <td>foldl' semantics changed from 4.7 to 4.8</td></tr>
  <tr><th>[\#12179](https://gitlab.haskell.org//ghc/ghc/issues/12179)</th>
  <td>Incorrect parsing of a pattern synonym type</td></tr>
  <tr><th>[\#12180](https://gitlab.haskell.org//ghc/ghc/issues/12180)</th>
  <td>Ctrl-c during build produces \*some\* outputs for a file, and GHC --make fails handling this</td></tr>
  <tr><th>[\#12181](https://gitlab.haskell.org//ghc/ghc/issues/12181)</th>
  <td>Multi-threaded code on ARM64 GHC runtime doesn't use all available cores</td></tr>
  <tr><th>[\#12184](https://gitlab.haskell.org//ghc/ghc/issues/12184)</th>
  <td>unsafeCoerce\# causing invalid assembly generation</td></tr>
  <tr><th>[\#12187](https://gitlab.haskell.org//ghc/ghc/issues/12187)</th>
  <td>Clarify the scoping of existentials for pattern synonym signatures</td></tr>
  <tr><th>[\#12193](https://gitlab.haskell.org//ghc/ghc/issues/12193)</th>
  <td>Include target versions of unlit and hsc2hs when cross-compiling</td></tr>
  <tr><th>[\#12200](https://gitlab.haskell.org//ghc/ghc/issues/12200)</th>
  <td>ghc-pkg check complains about missing libCffi on dynamic-only install</td></tr>
  <tr><th>[\#12205](https://gitlab.haskell.org//ghc/ghc/issues/12205)</th>
  <td>Program develops space leak with -fprof-auto</td></tr>
  <tr><th>[\#12210](https://gitlab.haskell.org//ghc/ghc/issues/12210)</th>
  <td>allocateExec: can't handle large objects</td></tr>
  <tr><th>[\#12214](https://gitlab.haskell.org//ghc/ghc/issues/12214)</th>
  <td>In event of framework failure, test suite still deletes temporary directory</td></tr>
  <tr><th>[\#12221](https://gitlab.haskell.org//ghc/ghc/issues/12221)</th>
  <td>GHC's signal handlers break C-c C-c force terminate</td></tr>
  <tr><th>[\#12225](https://gitlab.haskell.org//ghc/ghc/issues/12225)</th>
  <td>Warn if test setting has no effect (e.g. compile_timeout_multiplier on run_command)</td></tr>
  <tr><th>[\#12226](https://gitlab.haskell.org//ghc/ghc/issues/12226)</th>
  <td>C-c test suite does not force kill hung GHC processes</td></tr>
  <tr><th>[\#12231](https://gitlab.haskell.org//ghc/ghc/issues/12231)</th>
  <td>Eliminate redundant heap allocations/deallocations</td></tr>
  <tr><th>[\#12232](https://gitlab.haskell.org//ghc/ghc/issues/12232)</th>
  <td>Opportunity to do better in register allocations</td></tr>
  <tr><th>[\#12236](https://gitlab.haskell.org//ghc/ghc/issues/12236)</th>
  <td>Windows profiling: T11627b segfaults for WAY=prof_hc_hb</td></tr>
  <tr><th>[\#12249](https://gitlab.haskell.org//ghc/ghc/issues/12249)</th>
  <td>Template Haskell top level scoping error</td></tr>
  <tr><th>[\#12274](https://gitlab.haskell.org//ghc/ghc/issues/12274)</th>
  <td>GHC panic: simplifier ticks exhausted</td></tr>
  <tr><th>[\#12347](https://gitlab.haskell.org//ghc/ghc/issues/12347)</th>
  <td>Parallel make should eagerly report when compilation of a module starts</td></tr>
  <tr><th>[\#12377](https://gitlab.haskell.org//ghc/ghc/issues/12377)</th>
  <td>getExecutablePath doesn't return absolute path on OpenBSD (and maybe other OS also)</td></tr>
  <tr><th>[\#12383](https://gitlab.haskell.org//ghc/ghc/issues/12383)</th>
  <td>ghc: internal error: TSO object entered</td></tr>
  <tr><th>[\#12388](https://gitlab.haskell.org//ghc/ghc/issues/12388)</th>
  <td>Don't barf on failures in the RTS linker</td></tr>
  <tr><th>[\#12391](https://gitlab.haskell.org//ghc/ghc/issues/12391)</th>
  <td>LANGUAGE CPP messes up parsing when backslash like \\\\ is at end of line (eol)</td></tr>
  <tr><th>[\#12394](https://gitlab.haskell.org//ghc/ghc/issues/12394)</th>
  <td>broken (obsolete?) links to user guide</td></tr>
  <tr><th>[\#12395](https://gitlab.haskell.org//ghc/ghc/issues/12395)</th>
  <td>Misleading GHCi errors when package is installed</td></tr>
  <tr><th>[\#12410](https://gitlab.haskell.org//ghc/ghc/issues/12410)</th>
  <td>Somehow detect splicing in ghci</td></tr>
  <tr><th>[\#12412](https://gitlab.haskell.org//ghc/ghc/issues/12412)</th>
  <td>SIMD things introduce a metric ton of known key things</td></tr>
  <tr><th>[\#12416](https://gitlab.haskell.org//ghc/ghc/issues/12416)</th>
  <td>Some GCC versions warn about failed inlines</td></tr>
  <tr><th>[\#12421](https://gitlab.haskell.org//ghc/ghc/issues/12421)</th>
  <td>TestEquality and TestCoercion documentation is confusing</td></tr>
  <tr><th>[\#12434](https://gitlab.haskell.org//ghc/ghc/issues/12434)</th>
  <td>Test suite should not copy in un-versioned files</td></tr>
  <tr><th>[\#12436](https://gitlab.haskell.org//ghc/ghc/issues/12436)</th>
  <td>Too many nested forkProcess's eventually cause SIGSEGV in the child</td></tr>
  <tr><th>[\#12437](https://gitlab.haskell.org//ghc/ghc/issues/12437)</th>
  <td>20% regression in max_bytes_used for T1969</td></tr>
  <tr><th>[\#12440](https://gitlab.haskell.org//ghc/ghc/issues/12440)</th>
  <td>Strictness of span and break does not match documentation</td></tr>
  <tr><th>[\#12446](https://gitlab.haskell.org//ghc/ghc/issues/12446)</th>
  <td>Doesn't suggest TypeApplications when \`\~\` used prefix</td></tr>
  <tr><th>[\#12449](https://gitlab.haskell.org//ghc/ghc/issues/12449)</th>
  <td>Broken types in identifiers bound by :print</td></tr>
  <tr><th>[\#12451](https://gitlab.haskell.org//ghc/ghc/issues/12451)</th>
  <td>TemplateHaskell and Data.Typeable - tcIfaceGlobal (local): not found</td></tr>
  <tr><th>[\#12452](https://gitlab.haskell.org//ghc/ghc/issues/12452)</th>
  <td>TemplateHaskell - variables in top level splices and loading modules.</td></tr>
  <tr><th>[\#12454](https://gitlab.haskell.org//ghc/ghc/issues/12454)</th>
  <td>Cross-module specialisation of recursive functions</td></tr>
  <tr><th>[\#12462](https://gitlab.haskell.org//ghc/ghc/issues/12462)</th>
  <td>Cannot add directories with colon to include path</td></tr>
  <tr><th>[\#12471](https://gitlab.haskell.org//ghc/ghc/issues/12471)</th>
  <td>Weirdness when using fromIntegral in quosiquoter</td></tr>
  <tr><th>[\#12475](https://gitlab.haskell.org//ghc/ghc/issues/12475)</th>
  <td>GHCi no longer handles stdin being closed gracefully</td></tr>
  <tr><th>[\#12482](https://gitlab.haskell.org//ghc/ghc/issues/12482)</th>
  <td>Infinite compilation time when using wrongly ordered constraints</td></tr>
  <tr><th>[\#12488](https://gitlab.haskell.org//ghc/ghc/issues/12488)</th>
  <td>Explicit namespaces doesn't enforce namespaces</td></tr>
  <tr><th>[\#12506](https://gitlab.haskell.org//ghc/ghc/issues/12506)</th>
  <td>Compile time regression in GHC 8.</td></tr>
  <tr><th>[\#12516](https://gitlab.haskell.org//ghc/ghc/issues/12516)</th>
  <td>Preprocessing: no way to portably use stringize and string concatenation</td></tr>
  <tr><th>[\#12517](https://gitlab.haskell.org//ghc/ghc/issues/12517)</th>
  <td>Simplify runghc command line options</td></tr>
  <tr><th>[\#12527](https://gitlab.haskell.org//ghc/ghc/issues/12527)</th>
  <td>GHC segfault while linking llvm-general while compiling a file using Template Haskell</td></tr>
  <tr><th>[\#12535](https://gitlab.haskell.org//ghc/ghc/issues/12535)</th>
  <td>Bad error message when unidirectional pattern synonym used in bidirectional pattern synonym</td></tr>
  <tr><th>[\#12540](https://gitlab.haskell.org//ghc/ghc/issues/12540)</th>
  <td>RFC: Allow not quantifying every top-level quantifiee</td></tr>
  <tr><th>[\#12542](https://gitlab.haskell.org//ghc/ghc/issues/12542)</th>
  <td>Unexpected failure.. (bug?)</td></tr>
  <tr><th>[\#12560](https://gitlab.haskell.org//ghc/ghc/issues/12560)</th>
  <td>‘:info TYPE’ mentions any instance that includes ‘Type’</td></tr>
  <tr><th>[\#12561](https://gitlab.haskell.org//ghc/ghc/issues/12561)</th>
  <td>Scope extrusion in Template Haskell</td></tr>
  <tr><th>[\#12564](https://gitlab.haskell.org//ghc/ghc/issues/12564)</th>
  <td>Type family in type pattern kind</td></tr>
  <tr><th>[\#12566](https://gitlab.haskell.org//ghc/ghc/issues/12566)</th>
  <td>Memory leak</td></tr>
  <tr><th>[\#12569](https://gitlab.haskell.org//ghc/ghc/issues/12569)</th>
  <td>TypeApplications allows instantiation of implicitly-quantified kind variables</td></tr>
  <tr><th>[\#12570](https://gitlab.haskell.org//ghc/ghc/issues/12570)</th>
  <td>Different behaviour in Linux and Mac OS when using some locale environments</td></tr>
  <tr><th>[\#12576](https://gitlab.haskell.org//ghc/ghc/issues/12576)</th>
  <td>Large Address space is not supported on Windows</td></tr>
  <tr><th>[\#12577](https://gitlab.haskell.org//ghc/ghc/issues/12577)</th>
  <td>The flag -xb has no effect on Windows</td></tr>
  <tr><th>[\#12581](https://gitlab.haskell.org//ghc/ghc/issues/12581)</th>
  <td>Testsuite segfaults on OS X</td></tr>
  <tr><th>[\#12596](https://gitlab.haskell.org//ghc/ghc/issues/12596)</th>
  <td>can't find interface-file declaration</td></tr>
  <tr><th>[\#12598](https://gitlab.haskell.org//ghc/ghc/issues/12598)</th>
  <td>configure script: --enable-unregisterised default printed incorrectly</td></tr>
  <tr><th>[\#12607](https://gitlab.haskell.org//ghc/ghc/issues/12607)</th>
  <td>Memory effects in doomed STM transactions</td></tr>
  <tr><th>[\#12612](https://gitlab.haskell.org//ghc/ghc/issues/12612)</th>
  <td>Allow kinds of associated types to depend on earlier associated types</td></tr>
  <tr><th>[\#12629](https://gitlab.haskell.org//ghc/ghc/issues/12629)</th>
  <td>Worse performance with -O1 or -O2 due to GC cost</td></tr>
  <tr><th>[\#12631](https://gitlab.haskell.org//ghc/ghc/issues/12631)</th>
  <td>\`hpc report\` silently ignore non-existent modules</td></tr>
  <tr><th>[\#12632](https://gitlab.haskell.org//ghc/ghc/issues/12632)</th>
  <td>Inline and Noinline pragmas ignored for instance functions</td></tr>
  <tr><th>[\#12636](https://gitlab.haskell.org//ghc/ghc/issues/12636)</th>
  <td>ProfHeap's printf modifiers are incorrect</td></tr>
  <tr><th>[\#12640](https://gitlab.haskell.org//ghc/ghc/issues/12640)</th>
  <td>Class member functions not substituted for MultiParamTypeClasses</td></tr>
  <tr><th>[\#12642](https://gitlab.haskell.org//ghc/ghc/issues/12642)</th>
  <td>reports incorrect target arch on mips64el</td></tr>
  <tr><th>[\#12643](https://gitlab.haskell.org//ghc/ghc/issues/12643)</th>
  <td>class declaration works in ghci, but not in a file</td></tr>
  <tr><th>[\#12645](https://gitlab.haskell.org//ghc/ghc/issues/12645)</th>
  <td>7.10.3 porting feedback</td></tr>
  <tr><th>[\#12652](https://gitlab.haskell.org//ghc/ghc/issues/12652)</th>
  <td>Type checker no longer accepting code using function composition and rank-n types</td></tr>
  <tr><th>[\#12656](https://gitlab.haskell.org//ghc/ghc/issues/12656)</th>
  <td>ghc floats out constant despite -fno-cse</td></tr>
  <tr><th>[\#12657](https://gitlab.haskell.org//ghc/ghc/issues/12657)</th>
  <td>GHC and GHCi: RWX mmap denied by GrSec, results in a segfault</td></tr>
  <tr><th>[\#12659](https://gitlab.haskell.org//ghc/ghc/issues/12659)</th>
  <td>Unactionable core lint warning due to floating out</td></tr>
  <tr><th>[\#12671](https://gitlab.haskell.org//ghc/ghc/issues/12671)</th>
  <td>enumFrom error thwarts checkOldIface's exception handling</td></tr>
  <tr><th>[\#12675](https://gitlab.haskell.org//ghc/ghc/issues/12675)</th>
  <td>Simplifier ticks exhausted</td></tr>
  <tr><th>[\#12678](https://gitlab.haskell.org//ghc/ghc/issues/12678)</th>
  <td>-threaded is listed as a dynamic flag but is silently ignored in OPTIONS_GHC</td></tr>
  <tr><th>[\#12684](https://gitlab.haskell.org//ghc/ghc/issues/12684)</th>
  <td>GHC panic due to bindnow linker flag</td></tr>
  <tr><th>[\#12685](https://gitlab.haskell.org//ghc/ghc/issues/12685)</th>
  <td>getNumProcessors semantics dont match documentation</td></tr>
  <tr><th>[\#12689](https://gitlab.haskell.org//ghc/ghc/issues/12689)</th>
  <td>DataCon wrappers get in the way of rules</td></tr>
  <tr><th>[\#12694](https://gitlab.haskell.org//ghc/ghc/issues/12694)</th>
  <td>GHC HEAD no longer reports inaccessible code</td></tr>
  <tr><th>[\#12696](https://gitlab.haskell.org//ghc/ghc/issues/12696)</th>
  <td>Exception gives not enough information to be useful</td></tr>
  <tr><th>[\#12700](https://gitlab.haskell.org//ghc/ghc/issues/12700)</th>
  <td>Don't warn about redundant constraints for type equalities</td></tr>
  <tr><th>[\#12704](https://gitlab.haskell.org//ghc/ghc/issues/12704)</th>
  <td>Check if constraint synonym satisfies functional dependencies</td></tr>
  <tr><th>[\#12705](https://gitlab.haskell.org//ghc/ghc/issues/12705)</th>
  <td>Renamer should reject signatures that reexport only part of a declaration</td></tr>
  <tr><th>[\#12706](https://gitlab.haskell.org//ghc/ghc/issues/12706)</th>
  <td>Collecting type info is slow</td></tr>
  <tr><th>[\#12714](https://gitlab.haskell.org//ghc/ghc/issues/12714)</th>
  <td>T9405 fails on Windows</td></tr>
  <tr><th>[\#12723](https://gitlab.haskell.org//ghc/ghc/issues/12723)</th>
  <td>Family instance modules are not fingerprinted in ABI</td></tr>
  <tr><th>[\#12724](https://gitlab.haskell.org//ghc/ghc/issues/12724)</th>
  <td>Be lazier about reducing type-function applications</td></tr>
  <tr><th>[\#12731](https://gitlab.haskell.org//ghc/ghc/issues/12731)</th>
  <td>Generic type class has type family; leads to big dep_finsts</td></tr>
  <tr><th>[\#12734](https://gitlab.haskell.org//ghc/ghc/issues/12734)</th>
  <td>Missed use of solved dictionaries leads to context stack overflow</td></tr>
  <tr><th>[\#12737](https://gitlab.haskell.org//ghc/ghc/issues/12737)</th>
  <td>T12227 is failing on ghc-8.0</td></tr>
  <tr><th>[\#12739](https://gitlab.haskell.org//ghc/ghc/issues/12739)</th>
  <td>Failure installing elm-init-1.0.5 (ExitFailure (-6))</td></tr>
  <tr><th>[\#12740](https://gitlab.haskell.org//ghc/ghc/issues/12740)</th>
  <td>generic Linux installer contains dynamically linked helpers failing to run on non glibc systems</td></tr>
  <tr><th>[\#12743](https://gitlab.haskell.org//ghc/ghc/issues/12743)</th>
  <td>Profiling wrongly attributes allocations to a function with Int\# result</td></tr>
  <tr><th>[\#12750](https://gitlab.haskell.org//ghc/ghc/issues/12750)</th>
  <td>hGetContents leads to late/silent failures</td></tr>
  <tr><th>[\#12751](https://gitlab.haskell.org//ghc/ghc/issues/12751)</th>
  <td>T5611 fails non-deterministically on OSX</td></tr>
  <tr><th>[\#12753](https://gitlab.haskell.org//ghc/ghc/issues/12753)</th>
  <td>GHCi/Template Haskell: Don't panic on linker errors.</td></tr>
  <tr><th>[\#12756](https://gitlab.haskell.org//ghc/ghc/issues/12756)</th>
  <td>ghci gives stg_ap_v_ret error</td></tr>
  <tr><th>[\#12760](https://gitlab.haskell.org//ghc/ghc/issues/12760)</th>
  <td>Assertion failed with BuildFlavour = devel2 (yet another)</td></tr>
  <tr><th>[\#12761](https://gitlab.haskell.org//ghc/ghc/issues/12761)</th>
  <td>Type aliases for TypeError constrains fire during compilation time</td></tr>
  <tr><th>[\#12762](https://gitlab.haskell.org//ghc/ghc/issues/12762)</th>
  <td>ASSERT failures on HEAD following typechecker refactoring</td></tr>
  <tr><th>[\#12770](https://gitlab.haskell.org//ghc/ghc/issues/12770)</th>
  <td>Shrink list of RUNPATH entries for GHC libraries</td></tr>
  <tr><th>[\#12778](https://gitlab.haskell.org//ghc/ghc/issues/12778)</th>
  <td>Expose variables bound in quotations to reify</td></tr>
  <tr><th>[\#12779](https://gitlab.haskell.org//ghc/ghc/issues/12779)</th>
  <td>isTrue\# doesn't work in ghci anymore</td></tr>
  <tr><th>[\#12780](https://gitlab.haskell.org//ghc/ghc/issues/12780)</th>
  <td>Calling "do nothing" type checker plugin affects type checking when it shouldn't</td></tr>
  <tr><th>[\#12792](https://gitlab.haskell.org//ghc/ghc/issues/12792)</th>
  <td>Wrong error message when using a data type as a class instance head</td></tr>
  <tr><th>[\#12794](https://gitlab.haskell.org//ghc/ghc/issues/12794)</th>
  <td>Out of scope error not reported</td></tr>
  <tr><th>[\#12798](https://gitlab.haskell.org//ghc/ghc/issues/12798)</th>
  <td>LLVM seeming to over optimize, producing inefficient assembly code...</td></tr>
  <tr><th>[\#12808](https://gitlab.haskell.org//ghc/ghc/issues/12808)</th>
  <td>For closures, Loop Invariant Code Flow related to captured free values not lifted outside the loop...</td></tr>
  <tr><th>[\#12813](https://gitlab.haskell.org//ghc/ghc/issues/12813)</th>
  <td>GHC panic when installing haskell-opencv with nix</td></tr>
  <tr><th>[\#12817](https://gitlab.haskell.org//ghc/ghc/issues/12817)</th>
  <td>Degraded performance with constraint synonyms</td></tr>
  <tr><th>[\#12818](https://gitlab.haskell.org//ghc/ghc/issues/12818)</th>
  <td>Allow reify to find top-level bindings in later declaration groups</td></tr>
  <tr><th>[\#12820](https://gitlab.haskell.org//ghc/ghc/issues/12820)</th>
  <td>Regression around pattern synonyms and higher-rank types</td></tr>
  <tr><th>[\#12829](https://gitlab.haskell.org//ghc/ghc/issues/12829)</th>
  <td>Multiline input (‘:set +m’) terminated by trailing whitespace</td></tr>
  <tr><th>[\#12831](https://gitlab.haskell.org//ghc/ghc/issues/12831)</th>
  <td>Fulltext search SQL error in Trac</td></tr>
  <tr><th>[\#12832](https://gitlab.haskell.org//ghc/ghc/issues/12832)</th>
  <td>GHC infers too simplified contexts</td></tr>
  <tr><th>[\#12841](https://gitlab.haskell.org//ghc/ghc/issues/12841)</th>
  <td>Remove or explain target triple normalization</td></tr>
  <tr><th>[\#12849](https://gitlab.haskell.org//ghc/ghc/issues/12849)</th>
  <td>hsc2hs trouble with floating-point constants in cross-compilation mode</td></tr>
  <tr><th>[\#12852](https://gitlab.haskell.org//ghc/ghc/issues/12852)</th>
  <td>threadWaitReadSTM does not provide a way to unregister action.</td></tr>
  <tr><th>[\#12854](https://gitlab.haskell.org//ghc/ghc/issues/12854)</th>
  <td>ghc-8 prints mangled names in error message: ‘GHC.Base.$dm\<$’</td></tr>
  <tr><th>[\#12859](https://gitlab.haskell.org//ghc/ghc/issues/12859)</th>
  <td>ghc/packages/Cabal is not a valid repository name</td></tr>
  <tr><th>[\#12860](https://gitlab.haskell.org//ghc/ghc/issues/12860)</th>
  <td>GeneralizedNewtypeDeriving + MultiParamTypeClasses sends typechecker into an infinite loop</td></tr>
  <tr><th>[\#12861](https://gitlab.haskell.org//ghc/ghc/issues/12861)</th>
  <td>"ghc-pkg-6.9"</td></tr>
  <tr><th>[\#12862](https://gitlab.haskell.org//ghc/ghc/issues/12862)</th>
  <td>Operator (!) causes weird pretty printing and parsing</td></tr>
  <tr><th>[\#12869](https://gitlab.haskell.org//ghc/ghc/issues/12869)</th>
  <td>getChar doesn't work on a new Windows build</td></tr>
  <tr><th>[\#12873](https://gitlab.haskell.org//ghc/ghc/issues/12873)</th>
  <td>hWaitForInput with socket as handle excepts on windows</td></tr>
  <tr><th>[\#12875](https://gitlab.haskell.org//ghc/ghc/issues/12875)</th>
  <td>GHC fails to link all StaticPointers-defining modules of a library in an executable</td></tr>
  <tr><th>[\#12884](https://gitlab.haskell.org//ghc/ghc/issues/12884)</th>
  <td>Parsing of literate files fails because of special character (\#)</td></tr>
  <tr><th>[\#12890](https://gitlab.haskell.org//ghc/ghc/issues/12890)</th>
  <td>Stdcall - treating as CCall (bogus warning on win 64 bit)</td></tr>
  <tr><th>[\#12893](https://gitlab.haskell.org//ghc/ghc/issues/12893)</th>
  <td>Profiling defeats stream fusion when using vector library</td></tr>
  <tr><th>[\#12908](https://gitlab.haskell.org//ghc/ghc/issues/12908)</th>
  <td>Tuple constraints refactoring seems to regress performance considerably</td></tr>
  <tr><th>[\#12915](https://gitlab.haskell.org//ghc/ghc/issues/12915)</th>
  <td>cmmImplementSwitchPlans creates duplicate blocks</td></tr>
  <tr><th>[\#12917](https://gitlab.haskell.org//ghc/ghc/issues/12917)</th>
  <td>Location info for error message with multiple source locations</td></tr>
  <tr><th>[\#12920](https://gitlab.haskell.org//ghc/ghc/issues/12920)</th>
  <td>Overzealous unused-top-binds</td></tr>
  <tr><th>[\#12926](https://gitlab.haskell.org//ghc/ghc/issues/12926)</th>
  <td>ghc master (8.1.20161202) panics with assertion failure with devel2 flavor and -O2</td></tr>
  <tr><th>[\#12929](https://gitlab.haskell.org//ghc/ghc/issues/12929)</th>
  <td>GHC 8.0.1 Armv7 Missing -N option</td></tr>
  <tr><th>[\#12932](https://gitlab.haskell.org//ghc/ghc/issues/12932)</th>
  <td>-fexternal-interpreter\` fails to find symbols</td></tr>
  <tr><th>[\#12934](https://gitlab.haskell.org//ghc/ghc/issues/12934)</th>
  <td>Testsuite driver buffering behavior has changed with Python 3</td></tr>
  <tr><th>[\#12935](https://gitlab.haskell.org//ghc/ghc/issues/12935)</th>
  <td>Object code produced by GHC is non-deterministic</td></tr>
  <tr><th>[\#12937](https://gitlab.haskell.org//ghc/ghc/issues/12937)</th>
  <td>String merging broken on Darwin</td></tr>
  <tr><th>[\#12939](https://gitlab.haskell.org//ghc/ghc/issues/12939)</th>
  <td>ghc-8.0.1.20161117 did not install ghc.1 manpage</td></tr>
  <tr><th>[\#12940](https://gitlab.haskell.org//ghc/ghc/issues/12940)</th>
  <td>ghc-8.0.2 RC1 libs installed under package dirs vs Cabal installing packages under abi dir</td></tr>
  <tr><th>[\#12949](https://gitlab.haskell.org//ghc/ghc/issues/12949)</th>
  <td>Pattern coverage checker ignores dictionary arguments</td></tr>
  <tr><th>[\#12952](https://gitlab.haskell.org//ghc/ghc/issues/12952)</th>
  <td>Broken tests: ghci014 maessen_hashtab qq006</td></tr>
  <tr><th>[\#12965](https://gitlab.haskell.org//ghc/ghc/issues/12965)</th>
  <td>String merging broken on Windows</td></tr>
  <tr><th>[\#12971](https://gitlab.haskell.org//ghc/ghc/issues/12971)</th>
  <td>Paths are encoded incorrectly when invoking GCC</td></tr>
  <tr><th>[\#12972](https://gitlab.haskell.org//ghc/ghc/issues/12972)</th>
  <td>Missed specialisation opportunity with phantom type class parameter?</td></tr>
  <tr><th>[\#12975](https://gitlab.haskell.org//ghc/ghc/issues/12975)</th>
  <td>Suggested type signature for a pattern synonym causes program to fail to type check</td></tr>
  <tr><th>[\#12980](https://gitlab.haskell.org//ghc/ghc/issues/12980)</th>
  <td>Unlifted class method rejected</td></tr>
  <tr><th>[\#12983](https://gitlab.haskell.org//ghc/ghc/issues/12983)</th>
  <td>Loading temp shared object failed: TemplateHaskell and recompilation</td></tr>
  <tr><th>[\#12989](https://gitlab.haskell.org//ghc/ghc/issues/12989)</th>
  <td>($) can have a more general type</td></tr>
  <tr><th>[\#12991](https://gitlab.haskell.org//ghc/ghc/issues/12991)</th>
  <td>panic when using IrredPreds in a type checker plugin.</td></tr>
  <tr><th>[\#13000](https://gitlab.haskell.org//ghc/ghc/issues/13000)</th>
  <td>minor doc bug about list fusion in GHC user's guide</td></tr>
  <tr><th>[\#13002](https://gitlab.haskell.org//ghc/ghc/issues/13002)</th>
  <td>:set -O does not work in .ghci file</td></tr>
  <tr><th>[\#13003](https://gitlab.haskell.org//ghc/ghc/issues/13003)</th>
  <td>improve documentation for typed TH and make it more discoverable</td></tr>
  <tr><th>[\#13010](https://gitlab.haskell.org//ghc/ghc/issues/13010)</th>
  <td>module imports form a cycle instead of failing to parse</td></tr>
  <tr><th>[\#13011](https://gitlab.haskell.org//ghc/ghc/issues/13011)</th>
  <td>Simplifier ticks exhausted: a 10-line case</td></tr>
  <tr><th>[\#13012](https://gitlab.haskell.org//ghc/ghc/issues/13012)</th>
  <td>ApiAnnotations comments are not machine checked</td></tr>
  <tr><th>[\#13014](https://gitlab.haskell.org//ghc/ghc/issues/13014)</th>
  <td>Seemingly unnecessary marking of a SpecConstr specialization as a loopbreaker</td></tr>
  <tr><th>[\#13016](https://gitlab.haskell.org//ghc/ghc/issues/13016)</th>
  <td>SPECIALIZE INLINE doesn't necessarily inline specializations of a recursive function</td></tr>
  <tr><th>[\#13017](https://gitlab.haskell.org//ghc/ghc/issues/13017)</th>
  <td>GHC panics during build of etkmett/free</td></tr>
  <tr><th>[\#13021](https://gitlab.haskell.org//ghc/ghc/issues/13021)</th>
  <td>Inaccessible RHS warning is confusing for users</td></tr>
  <tr><th>[\#13024](https://gitlab.haskell.org//ghc/ghc/issues/13024)</th>
  <td>T12877 intermittently fails</td></tr>
  <tr><th>[\#13030](https://gitlab.haskell.org//ghc/ghc/issues/13030)</th>
  <td>Build error from following default nixos instructions and -Werror</td></tr>
  <tr><th>[\#13031](https://gitlab.haskell.org//ghc/ghc/issues/13031)</th>
  <td>Bogus calculation of bottoming arity</td></tr>
  <tr><th>[\#13045](https://gitlab.haskell.org//ghc/ghc/issues/13045)</th>
  <td>LLVM code generation causes segfaults on FreeBSD</td></tr>
  <tr><th>[\#13046](https://gitlab.haskell.org//ghc/ghc/issues/13046)</th>
  <td>TH Names can't be used in patterns</td></tr>
  <tr><th>[\#13047](https://gitlab.haskell.org//ghc/ghc/issues/13047)</th>
  <td>Can create bindings of kind Constraint without ConstraintKind, only TypeFamilies</td></tr>
  <tr><th>[\#13048](https://gitlab.haskell.org//ghc/ghc/issues/13048)</th>
  <td>Splitter is O(n\^2)</td></tr>
  <tr><th>[\#13052](https://gitlab.haskell.org//ghc/ghc/issues/13052)</th>
  <td>unsafePerformIO duped on multithread if within the same IO thunk</td></tr>
  <tr><th>[\#13053](https://gitlab.haskell.org//ghc/ghc/issues/13053)</th>
  <td>Inferred type for hole is not general enough</td></tr>
  <tr><th>[\#13054](https://gitlab.haskell.org//ghc/ghc/issues/13054)</th>
  <td>Generating unique names with template haskell</td></tr>
  <tr><th>[\#13061](https://gitlab.haskell.org//ghc/ghc/issues/13061)</th>
  <td>Incorrect constraints given single flexible undecidable instance.</td></tr>
  <tr><th>[\#13063](https://gitlab.haskell.org//ghc/ghc/issues/13063)</th>
  <td>Program uses 8GB of memory</td></tr>
  <tr><th>[\#13069](https://gitlab.haskell.org//ghc/ghc/issues/13069)</th>
  <td>hs-boot files permit default methods in type class (but don't typecheck them)</td></tr>
  <tr><th>[\#13078](https://gitlab.haskell.org//ghc/ghc/issues/13078)</th>
  <td>Panic from ghc-stage1 when building HEAD with profiling</td></tr>
  <tr><th>[\#13080](https://gitlab.haskell.org//ghc/ghc/issues/13080)</th>
  <td>Memory leak caused by nested monadic loops</td></tr>
  <tr><th>[\#13084](https://gitlab.haskell.org//ghc/ghc/issues/13084)</th>
  <td>'foreign import prim' are marked PlaySafe by the parser</td></tr>
  <tr><th>[\#13085](https://gitlab.haskell.org//ghc/ghc/issues/13085)</th>
  <td>-fregs-graph is ignored but this is undocumented</td></tr>
  <tr><th>[\#13086](https://gitlab.haskell.org//ghc/ghc/issues/13086)</th>
  <td>(\\\\) in Data.List uses foldl instead of foldl'</td></tr>
  <tr><th>[\#13090](https://gitlab.haskell.org//ghc/ghc/issues/13090)</th>
  <td>Expose all unfoldings of overloaded functions by default</td></tr>
  <tr><th>[\#13091](https://gitlab.haskell.org//ghc/ghc/issues/13091)</th>
  <td>Build broken on amd64 solaris 11</td></tr>
  <tr><th>[\#13092](https://gitlab.haskell.org//ghc/ghc/issues/13092)</th>
  <td>family instance consistency checks are too pessimistic</td></tr>
  <tr><th>[\#13093](https://gitlab.haskell.org//ghc/ghc/issues/13093)</th>
  <td>Runtime linker chokes on object files created by MSVC++</td></tr>
  <tr><th>[\#13094](https://gitlab.haskell.org//ghc/ghc/issues/13094)</th>
  <td>Poor register allocation and redundant moves when using \`foreign import prim\`</td></tr>
  <tr><th>[\#13099](https://gitlab.haskell.org//ghc/ghc/issues/13099)</th>
  <td>recompilation can fail to recheck type family instance consistency</td></tr>
  <tr><th>[\#13102](https://gitlab.haskell.org//ghc/ghc/issues/13102)</th>
  <td>orphan family instances can leak through the EPS in --make mode</td></tr>
  <tr><th>[\#13104](https://gitlab.haskell.org//ghc/ghc/issues/13104)</th>
  <td>runRW\# ruins join points</td></tr>
  <tr><th>[\#13105](https://gitlab.haskell.org//ghc/ghc/issues/13105)</th>
  <td>Allow type families in RuntimeReps</td></tr>
  <tr><th>[\#13110](https://gitlab.haskell.org//ghc/ghc/issues/13110)</th>
  <td>GHC API allocates memory which is never GC'd</td></tr>
  <tr><th>[\#13112](https://gitlab.haskell.org//ghc/ghc/issues/13112)</th>
  <td>Windows 64-bit GHC HEAD segfaults on the code with a lot of TH stuff.</td></tr>
  <tr><th>[\#13139](https://gitlab.haskell.org//ghc/ghc/issues/13139)</th>
  <td>Documents not updating correctly?</td></tr>
  <tr><th>[\#13142](https://gitlab.haskell.org//ghc/ghc/issues/13142)</th>
  <td>Substitution invariant failure arising from OptCoercion</td></tr>
  <tr><th>[\#13145](https://gitlab.haskell.org//ghc/ghc/issues/13145)</th>
  <td>Documentation shouldn't call things functions that aren't functions</td></tr>
  <tr><th>[\#13146](https://gitlab.haskell.org//ghc/ghc/issues/13146)</th>
  <td>Doc for RealWorld refers to non-existent "ptrArg"</td></tr>
  <tr><th>[\#13148](https://gitlab.haskell.org//ghc/ghc/issues/13148)</th>
  <td>Adding weak pointers to non-mutable unboxed values segfaults</td></tr>
  <tr><th>[\#13150](https://gitlab.haskell.org//ghc/ghc/issues/13150)</th>
  <td>unknown warning is not reported by GHC</td></tr>
  <tr><th>[\#13153](https://gitlab.haskell.org//ghc/ghc/issues/13153)</th>
  <td>Several Traversable instances have an extra fmap</td></tr>
  <tr><th>[\#13154](https://gitlab.haskell.org//ghc/ghc/issues/13154)</th>
  <td>Standalone-derived anyclass instances aren't as permissive as empty instances</td></tr>
  <tr><th>[\#13157](https://gitlab.haskell.org//ghc/ghc/issues/13157)</th>
  <td>Opportunity to improve case-of-case</td></tr>
  <tr><th>[\#13165](https://gitlab.haskell.org//ghc/ghc/issues/13165)</th>
  <td>Speed up the RTS hash table</td></tr>
  <tr><th>[\#13169](https://gitlab.haskell.org//ghc/ghc/issues/13169)</th>
  <td>Documentation for CoreMonad.getAnnotations</td></tr>
  <tr><th>[\#13180](https://gitlab.haskell.org//ghc/ghc/issues/13180)</th>
  <td>Confusing error when hs-boot abstract data implemented using synonym</td></tr>
  <tr><th>[\#13192](https://gitlab.haskell.org//ghc/ghc/issues/13192)</th>
  <td>Ambiguity Caused By PolyKind and Not Helpful Error Messages</td></tr>
  <tr><th>[\#13193](https://gitlab.haskell.org//ghc/ghc/issues/13193)</th>
  <td>Integer (gmp) performance regression?</td></tr>
  <tr><th>[\#13194](https://gitlab.haskell.org//ghc/ghc/issues/13194)</th>
  <td>Concurrent modifications of package.cache are not safe</td></tr>
  <tr><th>[\#13195](https://gitlab.haskell.org//ghc/ghc/issues/13195)</th>
  <td>Ticks panic</td></tr>
  <tr><th>[\#13200](https://gitlab.haskell.org//ghc/ghc/issues/13200)</th>
  <td>Old links to snapshot releases in GHC user's guide</td></tr>
  <tr><th>[\#13201](https://gitlab.haskell.org//ghc/ghc/issues/13201)</th>
  <td>Type-level naturals aren't instantiated with GHCi debugger</td></tr>
  <tr><th>[\#13207](https://gitlab.haskell.org//ghc/ghc/issues/13207)</th>
  <td>Performance regressions from removing LNE analysis</td></tr>
  <tr><th>[\#13208](https://gitlab.haskell.org//ghc/ghc/issues/13208)</th>
  <td>Do two-phase inlining in simpleOptPgm</td></tr>
  <tr><th>[\#13209](https://gitlab.haskell.org//ghc/ghc/issues/13209)</th>
  <td>ghc panic with optimization.</td></tr>
  <tr><th>[\#13210](https://gitlab.haskell.org//ghc/ghc/issues/13210)</th>
  <td>Can't run terminfo code in GHCi on Windows</td></tr>
  <tr><th>[\#13216](https://gitlab.haskell.org//ghc/ghc/issues/13216)</th>
  <td>internal error: stg_ap_pppppp_ret</td></tr>
  <tr><th>[\#13219](https://gitlab.haskell.org//ghc/ghc/issues/13219)</th>
  <td>CSE for join points</td></tr>
  <tr><th>[\#13223](https://gitlab.haskell.org//ghc/ghc/issues/13223)</th>
  <td>Core Prep sometimes generates case of type lambda</td></tr>
  <tr><th>[\#13224](https://gitlab.haskell.org//ghc/ghc/issues/13224)</th>
  <td>Rules and join points</td></tr>
  <tr><th>[\#13225](https://gitlab.haskell.org//ghc/ghc/issues/13225)</th>
  <td>Fannkuch-redux time regression from join point patch</td></tr>
  <tr><th>[\#13226](https://gitlab.haskell.org//ghc/ghc/issues/13226)</th>
  <td>Compiler allocation regressions from top-level string literal patch</td></tr>
  <tr><th>[\#13233](https://gitlab.haskell.org//ghc/ghc/issues/13233)</th>
  <td>typePrimRep panic while compiling GHC with profiling</td></tr>
  <tr><th>[\#13235](https://gitlab.haskell.org//ghc/ghc/issues/13235)</th>
  <td>(makeVersion \[4, 9, 0, 0\] \<= makeVersion \[4, 9\]) == False</td></tr>
  <tr><th>[\#13236](https://gitlab.haskell.org//ghc/ghc/issues/13236)</th>
  <td>Improve floating for join points</td></tr>
  <tr><th>[\#13243](https://gitlab.haskell.org//ghc/ghc/issues/13243)</th>
  <td>make test in non-validate configuration fails with a variety of ghci errors</td></tr>
  <tr><th>[\#13247](https://gitlab.haskell.org//ghc/ghc/issues/13247)</th>
  <td>hPutBufNonBlocking can block</td></tr>
  <tr><th>[\#13251](https://gitlab.haskell.org//ghc/ghc/issues/13251)</th>
  <td>Must perform family consistency check on non-imported identifiers</td></tr>
  <tr><th>[\#13253](https://gitlab.haskell.org//ghc/ghc/issues/13253)</th>
  <td>Exponential compilation time with RWST & ReaderT stack with \`-02\`</td></tr>
  <tr><th>[\#13254](https://gitlab.haskell.org//ghc/ghc/issues/13254)</th>
  <td>Confusing error message from GHCI - "defined in multiple files" shows the same file</td></tr>
  <tr><th>[\#13266](https://gitlab.haskell.org//ghc/ghc/issues/13266)</th>
  <td>Source locations from signature merging/matching are bad</td></tr>
  <tr><th>[\#13269](https://gitlab.haskell.org//ghc/ghc/issues/13269)</th>
  <td>Changes in foreign code used in TH do not trigger recompilation</td></tr>
  <tr><th>[\#13273](https://gitlab.haskell.org//ghc/ghc/issues/13273)</th>
  <td>AttributeError: 'Environment' object has no attribute 'get_db_cnx'</td></tr>
  <tr><th>[\#13284](https://gitlab.haskell.org//ghc/ghc/issues/13284)</th>
  <td>Incoherent instance solving is over-eager</td></tr>
  <tr><th>[\#13289](https://gitlab.haskell.org//ghc/ghc/issues/13289)</th>
  <td>Terrible loss of optimisation in presence of ticks</td></tr>
  <tr><th>[\#13291](https://gitlab.haskell.org//ghc/ghc/issues/13291)</th>
  <td>bpk15 and bkp47 fail with -dunique-increment=-1</td></tr>
  <tr><th>[\#13294](https://gitlab.haskell.org//ghc/ghc/issues/13294)</th>
  <td>compactResize is a terrible primop and a terrible name</td></tr>
  <tr><th>[\#13295](https://gitlab.haskell.org//ghc/ghc/issues/13295)</th>
  <td>Failure to resolve type parameter determined by type family</td></tr>
  <tr><th>[\#13296](https://gitlab.haskell.org//ghc/ghc/issues/13296)</th>
  <td>stat() calls can block Haskell runtime</td></tr>
  <tr><th>[\#13303](https://gitlab.haskell.org//ghc/ghc/issues/13303)</th>
  <td>Bad pretty printer for let bindings in a do-notation with braces</td></tr>
  <tr><th>[\#13306](https://gitlab.haskell.org//ghc/ghc/issues/13306)</th>
  <td>Problems with type inference for static expressions</td></tr>
  <tr><th>[\#13307](https://gitlab.haskell.org//ghc/ghc/issues/13307)</th>
  <td>Record pattern synonym fields have to be manually exported</td></tr>
  <tr><th>[\#13308](https://gitlab.haskell.org//ghc/ghc/issues/13308)</th>
  <td>Treat leading whitespace in rts flag arguments in a uniform way</td></tr>
  <tr><th>[\#13316](https://gitlab.haskell.org//ghc/ghc/issues/13316)</th>
  <td>Bad inlining cascade leads to slow optimisation</td></tr>
  <tr><th>[\#13321](https://gitlab.haskell.org//ghc/ghc/issues/13321)</th>
  <td>Importing a bundled pattern "infects" all other imports of that pattern</td></tr>
  <tr><th>[\#13327](https://gitlab.haskell.org//ghc/ghc/issues/13327)</th>
  <td>Figure out how to make sense of Data instances for poly-kinded datatypes</td></tr>
  <tr><th>[\#13329](https://gitlab.haskell.org//ghc/ghc/issues/13329)</th>
  <td>Windows CMD.EXE "Quick Edit Mode"</td></tr>
  <tr><th>[\#13331](https://gitlab.haskell.org//ghc/ghc/issues/13331)</th>
  <td>Worker/wrapper can lead to sharing failure</td></tr>
  <tr><th>[\#13332](https://gitlab.haskell.org//ghc/ghc/issues/13332)</th>
  <td>Report unrecognized pragmas earlier</td></tr>
  <tr><th>[\#13339](https://gitlab.haskell.org//ghc/ghc/issues/13339)</th>
  <td>Arbitrarily large expressions built out of cheap primops are not floated out</td></tr>
  <tr><th>[\#13347](https://gitlab.haskell.org//ghc/ghc/issues/13347)</th>
  <td>Abstract classes in hs-boot should not be treated as injective</td></tr>
  <tr><th>[\#13352](https://gitlab.haskell.org//ghc/ghc/issues/13352)</th>
  <td>Strange requirement for re-exported duplicate record fields</td></tr>
  <tr><th>[\#13353](https://gitlab.haskell.org//ghc/ghc/issues/13353)</th>
  <td>foldr/nil rule not applied consistently</td></tr>
  <tr><th>[\#13355](https://gitlab.haskell.org//ghc/ghc/issues/13355)</th>
  <td>gmp doesn't build correctly when cross-compiling with clang</td></tr>
  <tr><th>[\#13356](https://gitlab.haskell.org//ghc/ghc/issues/13356)</th>
  <td>gmp/ghc.mk's use of TARGETPLATFORM and BUILDPLATFORM is wrong</td></tr>
  <tr><th>[\#13359](https://gitlab.haskell.org//ghc/ghc/issues/13359)</th>
  <td>GHCi crash in a standard Windows console (cmd.exe) after Control-c</td></tr>
  <tr><th>[\#13361](https://gitlab.haskell.org//ghc/ghc/issues/13361)</th>
  <td>Better type synonym merging/subtyping for Backpack</td></tr>
  <tr><th>[\#13363](https://gitlab.haskell.org//ghc/ghc/issues/13363)</th>
  <td>Wildcard patterns and COMPLETE sets can lead to misleading redundant pattern-match warnings</td></tr>
  <tr><th>[\#13365](https://gitlab.haskell.org//ghc/ghc/issues/13365)</th>
  <td>Notify user when adding a CUSK might help fix a type error</td></tr>
  <tr><th>[\#13370](https://gitlab.haskell.org//ghc/ghc/issues/13370)</th>
  <td>exprIsBottom inconsistent with strictness analyser</td></tr>
  <tr><th>[\#13380](https://gitlab.haskell.org//ghc/ghc/issues/13380)</th>
  <td>raiseIO\# result looks wrong</td></tr>
  <tr><th>[\#13386](https://gitlab.haskell.org//ghc/ghc/issues/13386)</th>
  <td>Poor compiler performance with type families</td></tr>
  <tr><th>[\#13389](https://gitlab.haskell.org//ghc/ghc/issues/13389)</th>
  <td>Strange behaviours of memory with GHCi under Windows and Linux</td></tr>
  <tr><th>[\#13390](https://gitlab.haskell.org//ghc/ghc/issues/13390)</th>
  <td>String literal float-out during desugaring regresses T1969 at -O0</td></tr>
  <tr><th>[\#13396](https://gitlab.haskell.org//ghc/ghc/issues/13396)</th>
  <td>thread blocked when running \`deepseq\` with infinite list</td></tr>
  <tr><th>[\#13402](https://gitlab.haskell.org//ghc/ghc/issues/13402)</th>
  <td>GHC .prof tabular output glues columns together</td></tr>
  <tr><th>[\#13406](https://gitlab.haskell.org//ghc/ghc/issues/13406)</th>
  <td>IO hack in demand analyzer can miss I/O</td></tr>
  <tr><th>[\#13421](https://gitlab.haskell.org//ghc/ghc/issues/13421)</th>
  <td>Confusing error when using BangPatterns without pragma</td></tr>
  <tr><th>[\#13423](https://gitlab.haskell.org//ghc/ghc/issues/13423)</th>
  <td>Exception to I/O hack in demand analyzer too broad</td></tr>
  <tr><th>[\#13426](https://gitlab.haskell.org//ghc/ghc/issues/13426)</th>
  <td>compile-time memory-usage regression for DynFlags between GHC 8.0 and GHC 8.2</td></tr>
  <tr><th>[\#13438](https://gitlab.haskell.org//ghc/ghc/issues/13438)</th>
  <td>ghci :browse does not work with DuplicateRecordFields</td></tr>
  <tr><th>[\#13439](https://gitlab.haskell.org//ghc/ghc/issues/13439)</th>
  <td>ForeignPtr finalizers not searched for reachable objects?</td></tr>
  <tr><th>[\#13452](https://gitlab.haskell.org//ghc/ghc/issues/13452)</th>
  <td>Lock .tix file</td></tr>
  <tr><th>[\#13455](https://gitlab.haskell.org//ghc/ghc/issues/13455)</th>
  <td>GHCi crash GHC with creation of a "crash dump file" in Windows</td></tr>
  <tr><th>[\#13456](https://gitlab.haskell.org//ghc/ghc/issues/13456)</th>
  <td>GHCi run commands that are not documented in Users Guide Documentation</td></tr>
  <tr><th>[\#13459](https://gitlab.haskell.org//ghc/ghc/issues/13459)</th>
  <td>sdist make target doesn't create required ghc-tarballs dir</td></tr>
  <tr><th>[\#13465](https://gitlab.haskell.org//ghc/ghc/issues/13465)</th>
  <td>Foldable deriving treatment of tuples is too surprising</td></tr>
  <tr><th>[\#13475](https://gitlab.haskell.org//ghc/ghc/issues/13475)</th>
  <td>Possible missing case in ghc-pkg</td></tr>
  <tr><th>[\#13476](https://gitlab.haskell.org//ghc/ghc/issues/13476)</th>
  <td>Coercion maching in RULES is fragile</td></tr>
  <tr><th>[\#13486](https://gitlab.haskell.org//ghc/ghc/issues/13486)</th>
  <td>inconsistency in handling the BOM Byte-order-mark in reading and putStrLn</td></tr>
  <tr><th>[\#13492](https://gitlab.haskell.org//ghc/ghc/issues/13492)</th>
  <td>-p option report much less time than actual on high intensity of FFI calls application</td></tr>
  <tr><th>[\#13493](https://gitlab.haskell.org//ghc/ghc/issues/13493)</th>
  <td>Recompilation avoidance and Backpack</td></tr>
  <tr><th>[\#13497](https://gitlab.haskell.org//ghc/ghc/issues/13497)</th>
  <td>GHC does not use select()/poll() correctly on non-Linux platforms</td></tr>
  <tr><th>[\#13499](https://gitlab.haskell.org//ghc/ghc/issues/13499)</th>
  <td>"Panic: no skolem info" with StaticPointers and typed hole</td></tr>
  <tr><th>[\#13501](https://gitlab.haskell.org//ghc/ghc/issues/13501)</th>
  <td>TH segmentation fault on Linux when calling function from another package</td></tr>
  <tr><th>[\#13507](https://gitlab.haskell.org//ghc/ghc/issues/13507)</th>
  <td>Changes to environment files don't apply in GHCi on :reload</td></tr>
  <tr><th>[\#13512](https://gitlab.haskell.org//ghc/ghc/issues/13512)</th>
  <td>GHC incorrectly warns that a variable used in a type application is unused</td></tr>
  <tr><th>[\#13513](https://gitlab.haskell.org//ghc/ghc/issues/13513)</th>
  <td>Incorrect behavior on arm64 with optimisations</td></tr>
  <tr><th>[\#13515](https://gitlab.haskell.org//ghc/ghc/issues/13515)</th>
  <td>Unexpected failure of T11223_simple_duplicate_lib on 32-bit Windows</td></tr>
  <tr><th>[\#13516](https://gitlab.haskell.org//ghc/ghc/issues/13516)</th>
  <td>broken pipe when interrupting on Windows</td></tr>
  <tr><th>[\#13519](https://gitlab.haskell.org//ghc/ghc/issues/13519)</th>
  <td>hWaitForInput does not work on linux.</td></tr>
  <tr><th>[\#13522](https://gitlab.haskell.org//ghc/ghc/issues/13522)</th>
  <td>unhandled ELF relocation(RelA) type 19</td></tr>
  <tr><th>[\#13535](https://gitlab.haskell.org//ghc/ghc/issues/13535)</th>
  <td>vector test suite uses excessive memory on GHC 8.2</td></tr>
  <tr><th>[\#13539](https://gitlab.haskell.org//ghc/ghc/issues/13539)</th>
  <td>Options -pgmlclang and -pgmlc overlap</td></tr>
  <tr><th>[\#13542](https://gitlab.haskell.org//ghc/ghc/issues/13542)</th>
  <td>Solaris build fails with collect2: execv: Arg list too long</td></tr>
  <tr><th>[\#13547](https://gitlab.haskell.org//ghc/ghc/issues/13547)</th>
  <td>Lint error in arrows program</td></tr>
  <tr><th>[\#13552](https://gitlab.haskell.org//ghc/ghc/issues/13552)</th>
  <td>Enum Float/Double does not match Haskell Report</td></tr>
  <tr><th>[\#13563](https://gitlab.haskell.org//ghc/ghc/issues/13563)</th>
  <td>Avoid redundant MOVes in generated code</td></tr>
  <tr><th>[\#13570](https://gitlab.haskell.org//ghc/ghc/issues/13570)</th>
  <td>CoreFVs patch makes n-body slower</td></tr>
  <tr><th>[\#13571](https://gitlab.haskell.org//ghc/ghc/issues/13571)</th>
  <td>Injective type family syntax accepted without TypeFamilyDependencies</td></tr>
  <tr><th>[\#13576](https://gitlab.haskell.org//ghc/ghc/issues/13576)</th>
  <td>Runtime crashes on arm64 (iOS)</td></tr>
  <tr><th>[\#13577](https://gitlab.haskell.org//ghc/ghc/issues/13577)</th>
  <td>Compiler performance on open-symbology parser module regressed in GHC 8.2</td></tr>
  <tr><th>[\#13582](https://gitlab.haskell.org//ghc/ghc/issues/13582)</th>
  <td>Confusing error message with multiparameter type classes.</td></tr>
  <tr><th>[\#13584](https://gitlab.haskell.org//ghc/ghc/issues/13584)</th>
  <td>ghci parse error on operator info</td></tr>
  <tr><th>[\#13586](https://gitlab.haskell.org//ghc/ghc/issues/13586)</th>
  <td>ghc --make seems to leak memory</td></tr>
  <tr><th>[\#13587](https://gitlab.haskell.org//ghc/ghc/issues/13587)</th>
  <td>addTopDecls fails with typed Template Haskell</td></tr>
  <tr><th>[\#13589](https://gitlab.haskell.org//ghc/ghc/issues/13589)</th>
  <td>Possible inconsistency in CSE's treatment of NOINLINE</td></tr>
  <tr><th>[\#13595](https://gitlab.haskell.org//ghc/ghc/issues/13595)</th>
  <td>Should ‘coerce’ be levity polymorphic?</td></tr>
  <tr><th>[\#13616](https://gitlab.haskell.org//ghc/ghc/issues/13616)</th>
  <td>Old file used when later calls to GHC omit -dynamic-too</td></tr>
  <tr><th>[\#13619](https://gitlab.haskell.org//ghc/ghc/issues/13619)</th>
  <td>hsc2hs parses incorrectly c99 style ("/// ...") comments in '--cross-compile' mode</td></tr>
  <tr><th>[\#13620](https://gitlab.haskell.org//ghc/ghc/issues/13620)</th>
  <td>hsc2hs parses incorrectly '\#ifdef' under '\#{enum' in '--cross-compile' mode</td></tr>
  <tr><th>[\#13624](https://gitlab.haskell.org//ghc/ghc/issues/13624)</th>
  <td>loadObj() does not respect alignment</td></tr>
  <tr><th>[\#13629](https://gitlab.haskell.org//ghc/ghc/issues/13629)</th>
  <td>sqrt should use machine instruction on x86_64</td></tr>
  <tr><th>[\#13630](https://gitlab.haskell.org//ghc/ghc/issues/13630)</th>
  <td>ByteString pinned memory can be leaky</td></tr>
  <tr><th>[\#13633](https://gitlab.haskell.org//ghc/ghc/issues/13633)</th>
  <td>testwsdeque failure on POWER8</td></tr>
  <tr><th>[\#13634](https://gitlab.haskell.org//ghc/ghc/issues/13634)</th>
  <td>num009 fails on POWER8</td></tr>
  <tr><th>[\#13637](https://gitlab.haskell.org//ghc/ghc/issues/13637)</th>
  <td>Printing type operators adds extraneous parenthesis</td></tr>
  <tr><th>[\#13645](https://gitlab.haskell.org//ghc/ghc/issues/13645)</th>
  <td>whoCreated produces an uninformative stack trace when an exception is raised in a CAF</td></tr>
  <tr><th>[\#13647](https://gitlab.haskell.org//ghc/ghc/issues/13647)</th>
  <td>Tidy up TcTypeable</td></tr>
  <tr><th>[\#13655](https://gitlab.haskell.org//ghc/ghc/issues/13655)</th>
  <td>Spurious untouchable type variable in connection with rank-2 type and constraint family</td></tr>
  <tr><th>[\#13657](https://gitlab.haskell.org//ghc/ghc/issues/13657)</th>
  <td>Documentation: Functional dependencies by other means</td></tr>
  <tr><th>[\#13660](https://gitlab.haskell.org//ghc/ghc/issues/13660)</th>
  <td>Tail after and including \`NUL\` character in \`FilePath\`s silently truncated</td></tr>
  <tr><th>[\#13668](https://gitlab.haskell.org//ghc/ghc/issues/13668)</th>
  <td>Space leak re-introduced</td></tr>
  <tr><th>[\#13670](https://gitlab.haskell.org//ghc/ghc/issues/13670)</th>
  <td>Improving Type Error Messages</td></tr>
  <tr><th>[\#13676](https://gitlab.haskell.org//ghc/ghc/issues/13676)</th>
  <td>Command line written to stats file by the rts is not executable</td></tr>
  <tr><th>[\#13678](https://gitlab.haskell.org//ghc/ghc/issues/13678)</th>
  <td>Overhaul the linker</td></tr>
  <tr><th>[\#13689](https://gitlab.haskell.org//ghc/ghc/issues/13689)</th>
  <td>Data.Either doesn't export INLINABLE short functions like "rights"</td></tr>
  <tr><th>[\#13692](https://gitlab.haskell.org//ghc/ghc/issues/13692)</th>
  <td>Constructors and such should be able to move around seq\# sometimes</td></tr>
  <tr><th>[\#13693](https://gitlab.haskell.org//ghc/ghc/issues/13693)</th>
  <td>RTS cannot be reinitialized reliably after hs_exit()</td></tr>
  <tr><th>[\#13694](https://gitlab.haskell.org//ghc/ghc/issues/13694)</th>
  <td>CSE runs before SpecConstr</td></tr>
  <tr><th>[\#13713](https://gitlab.haskell.org//ghc/ghc/issues/13713)</th>
  <td>fdefer-type-errors makes missing import errors disappear</td></tr>
  <tr><th>[\#13715](https://gitlab.haskell.org//ghc/ghc/issues/13715)</th>
  <td>test dynamic-paper for way profasm fails with Simplifier ticks exhausted</td></tr>
  <tr><th>[\#13717](https://gitlab.haskell.org//ghc/ghc/issues/13717)</th>
  <td>Pattern synonym exhaustiveness checks don't play well with EmptyCase</td></tr>
  <tr><th>[\#13721](https://gitlab.haskell.org//ghc/ghc/issues/13721)</th>
  <td>posix004 test succeeds but then I get an Apple problem report window that says: "posix004 quit unexpectedly"</td></tr>
  <tr><th>[\#13724](https://gitlab.haskell.org//ghc/ghc/issues/13724)</th>
  <td>Clamping of llvm llc to -O1 and -O2</td></tr>
  <tr><th>[\#13726](https://gitlab.haskell.org//ghc/ghc/issues/13726)</th>
  <td>Declaring GADT constructor and associated data family with the same name gives weird error</td></tr>
  <tr><th>[\#13728](https://gitlab.haskell.org//ghc/ghc/issues/13728)</th>
  <td>Clarify the difference between NameL and NameU in Template Haskell</td></tr>
  <tr><th>[\#13729](https://gitlab.haskell.org//ghc/ghc/issues/13729)</th>
  <td>ghc does not pick up TH changes across package boundaries</td></tr>
  <tr><th>[\#13731](https://gitlab.haskell.org//ghc/ghc/issues/13731)</th>
  <td>DeriveFunctor and friends don't understand type families</td></tr>
  <tr><th>[\#13732](https://gitlab.haskell.org//ghc/ghc/issues/13732)</th>
  <td>Incorrectly suggests ‘TypeOperators’</td></tr>
  <tr><th>[\#13739](https://gitlab.haskell.org//ghc/ghc/issues/13739)</th>
  <td>very slow linking of executables with ld.bfd \< 2.27</td></tr>
  <tr><th>[\#13742](https://gitlab.haskell.org//ghc/ghc/issues/13742)</th>
  <td>Code using ConstraintKinds needs explicit kind signature with GHC 8.2.1</td></tr>
  <tr><th>[\#13744](https://gitlab.haskell.org//ghc/ghc/issues/13744)</th>
  <td>Compile-time regression in 8.2 when compiling bloodhound's test suite</td></tr>
  <tr><th>[\#13748](https://gitlab.haskell.org//ghc/ghc/issues/13748)</th>
  <td>Variables pretty-printed from -ddump-deriv are not scoped properly</td></tr>
  <tr><th>[\#13755](https://gitlab.haskell.org//ghc/ghc/issues/13755)</th>
  <td>GHC-8.0.2+ spits out $dm names in error messages</td></tr>
  <tr><th>[\#13763](https://gitlab.haskell.org//ghc/ghc/issues/13763)</th>
  <td>Performance regression (\~34%) in 8.2.1, poor register allocation(?) in an inner loop over an array</td></tr>
  <tr><th>[\#13765](https://gitlab.haskell.org//ghc/ghc/issues/13765)</th>
  <td>GHC cannot parse valid Haskell98 whose first identifier is named signature</td></tr>
  <tr><th>[\#13766](https://gitlab.haskell.org//ghc/ghc/issues/13766)</th>
  <td>Confusing "redundant pattern match" in 8.0, no warning at all in 8.2</td></tr>
  <tr><th>[\#13769](https://gitlab.haskell.org//ghc/ghc/issues/13769)</th>
  <td>T9405 sporadic failure</td></tr>
  <tr><th>[\#13770](https://gitlab.haskell.org//ghc/ghc/issues/13770)</th>
  <td>HEAD: Type mentioned in error won't show up in pattern signature</td></tr>
  <tr><th>[\#13771](https://gitlab.haskell.org//ghc/ghc/issues/13771)</th>
  <td>ghc fails to build on openSUSE</td></tr>
  <tr><th>[\#13772](https://gitlab.haskell.org//ghc/ghc/issues/13772)</th>
  <td>Cannot put HasCallStack on instances</td></tr>
  <tr><th>[\#13773](https://gitlab.haskell.org//ghc/ghc/issues/13773)</th>
  <td>Types are not normalized in instance declarations</td></tr>
  <tr><th>[\#13775](https://gitlab.haskell.org//ghc/ghc/issues/13775)</th>
  <td>Type family expansion is too lazy, allows accepting of ill-typed terms</td></tr>
  <tr><th>[\#13778](https://gitlab.haskell.org//ghc/ghc/issues/13778)</th>
  <td>explicitly bidirectional patterns should not report Recursive definition" when used in view pattern expression position</td></tr>
  <tr><th>[\#13786](https://gitlab.haskell.org//ghc/ghc/issues/13786)</th>
  <td>GHCi linker is dependent upon object file order</td></tr>
  <tr><th>[\#13795](https://gitlab.haskell.org//ghc/ghc/issues/13795)</th>
  <td>:kind! is not expanding type synonyms anymore</td></tr>
  <tr><th>[\#13796](https://gitlab.haskell.org//ghc/ghc/issues/13796)</th>
  <td>hard to embed custom manifest on windows</td></tr>
  <tr><th>[\#13797](https://gitlab.haskell.org//ghc/ghc/issues/13797)</th>
  <td>Mark negation injective</td></tr>
  <tr><th>[\#13798](https://gitlab.haskell.org//ghc/ghc/issues/13798)</th>
  <td>Invalid transformation in simplOptExpr</td></tr>
  <tr><th>[\#13814](https://gitlab.haskell.org//ghc/ghc/issues/13814)</th>
  <td>Unable to resolve instance for polykinded superclass constraint on associated-type-family.</td></tr>
  <tr><th>[\#13817](https://gitlab.haskell.org//ghc/ghc/issues/13817)</th>
  <td>Simplifier and SpecConstr performance regression with 8.2.1</td></tr>
  <tr><th>[\#13818](https://gitlab.haskell.org//ghc/ghc/issues/13818)</th>
  <td>ANN pragmas can cause compilation to fail if .dyn_o unavailable</td></tr>
  <tr><th>[\#13820](https://gitlab.haskell.org//ghc/ghc/issues/13820)</th>
  <td>GHC goes out of memory while compiling nothing</td></tr>
  <tr><th>[\#13821](https://gitlab.haskell.org//ghc/ghc/issues/13821)</th>
  <td>bindings for unlifted types are allowed in .hs-boot files and .hsig files</td></tr>
  <tr><th>[\#13824](https://gitlab.haskell.org//ghc/ghc/issues/13824)</th>
  <td>ghc 8.2 does not build for me on ppc64le</td></tr>
  <tr><th>[\#13828](https://gitlab.haskell.org//ghc/ghc/issues/13828)</th>
  <td>Re-linking is not avoided when -dynamic is used</td></tr>
  <tr><th>[\#13829](https://gitlab.haskell.org//ghc/ghc/issues/13829)</th>
  <td>ghc --make should not relink when we know the binary doesn't change</td></tr>
  <tr><th>[\#13831](https://gitlab.haskell.org//ghc/ghc/issues/13831)</th>
  <td>GHCi ignores -fforce-recomp on first :reload when used with custom preprocessor</td></tr>
  <tr><th>[\#13834](https://gitlab.haskell.org//ghc/ghc/issues/13834)</th>
  <td>Error cascade with type applications</td></tr>
  <tr><th>[\#13839](https://gitlab.haskell.org//ghc/ghc/issues/13839)</th>
  <td>GHCi warnings do not respect the default module header</td></tr>
  <tr><th>[\#13842](https://gitlab.haskell.org//ghc/ghc/issues/13842)</th>
  <td>Is this output from :all-types correct</td></tr>
  <tr><th>[\#13844](https://gitlab.haskell.org//ghc/ghc/issues/13844)</th>
  <td>Surprising behavior with CPP extension</td></tr>
  <tr><th>[\#13851](https://gitlab.haskell.org//ghc/ghc/issues/13851)</th>
  <td>Change in specialisation(?) behaviour since 8.0.2 causes 6x slowdown</td></tr>
  <tr><th>[\#13858](https://gitlab.haskell.org//ghc/ghc/issues/13858)</th>
  <td>Compiling with LaTeX docs on Mint requires additional packages</td></tr>
  <tr><th>[\#13859](https://gitlab.haskell.org//ghc/ghc/issues/13859)</th>
  <td>Bad error message when compacting a Compact\#</td></tr>
  <tr><th>[\#13867](https://gitlab.haskell.org//ghc/ghc/issues/13867)</th>
  <td>Silly definitions remain after SpecConstr</td></tr>
  <tr><th>[\#13873](https://gitlab.haskell.org//ghc/ghc/issues/13873)</th>
  <td>Adding a SPECIALIZE at a callsite in Main.hs is causing a regression</td></tr>
  <tr><th>[\#13876](https://gitlab.haskell.org//ghc/ghc/issues/13876)</th>
  <td>Check 'pure' method of 'Applicative (WrappedMonad m)'</td></tr>
  <tr><th>[\#13882](https://gitlab.haskell.org//ghc/ghc/issues/13882)</th>
  <td>Template variable unbound in rewrite rule</td></tr>
  <tr><th>[\#13883](https://gitlab.haskell.org//ghc/ghc/issues/13883)</th>
  <td>T5435_dyn_asm fails with ld.gold</td></tr>
  <tr><th>[\#13904](https://gitlab.haskell.org//ghc/ghc/issues/13904)</th>
  <td>LLVM does not need to trash caller-saved registers.</td></tr>
  <tr><th>[\#13905](https://gitlab.haskell.org//ghc/ghc/issues/13905)</th>
  <td>ApplicativeDo is too strict with newtype patterns</td></tr>
  <tr><th>[\#13906](https://gitlab.haskell.org//ghc/ghc/issues/13906)</th>
  <td>ApplicativeDo doesn't handle existentials as well as it could</td></tr>
  <tr><th>[\#13912](https://gitlab.haskell.org//ghc/ghc/issues/13912)</th>
  <td>Disparity in error message in GHCi with : load vs writing directly in GHCi</td></tr>
  <tr><th>[\#13917](https://gitlab.haskell.org//ghc/ghc/issues/13917)</th>
  <td>The line on which to locate the error indication is bad.</td></tr>
  <tr><th>[\#13918](https://gitlab.haskell.org//ghc/ghc/issues/13918)</th>
  <td>No "warning: \[-Wunrecognised-pragmas\] Unrecognised pragma" when there is no name of pragmas</td></tr>
  <tr><th>[\#13921](https://gitlab.haskell.org//ghc/ghc/issues/13921)</th>
  <td>LANGUAGE pragma is accepted in module body</td></tr>
  <tr><th>[\#13924](https://gitlab.haskell.org//ghc/ghc/issues/13924)</th>
  <td>join001 doesn't test anything related to join points</td></tr>
  <tr><th>[\#13928](https://gitlab.haskell.org//ghc/ghc/issues/13928)</th>
  <td>Providing a more specific argument prevents fusion caused by join point floating.</td></tr>
  <tr><th>[\#13933](https://gitlab.haskell.org//ghc/ghc/issues/13933)</th>
  <td>Support Typeable instances for types with coercions</td></tr>
  <tr><th>[\#13951](https://gitlab.haskell.org//ghc/ghc/issues/13951)</th>
  <td>InScope set assertion failure from monad-skeleton</td></tr>
  <tr><th>[\#13956](https://gitlab.haskell.org//ghc/ghc/issues/13956)</th>
  <td>ghc panic compiling lame-0.1.1</td></tr>
  <tr><th>[\#13958](https://gitlab.haskell.org//ghc/ghc/issues/13958)</th>
  <td>Ghc linking failure for 8.2.1 rc3 on Alpine Linux</td></tr>
  <tr><th>[\#13959](https://gitlab.haskell.org//ghc/ghc/issues/13959)</th>
  <td>substTyVar's definition is highly dubious</td></tr>
  <tr><th>[\#13960](https://gitlab.haskell.org//ghc/ghc/issues/13960)</th>
  <td>Ticks exhausted with 8.0.2</td></tr>
  <tr><th>[\#13964](https://gitlab.haskell.org//ghc/ghc/issues/13964)</th>
  <td>Pattern-match warnings for datatypes with COMPLETE sets break abstraction</td></tr>
  <tr><th>[\#13965](https://gitlab.haskell.org//ghc/ghc/issues/13965)</th>
  <td>COMPLETE sets nerf redundant pattern-match warnings</td></tr>
  <tr><th>[\#13971](https://gitlab.haskell.org//ghc/ghc/issues/13971)</th>
  <td>Misleading "Kind mis-match on LHS of default declaration" error</td></tr>
  <tr><th>[\#13975](https://gitlab.haskell.org//ghc/ghc/issues/13975)</th>
  <td>GHC can't infer pattern signature, untoucable kinds</td></tr>
  <tr><th>[\#13981](https://gitlab.haskell.org//ghc/ghc/issues/13981)</th>
  <td>Family instance consistency checks happens too early when hs-boot defined type occurs on LHS</td></tr>
  <tr><th>[\#13993](https://gitlab.haskell.org//ghc/ghc/issues/13993)</th>
  <td>Certain inter-module specializations run out of simplifier ticks</td></tr>
  <tr><th>[\#13995](https://gitlab.haskell.org//ghc/ghc/issues/13995)</th>
  <td>provide hook for 'setNumCapabilities'</td></tr>
  <tr><th>[\#13996](https://gitlab.haskell.org//ghc/ghc/issues/13996)</th>
  <td>Non-cheap primop is duplicated</td></tr>
  <tr><th>[\#13997](https://gitlab.haskell.org//ghc/ghc/issues/13997)</th>
  <td>GHCi panic: "Loading temp shared object failed" when adding a new module</td></tr>
  <tr><th>[\#14001](https://gitlab.haskell.org//ghc/ghc/issues/14001)</th>
  <td>Inlining does not work between modules</td></tr>
  <tr><th>[\#14002](https://gitlab.haskell.org//ghc/ghc/issues/14002)</th>
  <td>Defining a function in GHCi results in different strictness behavior than defining it in a file</td></tr>
  <tr><th>[\#14008](https://gitlab.haskell.org//ghc/ghc/issues/14008)</th>
  <td>NondecreasingIndentation is problematic when the indentation can't be decreased</td></tr>
  <tr><th>[\#14010](https://gitlab.haskell.org//ghc/ghc/issues/14010)</th>
  <td>UndecidableSuperClasses - Could not deduce (Category d)</td></tr>
  <tr><th>[\#14011](https://gitlab.haskell.org//ghc/ghc/issues/14011)</th>
  <td>T3807 fails to build on FreeBSD</td></tr>
  <tr><th>[\#14012](https://gitlab.haskell.org//ghc/ghc/issues/14012)</th>
  <td>External interpreter fails on FreeBSD</td></tr>
  <tr><th>[\#14013](https://gitlab.haskell.org//ghc/ghc/issues/14013)</th>
  <td>Bad monads performance</td></tr>
  <tr><th>[\#14014](https://gitlab.haskell.org//ghc/ghc/issues/14014)</th>
  <td>trac guest account doesn't work</td></tr>
  <tr><th>[\#14017](https://gitlab.haskell.org//ghc/ghc/issues/14017)</th>
  <td>"make install" with non-standard umask causes bad permission on package.cache</td></tr>
  <tr><th>[\#14021](https://gitlab.haskell.org//ghc/ghc/issues/14021)</th>
  <td>8.2.1 deb8 bindist fails to install on Windows 10 WSL</td></tr>
  <tr><th>[\#14025](https://gitlab.haskell.org//ghc/ghc/issues/14025)</th>
  <td>Object file is put in wrong directory when any source has absolute path</td></tr>
  <tr><th>[\#14026](https://gitlab.haskell.org//ghc/ghc/issues/14026)</th>
  <td>hs_init is not idempotent</td></tr>
  <tr><th>[\#14031](https://gitlab.haskell.org//ghc/ghc/issues/14031)</th>
  <td>Linker paths carry substantial N\*M overhead when many libaries are used</td></tr>
  <tr><th>[\#14032](https://gitlab.haskell.org//ghc/ghc/issues/14032)</th>
  <td>Can't splice TH quote with infix declaration for name in two different namespaces</td></tr>
  <tr><th>[\#14035](https://gitlab.haskell.org//ghc/ghc/issues/14035)</th>
  <td>Weird performance results.</td></tr>
  <tr><th>[\#14040](https://gitlab.haskell.org//ghc/ghc/issues/14040)</th>
  <td>Typed holes regression in GHC 8.0.2: No skolem info: z_a1sY\[sk:2\]</td></tr>
  <tr><th>[\#14044](https://gitlab.haskell.org//ghc/ghc/issues/14044)</th>
  <td>ghc-8.2.1 installation fails on OpenBSD 6.0</td></tr>
  <tr><th>[\#14059](https://gitlab.haskell.org//ghc/ghc/issues/14059)</th>
  <td>COMPLETE sets don't work at all with data family instances</td></tr>
  <tr><th>[\#14062](https://gitlab.haskell.org//ghc/ghc/issues/14062)</th>
  <td>Pure syntax transformation affects performance.</td></tr>
  <tr><th>[\#14063](https://gitlab.haskell.org//ghc/ghc/issues/14063)</th>
  <td>Compiling with --backpack with undefined dependency results in "the 'impossible' happened"</td></tr>
  <tr><th>[\#14064](https://gitlab.haskell.org//ghc/ghc/issues/14064)</th>
  <td>Compiling problem on FreeBSD 11 ("failed in phase")</td></tr>
  <tr><th>[\#14068](https://gitlab.haskell.org//ghc/ghc/issues/14068)</th>
  <td>Loopification using join points</td></tr>
  <tr><th>[\#14069](https://gitlab.haskell.org//ghc/ghc/issues/14069)</th>
  <td>RTS linker maps code as writable</td></tr>
  <tr><th>[\#14072](https://gitlab.haskell.org//ghc/ghc/issues/14072)</th>
  <td>Code generated by GHC 8.2.1 faster than 8.0.1 but still somewhat slower than 7.10.3</td></tr>
  <tr><th>[\#14073](https://gitlab.haskell.org//ghc/ghc/issues/14073)</th>
  <td>Testsuite should pass even with LANG=C</td></tr>
  <tr><th>[\#14074](https://gitlab.haskell.org//ghc/ghc/issues/14074)</th>
  <td>fdReadBuf001 fails non-deterministically on FreeBSD</td></tr>
  <tr><th>[\#14090](https://gitlab.haskell.org//ghc/ghc/issues/14090)</th>
  <td>Static pointers are not being registered under certain conditions</td></tr>
  <tr><th>[\#14092](https://gitlab.haskell.org//ghc/ghc/issues/14092)</th>
  <td>hs-boot unfolding visibility not consistent between --make and -c</td></tr>
  <tr><th>[\#14097](https://gitlab.haskell.org//ghc/ghc/issues/14097)</th>
  <td>-ddump-json doesn't interact as expected with -ddump-to-file</td></tr>
  <tr><th>[\#14100](https://gitlab.haskell.org//ghc/ghc/issues/14100)</th>
  <td>Nested NPlusKPatterns</td></tr>
  <tr><th>[\#14103](https://gitlab.haskell.org//ghc/ghc/issues/14103)</th>
  <td>Retypechecking the loop in --make mode is super-linear when there are many .hs-boot modules</td></tr>
  <tr><th>[\#14106](https://gitlab.haskell.org//ghc/ghc/issues/14106)</th>
  <td>Out of scope errors appear after type errors caused by them</td></tr>
  <tr><th>[\#14109](https://gitlab.haskell.org//ghc/ghc/issues/14109)</th>
  <td>GHC matches -- as a varsym when lexing a qvarsym</td></tr>
  <tr><th>[\#14111](https://gitlab.haskell.org//ghc/ghc/issues/14111)</th>
  <td>strange error when using data families with levity polymorphism and unboxed sums and data families</td></tr>
  <tr><th>[\#14113](https://gitlab.haskell.org//ghc/ghc/issues/14113)</th>
  <td>Error message carets point at the wrong places in the presence of CPP macros</td></tr>
  <tr><th>[\#14133](https://gitlab.haskell.org//ghc/ghc/issues/14133)</th>
  <td>COMPLETE pragmas seem to be ignored when using view patterns</td></tr>
  <tr><th>[\#14141](https://gitlab.haskell.org//ghc/ghc/issues/14141)</th>
  <td>Custom type errors don't trigger when matching on a GADT constructor with an error in the constraint</td></tr>
  <tr><th>[\#14147](https://gitlab.haskell.org//ghc/ghc/issues/14147)</th>
  <td>Confusing error messages with PolyKinds and superclasses</td></tr>
  <tr><th>[\#14151](https://gitlab.haskell.org//ghc/ghc/issues/14151)</th>
  <td>Invisible kind variable referenced in typeclass instance error message</td></tr>
  <tr><th>[\#14155](https://gitlab.haskell.org//ghc/ghc/issues/14155)</th>
  <td>GHC mentions unlifted types out of the blue (to me anyway)</td></tr>
  <tr><th>[\#14156](https://gitlab.haskell.org//ghc/ghc/issues/14156)</th>
  <td>Document the thread wakeup/scheduling/fairness semantics for the STM primitives</td></tr>
  <tr><th>[\#14164](https://gitlab.haskell.org//ghc/ghc/issues/14164)</th>
  <td>GHC hangs on type family dependency</td></tr>
  <tr><th>[\#14165](https://gitlab.haskell.org//ghc/ghc/issues/14165)</th>
  <td>Investigate regressions from simplifier refactor</td></tr>
  <tr><th>[\#14180](https://gitlab.haskell.org//ghc/ghc/issues/14180)</th>
  <td>Strange/bad error message binding unboxed type variable</td></tr>
  <tr><th>[\#14182](https://gitlab.haskell.org//ghc/ghc/issues/14182)</th>
  <td>Allow full control over dyn lib names</td></tr>
  <tr><th>[\#14185](https://gitlab.haskell.org//ghc/ghc/issues/14185)</th>
  <td>Non-local bug reporting around levity polymorphism</td></tr>
  <tr><th>[\#14186](https://gitlab.haskell.org//ghc/ghc/issues/14186)</th>
  <td>CSE fails to CSE two identical large top-level functions</td></tr>
  <tr><th>[\#14190](https://gitlab.haskell.org//ghc/ghc/issues/14190)</th>
  <td>Typeable imposes seemingly redundant constraints on polykinded instances</td></tr>
  <tr><th>[\#14198](https://gitlab.haskell.org//ghc/ghc/issues/14198)</th>
  <td>Inconsistent treatment of implicitly bound kind variables as free-floating</td></tr>
  <tr><th>[\#14208](https://gitlab.haskell.org//ghc/ghc/issues/14208)</th>
  <td>Performance with O0 is much better than the default or with -O2, runghc performs the best</td></tr>
  <tr><th>[\#14211](https://gitlab.haskell.org//ghc/ghc/issues/14211)</th>
  <td>Compiler is unable to INLINE as well as the programmer can manually</td></tr>
  <tr><th>[\#14212](https://gitlab.haskell.org//ghc/ghc/issues/14212)</th>
  <td>Give better error message with non-supported Backpack/TH use</td></tr>
  <tr><th>[\#14214](https://gitlab.haskell.org//ghc/ghc/issues/14214)</th>
  <td>Users guide lies about default optimization level</td></tr>
  <tr><th>[\#14222](https://gitlab.haskell.org//ghc/ghc/issues/14222)</th>
  <td>Simple text fusion example results in rather duplicative code</td></tr>
  <tr><th>[\#14226](https://gitlab.haskell.org//ghc/ghc/issues/14226)</th>
  <td>Common Block Elimination pass doesn't eliminate common blocks</td></tr>
  <tr><th>[\#14229](https://gitlab.haskell.org//ghc/ghc/issues/14229)</th>
  <td>Contraditions in users_guide/using-warnings.html</td></tr>
  <tr><th>[\#14231](https://gitlab.haskell.org//ghc/ghc/issues/14231)</th>
  <td>Core lint error "in result of Static argument"</td></tr>
  <tr><th>[\#14242](https://gitlab.haskell.org//ghc/ghc/issues/14242)</th>
  <td>Ticks and join points don't play well</td></tr>
  <tr><th>[\#14250](https://gitlab.haskell.org//ghc/ghc/issues/14250)</th>
  <td>GHCi by default opens .ghci files in local directories.</td></tr>
  <tr><th>[\#14251](https://gitlab.haskell.org//ghc/ghc/issues/14251)</th>
  <td>LLVM Code Gen messes up registers</td></tr>
  <tr><th>[\#14253](https://gitlab.haskell.org//ghc/ghc/issues/14253)</th>
  <td>Pattern match checker mistakenly concludes pattern match on pattern synonym is unreachable</td></tr>
  <tr><th>[\#14256](https://gitlab.haskell.org//ghc/ghc/issues/14256)</th>
  <td>GHCi is faster than compiled code</td></tr>
  <tr><th>[\#14261](https://gitlab.haskell.org//ghc/ghc/issues/14261)</th>
  <td>ghc stopped recognizing some arm triplets that used to work: Failed to lookup the datalayout for armv7a-hardfloat-linux-gnueabi; available targets:</td></tr>
  <tr><th>[\#14264](https://gitlab.haskell.org//ghc/ghc/issues/14264)</th>
  <td>unregisterised GHC fails buid as: ghc-stage1: panic! (the 'impossible' happened): pprCLbl AsmTempLabel</td></tr>
  <tr><th>[\#14266](https://gitlab.haskell.org//ghc/ghc/issues/14266)</th>
  <td>AllowAmbiguousTypes doesn't play well with default class methods</td></tr>
  <tr><th>[\#14270](https://gitlab.haskell.org//ghc/ghc/issues/14270)</th>
  <td>GHC HEAD's ghc-stage1 panics on Data.Typeable.Internal</td></tr>
  <tr><th>[\#14274](https://gitlab.haskell.org//ghc/ghc/issues/14274)</th>
  <td>Host normalization causes problem with configure.</td></tr>
  <tr><th>[\#14275](https://gitlab.haskell.org//ghc/ghc/issues/14275)</th>
  <td>Large Haskell value unexpectedly gets an unfolding</td></tr>
  <tr><th>[\#14276](https://gitlab.haskell.org//ghc/ghc/issues/14276)</th>
  <td>T13168 is broken on Windows</td></tr>
  <tr><th>[\#14278](https://gitlab.haskell.org//ghc/ghc/issues/14278)</th>
  <td>undefined symbol: gsl_multiroot_fsolver_broyden</td></tr>
  <tr><th>[\#14279](https://gitlab.haskell.org//ghc/ghc/issues/14279)</th>
  <td>Type families interfere with specialisation rewrite rules</td></tr>
  <tr><th>[\#14281](https://gitlab.haskell.org//ghc/ghc/issues/14281)</th>
  <td>Minor regressions from removal of non-linear behavior from simplifier</td></tr>
  <tr><th>[\#14282](https://gitlab.haskell.org//ghc/ghc/issues/14282)</th>
  <td>tagToEnum\# . dataToTag\# not optimized away</td></tr>
  <tr><th>[\#14287](https://gitlab.haskell.org//ghc/ghc/issues/14287)</th>
  <td>Early inlining causes potential join points to be missed</td></tr>
  <tr><th>[\#14293](https://gitlab.haskell.org//ghc/ghc/issues/14293)</th>
  <td>View patterns with locally defined functions in restructuring don't compile</td></tr>
  <tr><th>[\#14295](https://gitlab.haskell.org//ghc/ghc/issues/14295)</th>
  <td>tagToEnum\# leads to some silly closures</td></tr>
  <tr><th>[\#14297](https://gitlab.haskell.org//ghc/ghc/issues/14297)</th>
  <td>make bindist packages the wrong binaries for cross compilers</td></tr>
  <tr><th>[\#14299](https://gitlab.haskell.org//ghc/ghc/issues/14299)</th>
  <td>GHCi for GHC 8.2.1 crashed with simple function?</td></tr>
  <tr><th>[\#14318](https://gitlab.haskell.org//ghc/ghc/issues/14318)</th>
  <td>TH shadowing bind statement triggers -Wunused-matches</td></tr>
  <tr><th>[\#14319](https://gitlab.haskell.org//ghc/ghc/issues/14319)</th>
  <td>Stuck type families can lead to lousy error messages</td></tr>
  <tr><th>[\#14321](https://gitlab.haskell.org//ghc/ghc/issues/14321)</th>
  <td>-fsolve-constant-dicts is not very robust when dealing with GADTs</td></tr>
  <tr><th>[\#14328](https://gitlab.haskell.org//ghc/ghc/issues/14328)</th>
  <td>ld.gold -r brokenness breaks SplitSections=YES builds</td></tr>
  <tr><th>[\#14329](https://gitlab.haskell.org//ghc/ghc/issues/14329)</th>
  <td>GHC 8.2.1 segfaults while bootstrapping master</td></tr>
  <tr><th>[\#14330](https://gitlab.haskell.org//ghc/ghc/issues/14330)</th>
  <td>Sparks are not started promptly</td></tr>
  <tr><th>[\#14331](https://gitlab.haskell.org//ghc/ghc/issues/14331)</th>
  <td>Overzealous free-floating kind check causes deriving clause to be rejected</td></tr>
  <tr><th>[\#14332](https://gitlab.haskell.org//ghc/ghc/issues/14332)</th>
  <td>Deriving clauses can have forall types</td></tr>
  <tr><th>[\#14334](https://gitlab.haskell.org//ghc/ghc/issues/14334)</th>
  <td>Large static object : getLabelBc: Ran out of labels</td></tr>
  <tr><th>[\#14337](https://gitlab.haskell.org//ghc/ghc/issues/14337)</th>
  <td>typeRepKind can perform substantial amounts of allocation</td></tr>
  <tr><th>[\#14341](https://gitlab.haskell.org//ghc/ghc/issues/14341)</th>
  <td>Show instance for TypeReps is a bit broken</td></tr>
  <tr><th>[\#14345](https://gitlab.haskell.org//ghc/ghc/issues/14345)</th>
  <td>Warning when linking with C++ code</td></tr>
  <tr><th>[\#14351](https://gitlab.haskell.org//ghc/ghc/issues/14351)</th>
  <td>reverse-errors doesn't affect instance errors</td></tr>
  <tr><th>[\#14358](https://gitlab.haskell.org//ghc/ghc/issues/14358)</th>
  <td>GHCi does not exit after heap overflow exception</td></tr>
  <tr><th>[\#14359](https://gitlab.haskell.org//ghc/ghc/issues/14359)</th>
  <td>C-- pipeline/NCG fails to optimize simple repeated addition</td></tr>
  <tr><th>[\#14360](https://gitlab.haskell.org//ghc/ghc/issues/14360)</th>
  <td>traceM documentation not clear (and possibly incorrect)</td></tr>
  <tr><th>[\#14373](https://gitlab.haskell.org//ghc/ghc/issues/14373)</th>
  <td>Introduce PTR-tagging for big constructor families</td></tr>
  <tr><th>[\#14374](https://gitlab.haskell.org//ghc/ghc/issues/14374)</th>
  <td>group by using groupWith seems stricter than it need be</td></tr>
  <tr><th>[\#14375](https://gitlab.haskell.org//ghc/ghc/issues/14375)</th>
  <td>Implement with\# primop</td></tr>
  <tr><th>[\#14380](https://gitlab.haskell.org//ghc/ghc/issues/14380)</th>
  <td>Compile error for PatternSynonyms together with OverloadedLists</td></tr>
  <tr><th>[\#14382](https://gitlab.haskell.org//ghc/ghc/issues/14382)</th>
  <td>The 'impossible' happened whilst installing gi-gtk via cabal</td></tr>
  <tr><th>[\#14383](https://gitlab.haskell.org//ghc/ghc/issues/14383)</th>
  <td>Allocation in VS up 500%</td></tr>
  <tr><th>[\#14386](https://gitlab.haskell.org//ghc/ghc/issues/14386)</th>
  <td>GHC doesn't allow Coercion between partly-saturated type constructors</td></tr>
  <tr><th>[\#14398](https://gitlab.haskell.org//ghc/ghc/issues/14398)</th>
  <td>Fail to install haskell platform on Windows</td></tr>
  <tr><th>[\#14399](https://gitlab.haskell.org//ghc/ghc/issues/14399)</th>
  <td>NCG: dead code stripping prevention wastes space</td></tr>
  <tr><th>[\#14403](https://gitlab.haskell.org//ghc/ghc/issues/14403)</th>
  <td>strange closure type 2136315671</td></tr>
  <tr><th>[\#14412](https://gitlab.haskell.org//ghc/ghc/issues/14412)</th>
  <td>Can't run tests with sdist -\> bindist -\> test</td></tr>
  <tr><th>[\#14414](https://gitlab.haskell.org//ghc/ghc/issues/14414)</th>
  <td>Profiled program runs 2.5x faster than non-profiled</td></tr>
  <tr><th>[\#14419](https://gitlab.haskell.org//ghc/ghc/issues/14419)</th>
  <td>Check kinds for ambiguity</td></tr>
  <tr><th>[\#14420](https://gitlab.haskell.org//ghc/ghc/issues/14420)</th>
  <td>Data families should not instantiate to non-Type kinds</td></tr>
  <tr><th>[\#14455](https://gitlab.haskell.org//ghc/ghc/issues/14455)</th>
  <td>PPC64: Wrong output in print022</td></tr>
  <tr><th>[\#14469](https://gitlab.haskell.org//ghc/ghc/issues/14469)</th>
  <td>Rebuilding profiled stage2 after building stage3 is broken</td></tr>
  <tr><th>[\#14472](https://gitlab.haskell.org//ghc/ghc/issues/14472)</th>
  <td>error __STDC_VERSION__ does not advertise C99 or later</td></tr>
  <tr><th>[\#14482](https://gitlab.haskell.org//ghc/ghc/issues/14482)</th>
  <td>GHC -M mode fails to ensure that boot files are built before source files</td></tr>
  <tr><th>[\#14490](https://gitlab.haskell.org//ghc/ghc/issues/14490)</th>
  <td>TTG Snags</td></tr>
  <tr><th>[\#14503](https://gitlab.haskell.org//ghc/ghc/issues/14503)</th>
  <td>Killing a thread will block if there is another process reading from a handle</td></tr>
  <tr><th>[\#14505](https://gitlab.haskell.org//ghc/ghc/issues/14505)</th>
  <td>CircleCI only builds pushed heads</td></tr>
  <tr><th>[\#14510](https://gitlab.haskell.org//ghc/ghc/issues/14510)</th>
  <td>GHC.ExecutionStack.showStackTrace broken</td></tr>
  <tr><th>[\#14512](https://gitlab.haskell.org//ghc/ghc/issues/14512)</th>
  <td>System-wide installed profile build cannot load libHSghc-prim.0.5.2.0.so</td></tr>
  <tr><th>[\#14514](https://gitlab.haskell.org//ghc/ghc/issues/14514)</th>
  <td>Error messages: suggest annotating with higher-rank kind</td></tr>
  <tr><th>[\#14522](https://gitlab.haskell.org//ghc/ghc/issues/14522)</th>
  <td>GHC recompilation checker doesn't take account of deprecated pragmas</td></tr>
  <tr><th>[\#14523](https://gitlab.haskell.org//ghc/ghc/issues/14523)</th>
  <td>Confusing link error when specifying the same object repeatedly</td></tr>
  <tr><th>[\#14528](https://gitlab.haskell.org//ghc/ghc/issues/14528)</th>
  <td>LLVM's CallAnalyzer Breaks</td></tr>
  <tr><th>[\#14530](https://gitlab.haskell.org//ghc/ghc/issues/14530)</th>
  <td>hWaitForInput causes the program to abort on Windows</td></tr>
  <tr><th>[\#14533](https://gitlab.haskell.org//ghc/ghc/issues/14533)</th>
  <td>Make GHC more robust against PC crashes by using atomic writes</td></tr>
  <tr><th>[\#14535](https://gitlab.haskell.org//ghc/ghc/issues/14535)</th>
  <td>ghc: panic! (the 'impossible' happened)   (GHC version 8.2.1 for x86_64-apple-darwin): 	stack depth overflow</td></tr>
  <tr><th>[\#14543](https://gitlab.haskell.org//ghc/ghc/issues/14543)</th>
  <td>Broken links to docs all over the web</td></tr>
  <tr><th>[\#14548](https://gitlab.haskell.org//ghc/ghc/issues/14548)</th>
  <td>Lexically scoped kind variables</td></tr>
  <tr><th>[\#14562](https://gitlab.haskell.org//ghc/ghc/issues/14562)</th>
  <td>IntRep vs WordRep</td></tr>
  <tr><th>[\#14564](https://gitlab.haskell.org//ghc/ghc/issues/14564)</th>
  <td>CAF isn't floated</td></tr>
  <tr><th>[\#14565](https://gitlab.haskell.org//ghc/ghc/issues/14565)</th>
  <td>Performance degrades from -O1 to -O2</td></tr>
  <tr><th>[\#14570](https://gitlab.haskell.org//ghc/ghc/issues/14570)</th>
  <td>Untouchable error arises from type equality, but not equivalent program with fundeps</td></tr>
  <tr><th>[\#14574](https://gitlab.haskell.org//ghc/ghc/issues/14574)</th>
  <td>Template Haskell Uniq \~ Int leads to external interpreter cross compilation trouble</td></tr>
  <tr><th>[\#14576](https://gitlab.haskell.org//ghc/ghc/issues/14576)</th>
  <td>Internal error when compiling TH code with profiling on Windows</td></tr>
  <tr><th>[\#14577](https://gitlab.haskell.org//ghc/ghc/issues/14577)</th>
  <td>Internal error when linker is initialized with -fexternal-interpreter set when compiling TH code with profiling</td></tr>
  <tr><th>[\#14582](https://gitlab.haskell.org//ghc/ghc/issues/14582)</th>
  <td>Review and improve the Typeable API</td></tr>
  <tr><th>[\#14589](https://gitlab.haskell.org//ghc/ghc/issues/14589)</th>
  <td>The isUpper function should return true for the '\\9438' character</td></tr>
  <tr><th>[\#14594](https://gitlab.haskell.org//ghc/ghc/issues/14594)</th>
  <td>2 modules / 2500LOC takes nearly 3 minutes to build</td></tr>
  <tr><th>[\#14599](https://gitlab.haskell.org//ghc/ghc/issues/14599)</th>
  <td>32-bit Windows test environment</td></tr>
  <tr><th>[\#14610](https://gitlab.haskell.org//ghc/ghc/issues/14610)</th>
  <td>newtype wrapping of a monadic stack kills performance</td></tr>
  <tr><th>[\#14613](https://gitlab.haskell.org//ghc/ghc/issues/14613)</th>
  <td>Validate Failure On OSX -- implicit fall through error for machO linker support  (master == ghc8.5)</td></tr>
  <tr><th>[\#14617](https://gitlab.haskell.org//ghc/ghc/issues/14617)</th>
  <td>Join point test join001 doesn't seem to be properly automated</td></tr>
  <tr><th>[\#14620](https://gitlab.haskell.org//ghc/ghc/issues/14620)</th>
  <td>Polymorphic functions not easily recognized as join points</td></tr>
  <tr><th>[\#14624](https://gitlab.haskell.org//ghc/ghc/issues/14624)</th>
  <td>capi panic (toCType Int\#)</td></tr>
  <tr><th>[\#14625](https://gitlab.haskell.org//ghc/ghc/issues/14625)</th>
  <td>Casts get in the way of calculating unfolding discount</td></tr>
  <tr><th>[\#14626](https://gitlab.haskell.org//ghc/ghc/issues/14626)</th>
  <td>No need to enter a scrutinised value</td></tr>
  <tr><th>[\#14628](https://gitlab.haskell.org//ghc/ghc/issues/14628)</th>
  <td>Panic (No skolem Info) in GHCi</td></tr>
  <tr><th>[\#14629](https://gitlab.haskell.org//ghc/ghc/issues/14629)</th>
  <td>Seemingly unused qualified import affects method visibility</td></tr>
  <tr><th>[\#14630](https://gitlab.haskell.org//ghc/ghc/issues/14630)</th>
  <td>name shadowing warnings by record pattern synonyms + RecordWildCards or NamedFieldPuns</td></tr>
  <tr><th>[\#14633](https://gitlab.haskell.org//ghc/ghc/issues/14633)</th>
  <td>-fwarn-redundant-constraints false positive</td></tr>
  <tr><th>[\#14637](https://gitlab.haskell.org//ghc/ghc/issues/14637)</th>
  <td>Simplifier Ticks Exhausted when compiling with profiling</td></tr>
  <tr><th>[\#14642](https://gitlab.haskell.org//ghc/ghc/issues/14642)</th>
  <td>‘class C using’ fails to parse with MonadComprehensions</td></tr>
  <tr><th>[\#14645](https://gitlab.haskell.org//ghc/ghc/issues/14645)</th>
  <td>Allow type family in data family return kind</td></tr>
  <tr><th>[\#14647](https://gitlab.haskell.org//ghc/ghc/issues/14647)</th>
  <td>Invalid C in the via-C backend due to EFF_</td></tr>
  <tr><th>[\#14648](https://gitlab.haskell.org//ghc/ghc/issues/14648)</th>
  <td>ghc-pkg does handle unitids</td></tr>
  <tr><th>[\#14649](https://gitlab.haskell.org//ghc/ghc/issues/14649)</th>
  <td>ghc panic: mergeSATInfo</td></tr>
  <tr><th>[\#14651](https://gitlab.haskell.org//ghc/ghc/issues/14651)</th>
  <td>recompilation checker overeager on executables produced with -o</td></tr>
  <tr><th>[\#14655](https://gitlab.haskell.org//ghc/ghc/issues/14655)</th>
  <td>Compiled nofib-analyse executable segfaults under windows</td></tr>
  <tr><th>[\#14659](https://gitlab.haskell.org//ghc/ghc/issues/14659)</th>
  <td>assertions are turned off if -O2 comes after -fno-ignore-asserts</td></tr>
  <tr><th>[\#14662](https://gitlab.haskell.org//ghc/ghc/issues/14662)</th>
  <td>Partial type signatures + mutual recursion = confusion</td></tr>
  <tr><th>[\#14663](https://gitlab.haskell.org//ghc/ghc/issues/14663)</th>
  <td>Deriving Typeable for enumerations seems expensive</td></tr>
  <tr><th>[\#14668](https://gitlab.haskell.org//ghc/ghc/issues/14668)</th>
  <td>Ordering of declarations can cause typechecking to fail</td></tr>
  <tr><th>[\#14673](https://gitlab.haskell.org//ghc/ghc/issues/14673)</th>
  <td>Unary Unboxed Tuple Type Constructor</td></tr>
  <tr><th>[\#14677](https://gitlab.haskell.org//ghc/ghc/issues/14677)</th>
  <td>Code generator does not correctly tag a pointer</td></tr>
  <tr><th>[\#14679](https://gitlab.haskell.org//ghc/ghc/issues/14679)</th>
  <td>The interpreter showed panic! (the 'impossible' happened)</td></tr>
  <tr><th>[\#14684](https://gitlab.haskell.org//ghc/ghc/issues/14684)</th>
  <td>combineIdenticalAlts is only partially implemented</td></tr>
  <tr><th>[\#14689](https://gitlab.haskell.org//ghc/ghc/issues/14689)</th>
  <td>Load order of .ghci config files is counterintuitive</td></tr>
  <tr><th>[\#14690](https://gitlab.haskell.org//ghc/ghc/issues/14690)</th>
  <td>Pattern match failure in GHCi with :steplocal</td></tr>
  <tr><th>[\#14694](https://gitlab.haskell.org//ghc/ghc/issues/14694)</th>
  <td>Incompleteness in the Coercible constraint solver</td></tr>
  <tr><th>[\#14700](https://gitlab.haskell.org//ghc/ghc/issues/14700)</th>
  <td>ApplicativeDo in MonadComprehensions</td></tr>
  <tr><th>[\#14703](https://gitlab.haskell.org//ghc/ghc/issues/14703)</th>
  <td>Uniques should not appear in error messages</td></tr>
  <tr><th>[\#14704](https://gitlab.haskell.org//ghc/ghc/issues/14704)</th>
  <td>Spurious cost-centre test failures</td></tr>
  <tr><th>[\#14727](https://gitlab.haskell.org//ghc/ghc/issues/14727)</th>
  <td>Unboxed sum performance surprisingly poor</td></tr>
  <tr><th>[\#14729](https://gitlab.haskell.org//ghc/ghc/issues/14729)</th>
  <td>normaliseType is not well-kinded</td></tr>
  <tr><th>[\#14730](https://gitlab.haskell.org//ghc/ghc/issues/14730)</th>
  <td>Missing predicate for "ResourceVanished" IOException/IOErrorType</td></tr>
  <tr><th>[\#14738](https://gitlab.haskell.org//ghc/ghc/issues/14738)</th>
  <td>Investigate performance of CoreTidy</td></tr>
  <tr><th>[\#14739](https://gitlab.haskell.org//ghc/ghc/issues/14739)</th>
  <td>Cannot compile ghc 8.2.1 or 8.2.2 on armv7l architectures</td></tr>
  <tr><th>[\#14741](https://gitlab.haskell.org//ghc/ghc/issues/14741)</th>
  <td>High-memory usage during compilation using Template Haskell</td></tr>
  <tr><th>[\#14745](https://gitlab.haskell.org//ghc/ghc/issues/14745)</th>
  <td>Functional dependency conflicts in givens</td></tr>
  <tr><th>[\#14753](https://gitlab.haskell.org//ghc/ghc/issues/14753)</th>
  <td>Assembly for stg_enter_info starts with a dead 'mov'</td></tr>
  <tr><th>[\#14757](https://gitlab.haskell.org//ghc/ghc/issues/14757)</th>
  <td>ghc recompilation check doesn't take into account headers directly included by CApiFFI</td></tr>
  <tr><th>[\#14759](https://gitlab.haskell.org//ghc/ghc/issues/14759)</th>
  <td>ListSetOps WARNING causes tests to fail</td></tr>
  <tr><th>[\#14760](https://gitlab.haskell.org//ghc/ghc/issues/14760)</th>
  <td>Error reporting on obsolete file</td></tr>
  <tr><th>[\#14762](https://gitlab.haskell.org//ghc/ghc/issues/14762)</th>
  <td>Foreign.Marshal.Pool functions use inefficient O(n) operations</td></tr>
  <tr><th>[\#14765](https://gitlab.haskell.org//ghc/ghc/issues/14765)</th>
  <td>Levity polymorphism panic</td></tr>
  <tr><th>[\#14766](https://gitlab.haskell.org//ghc/ghc/issues/14766)</th>
  <td>Hole-y partial type signatures greatly slow down compile times</td></tr>
  <tr><th>[\#14769](https://gitlab.haskell.org//ghc/ghc/issues/14769)</th>
  <td>The RecompBecause \[TH\] check is not resume-build-safe</td></tr>
  <tr><th>[\#14771](https://gitlab.haskell.org//ghc/ghc/issues/14771)</th>
  <td>TypeError reported too eagerly</td></tr>
  <tr><th>[\#14776](https://gitlab.haskell.org//ghc/ghc/issues/14776)</th>
  <td>Add equality rule for \`eqString\`</td></tr>
  <tr><th>[\#14778](https://gitlab.haskell.org//ghc/ghc/issues/14778)</th>
  <td>FunDep origin not correctly attributed</td></tr>
  <tr><th>[\#14780](https://gitlab.haskell.org//ghc/ghc/issues/14780)</th>
  <td>:type-at doesn't work when :load-ing with module name instead of file path</td></tr>
  <tr><th>[\#14781](https://gitlab.haskell.org//ghc/ghc/issues/14781)</th>
  <td>getNumCapabilities produces incorrect CPU counts</td></tr>
  <tr><th>[\#14782](https://gitlab.haskell.org//ghc/ghc/issues/14782)</th>
  <td>typeclass polymorphism defeats bang patterns</td></tr>
  <tr><th>[\#14788](https://gitlab.haskell.org//ghc/ghc/issues/14788)</th>
  <td>\`error\` in GHC internals can disappear by rerunning ghc again</td></tr>
  <tr><th>[\#14789](https://gitlab.haskell.org//ghc/ghc/issues/14789)</th>
  <td>GHCi fails to garbage collect declaration \`l = length \[1..10\^8\]\` entered at prompt</td></tr>
  <tr><th>[\#14792](https://gitlab.haskell.org//ghc/ghc/issues/14792)</th>
  <td>compiling cabal-1.24.2.0 requires almost 3 GB of memory</td></tr>
  <tr><th>[\#14794](https://gitlab.haskell.org//ghc/ghc/issues/14794)</th>
  <td>-Weverything should not enable -Wmissing-exported-signatures</td></tr>
  <tr><th>[\#14797](https://gitlab.haskell.org//ghc/ghc/issues/14797)</th>
  <td>High-residency modules during GHC build</td></tr>
  <tr><th>[\#14798](https://gitlab.haskell.org//ghc/ghc/issues/14798)</th>
  <td>Error message suggests applying (non-existant) function to more arguments</td></tr>
  <tr><th>[\#14802](https://gitlab.haskell.org//ghc/ghc/issues/14802)</th>
  <td>panic! (the 'impossible' happened)</td></tr>
  <tr><th>[\#14804](https://gitlab.haskell.org//ghc/ghc/issues/14804)</th>
  <td>hGetLine does not document whether newline is returned.</td></tr>
  <tr><th>[\#14810](https://gitlab.haskell.org//ghc/ghc/issues/14810)</th>
  <td>Define MVar "This function is atomic only if there are no other producers for this MVar."</td></tr>
  <tr><th>[\#14816](https://gitlab.haskell.org//ghc/ghc/issues/14816)</th>
  <td>Missed Called Arity opportunity?</td></tr>
  <tr><th>[\#14823](https://gitlab.haskell.org//ghc/ghc/issues/14823)</th>
  <td>Test profiling/should_run/scc001 fails on Circle CI</td></tr>
  <tr><th>[\#14828](https://gitlab.haskell.org//ghc/ghc/issues/14828)</th>
  <td>panic! when using :print on some functions with class constraints?</td></tr>
  <tr><th>[\#14829](https://gitlab.haskell.org//ghc/ghc/issues/14829)</th>
  <td>Linking error with ANN pragma</td></tr>
  <tr><th>[\#14831](https://gitlab.haskell.org//ghc/ghc/issues/14831)</th>
  <td>QuantifiedConstraints: Odd superclass constraint</td></tr>
  <tr><th>[\#14832](https://gitlab.haskell.org//ghc/ghc/issues/14832)</th>
  <td>QuantifiedConstraints: Adding to the context causes failure</td></tr>
  <tr><th>[\#14834](https://gitlab.haskell.org//ghc/ghc/issues/14834)</th>
  <td>Executable have problems with DWARF debug information</td></tr>
  <tr><th>[\#14838](https://gitlab.haskell.org//ghc/ghc/issues/14838)</th>
  <td>missing "incomplete-patterns" warning for TH-generated functions</td></tr>
  <tr><th>[\#14839](https://gitlab.haskell.org//ghc/ghc/issues/14839)</th>
  <td>Bits typeclass law for LSB</td></tr>
  <tr><th>[\#14841](https://gitlab.haskell.org//ghc/ghc/issues/14841)</th>
  <td>Inconsistent allocation stats</td></tr>
  <tr><th>[\#14842](https://gitlab.haskell.org//ghc/ghc/issues/14842)</th>
  <td>Layout extensions missing from documentation</td></tr>
  <tr><th>[\#14848](https://gitlab.haskell.org//ghc/ghc/issues/14848)</th>
  <td>-XDuplicateRecordFields breaks record expression splices</td></tr>
  <tr><th>[\#14850](https://gitlab.haskell.org//ghc/ghc/issues/14850)</th>
  <td>mallocBytes allows underflow</td></tr>
  <tr><th>[\#14851](https://gitlab.haskell.org//ghc/ghc/issues/14851)</th>
  <td>"Pattern match has inaccessible right hand side" with TypeRep</td></tr>
  <tr><th>[\#14854](https://gitlab.haskell.org//ghc/ghc/issues/14854)</th>
  <td>The size of FastString table is suboptimal for large codebases</td></tr>
  <tr><th>[\#14856](https://gitlab.haskell.org//ghc/ghc/issues/14856)</th>
  <td>GHC API: Linker failure on loading target multiple times</td></tr>
  <tr><th>[\#14858](https://gitlab.haskell.org//ghc/ghc/issues/14858)</th>
  <td>Typed hole subtitution search fails in the REPL</td></tr>
  <tr><th>[\#14859](https://gitlab.haskell.org//ghc/ghc/issues/14859)</th>
  <td>Allow explicit impredicativity</td></tr>
  <tr><th>[\#14860](https://gitlab.haskell.org//ghc/ghc/issues/14860)</th>
  <td>QuantifiedConstraints: Can't quantify constraint involving type family</td></tr>
  <tr><th>[\#14865](https://gitlab.haskell.org//ghc/ghc/issues/14865)</th>
  <td>GHC Defeats Manual Worker Wrapper with Unboxed Sum</td></tr>
  <tr><th>[\#14867](https://gitlab.haskell.org//ghc/ghc/issues/14867)</th>
  <td>Documentation targets are not working</td></tr>
  <tr><th>[\#14870](https://gitlab.haskell.org//ghc/ghc/issues/14870)</th>
  <td>Runtime performance regression in 8.4</td></tr>
  <tr><th>[\#14871](https://gitlab.haskell.org//ghc/ghc/issues/14871)</th>
  <td>With process substitiution, ghc didn't read filedescriptor.</td></tr>
  <tr><th>[\#14873](https://gitlab.haskell.org//ghc/ghc/issues/14873)</th>
  <td>The well-kinded type invariant (in TcType)</td></tr>
  <tr><th>[\#14874](https://gitlab.haskell.org//ghc/ghc/issues/14874)</th>
  <td>Trac: TypeError: can't compare datetime.datetime to str</td></tr>
  <tr><th>[\#14876](https://gitlab.haskell.org//ghc/ghc/issues/14876)</th>
  <td>Reading source files in text mode so that we get CRLF conversion under Windows?</td></tr>
  <tr><th>[\#14877](https://gitlab.haskell.org//ghc/ghc/issues/14877)</th>
  <td>QuantifiedConstraints: Can't deduce \`xx' from \`(xx =\> a, xx)'</td></tr>
  <tr><th>[\#14879](https://gitlab.haskell.org//ghc/ghc/issues/14879)</th>
  <td>QuantifiedConstraints: Big error message + can't substitute (=\>) with a class alias</td></tr>
  <tr><th>[\#14892](https://gitlab.haskell.org//ghc/ghc/issues/14892)</th>
  <td>Field imposters with DuplicateRecordFields and NamedFieldPuns.</td></tr>
  <tr><th>[\#14895](https://gitlab.haskell.org//ghc/ghc/issues/14895)</th>
  <td>STG CSE makes dead binders undead</td></tr>
  <tr><th>[\#14896](https://gitlab.haskell.org//ghc/ghc/issues/14896)</th>
  <td>QuantifiedConstraints: GHC does doesn't discharge constraints if they are quantified</td></tr>
  <tr><th>[\#14899](https://gitlab.haskell.org//ghc/ghc/issues/14899)</th>
  <td>Significant compilation time regression between 8.4 and HEAD due to coverage checking</td></tr>
  <tr><th>[\#14901](https://gitlab.haskell.org//ghc/ghc/issues/14901)</th>
  <td>dsrun014 fails with most ways</td></tr>
  <tr><th>[\#14902](https://gitlab.haskell.org//ghc/ghc/issues/14902)</th>
  <td>GHC HEAD cannot be booted with GHC 8.4.1</td></tr>
  <tr><th>[\#14908](https://gitlab.haskell.org//ghc/ghc/issues/14908)</th>
  <td>Compiling using O1 works but panic using O2 or O3</td></tr>
  <tr><th>[\#14913](https://gitlab.haskell.org//ghc/ghc/issues/14913)</th>
  <td>testsuite driver does not honor \`extra_run_opts\` for the ghci way</td></tr>
  <tr><th>[\#14920](https://gitlab.haskell.org//ghc/ghc/issues/14920)</th>
  <td>Flag reference doesn't include \`-haddock\` and \`-haddock-opts\`</td></tr>
  <tr><th>[\#14923](https://gitlab.haskell.org//ghc/ghc/issues/14923)</th>
  <td>Recompilation avoidance fails after a LANGUAGE change</td></tr>
  <tr><th>[\#14926](https://gitlab.haskell.org//ghc/ghc/issues/14926)</th>
  <td>failed to build cross-compiler</td></tr>
  <tr><th>[\#14929](https://gitlab.haskell.org//ghc/ghc/issues/14929)</th>
  <td>Program compiled with -O2 exhibits much worse performance</td></tr>
  <tr><th>[\#14935](https://gitlab.haskell.org//ghc/ghc/issues/14935)</th>
  <td>Vary default RTS settings so that performance does not degrade with increasing number of capabilities</td></tr>
  <tr><th>[\#14939](https://gitlab.haskell.org//ghc/ghc/issues/14939)</th>
  <td>Lint error in forall type</td></tr>
  <tr><th>[\#14940](https://gitlab.haskell.org//ghc/ghc/issues/14940)</th>
  <td>GHC doesn't accept url specifying output file that is in a directory that is created by the -outputdir flag</td></tr>
  <tr><th>[\#14941](https://gitlab.haskell.org//ghc/ghc/issues/14941)</th>
  <td>Switching direct type family application to EqPred (\~) prevents inlining in code using vector (10x slowdown)</td></tr>
  <tr><th>[\#14943](https://gitlab.haskell.org//ghc/ghc/issues/14943)</th>
  <td>Make (=\>) polykinded (:: k -\> k -\> Constraint)</td></tr>
  <tr><th>[\#14944](https://gitlab.haskell.org//ghc/ghc/issues/14944)</th>
  <td>Compile speed regression</td></tr>
  <tr><th>[\#14946](https://gitlab.haskell.org//ghc/ghc/issues/14946)</th>
  <td>GHC Calls CPP for HS with -undef</td></tr>
  <tr><th>[\#14948](https://gitlab.haskell.org//ghc/ghc/issues/14948)</th>
  <td>A program which benefits from a  late specialisation pass</td></tr>
  <tr><th>[\#14949](https://gitlab.haskell.org//ghc/ghc/issues/14949)</th>
  <td>Perform builds on non-Debian-based systems on Circle CI</td></tr>
  <tr><th>[\#14952](https://gitlab.haskell.org//ghc/ghc/issues/14952)</th>
  <td>Warning messages use white text which is unreadable on white background terminals</td></tr>
  <tr><th>[\#14956](https://gitlab.haskell.org//ghc/ghc/issues/14956)</th>
  <td>NUMA not detected on Aarch64 NUMA machine</td></tr>
  <tr><th>[\#14957](https://gitlab.haskell.org//ghc/ghc/issues/14957)</th>
  <td>Build failure in brew</td></tr>
  <tr><th>[\#14958](https://gitlab.haskell.org//ghc/ghc/issues/14958)</th>
  <td>QuantifiedConstraints: Doesn't apply implication for existential?</td></tr>
  <tr><th>[\#14960](https://gitlab.haskell.org//ghc/ghc/issues/14960)</th>
  <td>Invalid law for MonadPlus: v \>\> mzero = mzero</td></tr>
  <tr><th>[\#14963](https://gitlab.haskell.org//ghc/ghc/issues/14963)</th>
  <td>ghci -fdefer-type-errors can't run IO action from another module</td></tr>
  <tr><th>[\#14966](https://gitlab.haskell.org//ghc/ghc/issues/14966)</th>
  <td>Symbols in -ddump-asm output don't match real output</td></tr>
  <tr><th>[\#14968](https://gitlab.haskell.org//ghc/ghc/issues/14968)</th>
  <td>QuantifiedConstraints: Can't be RHS of type family instances</td></tr>
  <tr><th>[\#14974](https://gitlab.haskell.org//ghc/ghc/issues/14974)</th>
  <td>2-fold memory usage regression GHC 8.2.2 -\> GHC 8.4.1 compiling \`mmark\` package</td></tr>
  <tr><th>[\#14980](https://gitlab.haskell.org//ghc/ghc/issues/14980)</th>
  <td>Runtime performance regression with binary operations on vectors</td></tr>
  <tr><th>[\#14981](https://gitlab.haskell.org//ghc/ghc/issues/14981)</th>
  <td>GHC parallel GC is not doing well on modern many-core machine</td></tr>
  <tr><th>[\#14982](https://gitlab.haskell.org//ghc/ghc/issues/14982)</th>
  <td>LLVM default -mcpu setting inhibits customization</td></tr>
  <tr><th>[\#14985](https://gitlab.haskell.org//ghc/ghc/issues/14985)</th>
  <td>GHC flags missing from the user guide flag reference.</td></tr>
  <tr><th>[\#14987](https://gitlab.haskell.org//ghc/ghc/issues/14987)</th>
  <td>Memory usage exploding for complex pattern matching</td></tr>
  <tr><th>[\#14988](https://gitlab.haskell.org//ghc/ghc/issues/14988)</th>
  <td>Memory strain while compiling HLint</td></tr>
  <tr><th>[\#14995](https://gitlab.haskell.org//ghc/ghc/issues/14995)</th>
  <td>QuantifiedConstraints: Incorrect pretty printing</td></tr>
  <tr><th>[\#14997](https://gitlab.haskell.org//ghc/ghc/issues/14997)</th>
  <td>mut_list_size calculation is off</td></tr>
  <tr><th>[\#14998](https://gitlab.haskell.org//ghc/ghc/issues/14998)</th>
  <td>Sort out the strictness mess for exceptions</td></tr>
  <tr><th>[\#15010](https://gitlab.haskell.org//ghc/ghc/issues/15010)</th>
  <td>Application (warp) server crashing periodically with "TSO object entered"</td></tr>
  <tr><th>[\#15014](https://gitlab.haskell.org//ghc/ghc/issues/15014)</th>
  <td>Exhaustivity check should suggest when COMPLETE could be helpful</td></tr>
  <tr><th>[\#15015](https://gitlab.haskell.org//ghc/ghc/issues/15015)</th>
  <td>Parsing of exp_doc loses section markers</td></tr>
  <tr><th>[\#15016](https://gitlab.haskell.org//ghc/ghc/issues/15016)</th>
  <td>Referencing a do-bound variable in a rec block with ApplicativeDo results in variable not in scope during type checking</td></tr>
  <tr><th>[\#15019](https://gitlab.haskell.org//ghc/ghc/issues/15019)</th>
  <td>Fix performance regressions from \#14737</td></tr>
  <tr><th>[\#15020](https://gitlab.haskell.org//ghc/ghc/issues/15020)</th>
  <td>PatternSynonyms: Problems with quantified constraints / foralls</td></tr>
  <tr><th>[\#15029](https://gitlab.haskell.org//ghc/ghc/issues/15029)</th>
  <td>haddock parsing fails with valid input</td></tr>
  <tr><th>[\#15034](https://gitlab.haskell.org//ghc/ghc/issues/15034)</th>
  <td>Desugaring \`mdo\` moves a \`let\` where it shouldn't be</td></tr>
  <tr><th>[\#15054](https://gitlab.haskell.org//ghc/ghc/issues/15054)</th>
  <td>ghc internal error appeared in GHCI</td></tr>
  <tr><th>[\#15056](https://gitlab.haskell.org//ghc/ghc/issues/15056)</th>
  <td>Wrappers inlined too late</td></tr>
  <tr><th>[\#15059](https://gitlab.haskell.org//ghc/ghc/issues/15059)</th>
  <td>ghcpkg05 fails</td></tr>
  <tr><th>[\#15064](https://gitlab.haskell.org//ghc/ghc/issues/15064)</th>
  <td>T8089 mysteriously fails when GHC is built with LLVM</td></tr>
  <tr><th>[\#15069](https://gitlab.haskell.org//ghc/ghc/issues/15069)</th>
  <td>Missed SpecConstr opportunity</td></tr>
  <tr><th>[\#15070](https://gitlab.haskell.org//ghc/ghc/issues/15070)</th>
  <td>postInlineUnconditionally is too eager</td></tr>
  <tr><th>[\#15074](https://gitlab.haskell.org//ghc/ghc/issues/15074)</th>
  <td>Possible uninitialised values in ffi64.c</td></tr>
  <tr><th>[\#15075](https://gitlab.haskell.org//ghc/ghc/issues/15075)</th>
  <td>Sometimes '-optl-optl' used for linker options instead of just '-optl'</td></tr>
  <tr><th>[\#15081](https://gitlab.haskell.org//ghc/ghc/issues/15081)</th>
  <td>Finite list becomes infinite after maping fractional function for high numbers</td></tr>
  <tr><th>[\#15084](https://gitlab.haskell.org//ghc/ghc/issues/15084)</th>
  <td>Functions in HsUtils don't have the most general type</td></tr>
  <tr><th>[\#15096](https://gitlab.haskell.org//ghc/ghc/issues/15096)</th>
  <td>GHC fails to execute gcc on Windows when unicode character is in the path</td></tr>
  <tr><th>[\#15100](https://gitlab.haskell.org//ghc/ghc/issues/15100)</th>
  <td>\`ApplicativeDo\` needlessly uses \`join\` too much</td></tr>
  <tr><th>[\#15113](https://gitlab.haskell.org//ghc/ghc/issues/15113)</th>
  <td>Do not make CAFs from literal strings</td></tr>
  <tr><th>[\#15127](https://gitlab.haskell.org//ghc/ghc/issues/15127)</th>
  <td>Unbox around runRW\#</td></tr>
  <tr><th>[\#15130](https://gitlab.haskell.org//ghc/ghc/issues/15130)</th>
  <td>Hadrian doesn't rebuild changed \`CoreUtils.hs\`</td></tr>
  <tr><th>[\#15135](https://gitlab.haskell.org//ghc/ghc/issues/15135)</th>
  <td>Overlapping typeclass instance selection depends on the optimisation level</td></tr>
  <tr><th>[\#15147](https://gitlab.haskell.org//ghc/ghc/issues/15147)</th>
  <td>Type checker plugin receives Wanteds that are not completely unflattened</td></tr>
  <tr><th>[\#15153](https://gitlab.haskell.org//ghc/ghc/issues/15153)</th>
  <td>GHC uses O_NONBLOCK on regular files, which has no effect, and blocks the runtime</td></tr>
  <tr><th>[\#15155](https://gitlab.haskell.org//ghc/ghc/issues/15155)</th>
  <td>How untagged pointers sneak into banged fields</td></tr>
  <tr><th>[\#15159](https://gitlab.haskell.org//ghc/ghc/issues/15159)</th>
  <td>Expand compatibility note for readMVar</td></tr>
  <tr><th>[\#15161](https://gitlab.haskell.org//ghc/ghc/issues/15161)</th>
  <td>ghci cannot find symbols defined by TH's addForeignFilePath</td></tr>
  <tr><th>[\#15167](https://gitlab.haskell.org//ghc/ghc/issues/15167)</th>
  <td>DerivClause list is not populated for (TyConI (DataD ...))</td></tr>
  <tr><th>[\#15175](https://gitlab.haskell.org//ghc/ghc/issues/15175)</th>
  <td>ghc: panic! (the 'impossible' happened)</td></tr>
  <tr><th>[\#15176](https://gitlab.haskell.org//ghc/ghc/issues/15176)</th>
  <td>Superclass \`Monad m =\>\` makes program run 100 times slower</td></tr>
  <tr><th>[\#15177](https://gitlab.haskell.org//ghc/ghc/issues/15177)</th>
  <td>Faulty instance termination check, with PolyKinds and/or TypeInType</td></tr>
  <tr><th>[\#15179](https://gitlab.haskell.org//ghc/ghc/issues/15179)</th>
  <td>Unwinding info for stg_ap_v_info is wrong</td></tr>
  <tr><th>[\#15184](https://gitlab.haskell.org//ghc/ghc/issues/15184)</th>
  <td>T4442 fails on i386</td></tr>
  <tr><th>[\#15185](https://gitlab.haskell.org//ghc/ghc/issues/15185)</th>
  <td>Enum instance for IntX / WordX are inefficient</td></tr>
  <tr><th>[\#15190](https://gitlab.haskell.org//ghc/ghc/issues/15190)</th>
  <td>disable haddock disables building of manuals</td></tr>
  <tr><th>[\#15191](https://gitlab.haskell.org//ghc/ghc/issues/15191)</th>
  <td>Deriving via DeriveAnyClass not behaving the same as an emply instance declaration</td></tr>
  <tr><th>[\#15193](https://gitlab.haskell.org//ghc/ghc/issues/15193)</th>
  <td>QSem makes nonsense claim</td></tr>
  <tr><th>[\#15199](https://gitlab.haskell.org//ghc/ghc/issues/15199)</th>
  <td>Build fails on Debian armhf (armv7l-unknown-linux-gnueabihf)</td></tr>
  <tr><th>[\#15200](https://gitlab.haskell.org//ghc/ghc/issues/15200)</th>
  <td>RFC: export WordPtr from Data.Word and IntPtr from Data.Int rather than  only from Foreign.Ptr</td></tr>
  <tr><th>[\#15201](https://gitlab.haskell.org//ghc/ghc/issues/15201)</th>
  <td>GHC 8.4 fails to build on Debian s390x</td></tr>
  <tr><th>[\#15203](https://gitlab.haskell.org//ghc/ghc/issues/15203)</th>
  <td>Wrong location reported for kind error</td></tr>
  <tr><th>[\#15205](https://gitlab.haskell.org//ghc/ghc/issues/15205)</th>
  <td>Unnecessary equality superclass</td></tr>
  <tr><th>[\#15208](https://gitlab.haskell.org//ghc/ghc/issues/15208)</th>
  <td>GHC 8.4 fails to build on Debian armel (softfloat)</td></tr>
  <tr><th>[\#15211](https://gitlab.haskell.org//ghc/ghc/issues/15211)</th>
  <td>exprFreeVars does not include type variables</td></tr>
  <tr><th>[\#15220](https://gitlab.haskell.org//ghc/ghc/issues/15220)</th>
  <td>ScopedTypeVariables binds a non-existent variable</td></tr>
  <tr><th>[\#15241](https://gitlab.haskell.org//ghc/ghc/issues/15241)</th>
  <td>Validate failures in sanity way</td></tr>
  <tr><th>[\#15248](https://gitlab.haskell.org//ghc/ghc/issues/15248)</th>
  <td>Coercions from plugins cannot be stopped from floating out</td></tr>
  <tr><th>[\#15250](https://gitlab.haskell.org//ghc/ghc/issues/15250)</th>
  <td>Add support for _mm512_shuffle_epi8 intrinsic</td></tr>
  <tr><th>[\#15251](https://gitlab.haskell.org//ghc/ghc/issues/15251)</th>
  <td>Add support for _mm_shuffle_pi8 intrinsic</td></tr>
  <tr><th>[\#15252](https://gitlab.haskell.org//ghc/ghc/issues/15252)</th>
  <td>syn_arg_wraps and syn_res_wrap are only populated after typechecking</td></tr>
  <tr><th>[\#15256](https://gitlab.haskell.org//ghc/ghc/issues/15256)</th>
  <td>GHCi check .ghci permission on WSL(Linux on Windows)</td></tr>
  <tr><th>[\#15257](https://gitlab.haskell.org//ghc/ghc/issues/15257)</th>
  <td>Broken symlinks in lndir build tree</td></tr>
  <tr><th>[\#15262](https://gitlab.haskell.org//ghc/ghc/issues/15262)</th>
  <td>GHC and iserv cannot agree on what an Integer is; insanity ensues</td></tr>
  <tr><th>[\#15270](https://gitlab.haskell.org//ghc/ghc/issues/15270)</th>
  <td>TH doesn't verify name types during conversion</td></tr>
  <tr><th>[\#15275](https://gitlab.haskell.org//ghc/ghc/issues/15275)</th>
  <td>AArch64 validation fails with many invalid relocations</td></tr>
  <tr><th>[\#15286](https://gitlab.haskell.org//ghc/ghc/issues/15286)</th>
  <td>"Can't use Natural in base" when compiling GHC.Natural with -O0</td></tr>
  <tr><th>[\#15287](https://gitlab.haskell.org//ghc/ghc/issues/15287)</th>
  <td>T11627\[ab\] fail on some Darwin environments</td></tr>
  <tr><th>[\#15288](https://gitlab.haskell.org//ghc/ghc/issues/15288)</th>
  <td>Figure out what to do about retainer profiling debugging code</td></tr>
  <tr><th>[\#15291](https://gitlab.haskell.org//ghc/ghc/issues/15291)</th>
  <td>Incorrect SCC name parsing according to user manual</td></tr>
  <tr><th>[\#15295](https://gitlab.haskell.org//ghc/ghc/issues/15295)</th>
  <td>Haddock options should be concatenated</td></tr>
  <tr><th>[\#15304](https://gitlab.haskell.org//ghc/ghc/issues/15304)</th>
  <td>Huge increase of compile time and memory use from 8.0.2 to 8.2.2 or 8.4.2</td></tr>
  <tr><th>[\#15309](https://gitlab.haskell.org//ghc/ghc/issues/15309)</th>
  <td>mkLHsOpTy is discarding API Annotations</td></tr>
  <tr><th>[\#15312](https://gitlab.haskell.org//ghc/ghc/issues/15312)</th>
  <td>getChanContents exception behavior seems a bit odd</td></tr>
  <tr><th>[\#15313](https://gitlab.haskell.org//ghc/ghc/issues/15313)</th>
  <td>Framework failures on windows with plugins</td></tr>
  <tr><th>[\#15322](https://gitlab.haskell.org//ghc/ghc/issues/15322)</th>
  <td>\`KnownNat\` does not imply \`Typeable\` any more when used with plugin</td></tr>
  <tr><th>[\#15328](https://gitlab.haskell.org//ghc/ghc/issues/15328)</th>
  <td>cpphs: internal error: evacuate(static): strange closure type 8440</td></tr>
  <tr><th>[\#15333](https://gitlab.haskell.org//ghc/ghc/issues/15333)</th>
  <td>Nursery size adulterates cachegrind metrics in nofib</td></tr>
  <tr><th>[\#15336](https://gitlab.haskell.org//ghc/ghc/issues/15336)</th>
  <td>./System/IO.hs accidentally overridden when running ghci</td></tr>
  <tr><th>[\#15337](https://gitlab.haskell.org//ghc/ghc/issues/15337)</th>
  <td>Warning not showing up when deprecated variable is explicitly imported</td></tr>
  <tr><th>[\#15338](https://gitlab.haskell.org//ghc/ghc/issues/15338)</th>
  <td>ghc-pkg misbehaves after possible miscompilation on m68k and sh4</td></tr>
  <tr><th>[\#15344](https://gitlab.haskell.org//ghc/ghc/issues/15344)</th>
  <td>ApplicativeDo seems to prevent the fail method from being used</td></tr>
  <tr><th>[\#15347](https://gitlab.haskell.org//ghc/ghc/issues/15347)</th>
  <td>QuantifiedConstraints: Implication constraints with type families don't work</td></tr>
  <tr><th>[\#15350](https://gitlab.haskell.org//ghc/ghc/issues/15350)</th>
  <td>gcdExtInteger violates assertion</td></tr>
  <tr><th>[\#15351](https://gitlab.haskell.org//ghc/ghc/issues/15351)</th>
  <td>QuantifiedConstraints ignore FunctionalDependencies</td></tr>
  <tr><th>[\#15354](https://gitlab.haskell.org//ghc/ghc/issues/15354)</th>
  <td>QuantifiedConstraints not fully described in manual</td></tr>
  <tr><th>[\#15356](https://gitlab.haskell.org//ghc/ghc/issues/15356)</th>
  <td>Template Haskell should turn off RebindableSyntax in quotes</td></tr>
  <tr><th>[\#15366](https://gitlab.haskell.org//ghc/ghc/issues/15366)</th>
  <td>GHC.Conc.Windows has a surprising queue</td></tr>
  <tr><th>[\#15375](https://gitlab.haskell.org//ghc/ghc/issues/15375)</th>
  <td>GHC.Exts.Heap.getClosureData doesn't return the payload for AP_STACK</td></tr>
  <tr><th>[\#15376](https://gitlab.haskell.org//ghc/ghc/issues/15376)</th>
  <td>GHC determine illegal kind for standalone deriving with Deriving via</td></tr>
  <tr><th>[\#15377](https://gitlab.haskell.org//ghc/ghc/issues/15377)</th>
  <td>Cut an STM release</td></tr>
  <tr><th>[\#15382](https://gitlab.haskell.org//ghc/ghc/issues/15382)</th>
  <td>heapprof001 segfaults in prof_hc_hb way on i386</td></tr>
  <tr><th>[\#15383](https://gitlab.haskell.org//ghc/ghc/issues/15383)</th>
  <td>T3171 doesn't terminate with Interrupted message on Darwin</td></tr>
  <tr><th>[\#15388](https://gitlab.haskell.org//ghc/ghc/issues/15388)</th>
  <td>GHC reports missing INLINABLE pragmas in vector and ghc-prim</td></tr>
  <tr><th>[\#15389](https://gitlab.haskell.org//ghc/ghc/issues/15389)</th>
  <td>-Wall-missed-specialisations warnings not fatal with -Werror</td></tr>
  <tr><th>[\#15391](https://gitlab.haskell.org//ghc/ghc/issues/15391)</th>
  <td>Maybe ghc-pkg register should unregister packages with "incompatible" signatures</td></tr>
  <tr><th>[\#15397](https://gitlab.haskell.org//ghc/ghc/issues/15397)</th>
  <td>Linking Issue on Ubuntu and Fedora with Provided Bindists (GHC-8.4.2)</td></tr>
  <tr><th>[\#15399](https://gitlab.haskell.org//ghc/ghc/issues/15399)</th>
  <td>Build failure on PowerPC 64-bit big endian</td></tr>
  <tr><th>[\#15402](https://gitlab.haskell.org//ghc/ghc/issues/15402)</th>
  <td>The settings and behaviour of idle GC are very confusing</td></tr>
  <tr><th>[\#15409](https://gitlab.haskell.org//ghc/ghc/issues/15409)</th>
  <td>Quantified constraints half-work with equality constraints</td></tr>
  <tr><th>[\#15411](https://gitlab.haskell.org//ghc/ghc/issues/15411)</th>
  <td>urk! lookup local fingerprint</td></tr>
  <tr><th>[\#15416](https://gitlab.haskell.org//ghc/ghc/issues/15416)</th>
  <td>Higher rank types in pattern synonyms</td></tr>
  <tr><th>[\#15417](https://gitlab.haskell.org//ghc/ghc/issues/15417)</th>
  <td>Weak pointers to static objects are sometimes not detected as unreachable</td></tr>
  <tr><th>[\#15418](https://gitlab.haskell.org//ghc/ghc/issues/15418)</th>
  <td>Performance drop 60 times on non-profiling binary</td></tr>
  <tr><th>[\#15420](https://gitlab.haskell.org//ghc/ghc/issues/15420)</th>
  <td>executable with library and template haskell doesn't compile statically</td></tr>
  <tr><th>[\#15425](https://gitlab.haskell.org//ghc/ghc/issues/15425)</th>
  <td>GHC does not warn when two conflicting packages with same name are given via a dependent package and -package-db</td></tr>
  <tr><th>[\#15427](https://gitlab.haskell.org//ghc/ghc/issues/15427)</th>
  <td>Calling hs_try_putmvar from an unsafe foreign call can cause the RTS to hang</td></tr>
  <tr><th>[\#15433](https://gitlab.haskell.org//ghc/ghc/issues/15433)</th>
  <td>Internal error with PartialTypeSignatures and TH</td></tr>
  <tr><th>[\#15434](https://gitlab.haskell.org//ghc/ghc/issues/15434)</th>
  <td>DerivingVia (and perhaps even GND) works badly with DeriveGeneric</td></tr>
  <tr><th>[\#15435](https://gitlab.haskell.org//ghc/ghc/issues/15435)</th>
  <td>Make nofib-style anaysis for perf/compiler</td></tr>
  <tr><th>[\#15437](https://gitlab.haskell.org//ghc/ghc/issues/15437)</th>
  <td>Internal error when applying a scoped type variable inside a typed expression quotation</td></tr>
  <tr><th>[\#15442](https://gitlab.haskell.org//ghc/ghc/issues/15442)</th>
  <td>GhcStage3HcOpts passed to ghc-stage1</td></tr>
  <tr><th>[\#15444](https://gitlab.haskell.org//ghc/ghc/issues/15444)</th>
  <td>8.4.3 has an undocumented dependency on libnuma.</td></tr>
  <tr><th>[\#15449](https://gitlab.haskell.org//ghc/ghc/issues/15449)</th>
  <td>Nondeterministic Failure on aarch64 with -jn, n \> 1</td></tr>
  <tr><th>[\#15455](https://gitlab.haskell.org//ghc/ghc/issues/15455)</th>
  <td>Memory usage when compiling jsaddle-dom exploded in 8.4.3</td></tr>
  <tr><th>[\#15459](https://gitlab.haskell.org//ghc/ghc/issues/15459)</th>
  <td>Wredundant-constraints does not work when constraint synonym is used</td></tr>
  <tr><th>[\#15462](https://gitlab.haskell.org//ghc/ghc/issues/15462)</th>
  <td>fixST for lazy ST is a bit wrong</td></tr>
  <tr><th>[\#15464](https://gitlab.haskell.org//ghc/ghc/issues/15464)</th>
  <td>Template Haskell creates System names when it shouldn't</td></tr>
  <tr><th>[\#15465](https://gitlab.haskell.org//ghc/ghc/issues/15465)</th>
  <td>PAP object entered</td></tr>
  <tr><th>[\#15466](https://gitlab.haskell.org//ghc/ghc/issues/15466)</th>
  <td>debug validation failures</td></tr>
  <tr><th>[\#15467](https://gitlab.haskell.org//ghc/ghc/issues/15467)</th>
  <td>unregisterised validation failures</td></tr>
  <tr><th>[\#15474](https://gitlab.haskell.org//ghc/ghc/issues/15474)</th>
  <td>Error message mentions Any</td></tr>
  <tr><th>[\#15477](https://gitlab.haskell.org//ghc/ghc/issues/15477)</th>
  <td>Can't build \`prof\`-flavour with \`-fauto-all\`</td></tr>
  <tr><th>[\#15485](https://gitlab.haskell.org//ghc/ghc/issues/15485)</th>
  <td>GHC uses 300% CPU when calling into blocking C call</td></tr>
  <tr><th>[\#15488](https://gitlab.haskell.org//ghc/ghc/issues/15488)</th>
  <td>GHC takes up huge amount of memory when compiling accelerate 1.2.0</td></tr>
  <tr><th>[\#15494](https://gitlab.haskell.org//ghc/ghc/issues/15494)</th>
  <td>Cannot install GHC through stack on NixOS</td></tr>
  <tr><th>[\#15498](https://gitlab.haskell.org//ghc/ghc/issues/15498)</th>
  <td>HPC: do notation marks () as non-covered</td></tr>
  <tr><th>[\#15501](https://gitlab.haskell.org//ghc/ghc/issues/15501)</th>
  <td>Fix unknown symbols/addresses in perf output</td></tr>
  <tr><th>[\#15503](https://gitlab.haskell.org//ghc/ghc/issues/15503)</th>
  <td>interpreter: sequence_ (replicate 100000000 (return ()))  gobbles up memory</td></tr>
  <tr><th>[\#15504](https://gitlab.haskell.org//ghc/ghc/issues/15504)</th>
  <td>-XStrict doesn't prevent warnings from -Wunbanged-strict-patterns</td></tr>
  <tr><th>[\#15508](https://gitlab.haskell.org//ghc/ghc/issues/15508)</th>
  <td>concprog001 fails with various errors</td></tr>
  <tr><th>[\#15516](https://gitlab.haskell.org//ghc/ghc/issues/15516)</th>
  <td>ghci: dynamic linking fails on osx</td></tr>
  <tr><th>[\#15519](https://gitlab.haskell.org//ghc/ghc/issues/15519)</th>
  <td>Minor code refactoring leads to drastic performance degradation</td></tr>
  <tr><th>[\#15522](https://gitlab.haskell.org//ghc/ghc/issues/15522)</th>
  <td>Cannot bind symbolic names in a rule</td></tr>
  <tr><th>[\#15524](https://gitlab.haskell.org//ghc/ghc/issues/15524)</th>
  <td>Performance regression when using the GHC API to evaluate code compared to 8.4</td></tr>
  <tr><th>[\#15531](https://gitlab.haskell.org//ghc/ghc/issues/15531)</th>
  <td>CApiFFI generates bad prototypes for pointers of \`Foreign.C\` types</td></tr>
  <tr><th>[\#15538](https://gitlab.haskell.org//ghc/ghc/issues/15538)</th>
  <td>GHC boot script can't handle Git remote not named origin</td></tr>
  <tr><th>[\#15540](https://gitlab.haskell.org//ghc/ghc/issues/15540)</th>
  <td>GHCi does not follow the XDG Base Directory Specification</td></tr>
  <tr><th>[\#15541](https://gitlab.haskell.org//ghc/ghc/issues/15541)</th>
  <td>package environment files and the GHC API</td></tr>
  <tr><th>[\#15542](https://gitlab.haskell.org//ghc/ghc/issues/15542)</th>
  <td>DuplicateRecordFields not honored within a data family?</td></tr>
  <tr><th>[\#15543](https://gitlab.haskell.org//ghc/ghc/issues/15543)</th>
  <td>Binary crashes under dtrace</td></tr>
  <tr><th>[\#15549](https://gitlab.haskell.org//ghc/ghc/issues/15549)</th>
  <td>Core Lint error with EmptyCase</td></tr>
  <tr><th>[\#15552](https://gitlab.haskell.org//ghc/ghc/issues/15552)</th>
  <td>Infinite loop/panic with an existential type.</td></tr>
  <tr><th>[\#15553](https://gitlab.haskell.org//ghc/ghc/issues/15553)</th>
  <td>GHC.IO.Encoding not flushing partially converted input</td></tr>
  <tr><th>[\#15554](https://gitlab.haskell.org//ghc/ghc/issues/15554)</th>
  <td>COMPLETE pragmas make overlapping-patterns warnings behave oddly</td></tr>
  <tr><th>[\#15560](https://gitlab.haskell.org//ghc/ghc/issues/15560)</th>
  <td>Full laziness destroys opportunities for join points</td></tr>
  <tr><th>[\#15561](https://gitlab.haskell.org//ghc/ghc/issues/15561)</th>
  <td>TypeInType: Type error conditioned on ordering of GADT and type family definitions</td></tr>
  <tr><th>[\#15562](https://gitlab.haskell.org//ghc/ghc/issues/15562)</th>
  <td>\`-XStrict -XNoStrict\` is not neutral</td></tr>
  <tr><th>[\#15567](https://gitlab.haskell.org//ghc/ghc/issues/15567)</th>
  <td>security of package environment files</td></tr>
  <tr><th>[\#15570](https://gitlab.haskell.org//ghc/ghc/issues/15570)</th>
  <td>Core transformations generate bad indexCharOffAddr\# call</td></tr>
  <tr><th>[\#15574](https://gitlab.haskell.org//ghc/ghc/issues/15574)</th>
  <td>C wrappers for Haskell foreign exports don't have finalizers (causes memory leak).</td></tr>
  <tr><th>[\#15576](https://gitlab.haskell.org//ghc/ghc/issues/15576)</th>
  <td>Hadrian puts its build tree in the wrong place</td></tr>
  <tr><th>[\#15577](https://gitlab.haskell.org//ghc/ghc/issues/15577)</th>
  <td>TypeApplications-related infinite loop (GHC 8.6+ only)</td></tr>
  <tr><th>[\#15578](https://gitlab.haskell.org//ghc/ghc/issues/15578)</th>
  <td>Honour INLINE pragmas on 0-arity bindings</td></tr>
  <tr><th>[\#15582](https://gitlab.haskell.org//ghc/ghc/issues/15582)</th>
  <td>Phabricator shows "drafts" by default</td></tr>
  <tr><th>[\#15587](https://gitlab.haskell.org//ghc/ghc/issues/15587)</th>
  <td>traceEvent tests failing in slow validate</td></tr>
  <tr><th>[\#15588](https://gitlab.haskell.org//ghc/ghc/issues/15588)</th>
  <td>Panic when abusing kind inference</td></tr>
  <tr><th>[\#15589](https://gitlab.haskell.org//ghc/ghc/issues/15589)</th>
  <td>Always promoting metavariables during type inference may be wrong</td></tr>
  <tr><th>[\#15594](https://gitlab.haskell.org//ghc/ghc/issues/15594)</th>
  <td>--abi-hash with Backpack incorrectly loads modules from dependent packages</td></tr>
  <tr><th>[\#15595](https://gitlab.haskell.org//ghc/ghc/issues/15595)</th>
  <td>Stack overflow in withArgs leads to infinite memory-consuming loop</td></tr>
  <tr><th>[\#15598](https://gitlab.haskell.org//ghc/ghc/issues/15598)</th>
  <td>RebindableSyntax with RankNTypes and type class method call yields panic.</td></tr>
  <tr><th>[\#15599](https://gitlab.haskell.org//ghc/ghc/issues/15599)</th>
  <td>typeclass inference depends on whether a module is exposed.</td></tr>
  <tr><th>[\#15602](https://gitlab.haskell.org//ghc/ghc/issues/15602)</th>
  <td>PAP invariant of pointer tagging does not hold in profiling builds</td></tr>
  <tr><th>[\#15603](https://gitlab.haskell.org//ghc/ghc/issues/15603)</th>
  <td>ref6 example from StaticPointers documentation doesn't type check</td></tr>
  <tr><th>[\#15605](https://gitlab.haskell.org//ghc/ghc/issues/15605)</th>
  <td>Documentation of atomicModifyMutVar\# does not show properly</td></tr>
  <tr><th>[\#15606](https://gitlab.haskell.org//ghc/ghc/issues/15606)</th>
  <td>Don't float out lets in between lambdsa</td></tr>
  <tr><th>[\#15612](https://gitlab.haskell.org//ghc/ghc/issues/15612)</th>
  <td>Got Unable to commit 16777216 bytes of memory error on Ubuntu</td></tr>
  <tr><th>[\#15616](https://gitlab.haskell.org//ghc/ghc/issues/15616)</th>
  <td>Bug when using TimerManager/GHC.Event ?</td></tr>
  <tr><th>[\#15617](https://gitlab.haskell.org//ghc/ghc/issues/15617)</th>
  <td>Unboxed tuples/sum error message on \`a = show 5\` in expression evaluation and interactive modes</td></tr>
  <tr><th>[\#15619](https://gitlab.haskell.org//ghc/ghc/issues/15619)</th>
  <td>List comprehension seems to prevent some rewrite rules to fire</td></tr>
  <tr><th>[\#15620](https://gitlab.haskell.org//ghc/ghc/issues/15620)</th>
  <td>Speed up Data.Unique</td></tr>
  <tr><th>[\#15621](https://gitlab.haskell.org//ghc/ghc/issues/15621)</th>
  <td>Error message involving type families points to wrong location</td></tr>
  <tr><th>[\#15626](https://gitlab.haskell.org//ghc/ghc/issues/15626)</th>
  <td>Optimise wakeups for STM</td></tr>
  <tr><th>[\#15630](https://gitlab.haskell.org//ghc/ghc/issues/15630)</th>
  <td>panic! Simplifier ticks exhausted</td></tr>
  <tr><th>[\#15632](https://gitlab.haskell.org//ghc/ghc/issues/15632)</th>
  <td>Undependable Dependencies</td></tr>
  <tr><th>[\#15634](https://gitlab.haskell.org//ghc/ghc/issues/15634)</th>
  <td>GHCi: Segmentation fault Data.List.sum large number</td></tr>
  <tr><th>[\#15639](https://gitlab.haskell.org//ghc/ghc/issues/15639)</th>
  <td>Surprising failure combining QuantifiedConstraints with Coercible</td></tr>
  <tr><th>[\#15643](https://gitlab.haskell.org//ghc/ghc/issues/15643)</th>
  <td>Test Suite Unexpected failure for ghci063(ghci)</td></tr>
  <tr><th>[\#15644](https://gitlab.haskell.org//ghc/ghc/issues/15644)</th>
  <td>Test Suite Unexpected failure for ghci062(ghci-ext)</td></tr>
  <tr><th>[\#15646](https://gitlab.haskell.org//ghc/ghc/issues/15646)</th>
  <td>ghci takes super long time to find the type of large fractional number</td></tr>
  <tr><th>[\#15652](https://gitlab.haskell.org//ghc/ghc/issues/15652)</th>
  <td>SerializedCompact has a \[(Ptr a, Word)\] instead of a custom datatype</td></tr>
  <tr><th>[\#15653](https://gitlab.haskell.org//ghc/ghc/issues/15653)</th>
  <td>Both \`Ptr a\` in SerializedCompact are inaccurate because of the \`a\`</td></tr>
  <tr><th>[\#15655](https://gitlab.haskell.org//ghc/ghc/issues/15655)</th>
  <td>Simpliify tcTyConScopedTyVars</td></tr>
  <tr><th>[\#15660](https://gitlab.haskell.org//ghc/ghc/issues/15660)</th>
  <td>source file modify race leads to inconsistent error message</td></tr>
  <tr><th>[\#15661](https://gitlab.haskell.org//ghc/ghc/issues/15661)</th>
  <td>Nullary constraint in GHCi breaks \`:t\` command</td></tr>
  <tr><th>[\#15663](https://gitlab.haskell.org//ghc/ghc/issues/15663)</th>
  <td>T9675 inexplicably regressed in allocations due to text submodule bump</td></tr>
  <tr><th>[\#15668](https://gitlab.haskell.org//ghc/ghc/issues/15668)</th>
  <td>Allocations values for some compile tests are way too hight</td></tr>
  <tr><th>[\#15670](https://gitlab.haskell.org//ghc/ghc/issues/15670)</th>
  <td>FloatFnInverses seems to show some weird rounding/precision issues.</td></tr>
  <tr><th>[\#15672](https://gitlab.haskell.org//ghc/ghc/issues/15672)</th>
  <td>Flags missing documentation.</td></tr>
  <tr><th>[\#15676](https://gitlab.haskell.org//ghc/ghc/issues/15676)</th>
  <td>Users guide: broken external links</td></tr>
  <tr><th>[\#15677](https://gitlab.haskell.org//ghc/ghc/issues/15677)</th>
  <td>Valid hole fits and GADT type variable names</td></tr>
  <tr><th>[\#15678](https://gitlab.haskell.org//ghc/ghc/issues/15678)</th>
  <td>Provide the provenance of unification variables in error messages when possible</td></tr>
  <tr><th>[\#15679](https://gitlab.haskell.org//ghc/ghc/issues/15679)</th>
  <td>Use String rather than \[Char\] where possible</td></tr>
  <tr><th>[\#15680](https://gitlab.haskell.org//ghc/ghc/issues/15680)</th>
  <td>Flag for printing absolute paths in diagnostics</td></tr>
  <tr><th>[\#15681](https://gitlab.haskell.org//ghc/ghc/issues/15681)</th>
  <td>Take exhaustiveness checking into consideration when using MonadFailDesugaring</td></tr>
  <tr><th>[\#15682](https://gitlab.haskell.org//ghc/ghc/issues/15682)</th>
  <td>evolve / improve Native Gen Format in Format.hs (especially in context of post simd cleanup)</td></tr>
  <tr><th>[\#15683](https://gitlab.haskell.org//ghc/ghc/issues/15683)</th>
  <td>coerce fails for Coercible type families</td></tr>
  <tr><th>[\#15684](https://gitlab.haskell.org//ghc/ghc/issues/15684)</th>
  <td>Add tests for SIMD loads and stores</td></tr>
  <tr><th>[\#15688](https://gitlab.haskell.org//ghc/ghc/issues/15688)</th>
  <td>HAVE_LIBNUMA is defined as non-zero even when libnuma does not exist</td></tr>
  <tr><th>[\#15689](https://gitlab.haskell.org//ghc/ghc/issues/15689)</th>
  <td>s390x builds flood with -Wunused-label warnings</td></tr>
  <tr><th>[\#15693](https://gitlab.haskell.org//ghc/ghc/issues/15693)</th>
  <td>Abstracting out pattern into a pattern synonym fails with scary error</td></tr>
  <tr><th>[\#15694](https://gitlab.haskell.org//ghc/ghc/issues/15694)</th>
  <td>GHC panic from pattern synonym, "Type-correct unfilled coercion hole"</td></tr>
  <tr><th>[\#15697](https://gitlab.haskell.org//ghc/ghc/issues/15697)</th>
  <td>Typed holes inferring a more polymorphic type</td></tr>
  <tr><th>[\#15699](https://gitlab.haskell.org//ghc/ghc/issues/15699)</th>
  <td>Run sanity checker in more testsuite runs</td></tr>
  <tr><th>[\#15703](https://gitlab.haskell.org//ghc/ghc/issues/15703)</th>
  <td>Significant compilation time blowup when refactoring singletons-heavy code</td></tr>
  <tr><th>[\#15705](https://gitlab.haskell.org//ghc/ghc/issues/15705)</th>
  <td>Confusing parser error in 8.6</td></tr>
  <tr><th>[\#15708](https://gitlab.haskell.org//ghc/ghc/issues/15708)</th>
  <td>Cross-module SPECIALZE pragmas aren't typechecked in -O0</td></tr>
  <tr><th>[\#15710](https://gitlab.haskell.org//ghc/ghc/issues/15710)</th>
  <td>Should GHC accept a type signature that needs coercion quantification?</td></tr>
  <tr><th>[\#15712](https://gitlab.haskell.org//ghc/ghc/issues/15712)</th>
  <td>GHC panic with -XDerivingVia</td></tr>
  <tr><th>[\#15713](https://gitlab.haskell.org//ghc/ghc/issues/15713)</th>
  <td>Bogus -Woverlapping-patterns warning with OverloadedStrings</td></tr>
  <tr><th>[\#15717](https://gitlab.haskell.org//ghc/ghc/issues/15717)</th>
  <td>Performance regression in for_ alternatives from GHC 8.2.2 to newer GHCs</td></tr>
  <tr><th>[\#15720](https://gitlab.haskell.org//ghc/ghc/issues/15720)</th>
  <td>Assign to literals is allowed in ghci</td></tr>
  <tr><th>[\#15727](https://gitlab.haskell.org//ghc/ghc/issues/15727)</th>
  <td>bug: all generations are collected sequentially when compacting collection kicks in</td></tr>
  <tr><th>[\#15730](https://gitlab.haskell.org//ghc/ghc/issues/15730)</th>
  <td>SCC/HPC/CORE annotation can change the meaning of an expression</td></tr>
  <tr><th>[\#15732](https://gitlab.haskell.org//ghc/ghc/issues/15732)</th>
  <td>getArgsWithResponseFiles does not filter out RTS options</td></tr>
  <tr><th>[\#15734](https://gitlab.haskell.org//ghc/ghc/issues/15734)</th>
  <td>ghc-heap package is not on hackage</td></tr>
  <tr><th>[\#15735](https://gitlab.haskell.org//ghc/ghc/issues/15735)</th>
  <td>ghc-heap doesn't support profiling</td></tr>
  <tr><th>[\#15736](https://gitlab.haskell.org//ghc/ghc/issues/15736)</th>
  <td>Testsuite failures from validate --slow</td></tr>
  <tr><th>[\#15739](https://gitlab.haskell.org//ghc/ghc/issues/15739)</th>
  <td>ghc-heap: code field never seems to be populated in StgInfoTable</td></tr>
  <tr><th>[\#15742](https://gitlab.haskell.org//ghc/ghc/issues/15742)</th>
  <td>GHC configure step can fail when CC=clang</td></tr>
  <tr><th>[\#15744](https://gitlab.haskell.org//ghc/ghc/issues/15744)</th>
  <td>Existence of complete pattern synonym hides unrelated incomplete pattern warning</td></tr>
  <tr><th>[\#15746](https://gitlab.haskell.org//ghc/ghc/issues/15746)</th>
  <td>Memory leak in print when RTS option -N is \>= 2</td></tr>
  <tr><th>[\#15747](https://gitlab.haskell.org//ghc/ghc/issues/15747)</th>
  <td>GHC panics when builds for arm as: ghc-stage1: panic! (the 'impossible' happened): padLiveArgs -- i \> regNum ??</td></tr>
  <tr><th>[\#15749](https://gitlab.haskell.org//ghc/ghc/issues/15749)</th>
  <td>Long Harbormaster builds</td></tr>
  <tr><th>[\#15751](https://gitlab.haskell.org//ghc/ghc/issues/15751)</th>
  <td>GHC takes huge amounts of memory and compile time when compiling ZipWith from accelerate</td></tr>
  <tr><th>[\#15753](https://gitlab.haskell.org//ghc/ghc/issues/15753)</th>
  <td>Inconsistent pattern-match warnings when using guards versus case expressions</td></tr>
  <tr><th>[\#15759](https://gitlab.haskell.org//ghc/ghc/issues/15759)</th>
  <td>GHC doesn't use fixity in type instances</td></tr>
  <tr><th>[\#15760](https://gitlab.haskell.org//ghc/ghc/issues/15760)</th>
  <td>Preserve parens in TH</td></tr>
  <tr><th>[\#15763](https://gitlab.haskell.org//ghc/ghc/issues/15763)</th>
  <td>GHC forbids kind signatures in data family instances</td></tr>
  <tr><th>[\#15768](https://gitlab.haskell.org//ghc/ghc/issues/15768)</th>
  <td>Using oneshot kqueue() on macOS</td></tr>
  <tr><th>[\#15770](https://gitlab.haskell.org//ghc/ghc/issues/15770)</th>
  <td>Missing optimisation opportunity in code gen for always-saturated applications?</td></tr>
  <tr><th>[\#15772](https://gitlab.haskell.org//ghc/ghc/issues/15772)</th>
  <td>Strange constraint error that disappears when adding another top-level declaration</td></tr>
  <tr><th>[\#15774](https://gitlab.haskell.org//ghc/ghc/issues/15774)</th>
  <td>SIGQUIT only reports backtrace for one capability</td></tr>
  <tr><th>[\#15775](https://gitlab.haskell.org//ghc/ghc/issues/15775)</th>
  <td>Interpreter is treating a comment character as an identifier character.</td></tr>
  <tr><th>[\#15776](https://gitlab.haskell.org//ghc/ghc/issues/15776)</th>
  <td>Untested modules excluded from hpc coverage report</td></tr>
  <tr><th>[\#15784](https://gitlab.haskell.org//ghc/ghc/issues/15784)</th>
  <td>:doc shouldn't report \<has no documentation\> for a data constructor when it can show docs for the type constructor of the same name and type</td></tr>
  <tr><th>[\#15790](https://gitlab.haskell.org//ghc/ghc/issues/15790)</th>
  <td>String literals are not escaped in -ddump-splices</td></tr>
  <tr><th>[\#15803](https://gitlab.haskell.org//ghc/ghc/issues/15803)</th>
  <td>Put more info with flag description</td></tr>
  <tr><th>[\#15808](https://gitlab.haskell.org//ghc/ghc/issues/15808)</th>
  <td>Loading libraries with FFI exports may cause segfaults in the compiler if they are loaded far from the rts in memory.</td></tr>
  <tr><th>[\#15809](https://gitlab.haskell.org//ghc/ghc/issues/15809)</th>
  <td>Use level numbers for generalisation</td></tr>
  <tr><th>[\#15813](https://gitlab.haskell.org//ghc/ghc/issues/15813)</th>
  <td>-Wredundant-constraints emits warning even if constraint is narrowing type</td></tr>
  <tr><th>[\#15819](https://gitlab.haskell.org//ghc/ghc/issues/15819)</th>
  <td>COMPLETE pragma not listed in GHC manual index</td></tr>
  <tr><th>[\#15820](https://gitlab.haskell.org//ghc/ghc/issues/15820)</th>
  <td>Document the proposals process in the GHC manual</td></tr>
  <tr><th>[\#15822](https://gitlab.haskell.org//ghc/ghc/issues/15822)</th>
  <td>template-haskell package lacks any real documentation</td></tr>
  <tr><th>[\#15823](https://gitlab.haskell.org//ghc/ghc/issues/15823)</th>
  <td>Incorrect link in the doc (TH API)</td></tr>
  <tr><th>[\#15824](https://gitlab.haskell.org//ghc/ghc/issues/15824)</th>
  <td>Prefix/infix distinction in TemplateHaskell types is lost</td></tr>
  <tr><th>[\#15830](https://gitlab.haskell.org//ghc/ghc/issues/15830)</th>
  <td>Plugins Tests are Skipped</td></tr>
  <tr><th>[\#15831](https://gitlab.haskell.org//ghc/ghc/issues/15831)</th>
  <td>DerivingVia allows bogus implicit quantification in \`via\` type</td></tr>
  <tr><th>[\#15832](https://gitlab.haskell.org//ghc/ghc/issues/15832)</th>
  <td>fprintCCS_stderr (+RTS -xc) should be able to traverse into stacks that evaluated a given stack even if the latter does not come from a CAF</td></tr>
  <tr><th>[\#15833](https://gitlab.haskell.org//ghc/ghc/issues/15833)</th>
  <td>Typed template haskell quote fails to typecheck when spliced due to an ambiguous type variable</td></tr>
  <tr><th>[\#15835](https://gitlab.haskell.org//ghc/ghc/issues/15835)</th>
  <td>Internal error when splicing value constructed using typed template haskell</td></tr>
  <tr><th>[\#15836](https://gitlab.haskell.org//ghc/ghc/issues/15836)</th>
  <td>ghc-in-ghci script fails when there is a Main.hs in the top-level directory</td></tr>
  <tr><th>[\#15839](https://gitlab.haskell.org//ghc/ghc/issues/15839)</th>
  <td>DerivingStrategies defaulting warning has no associated enable/suppress flag</td></tr>
  <tr><th>[\#15843](https://gitlab.haskell.org//ghc/ghc/issues/15843)</th>
  <td>Tuple sections can't be quoted</td></tr>
  <tr><th>[\#15844](https://gitlab.haskell.org//ghc/ghc/issues/15844)</th>
  <td>Error fetching msys2 tarballs - crt-git</td></tr>
  <tr><th>[\#15846](https://gitlab.haskell.org//ghc/ghc/issues/15846)</th>
  <td>Re-examine mkResidualConstraints</td></tr>
  <tr><th>[\#15847](https://gitlab.haskell.org//ghc/ghc/issues/15847)</th>
  <td>GHCi cannot load .o built from c with -fPIC on i386 linux</td></tr>
  <tr><th>[\#15849](https://gitlab.haskell.org//ghc/ghc/issues/15849)</th>
  <td>Error message: "Perhaps you need a let in a do block", when there is no do block.</td></tr>
  <tr><th>[\#15860](https://gitlab.haskell.org//ghc/ghc/issues/15860)</th>
  <td>Hadrian build fails on FreeBSD</td></tr>
  <tr><th>[\#15862](https://gitlab.haskell.org//ghc/ghc/issues/15862)</th>
  <td>Panic with promoted types that Typeable doesn't support</td></tr>
  <tr><th>[\#15863](https://gitlab.haskell.org//ghc/ghc/issues/15863)</th>
  <td>Splcing a type class method selects the wrong instance</td></tr>
  <tr><th>[\#15865](https://gitlab.haskell.org//ghc/ghc/issues/15865)</th>
  <td>Typed template haskell and implicit parameters lead to incorrect results</td></tr>
  <tr><th>[\#15869](https://gitlab.haskell.org//ghc/ghc/issues/15869)</th>
  <td>Discrepancy between seemingly equivalent type synonym and type family</td></tr>
  <tr><th>[\#15872](https://gitlab.haskell.org//ghc/ghc/issues/15872)</th>
  <td>Odd pretty printing of equality constraint in kind ('GHC.Types.Eq\# \<\>)</td></tr>
  <tr><th>[\#15877](https://gitlab.haskell.org//ghc/ghc/issues/15877)</th>
  <td>--flavour=quick followed by --flavour=prof fails</td></tr>
  <tr><th>[\#15878](https://gitlab.haskell.org//ghc/ghc/issues/15878)</th>
  <td>Unused data type with a "deriving" clause is falsely considered used</td></tr>
  <tr><th>[\#15883](https://gitlab.haskell.org//ghc/ghc/issues/15883)</th>
  <td>GHC panic: newtype F rep = F (forall (a :: TYPE rep). a)</td></tr>
  <tr><th>[\#15888](https://gitlab.haskell.org//ghc/ghc/issues/15888)</th>
  <td>Quantified constraints can be loopy</td></tr>
  <tr><th>[\#15889](https://gitlab.haskell.org//ghc/ghc/issues/15889)</th>
  <td>ghc documentation doesn't explain difference between dwarf levels 1 2 and 3</td></tr>
  <tr><th>[\#15891](https://gitlab.haskell.org//ghc/ghc/issues/15891)</th>
  <td>Excessive system time during execution of GHC-built executables on macOS</td></tr>
  <tr><th>[\#15895](https://gitlab.haskell.org//ghc/ghc/issues/15895)</th>
  <td>Unable to match instance signatures</td></tr>
  <tr><th>[\#15897](https://gitlab.haskell.org//ghc/ghc/issues/15897)</th>
  <td>Negative MUT time in +RTS -s -RTS when heap profiling is enabled</td></tr>
  <tr><th>[\#15899](https://gitlab.haskell.org//ghc/ghc/issues/15899)</th>
  <td>Testcase tcfail158 is broken</td></tr>
  <tr><th>[\#15903](https://gitlab.haskell.org//ghc/ghc/issues/15903)</th>
  <td>Assertion failure in LDV profiler</td></tr>
  <tr><th>[\#15905](https://gitlab.haskell.org//ghc/ghc/issues/15905)</th>
  <td>Data familes should end in Type</td></tr>
  <tr><th>[\#15907](https://gitlab.haskell.org//ghc/ghc/issues/15907)</th>
  <td>memo001 causes compile time panic with fails with -O -dannot-lint</td></tr>
  <tr><th>[\#15908](https://gitlab.haskell.org//ghc/ghc/issues/15908)</th>
  <td>Hadrian: Spurious build failure on fresh build</td></tr>
  <tr><th>[\#15909](https://gitlab.haskell.org//ghc/ghc/issues/15909)</th>
  <td>prepareAlts does not take into account equalities which are in scope</td></tr>
  <tr><th>[\#15911](https://gitlab.haskell.org//ghc/ghc/issues/15911)</th>
  <td>internal error: evacuate(static): strange closure type -127533247</td></tr>
  <tr><th>[\#15913](https://gitlab.haskell.org//ghc/ghc/issues/15913)</th>
  <td>ghc-8.6.2 fails to build on s390x (unregisterised): ‘stg_MUT_ARR_PTRS_FROZEN0_info’ undeclared (first use in this function); did you mean ‘stg_MUT_ARR_PTRS_FROZEN_DIRTY_info’?</td></tr>
  <tr><th>[\#15917](https://gitlab.haskell.org//ghc/ghc/issues/15917)</th>
  <td>GHC seems to re-read source of module to produce fancy error msgs</td></tr>
  <tr><th>[\#15918](https://gitlab.haskell.org//ghc/ghc/issues/15918)</th>
  <td>mkCastTy sometimes drops insoluble (Type \~ Constraint) coercions</td></tr>
  <tr><th>[\#15921](https://gitlab.haskell.org//ghc/ghc/issues/15921)</th>
  <td>Data.List.maximumBy uses counter-intuitive ordering</td></tr>
  <tr><th>[\#15926](https://gitlab.haskell.org//ghc/ghc/issues/15926)</th>
  <td>conversion from Integer to Double rounds differently depending on the value</td></tr>
  <tr><th>[\#15927](https://gitlab.haskell.org//ghc/ghc/issues/15927)</th>
  <td>Weird interaction between fundeps and overlappable instances</td></tr>
  <tr><th>[\#15928](https://gitlab.haskell.org//ghc/ghc/issues/15928)</th>
  <td>Improve error message: Reduction stack overflow when using "coerce"</td></tr>
  <tr><th>[\#15931](https://gitlab.haskell.org//ghc/ghc/issues/15931)</th>
  <td>MonoLocalBinds + MonomorphismRestriction prevents generalization for a top level definition</td></tr>
  <tr><th>[\#15932](https://gitlab.haskell.org//ghc/ghc/issues/15932)</th>
  <td>DeriveFunctor and GeneralizedNewtypeDeriving instances never reporting as covered</td></tr>
  <tr><th>[\#15933](https://gitlab.haskell.org//ghc/ghc/issues/15933)</th>
  <td>Wrong size for int for callbacks into Haskell from foreign code</td></tr>
  <tr><th>[\#15935](https://gitlab.haskell.org//ghc/ghc/issues/15935)</th>
  <td>TYPE is not generated by genprimops</td></tr>
  <tr><th>[\#15940](https://gitlab.haskell.org//ghc/ghc/issues/15940)</th>
  <td>Source plugins should be able to opt-out from appearing in dependencies</td></tr>
  <tr><th>[\#15942](https://gitlab.haskell.org//ghc/ghc/issues/15942)</th>
  <td>Associated type family can't be used at the kind level within other parts of parent class</td></tr>
  <tr><th>[\#15944](https://gitlab.haskell.org//ghc/ghc/issues/15944)</th>
  <td>Wrong warning given ViewPatterns and -Wmonomorphism-restriction</td></tr>
  <tr><th>[\#15948](https://gitlab.haskell.org//ghc/ghc/issues/15948)</th>
  <td>Hadrian build fails on Windows when invoked without --configure flag</td></tr>
  <tr><th>[\#15958](https://gitlab.haskell.org//ghc/ghc/issues/15958)</th>
  <td>Missing documentation for '-fno-ghci-sandbox'</td></tr>
  <tr><th>[\#15960](https://gitlab.haskell.org//ghc/ghc/issues/15960)</th>
  <td>Using -g causes differences in generated core.</td></tr>
  <tr><th>[\#15963](https://gitlab.haskell.org//ghc/ghc/issues/15963)</th>
  <td>Test suite should report timeouts as timeouts</td></tr>
  <tr><th>[\#15966](https://gitlab.haskell.org//ghc/ghc/issues/15966)</th>
  <td>panic when using RebindableSyntax</td></tr>
  <tr><th>[\#15967](https://gitlab.haskell.org//ghc/ghc/issues/15967)</th>
  <td>can't build ghc on Mac with any level of dwarf for base library</td></tr>
  <tr><th>[\#15969](https://gitlab.haskell.org//ghc/ghc/issues/15969)</th>
  <td>Generic1 deriving should use more coercions</td></tr>
  <tr><th>[\#15971](https://gitlab.haskell.org//ghc/ghc/issues/15971)</th>
  <td>Hadrian fails Shake's linter on Windows</td></tr>
  <tr><th>[\#15972](https://gitlab.haskell.org//ghc/ghc/issues/15972)</th>
  <td>Test nofib tests for correctness in CI</td></tr>
  <tr><th>[\#15973](https://gitlab.haskell.org//ghc/ghc/issues/15973)</th>
  <td>Int used to represent target integer literals</td></tr>
  <tr><th>[\#15975](https://gitlab.haskell.org//ghc/ghc/issues/15975)</th>
  <td>semantics of concurrent program depends on -O level, -f\[no-\]omit-yields</td></tr>
  <tr><th>[\#15976](https://gitlab.haskell.org//ghc/ghc/issues/15976)</th>
  <td>Can't run nofib in parallel</td></tr>
  <tr><th>[\#15978](https://gitlab.haskell.org//ghc/ghc/issues/15978)</th>
  <td>slow test suite failures on 8.6 DARWIN :</td></tr>
  <tr><th>[\#15979](https://gitlab.haskell.org//ghc/ghc/issues/15979)</th>
  <td>Core Lint error with LiberalTypeSynonyms</td></tr>
  <tr><th>[\#15982](https://gitlab.haskell.org//ghc/ghc/issues/15982)</th>
  <td>Hadrian's \`--configure\` flag is broken on Windows</td></tr>
  <tr><th>[\#15984](https://gitlab.haskell.org//ghc/ghc/issues/15984)</th>
  <td>Backpack accepts ill-kinded instantiations. Can cause GHC panic</td></tr>
  <tr><th>[\#15989](https://gitlab.haskell.org//ghc/ghc/issues/15989)</th>
  <td>Adding extra quantified constraints leads to resolution failure</td></tr>
  <tr><th>[\#15990](https://gitlab.haskell.org//ghc/ghc/issues/15990)</th>
  <td>Dynamically built GHC crashes on MacOS</td></tr>
  <tr><th>[\#15991](https://gitlab.haskell.org//ghc/ghc/issues/15991)</th>
  <td>Regression in error message when attempting to let bind an existentially quantified type</td></tr>
  <tr><th>[\#15992](https://gitlab.haskell.org//ghc/ghc/issues/15992)</th>
  <td>Alternative instance for Data.Functor.Compose causes \<\<loop\>\>s with some types</td></tr>
  <tr><th>[\#16004](https://gitlab.haskell.org//ghc/ghc/issues/16004)</th>
  <td>Vector performance regression in GHC 8.6</td></tr>
  <tr><th>[\#16011](https://gitlab.haskell.org//ghc/ghc/issues/16011)</th>
  <td>GHCi leaks memory even with -fno-it.</td></tr>
  <tr><th>[\#16012](https://gitlab.haskell.org//ghc/ghc/issues/16012)</th>
  <td>set/getAllocationCounter is broken in GHCi</td></tr>
  <tr><th>[\#16016](https://gitlab.haskell.org//ghc/ghc/issues/16016)</th>
  <td>Hadrian build fails on FreeBSD</td></tr>
  <tr><th>[\#16017](https://gitlab.haskell.org//ghc/ghc/issues/16017)</th>
  <td>ghc-8.6.1 and ghc-8.6.2 use a log of memory</td></tr>
  <tr><th>[\#16018](https://gitlab.haskell.org//ghc/ghc/issues/16018)</th>
  <td>Disabling core optimizations ignores code that would otherwise warn.</td></tr>
  <tr><th>[\#16021](https://gitlab.haskell.org//ghc/ghc/issues/16021)</th>
  <td>Hadrian: remove/refactor package specific hacks</td></tr>
  <tr><th>[\#16022](https://gitlab.haskell.org//ghc/ghc/issues/16022)</th>
  <td>Hadrian appears to link against libffi unconditionally</td></tr>
  <tr><th>[\#16028](https://gitlab.haskell.org//ghc/ghc/issues/16028)</th>
  <td>tyThingCoAxiom panics while building Agda</td></tr>
  <tr><th>[\#16032](https://gitlab.haskell.org//ghc/ghc/issues/16032)</th>
  <td>Compile speed regression</td></tr>
  <tr><th>[\#16034](https://gitlab.haskell.org//ghc/ghc/issues/16034)</th>
  <td>Quadratic GC slowdown using RTS debug</td></tr>
  <tr><th>[\#16035](https://gitlab.haskell.org//ghc/ghc/issues/16035)</th>
  <td>keep-cafs fails on FreeBSD 11</td></tr>
  <tr><th>[\#16037](https://gitlab.haskell.org//ghc/ghc/issues/16037)</th>
  <td>memcpy test inexplicably failing</td></tr>
  <tr><th>[\#16039](https://gitlab.haskell.org//ghc/ghc/issues/16039)</th>
  <td>'GHC.Magic.noinline \<var\>' should not float out</td></tr>
  <tr><th>[\#16040](https://gitlab.haskell.org//ghc/ghc/issues/16040)</th>
  <td>Unboxing-Related Performance Issue with Polymorphic Functions</td></tr>
  <tr><th>[\#16042](https://gitlab.haskell.org//ghc/ghc/issues/16042)</th>
  <td>T13031 doesn't get run</td></tr>
  <tr><th>[\#16043](https://gitlab.haskell.org//ghc/ghc/issues/16043)</th>
  <td>Lots of testsuite output breaks with integer-simple</td></tr>
  <tr><th>[\#16045](https://gitlab.haskell.org//ghc/ghc/issues/16045)</th>
  <td>improve dwarf support on OSX</td></tr>
  <tr><th>[\#16047](https://gitlab.haskell.org//ghc/ghc/issues/16047)</th>
  <td>Incorrect hscTarget in DynFlags when building hs-boot files</td></tr>
  <tr><th>[\#16048](https://gitlab.haskell.org//ghc/ghc/issues/16048)</th>
  <td>Show Instance for IOException discards error code</td></tr>
  <tr><th>[\#16051](https://gitlab.haskell.org//ghc/ghc/issues/16051)</th>
  <td>Cross compilation broken under Hadrian</td></tr>
  <tr><th>[\#16058](https://gitlab.haskell.org//ghc/ghc/issues/16058)</th>
  <td>GHC built on macOS Mojave nondeterministically segfaults</td></tr>
  <tr><th>[\#16060](https://gitlab.haskell.org//ghc/ghc/issues/16060)</th>
  <td>Missing AArch64 builds for latest GHC 8.6.3</td></tr>
  <tr><th>[\#16061](https://gitlab.haskell.org//ghc/ghc/issues/16061)</th>
  <td>Missing FreeBSD build for 8.6.2</td></tr>
  <tr><th>[\#16063](https://gitlab.haskell.org//ghc/ghc/issues/16063)</th>
  <td>ghc-8.6.3 + Mac OSX +  FFI dependency causes 'impossible happened' compiler failure</td></tr>
  <tr><th>[\#16066](https://gitlab.haskell.org//ghc/ghc/issues/16066)</th>
  <td>internal error: PAP object entered!</td></tr>
  <tr><th>[\#16067](https://gitlab.haskell.org//ghc/ghc/issues/16067)</th>
  <td>Profiled GHCi segfaults under Windows.</td></tr>
  <tr><th>[\#16070](https://gitlab.haskell.org//ghc/ghc/issues/16070)</th>
  <td>Better inferred signatures</td></tr>
  <tr><th>[\#16072](https://gitlab.haskell.org//ghc/ghc/issues/16072)</th>
  <td>All dependencies of packages must be explicitly listed when defining flavour packages</td></tr>
  <tr><th>[\#16073](https://gitlab.haskell.org//ghc/ghc/issues/16073)</th>
  <td>Hadrian build fails on Windows</td></tr>
  <tr><th>[\#16076](https://gitlab.haskell.org//ghc/ghc/issues/16076)</th>
  <td>Internal compiler error when instance uses FFI function and defining other instance of the same class through Template Haskell</td></tr>
  <tr><th>[\#16077](https://gitlab.haskell.org//ghc/ghc/issues/16077)</th>
  <td>AvailTC Invariant being violated</td></tr>
  <tr><th>[\#16078](https://gitlab.haskell.org//ghc/ghc/issues/16078)</th>
  <td>Hide -Weverything warnings for GHCi internals</td></tr>
  <tr><th>[\#16081](https://gitlab.haskell.org//ghc/ghc/issues/16081)</th>
  <td>Clean up family instance consistency checking</td></tr>
  <tr><th>[\#16082](https://gitlab.haskell.org//ghc/ghc/issues/16082)</th>
  <td>Sort out treatment of underscores in types</td></tr>
  <tr><th>[\#16083](https://gitlab.haskell.org//ghc/ghc/issues/16083)</th>
  <td>tests relying on \<iostream\> are broken on Mojave builder</td></tr>
  <tr><th>[\#16084](https://gitlab.haskell.org//ghc/ghc/issues/16084)</th>
  <td>Improve link times on Windows</td></tr>
  <tr><th>[\#16085](https://gitlab.haskell.org//ghc/ghc/issues/16085)</th>
  <td>ffi018_ghci fails when unregisterised</td></tr>
  <tr><th>[\#16087](https://gitlab.haskell.org//ghc/ghc/issues/16087)</th>
  <td>TH tests fail in ext-interp way when ghc-stage2 is built using LLVM</td></tr>
  <tr><th>[\#16093](https://gitlab.haskell.org//ghc/ghc/issues/16093)</th>
  <td>mkPluginUsage: file not found</td></tr>
  <tr><th>[\#16095](https://gitlab.haskell.org//ghc/ghc/issues/16095)</th>
  <td>Infinite loop during error reporting (ignores SIGINT/SIGTERM, then OOMs)</td></tr>
  <tr><th>[\#16097](https://gitlab.haskell.org//ghc/ghc/issues/16097)</th>
  <td>Bad error message for a misaligned case block in a do-let expression</td></tr>
  <tr><th>[\#16100](https://gitlab.haskell.org//ghc/ghc/issues/16100)</th>
  <td>T11627a fails sometimes on CI</td></tr>
  <tr><th>[\#16110](https://gitlab.haskell.org//ghc/ghc/issues/16110)</th>
  <td>Explicit foralls in associated type family defaults are completely ignored?</td></tr>
  <tr><th>[\#16111](https://gitlab.haskell.org//ghc/ghc/issues/16111)</th>
  <td>Inconsistent behavior of Data.Bits.shiftL with different optimization levels and -fllvm</td></tr>
  <tr><th>[\#16115](https://gitlab.haskell.org//ghc/ghc/issues/16115)</th>
  <td>Missing associated type instance not reported with error</td></tr>
  <tr><th>[\#16117](https://gitlab.haskell.org//ghc/ghc/issues/16117)</th>
  <td>Random segfaults at runtime on macOS Mojave caused by GHC linker misaligning sections</td></tr>
  <tr><th>[\#16118](https://gitlab.haskell.org//ghc/ghc/issues/16118)</th>
  <td>SYB link in Data.Data is dead</td></tr>
  <tr><th>[\#16121](https://gitlab.haskell.org//ghc/ghc/issues/16121)</th>
  <td>testsuite: only_ways(llvm_ways) behaves strangely</td></tr>
  <tr><th>[\#16122](https://gitlab.haskell.org//ghc/ghc/issues/16122)</th>
  <td>\`round :: Double -\> Int64\` much slower than \`fromIntegral \@Int \@Int64 . round\`</td></tr>
  <tr><th>[\#16124](https://gitlab.haskell.org//ghc/ghc/issues/16124)</th>
  <td>prof_hc_hb way failures on CI</td></tr>
  <tr><th>[\#16127](https://gitlab.haskell.org//ghc/ghc/issues/16127)</th>
  <td>Panic: piResultTys1 in compiler/types/Type.hs:1022:5</td></tr>
  <tr><th>[\#16130](https://gitlab.haskell.org//ghc/ghc/issues/16130)</th>
  <td>GHC Panic on OS X: Data.Binary.Get.runGet: Invalid magic number "INPUT(-l"</td></tr>
  <tr><th>[\#16132](https://gitlab.haskell.org//ghc/ghc/issues/16132)</th>
  <td>./validate --slow: "dist-install/build/HSghc-prim-0.5.3.o: copyFile: does not exist (No such file or directory)"</td></tr>
  <tr><th>[\#16134](https://gitlab.haskell.org//ghc/ghc/issues/16134)</th>
  <td>Continuous integration on FreeBSD</td></tr>
  <tr><th>[\#16135](https://gitlab.haskell.org//ghc/ghc/issues/16135)</th>
  <td>Panic with ExistentialQuantification and ApplicativeDo</td></tr>
  <tr><th>[\#16136](https://gitlab.haskell.org//ghc/ghc/issues/16136)</th>
  <td>Broken link to GHC-Prim.html\#chr%23</td></tr>
  <tr><th>[\#16139](https://gitlab.haskell.org//ghc/ghc/issues/16139)</th>
  <td>GHC confused about type synonym kind with QuantifiedConstraints</td></tr>
  <tr><th>[\#16140](https://gitlab.haskell.org//ghc/ghc/issues/16140)</th>
  <td>Cannot create type synonym for quantified constraint without ImpredicativeTypes</td></tr>
  <tr><th>[\#16141](https://gitlab.haskell.org//ghc/ghc/issues/16141)</th>
  <td>StrictData and TypeFamilies regression</td></tr>
  <tr><th>[\#16142](https://gitlab.haskell.org//ghc/ghc/issues/16142)</th>
  <td>RTS statistic TASKS is not documented</td></tr>
  <tr><th>[\#16143](https://gitlab.haskell.org//ghc/ghc/issues/16143)</th>
  <td>Some cost centre stacks are not registered via registerCcsList()</td></tr>
  <tr><th>[\#16145](https://gitlab.haskell.org//ghc/ghc/issues/16145)</th>
  <td>runghc with -fobject-code and -osuf fails to find dependencies</td></tr>
  <tr><th>[\#16146](https://gitlab.haskell.org//ghc/ghc/issues/16146)</th>
  <td>Trivial partial type signature kills type inference in the presence of GADTs</td></tr>
  <tr><th>[\#16147](https://gitlab.haskell.org//ghc/ghc/issues/16147)</th>
  <td>ghc-pkg deadlocks on macOS</td></tr>
  <tr><th>[\#16148](https://gitlab.haskell.org//ghc/ghc/issues/16148)</th>
  <td>Better type inference for Constraint vs Type</td></tr>
  <tr><th>[\#16151](https://gitlab.haskell.org//ghc/ghc/issues/16151)</th>
  <td>GHC 8.6 build failure on ARM</td></tr>
  <tr><th>[\#16152](https://gitlab.haskell.org//ghc/ghc/issues/16152)</th>
  <td>Core lint error from PartialTypeSignatures</td></tr>
  <tr><th>[\#16153](https://gitlab.haskell.org//ghc/ghc/issues/16153)</th>
  <td>ghc 8.6.3 fails to build on macOS Sierra, High Sierra and Mojave (10.12 to 10.14)</td></tr>
  <tr><th>[\#16154](https://gitlab.haskell.org//ghc/ghc/issues/16154)</th>
  <td>-Wredundant-constraints: False positive</td></tr>
  <tr><th>[\#16156](https://gitlab.haskell.org//ghc/ghc/issues/16156)</th>
  <td>Broken tests on Windows</td></tr>
  <tr><th>[\#16157](https://gitlab.haskell.org//ghc/ghc/issues/16157)</th>
  <td>Duplicate symbol error on Windows</td></tr>
  <tr><th>[\#16158](https://gitlab.haskell.org//ghc/ghc/issues/16158)</th>
  <td>T15094 fails on Windows</td></tr>
  <tr><th>[\#16159](https://gitlab.haskell.org//ghc/ghc/issues/16159)</th>
  <td>plugin-recomp-change fails on Windows</td></tr>
  <tr><th>[\#16160](https://gitlab.haskell.org//ghc/ghc/issues/16160)</th>
  <td>TH_NestedSplices fails on Windows</td></tr>
  <tr><th>[\#16161](https://gitlab.haskell.org//ghc/ghc/issues/16161)</th>
  <td>plugin-recomp-impure fails on Windows</td></tr>
  <tr><th>[\#16162](https://gitlab.haskell.org//ghc/ghc/issues/16162)</th>
  <td>External interpreter is broken on Windows</td></tr>
  <tr><th>[\#16167](https://gitlab.haskell.org//ghc/ghc/issues/16167)</th>
  <td>-ddump-json doesn't work with -e</td></tr>
  <tr><th>[\#16169](https://gitlab.haskell.org//ghc/ghc/issues/16169)</th>
  <td>Unused variable warning affects compositionality when generating code</td></tr>
  <tr><th>[\#16171](https://gitlab.haskell.org//ghc/ghc/issues/16171)</th>
  <td>"ApplicativeDo" disables -Wunused-do-binds?</td></tr>
  <tr><th>[\#16172](https://gitlab.haskell.org//ghc/ghc/issues/16172)</th>
  <td>GHC 8.4 build failure on ARM</td></tr>
  <tr><th>[\#16174](https://gitlab.haskell.org//ghc/ghc/issues/16174)</th>
  <td>GhcEnableTablesNextToCode=NO breaks NCG on macOS</td></tr>
  <tr><th>[\#16175](https://gitlab.haskell.org//ghc/ghc/issues/16175)</th>
  <td>wrong wording (and possibly wrong location) in parse error message for "do $ bar"</td></tr>
  <tr><th>[\#16181](https://gitlab.haskell.org//ghc/ghc/issues/16181)</th>
  <td>ghc panic when using DerivingVia</td></tr>
  <tr><th>[\#16183](https://gitlab.haskell.org//ghc/ghc/issues/16183)</th>
  <td>GHC HEAD regression: -ddump-splices incorrectly parenthesizes HsKindSig applications</td></tr>
  <tr><th>[\#16187](https://gitlab.haskell.org//ghc/ghc/issues/16187)</th>
  <td>Hadrian: Build fails when using absolute path for build root</td></tr>
  <tr><th>[\#16188](https://gitlab.haskell.org//ghc/ghc/issues/16188)</th>
  <td>GHC HEAD-only panic (buildKindCoercion)</td></tr>
  <tr><th>[\#16192](https://gitlab.haskell.org//ghc/ghc/issues/16192)</th>
  <td>Simplifier does not preserve dependency ordering of the program</td></tr>
  <tr><th>[\#16193](https://gitlab.haskell.org//ghc/ghc/issues/16193)</th>
  <td>Nondeterministic T15897 timeout failures</td></tr>
  <tr><th>[\#16195](https://gitlab.haskell.org//ghc/ghc/issues/16195)</th>
  <td>Program with trivial polymorphism leads to out of scope dictionary</td></tr>
  <tr><th>[\#16196](https://gitlab.haskell.org//ghc/ghc/issues/16196)</th>
  <td>Update README.md to reflect gitlab</td></tr>
  <tr><th>[\#16200](https://gitlab.haskell.org//ghc/ghc/issues/16200)</th>
  <td>Mechanical checking of submodule versions for releases</td></tr>
  <tr><th>[\#16201](https://gitlab.haskell.org//ghc/ghc/issues/16201)</th>
  <td>ghci063 failing on Darwin</td></tr>
  <tr><th>[\#16203](https://gitlab.haskell.org//ghc/ghc/issues/16203)</th>
  <td>Unhelpful names for wildcard type variables</td></tr>
  <tr><th>[\#16204](https://gitlab.haskell.org//ghc/ghc/issues/16204)</th>
  <td>GHC HEAD-only Core Lint error (Argument value doesn't match argument type)</td></tr>
  <tr><th>[\#16205](https://gitlab.haskell.org//ghc/ghc/issues/16205)</th>
  <td>Printing a large (GMP) Integer in ghci-ext with LLVM causes a segfault</td></tr>
  <tr><th>[\#16209](https://gitlab.haskell.org//ghc/ghc/issues/16209)</th>
  <td>GHC behaves differently when binding has separate or inlined type declaration</td></tr>
  <tr><th>[\#16210](https://gitlab.haskell.org//ghc/ghc/issues/16210)</th>
  <td>Hadrian devel2 flavour builds profiling way</td></tr>
  <tr><th>[\#16211](https://gitlab.haskell.org//ghc/ghc/issues/16211)</th>
  <td>Recompiling results in a missing instance</td></tr>
  <tr><th>[\#16219](https://gitlab.haskell.org//ghc/ghc/issues/16219)</th>
  <td>Backpack - TH+indefinite module interface file error</td></tr>
  <tr><th>[\#16222](https://gitlab.haskell.org//ghc/ghc/issues/16222)</th>
  <td>PPC NCG: C calling convention violated for integers smaller than word size</td></tr>
  <tr><th>[\#16223](https://gitlab.haskell.org//ghc/ghc/issues/16223)</th>
  <td>hWaitForInput is broken on Windows</td></tr>
  <tr><th>[\#16224](https://gitlab.haskell.org//ghc/ghc/issues/16224)</th>
  <td>CPUTime001 is fragile</td></tr>
  <tr><th>[\#16225](https://gitlab.haskell.org//ghc/ghc/issues/16225)</th>
  <td>GHC HEAD-only Core Lint error (Trans coercion mis-match)</td></tr>
  <tr><th>[\#16226](https://gitlab.haskell.org//ghc/ghc/issues/16226)</th>
  <td>printf g format specifier doesn't match the C standard behaviour</td></tr>
  <tr><th>[\#16228](https://gitlab.haskell.org//ghc/ghc/issues/16228)</th>
  <td>ghc-pkg and ghc do not agree about package with internal libraries</td></tr>
  <tr><th>[\#16230](https://gitlab.haskell.org//ghc/ghc/issues/16230)</th>
  <td>API Annotations: more explicit foralls fixup</td></tr>
  <tr><th>[\#16231](https://gitlab.haskell.org//ghc/ghc/issues/16231)</th>
  <td>GHCi under powershell fails to find libgmp-10.dll when built with hadrian.</td></tr>
  <tr><th>[\#16233](https://gitlab.haskell.org//ghc/ghc/issues/16233)</th>
  <td>HIE file generation is inefficient</td></tr>
  <tr><th>[\#16234](https://gitlab.haskell.org//ghc/ghc/issues/16234)</th>
  <td>Unable to resolve type families</td></tr>
  <tr><th>[\#16235](https://gitlab.haskell.org//ghc/ghc/issues/16235)</th>
  <td>Hadrian devel2 builds Haddock</td></tr>
  <tr><th>[\#16236](https://gitlab.haskell.org//ghc/ghc/issues/16236)</th>
  <td>API Annotations: AnnAt disconnected for TYPEAPP</td></tr>
  <tr><th>[\#16241](https://gitlab.haskell.org//ghc/ghc/issues/16241)</th>
  <td>Avoid orphan instances with OVERLAPPABLE (sometimes)</td></tr>
  <tr><th>[\#16242](https://gitlab.haskell.org//ghc/ghc/issues/16242)</th>
  <td>Hadrian is too aggressive in rebuilding</td></tr>
  <tr><th>[\#16244](https://gitlab.haskell.org//ghc/ghc/issues/16244)</th>
  <td>Couldn't match kind ‘k1’ with ‘k1’</td></tr>
  <tr><th>[\#16245](https://gitlab.haskell.org//ghc/ghc/issues/16245)</th>
  <td>GHC panic (No skolem info) with QuantifiedConstraints and strange scoping</td></tr>
  <tr><th>[\#16246](https://gitlab.haskell.org//ghc/ghc/issues/16246)</th>
  <td>GHC HEAD-only Core Lint error with unboxed equality (Non-CoVar has coercion type)</td></tr>
  <tr><th>[\#16247](https://gitlab.haskell.org//ghc/ghc/issues/16247)</th>
  <td>GHC 8.6 Core Lint regression (Kind application error)</td></tr>
  <tr><th>[\#16249](https://gitlab.haskell.org//ghc/ghc/issues/16249)</th>
  <td>no runtime error for -fdefer-type-errors with TypeError constraint</td></tr>
  <tr><th>[\#16250](https://gitlab.haskell.org//ghc/ghc/issues/16250)</th>
  <td>Make it easy to invoke Hadrian</td></tr>
  <tr><th>[\#16252](https://gitlab.haskell.org//ghc/ghc/issues/16252)</th>
  <td>QuantifiedConstraints: lack of inference really is a problem</td></tr>
  <tr><th>[\#16255](https://gitlab.haskell.org//ghc/ghc/issues/16255)</th>
  <td>Visible kind application defeats type family with higher-rank result kind</td></tr>
  <tr><th>[\#16256](https://gitlab.haskell.org//ghc/ghc/issues/16256)</th>
  <td>-fexternal-interpreter slow for profiling with template haskell</td></tr>
  <tr><th>[\#16257](https://gitlab.haskell.org//ghc/ghc/issues/16257)</th>
  <td>-fexternal-interpreter with external C shared library leads to undefined symbol during template haskell phase</td></tr>
  <tr><th>[\#16258](https://gitlab.haskell.org//ghc/ghc/issues/16258)</th>
  <td>PowerPC Big-Endian: ArithInt16, ArithInt8, ArithWord16, and ArithWord8 fail</td></tr>
  <tr><th>[\#16259](https://gitlab.haskell.org//ghc/ghc/issues/16259)</th>
  <td>Hadrian does not work with a cabal v2-installed "Happy"</td></tr>
  <tr><th>[\#16262](https://gitlab.haskell.org//ghc/ghc/issues/16262)</th>
  <td>Hadrian: respect dynamicGhcPrograms</td></tr>
  <tr><th>[\#16264](https://gitlab.haskell.org//ghc/ghc/issues/16264)</th>
  <td>Install reqlib'd libraries during CI</td></tr>
  <tr><th>[\#16265](https://gitlab.haskell.org//ghc/ghc/issues/16265)</th>
  <td>API Annotations: parens anns discarded for \`(\*)\` operator</td></tr>
  <tr><th>[\#16268](https://gitlab.haskell.org//ghc/ghc/issues/16268)</th>
  <td>Potential race condition in Hadrian?</td></tr>
  <tr><th>[\#16272](https://gitlab.haskell.org//ghc/ghc/issues/16272)</th>
  <td>libffi copies files into rts build tree</td></tr>
  <tr><th>[\#16273](https://gitlab.haskell.org//ghc/ghc/issues/16273)</th>
  <td>Hadrian turns on \`-Wno-unused-imports\` for text when using integer-simple</td></tr>
  <tr><th>[\#16275](https://gitlab.haskell.org//ghc/ghc/issues/16275)</th>
  <td>type hole in hs-boot file triggers GHC internal error</td></tr>
  <tr><th>[\#16278](https://gitlab.haskell.org//ghc/ghc/issues/16278)</th>
  <td>Exhaustivity checking GADT with free variables</td></tr>
  <tr><th>[\#16279](https://gitlab.haskell.org//ghc/ghc/issues/16279)</th>
  <td>Lexer: Alternate Layout Rule injects actual not virtual braces</td></tr>
  <tr><th>[\#16282](https://gitlab.haskell.org//ghc/ghc/issues/16282)</th>
  <td>all-missed-specializations doesn't suggest warning</td></tr>
  <tr><th>[\#16283](https://gitlab.haskell.org//ghc/ghc/issues/16283)</th>
  <td>StaticPointers pragma changes generated code even when the feature is not used</td></tr>
  <tr><th>[\#16285](https://gitlab.haskell.org//ghc/ghc/issues/16285)</th>
  <td>Hadrian fails to run the testsuite.</td></tr>
  <tr><th>[\#16286](https://gitlab.haskell.org//ghc/ghc/issues/16286)</th>
  <td>Continuations are not labelled in the binaries even with -g3</td></tr>
  <tr><th>[\#16287](https://gitlab.haskell.org//ghc/ghc/issues/16287)</th>
  <td>:kind accepts bogus type</td></tr>
  <tr><th>[\#16289](https://gitlab.haskell.org//ghc/ghc/issues/16289)</th>
  <td>GHC thinks pattern match is exhaustive</td></tr>
  <tr><th>[\#16292](https://gitlab.haskell.org//ghc/ghc/issues/16292)</th>
  <td>Wildcards in visible kind application</td></tr>
  <tr><th>[\#16295](https://gitlab.haskell.org//ghc/ghc/issues/16295)</th>
  <td>Enable cached builds with Hadrian</td></tr>
  <tr><th>[\#16296](https://gitlab.haskell.org//ghc/ghc/issues/16296)</th>
  <td>OccurAnal should not break the linter assumptions</td></tr>
  <tr><th>[\#16298](https://gitlab.haskell.org//ghc/ghc/issues/16298)</th>
  <td>Awful(?) code in AArch64 stg_BLACKHOLE entry code</td></tr>
  <tr><th>[\#16303](https://gitlab.haskell.org//ghc/ghc/issues/16303)</th>
  <td>checkStack sanity check fails</td></tr>
  <tr><th>[\#16305](https://gitlab.haskell.org//ghc/ghc/issues/16305)</th>
  <td>When should -Wmissed-specializations fire?</td></tr>
  <tr><th>[\#16306](https://gitlab.haskell.org//ghc/ghc/issues/16306)</th>
  <td>Hadrian: First test after a full rebuild normally fails due to an additional stdout line</td></tr>
  <tr><th>[\#16307](https://gitlab.haskell.org//ghc/ghc/issues/16307)</th>
  <td>gloss-examples' fluid program takes close to an eternity to compile</td></tr>
  <tr><th>[\#16308](https://gitlab.haskell.org//ghc/ghc/issues/16308)</th>
  <td>testsuite driver is sensitive to registration of multiple boot package versions</td></tr>
  <tr><th>[\#16312](https://gitlab.haskell.org//ghc/ghc/issues/16312)</th>
  <td>Optimization + adding an INLINE pragma triggers Core Lint error (Type of case alternatives not the same as the annotation on case)</td></tr>
  <tr><th>[\#16313](https://gitlab.haskell.org//ghc/ghc/issues/16313)</th>
  <td>Core Lint warning (Unsafe coercion: {left,right}-hand type is levity-polymorphic)</td></tr>
  <tr><th>[\#16317](https://gitlab.haskell.org//ghc/ghc/issues/16317)</th>
  <td>HIE files don't include module import/export information</td></tr>
  <tr><th>[\#16318](https://gitlab.haskell.org//ghc/ghc/issues/16318)</th>
  <td>Explicitly passing -package-env causes "Loaded package environment from .ghc.environment" message to be printed twice</td></tr>
  <tr><th>[\#16319](https://gitlab.haskell.org//ghc/ghc/issues/16319)</th>
  <td>unexpected difference between newtype and data</td></tr>
  <tr><th>[\#16320](https://gitlab.haskell.org//ghc/ghc/issues/16320)</th>
  <td>Clean up printing of foralls</td></tr>
  <tr><th>[\#16324](https://gitlab.haskell.org//ghc/ghc/issues/16324)</th>
  <td>ghc-pkg: Batched package unregister takes time linear to the batch size</td></tr>
  <tr><th>[\#16325](https://gitlab.haskell.org//ghc/ghc/issues/16325)</th>
  <td>Hadrian should respect the build root setting for the testsuite and its own binaries</td></tr>
  <tr><th>[\#16328](https://gitlab.haskell.org//ghc/ghc/issues/16328)</th>
  <td>NCG: Only adjust al before foreign calls if required.</td></tr>
  <tr><th>[\#16329](https://gitlab.haskell.org//ghc/ghc/issues/16329)</th>
  <td>Simplifier ticks exhausted when fusing list literals</td></tr>
  <tr><th>[\#16331](https://gitlab.haskell.org//ghc/ghc/issues/16331)</th>
  <td>REGRESSION: --supported-languages lies about supported languages, again</td></tr>
  <tr><th>[\#16336](https://gitlab.haskell.org//ghc/ghc/issues/16336)</th>
  <td>Build and deploy docker images on gitlab CI</td></tr>
  <tr><th>[\#16339](https://gitlab.haskell.org//ghc/ghc/issues/16339)</th>
  <td>Cannot put (.) or (!) type operators into an export list</td></tr>
  <tr><th>[\#16341](https://gitlab.haskell.org//ghc/ghc/issues/16341)</th>
  <td>Standalone deriving for GADTs should avoid impossible cases</td></tr>
  <tr><th>[\#16343](https://gitlab.haskell.org//ghc/ghc/issues/16343)</th>
  <td>Changing an irrelevant constraint leads to inlining failure</td></tr>
  <tr><th>[\#16344](https://gitlab.haskell.org//ghc/ghc/issues/16344)</th>
  <td>GHC infers over-polymorphic kinds</td></tr>
  <tr><th>[\#16345](https://gitlab.haskell.org//ghc/ghc/issues/16345)</th>
  <td>Duplicated allocation in case-of-known-constructor</td></tr>
  <tr><th>[\#16347](https://gitlab.haskell.org//ghc/ghc/issues/16347)</th>
  <td>GHC HEAD regression: piResultTys1</td></tr>
  <tr><th>[\#16349](https://gitlab.haskell.org//ghc/ghc/issues/16349)</th>
  <td>Nondeterministic T3424 timeouts on CI</td></tr>
  <tr><th>[\#16350](https://gitlab.haskell.org//ghc/ghc/issues/16350)</th>
  <td>Nondeterministic T5559 failures on CI</td></tr>
  <tr><th>[\#16352](https://gitlab.haskell.org//ghc/ghc/issues/16352)</th>
  <td>Hadrian: generation of PDF documents doesn't work</td></tr>
  <tr><th>[\#16353](https://gitlab.haskell.org//ghc/ghc/issues/16353)</th>
  <td>./validate --hadrian doesn't quite work on Windows</td></tr>
  <tr><th>[\#16354](https://gitlab.haskell.org//ghc/ghc/issues/16354)</th>
  <td>LLVM Backend generates invalid assembly.</td></tr>
  <tr><th>[\#16356](https://gitlab.haskell.org//ghc/ghc/issues/16356)</th>
  <td>Unexpected type application in default declaration</td></tr>
  <tr><th>[\#16357](https://gitlab.haskell.org//ghc/ghc/issues/16357)</th>
  <td>Add \`oneShot\` to the implementation of foldlM</td></tr>
  <tr><th>[\#16359](https://gitlab.haskell.org//ghc/ghc/issues/16359)</th>
  <td>Test runner warns of missing basline for performance tests with expected changes</td></tr>
  <tr><th>[\#16360](https://gitlab.haskell.org//ghc/ghc/issues/16360)</th>
  <td>GHC fails when GHC_PACKAGE_PATH contains trailing slash</td></tr>
  <tr><th>[\#16361](https://gitlab.haskell.org//ghc/ghc/issues/16361)</th>
  <td>Non-deterministic hs_try_putmvar00 failure on CI</td></tr>
  <tr><th>[\#16364](https://gitlab.haskell.org//ghc/ghc/issues/16364)</th>
  <td>Derived Enum for small number of constructors seems suboptimal</td></tr>
  <tr><th>[\#16365](https://gitlab.haskell.org//ghc/ghc/issues/16365)</th>
  <td>Inconsistency in quantified constraint solving</td></tr>
  <tr><th>[\#16370](https://gitlab.haskell.org//ghc/ghc/issues/16370)</th>
  <td>Hadrian: test T3807 failure</td></tr>
  <tr><th>[\#16371](https://gitlab.haskell.org//ghc/ghc/issues/16371)</th>
  <td>GHC should be more forgiving with visible dependent quantification in visible type applications</td></tr>
  <tr><th>[\#16372](https://gitlab.haskell.org//ghc/ghc/issues/16372)</th>
  <td>GHC can't constant fold even basic power (\^) applications for Int (and others?)</td></tr>
  <tr><th>[\#16373](https://gitlab.haskell.org//ghc/ghc/issues/16373)</th>
  <td>Strings from symbolVal not simplified at compile time</td></tr>
  <tr><th>[\#16374](https://gitlab.haskell.org//ghc/ghc/issues/16374)</th>
  <td>Cannot deduce constraint from itself with poly-kinded type family</td></tr>
  <tr><th>[\#16378](https://gitlab.haskell.org//ghc/ghc/issues/16378)</th>
  <td>bkpcabal01 fails with recent Cabal</td></tr>
  <tr><th>[\#16382](https://gitlab.haskell.org//ghc/ghc/issues/16382)</th>
  <td>Lifting a function from where clause to top level causes compilation time to triple</td></tr>
  <tr><th>[\#16383](https://gitlab.haskell.org//ghc/ghc/issues/16383)</th>
  <td>ghc: internal error: getMBlock: mmap: Invalid argument (gi-gtk on armv7)</td></tr>
  <tr><th>[\#16384](https://gitlab.haskell.org//ghc/ghc/issues/16384)</th>
  <td>GHC infers ill-kinded type for typed TH splice with unlifted values</td></tr>
  <tr><th>[\#16386](https://gitlab.haskell.org//ghc/ghc/issues/16386)</th>
  <td>T16219 fails on Windows</td></tr>
  <tr><th>[\#16387](https://gitlab.haskell.org//ghc/ghc/issues/16387)</th>
  <td>Duplicate vsnwprintf symbol on Windows</td></tr>
  <tr><th>[\#16388](https://gitlab.haskell.org//ghc/ghc/issues/16388)</th>
  <td>T15904 fails on Windows</td></tr>
  <tr><th>[\#16389](https://gitlab.haskell.org//ghc/ghc/issues/16389)</th>
  <td>T16190 fails on Windows</td></tr>
  <tr><th>[\#16390](https://gitlab.haskell.org//ghc/ghc/issues/16390)</th>
  <td>T10672 fails</td></tr>
  <tr><th>[\#16392](https://gitlab.haskell.org//ghc/ghc/issues/16392)</th>
  <td>In ghci, revertCAFs should be executed in the external interpreter when necessary</td></tr>
  <tr><th>[\#16395](https://gitlab.haskell.org//ghc/ghc/issues/16395)</th>
  <td>Sort out long tickets</td></tr>
  <tr><th>[\#16396](https://gitlab.haskell.org//ghc/ghc/issues/16396)</th>
  <td>TH doesn't preserve \`forall\`</td></tr>
  <tr><th>[\#16397](https://gitlab.haskell.org//ghc/ghc/issues/16397)</th>
  <td>--no-colour doesn't seem to do anything.</td></tr>
  <tr><th>[\#16398](https://gitlab.haskell.org//ghc/ghc/issues/16398)</th>
  <td>Missing documentation in Windows bindist tarball</td></tr>
  <tr><th>[\#16399](https://gitlab.haskell.org//ghc/ghc/issues/16399)</th>
  <td>Error when running --share</td></tr>
  <tr><th>[\#16400](https://gitlab.haskell.org//ghc/ghc/issues/16400)</th>
  <td>Hadrian: support shake's --lint-fsatrace feature</td></tr>
  <tr><th>[\#16402](https://gitlab.haskell.org//ghc/ghc/issues/16402)</th>
  <td>GHC doesnt' notice that (narrowWordFOO (x .&. FOO)) is just \`x\`.</td></tr>
  <tr><th>[\#16404](https://gitlab.haskell.org//ghc/ghc/issues/16404)</th>
  <td>Type error recovery crash</td></tr>
  <tr><th>[\#16405](https://gitlab.haskell.org//ghc/ghc/issues/16405)</th>
  <td>Plugin tests are remarkably fragile on Windows</td></tr>
  <tr><th>[\#16406](https://gitlab.haskell.org//ghc/ghc/issues/16406)</th>
  <td>Array code works with GHC 8.4.4, hangs with GHC 8.6.4</td></tr>
  <tr><th>[\#16407](https://gitlab.haskell.org//ghc/ghc/issues/16407)</th>
  <td>Non-versioned haddock.exe is not part of the Windows bindist tarball</td></tr>
  <tr><th>[\#16408](https://gitlab.haskell.org//ghc/ghc/issues/16408)</th>
  <td>incomplete ghc-8.6.4 windows binary tarball</td></tr>
  <tr><th>[\#16409](https://gitlab.haskell.org//ghc/ghc/issues/16409)</th>
  <td>fresh install crashes</td></tr>
  <tr><th>[\#16411](https://gitlab.haskell.org//ghc/ghc/issues/16411)</th>
  <td>Inconsistent -Wpartial-fields warnings with (\~) vs. (\~\~)</td></tr>
  <tr><th>[\#16413](https://gitlab.haskell.org//ghc/ghc/issues/16413)</th>
  <td>GHC bindist with DWARF support seems much slower than non-DWARF</td></tr></table>

   bugs, 

  <table><tr><th>[\#307](https://gitlab.haskell.org//ghc/ghc/issues/307)</th>
  <td>Implicit Parameters and monomorphism</td></tr>
  <tr><th>[\#393](https://gitlab.haskell.org//ghc/ghc/issues/393)</th>
  <td>functions without implementations</td></tr>
  <tr><th>[\#472](https://gitlab.haskell.org//ghc/ghc/issues/472)</th>
  <td>Supertyping of classes</td></tr>
  <tr><th>[\#728](https://gitlab.haskell.org//ghc/ghc/issues/728)</th>
  <td>switch to compacting collection when swapping occurs</td></tr>
  <tr><th>[\#750](https://gitlab.haskell.org//ghc/ghc/issues/750)</th>
  <td>Set -M, -H, -c and other memory-related values based on available virtual/physical memory</td></tr>
  <tr><th>[\#849](https://gitlab.haskell.org//ghc/ghc/issues/849)</th>
  <td>Offer control over branch prediction</td></tr>
  <tr><th>[\#860](https://gitlab.haskell.org//ghc/ghc/issues/860)</th>
  <td>CPP fails when a macro is used on a line containing a single quote character</td></tr>
  <tr><th>[\#881](https://gitlab.haskell.org//ghc/ghc/issues/881)</th>
  <td>Improve space profiling for references</td></tr>
  <tr><th>[\#913](https://gitlab.haskell.org//ghc/ghc/issues/913)</th>
  <td>instance Ord (StableName a)</td></tr>
  <tr><th>[\#989](https://gitlab.haskell.org//ghc/ghc/issues/989)</th>
  <td>Build GHC on Windows using Microsoft toolchain</td></tr>
  <tr><th>[\#1192](https://gitlab.haskell.org//ghc/ghc/issues/1192)</th>
  <td>GHC-only IOErrorType constructors, and is\*Error(Type) functions</td></tr>
  <tr><th>[\#1231](https://gitlab.haskell.org//ghc/ghc/issues/1231)</th>
  <td>deprecation warnings are reported too often</td></tr>
  <tr><th>[\#1273](https://gitlab.haskell.org//ghc/ghc/issues/1273)</th>
  <td>would like to print partial application values when at a breakpoint</td></tr>
  <tr><th>[\#1311](https://gitlab.haskell.org//ghc/ghc/issues/1311)</th>
  <td>newtypes of unboxed types disallowed - documentation bug and/or feature request</td></tr>
  <tr><th>[\#1365](https://gitlab.haskell.org//ghc/ghc/issues/1365)</th>
  <td>-fbyte-code is ignored in a OPTIONS_GHC pragma</td></tr>
  <tr><th>[\#1379](https://gitlab.haskell.org//ghc/ghc/issues/1379)</th>
  <td>Allow breakpoints and single-stepping for functions defined interactively</td></tr>
  <tr><th>[\#1399](https://gitlab.haskell.org//ghc/ghc/issues/1399)</th>
  <td>better support for developing threaded applications in ghci</td></tr>
  <tr><th>[\#1409](https://gitlab.haskell.org//ghc/ghc/issues/1409)</th>
  <td>Allow recursively dependent modules transparently (without .hs-boot or anything)</td></tr>
  <tr><th>[\#1420](https://gitlab.haskell.org//ghc/ghc/issues/1420)</th>
  <td>Automatic heap profile intervals</td></tr>
  <tr><th>[\#1444](https://gitlab.haskell.org//ghc/ghc/issues/1444)</th>
  <td>Template Haskell: add proper support for qualified names in non-splicing applications</td></tr>
  <tr><th>[\#1451](https://gitlab.haskell.org//ghc/ghc/issues/1451)</th>
  <td>Provide way to show the origin of a constraint</td></tr>
  <tr><th>[\#1475](https://gitlab.haskell.org//ghc/ghc/issues/1475)</th>
  <td>Adding imports and exports with Template Haskell</td></tr>
  <tr><th>[\#1532](https://gitlab.haskell.org//ghc/ghc/issues/1532)</th>
  <td>Implicit parameters are not available in breakpoints</td></tr>
  <tr><th>[\#1534](https://gitlab.haskell.org//ghc/ghc/issues/1534)</th>
  <td>\[Debugger\] Watch on accesses of "variables"</td></tr>
  <tr><th>[\#1628](https://gitlab.haskell.org//ghc/ghc/issues/1628)</th>
  <td>warning(s) for using stolen syntax that's not currently enabled</td></tr>
  <tr><th>[\#1768](https://gitlab.haskell.org//ghc/ghc/issues/1768)</th>
  <td>More flexible type signatures for data constructors</td></tr>
  <tr><th>[\#1800](https://gitlab.haskell.org//ghc/ghc/issues/1800)</th>
  <td>Template Haskell support for running functions defined in the same  module</td></tr>
  <tr><th>[\#1826](https://gitlab.haskell.org//ghc/ghc/issues/1826)</th>
  <td>unable to list source for \<exception thrown\> should never occur</td></tr>
  <tr><th>[\#1885](https://gitlab.haskell.org//ghc/ghc/issues/1885)</th>
  <td>Improve CPR analysis</td></tr>
  <tr><th>[\#1894](https://gitlab.haskell.org//ghc/ghc/issues/1894)</th>
  <td>Add a total order on type constructors</td></tr>
  <tr><th>[\#1921](https://gitlab.haskell.org//ghc/ghc/issues/1921)</th>
  <td>change default to support extensions that involve a change of syntax</td></tr>
  <tr><th>[\#1965](https://gitlab.haskell.org//ghc/ghc/issues/1965)</th>
  <td>Allow unconstrained existential contexts in newtypes</td></tr>
  <tr><th>[\#2041](https://gitlab.haskell.org//ghc/ghc/issues/2041)</th>
  <td>Allow splicing in concrete syntax</td></tr>
  <tr><th>[\#2075](https://gitlab.haskell.org//ghc/ghc/issues/2075)</th>
  <td>hpc should render information about the run in its html markup</td></tr>
  <tr><th>[\#2101](https://gitlab.haskell.org//ghc/ghc/issues/2101)</th>
  <td>Allow some form of type-level lemma</td></tr>
  <tr><th>[\#2119](https://gitlab.haskell.org//ghc/ghc/issues/2119)</th>
  <td>explicitly importing deprecated symbols should elicit the deprecation warning</td></tr>
  <tr><th>[\#2135](https://gitlab.haskell.org//ghc/ghc/issues/2135)</th>
  <td>Warn if functions are exported whose types cannot be written</td></tr>
  <tr><th>[\#2180](https://gitlab.haskell.org//ghc/ghc/issues/2180)</th>
  <td>Any installed signal handler stops deadlock detection, but XCPU never happens in a deadlock</td></tr>
  <tr><th>[\#2200](https://gitlab.haskell.org//ghc/ghc/issues/2200)</th>
  <td>big static random access arrays</td></tr>
  <tr><th>[\#2207](https://gitlab.haskell.org//ghc/ghc/issues/2207)</th>
  <td>Load the interface details for GHC.\* even without -O</td></tr>
  <tr><th>[\#2215](https://gitlab.haskell.org//ghc/ghc/issues/2215)</th>
  <td>:disable command to disable breakpoints</td></tr>
  <tr><th>[\#2258](https://gitlab.haskell.org//ghc/ghc/issues/2258)</th>
  <td>ghc --cleanup</td></tr>
  <tr><th>[\#2269](https://gitlab.haskell.org//ghc/ghc/issues/2269)</th>
  <td>Word type to Double or Float conversions are slower than Int conversions</td></tr>
  <tr><th>[\#2340](https://gitlab.haskell.org//ghc/ghc/issues/2340)</th>
  <td>Improve Template Haskell error recovery</td></tr>
  <tr><th>[\#2344](https://gitlab.haskell.org//ghc/ghc/issues/2344)</th>
  <td>oddity with package prefixes for data constructors</td></tr>
  <tr><th>[\#2345](https://gitlab.haskell.org//ghc/ghc/issues/2345)</th>
  <td>:browse limitations (browsing virtual namespaces, listing namespaces)</td></tr>
  <tr><th>[\#2365](https://gitlab.haskell.org//ghc/ghc/issues/2365)</th>
  <td>Warn about usage of \`OPTIONS_GHC -XLanguageExtension\`</td></tr>
  <tr><th>[\#2403](https://gitlab.haskell.org//ghc/ghc/issues/2403)</th>
  <td>caching for runghc (runhaskell)</td></tr>
  <tr><th>[\#2427](https://gitlab.haskell.org//ghc/ghc/issues/2427)</th>
  <td>Allow compilation of source from stdin</td></tr>
  <tr><th>[\#2460](https://gitlab.haskell.org//ghc/ghc/issues/2460)</th>
  <td>provide -mwindows option like gcc</td></tr>
  <tr><th>[\#2465](https://gitlab.haskell.org//ghc/ghc/issues/2465)</th>
  <td>Fusion of recursive functions</td></tr>
  <tr><th>[\#2522](https://gitlab.haskell.org//ghc/ghc/issues/2522)</th>
  <td>Warning for missing export lists</td></tr>
  <tr><th>[\#2550](https://gitlab.haskell.org//ghc/ghc/issues/2550)</th>
  <td>Add an option to read file names from a file instead of the command line</td></tr>
  <tr><th>[\#2551](https://gitlab.haskell.org//ghc/ghc/issues/2551)</th>
  <td>Allow multiple modules per source file</td></tr>
  <tr><th>[\#2595](https://gitlab.haskell.org//ghc/ghc/issues/2595)</th>
  <td>Implement record update for existential and GADT data types</td></tr>
  <tr><th>[\#2598](https://gitlab.haskell.org//ghc/ghc/issues/2598)</th>
  <td>Avoid excessive specialisation in SpecConstr</td></tr>
  <tr><th>[\#2614](https://gitlab.haskell.org//ghc/ghc/issues/2614)</th>
  <td>Enumeration of values for \`Sys.Info.os\`, \`Sys.Info.arch\`</td></tr>
  <tr><th>[\#2630](https://gitlab.haskell.org//ghc/ghc/issues/2630)</th>
  <td>installed packages should have a src-dirs field, for access to optionally installed sources</td></tr>
  <tr><th>[\#2640](https://gitlab.haskell.org//ghc/ghc/issues/2640)</th>
  <td>Treat -X flags consistently in GHCi</td></tr>
  <tr><th>[\#2641](https://gitlab.haskell.org//ghc/ghc/issues/2641)</th>
  <td>Revise the rules for -XExtendedDefaultRules</td></tr>
  <tr><th>[\#2648](https://gitlab.haskell.org//ghc/ghc/issues/2648)</th>
  <td>Report out of date interface files robustly</td></tr>
  <tr><th>[\#2708](https://gitlab.haskell.org//ghc/ghc/issues/2708)</th>
  <td>Error message should suggest UnboxedTuples language extension</td></tr>
  <tr><th>[\#2737](https://gitlab.haskell.org//ghc/ghc/issues/2737)</th>
  <td>add :tracelocal to ghci debugger to trace only the expressions in a given function</td></tr>
  <tr><th>[\#2742](https://gitlab.haskell.org//ghc/ghc/issues/2742)</th>
  <td>The -\> in ViewPatterns binds more weakly than infix data constructors.</td></tr>
  <tr><th>[\#2803](https://gitlab.haskell.org//ghc/ghc/issues/2803)</th>
  <td>bring full top level of a module in scope when a breakpoint is hit in the module</td></tr>
  <tr><th>[\#2867](https://gitlab.haskell.org//ghc/ghc/issues/2867)</th>
  <td>Make a way to tell GHC that a pragma name should be "recognised"</td></tr>
  <tr><th>[\#2895](https://gitlab.haskell.org//ghc/ghc/issues/2895)</th>
  <td>Implement the "Class System Extension" proposal</td></tr>
  <tr><th>[\#2896](https://gitlab.haskell.org//ghc/ghc/issues/2896)</th>
  <td>Warning suggestion: argument not necessarily a binary operator</td></tr>
  <tr><th>[\#2945](https://gitlab.haskell.org//ghc/ghc/issues/2945)</th>
  <td>add command :mergetrace</td></tr>
  <tr><th>[\#2946](https://gitlab.haskell.org//ghc/ghc/issues/2946)</th>
  <td>tracing should be controled by a global flag</td></tr>
  <tr><th>[\#2950](https://gitlab.haskell.org//ghc/ghc/issues/2950)</th>
  <td>show breakpoint numbers of breakpoints which were ignored during :force</td></tr>
  <tr><th>[\#2986](https://gitlab.haskell.org//ghc/ghc/issues/2986)</th>
  <td>:info printing instances often isn't wanted</td></tr>
  <tr><th>[\#3000](https://gitlab.haskell.org//ghc/ghc/issues/3000)</th>
  <td>:break command should recognize also nonexported top level symbols in qualified IDs</td></tr>
  <tr><th>[\#3052](https://gitlab.haskell.org//ghc/ghc/issues/3052)</th>
  <td>ghc FFI doesn't support thiscall</td></tr>
  <tr><th>[\#3085](https://gitlab.haskell.org//ghc/ghc/issues/3085)</th>
  <td>warn about language extensions that are not used</td></tr>
  <tr><th>[\#3192](https://gitlab.haskell.org//ghc/ghc/issues/3192)</th>
  <td>Add dynCompileCoreExpr :: GhcMonad m =\> Bool -\> Expr CoreBind -\> m Dynamic to ghc-api</td></tr>
  <tr><th>[\#3205](https://gitlab.haskell.org//ghc/ghc/issues/3205)</th>
  <td>Generalized isomorphic deriving</td></tr>
  <tr><th>[\#3282](https://gitlab.haskell.org//ghc/ghc/issues/3282)</th>
  <td>How to start an emacs editor within ghci asynchronously with :edit filename.hs      :set editor emacs &   don't go</td></tr>
  <tr><th>[\#3283](https://gitlab.haskell.org//ghc/ghc/issues/3283)</th>
  <td>Selective disabling of unused bind warnings</td></tr>
  <tr><th>[\#3372](https://gitlab.haskell.org//ghc/ghc/issues/3372)</th>
  <td>Allow for multiple linker instances</td></tr>
  <tr><th>[\#3427](https://gitlab.haskell.org//ghc/ghc/issues/3427)</th>
  <td>control what sort of entity a deprecated pragma applies to</td></tr>
  <tr><th>[\#3452](https://gitlab.haskell.org//ghc/ghc/issues/3452)</th>
  <td>Show type of most recent expression in GHCi</td></tr>
  <tr><th>[\#3464](https://gitlab.haskell.org//ghc/ghc/issues/3464)</th>
  <td>Find import declaration importing a certain function</td></tr>
  <tr><th>[\#3483](https://gitlab.haskell.org//ghc/ghc/issues/3483)</th>
  <td>Some mechanism for eliminating "absurd" patterns</td></tr>
  <tr><th>[\#3490](https://gitlab.haskell.org//ghc/ghc/issues/3490)</th>
  <td>Relax superclass restrictions</td></tr>
  <tr><th>[\#3517](https://gitlab.haskell.org//ghc/ghc/issues/3517)</th>
  <td>GHC has lots of extra hidden IOErrorType values</td></tr>
  <tr><th>[\#3541](https://gitlab.haskell.org//ghc/ghc/issues/3541)</th>
  <td>Allow local foreign imports</td></tr>
  <tr><th>[\#3545](https://gitlab.haskell.org//ghc/ghc/issues/3545)</th>
  <td>As-patterns for type signatures</td></tr>
  <tr><th>[\#3547](https://gitlab.haskell.org//ghc/ghc/issues/3547)</th>
  <td>Improve granularity of UndecidableInstances</td></tr>
  <tr><th>[\#3557](https://gitlab.haskell.org//ghc/ghc/issues/3557)</th>
  <td>CPU Vector instructions in GHC.Prim</td></tr>
  <tr><th>[\#3577](https://gitlab.haskell.org//ghc/ghc/issues/3577)</th>
  <td>Complete support for Data Parallel Haskell</td></tr>
  <tr><th>[\#3583](https://gitlab.haskell.org//ghc/ghc/issues/3583)</th>
  <td>Default view patterns</td></tr>
  <tr><th>[\#3601](https://gitlab.haskell.org//ghc/ghc/issues/3601)</th>
  <td>When running two or more instances of GHCi, persistent history is only kept for the first one</td></tr>
  <tr><th>[\#3615](https://gitlab.haskell.org//ghc/ghc/issues/3615)</th>
  <td>GHCi doesn't allow the use of imported data contructors</td></tr>
  <tr><th>[\#3619](https://gitlab.haskell.org//ghc/ghc/issues/3619)</th>
  <td>allow to set ghc search path globally (a'la CPATH)</td></tr>
  <tr><th>[\#3632](https://gitlab.haskell.org//ghc/ghc/issues/3632)</th>
  <td>lift restrictions on records with existential fields, especially in the presence of class constraints</td></tr>
  <tr><th>[\#3645](https://gitlab.haskell.org//ghc/ghc/issues/3645)</th>
  <td>Layout and pragmas</td></tr>
  <tr><th>[\#3701](https://gitlab.haskell.org//ghc/ghc/issues/3701)</th>
  <td>allow existential wrapper newtypes</td></tr>
  <tr><th>[\#3744](https://gitlab.haskell.org//ghc/ghc/issues/3744)</th>
  <td>Comparisons against minBound/maxBound not optimised for (Int\|Word)(8\|16\|32)</td></tr>
  <tr><th>[\#3753](https://gitlab.haskell.org//ghc/ghc/issues/3753)</th>
  <td>Make ghci's -l option consistent with GNU ld's -l option</td></tr>
  <tr><th>[\#3769](https://gitlab.haskell.org//ghc/ghc/issues/3769)</th>
  <td>Add manpages for programs installed alongside ghc</td></tr>
  <tr><th>[\#3786](https://gitlab.haskell.org//ghc/ghc/issues/3786)</th>
  <td>showing function arguments when stopped at its definition</td></tr>
  <tr><th>[\#3869](https://gitlab.haskell.org//ghc/ghc/issues/3869)</th>
  <td>RTS GC Statistics from -S should be logged via the eventlog system</td></tr>
  <tr><th>[\#3870](https://gitlab.haskell.org//ghc/ghc/issues/3870)</th>
  <td>Avoid Haddock-links to the Prelude</td></tr>
  <tr><th>[\#3895](https://gitlab.haskell.org//ghc/ghc/issues/3895)</th>
  <td>"Fix" pervasive-but-unnecessary signedness in GHC.Prim</td></tr>
  <tr><th>[\#3919](https://gitlab.haskell.org//ghc/ghc/issues/3919)</th>
  <td>Or-patterns as GHC extension</td></tr>
  <tr><th>[\#3980](https://gitlab.haskell.org//ghc/ghc/issues/3980)</th>
  <td>System.Posix.Signals should provide a way to set the SA_NOCLDWAIT flag</td></tr>
  <tr><th>[\#3984](https://gitlab.haskell.org//ghc/ghc/issues/3984)</th>
  <td>Handle multiline input in GHCi history</td></tr>
  <tr><th>[\#4016](https://gitlab.haskell.org//ghc/ghc/issues/4016)</th>
  <td>Strange display behaviour in GHCi</td></tr>
  <tr><th>[\#4020](https://gitlab.haskell.org//ghc/ghc/issues/4020)</th>
  <td>Please consider adding support for local type synonyms</td></tr>
  <tr><th>[\#4052](https://gitlab.haskell.org//ghc/ghc/issues/4052)</th>
  <td>Two sided sections</td></tr>
  <tr><th>[\#4091](https://gitlab.haskell.org//ghc/ghc/issues/4091)</th>
  <td>Parse error on curried context of instance declaration</td></tr>
  <tr><th>[\#4096](https://gitlab.haskell.org//ghc/ghc/issues/4096)</th>
  <td>New primops for indexing: index\*OffAddrUsing\# etc</td></tr>
  <tr><th>[\#4180](https://gitlab.haskell.org//ghc/ghc/issues/4180)</th>
  <td>do not consider associativity for unary minus for fixity resolution</td></tr>
  <tr><th>[\#4222](https://gitlab.haskell.org//ghc/ghc/issues/4222)</th>
  <td>Template Haskell lets you reify supposedly-abstract data types</td></tr>
  <tr><th>[\#4259](https://gitlab.haskell.org//ghc/ghc/issues/4259)</th>
  <td>Relax restrictions on type family instance overlap</td></tr>
  <tr><th>[\#4316](https://gitlab.haskell.org//ghc/ghc/issues/4316)</th>
  <td>Interactive "do" notation in GHCi</td></tr>
  <tr><th>[\#4329](https://gitlab.haskell.org//ghc/ghc/issues/4329)</th>
  <td>GHC.Conc modifyTVar primitive</td></tr>
  <tr><th>[\#4453](https://gitlab.haskell.org//ghc/ghc/issues/4453)</th>
  <td>Allow specifying .hi files of imports on command line in batch mode</td></tr>
  <tr><th>[\#4459](https://gitlab.haskell.org//ghc/ghc/issues/4459)</th>
  <td>Polymorphic Data.Dynamic</td></tr>
  <tr><th>[\#4470](https://gitlab.haskell.org//ghc/ghc/issues/4470)</th>
  <td>Loop optimization: identical counters</td></tr>
  <tr><th>[\#4479](https://gitlab.haskell.org//ghc/ghc/issues/4479)</th>
  <td>Implement TDNR</td></tr>
  <tr><th>[\#4823](https://gitlab.haskell.org//ghc/ghc/issues/4823)</th>
  <td>Loop strength reduction for array indexing</td></tr>
  <tr><th>[\#4879](https://gitlab.haskell.org//ghc/ghc/issues/4879)</th>
  <td>Deprecate exports</td></tr>
  <tr><th>[\#4894](https://gitlab.haskell.org//ghc/ghc/issues/4894)</th>
  <td>Missing improvement for fun. deps.</td></tr>
  <tr><th>[\#4900](https://gitlab.haskell.org//ghc/ghc/issues/4900)</th>
  <td>Consider usage files in the GHCi recompilation check</td></tr>
  <tr><th>[\#4913](https://gitlab.haskell.org//ghc/ghc/issues/4913)</th>
  <td>Make event tracing conditional on an RTS flag only</td></tr>
  <tr><th>[\#4937](https://gitlab.haskell.org//ghc/ghc/issues/4937)</th>
  <td>Remove indirections caused by sum types, such as Maybe</td></tr>
  <tr><th>[\#4955](https://gitlab.haskell.org//ghc/ghc/issues/4955)</th>
  <td>increase error message detail for module lookups failure due to hi references</td></tr>
  <tr><th>[\#4959](https://gitlab.haskell.org//ghc/ghc/issues/4959)</th>
  <td>Warning about variables with leading underscore that are used anyway</td></tr>
  <tr><th>[\#4980](https://gitlab.haskell.org//ghc/ghc/issues/4980)</th>
  <td>Warning about module abbreviation clashes</td></tr>
  <tr><th>[\#5016](https://gitlab.haskell.org//ghc/ghc/issues/5016)</th>
  <td>Make Template Haskell: -ddump-splices generate executable code</td></tr>
  <tr><th>[\#5059](https://gitlab.haskell.org//ghc/ghc/issues/5059)</th>
  <td>Pragma to SPECIALISE on value arguments</td></tr>
  <tr><th>[\#5073](https://gitlab.haskell.org//ghc/ghc/issues/5073)</th>
  <td>Add blockST for nested ST scopes</td></tr>
  <tr><th>[\#5075](https://gitlab.haskell.org//ghc/ghc/issues/5075)</th>
  <td>CPR optimisation for sum types if only one constructor is used</td></tr>
  <tr><th>[\#5108](https://gitlab.haskell.org//ghc/ghc/issues/5108)</th>
  <td>Allow unicode sub/superscript symbols in operators</td></tr>
  <tr><th>[\#5171](https://gitlab.haskell.org//ghc/ghc/issues/5171)</th>
  <td>Misfeature of Cmm optimiser: no way to extract a branch of expression into a separate statement</td></tr>
  <tr><th>[\#5197](https://gitlab.haskell.org//ghc/ghc/issues/5197)</th>
  <td>Support static linker semantics for archives and weak symbols</td></tr>
  <tr><th>[\#5218](https://gitlab.haskell.org//ghc/ghc/issues/5218)</th>
  <td>Add unpackCStringLen\# to create Strings from string literals</td></tr>
  <tr><th>[\#5219](https://gitlab.haskell.org//ghc/ghc/issues/5219)</th>
  <td>need a version of hs_init that returns an error code for command-line errors</td></tr>
  <tr><th>[\#5266](https://gitlab.haskell.org//ghc/ghc/issues/5266)</th>
  <td>Licensing requirements and copyright notices</td></tr>
  <tr><th>[\#5288](https://gitlab.haskell.org//ghc/ghc/issues/5288)</th>
  <td>Less noisy version of -fwarn-name-shadowing</td></tr>
  <tr><th>[\#5324](https://gitlab.haskell.org//ghc/ghc/issues/5324)</th>
  <td>Locally-scoped RULES</td></tr>
  <tr><th>[\#5344](https://gitlab.haskell.org//ghc/ghc/issues/5344)</th>
  <td>CSE should look through coercions</td></tr>
  <tr><th>[\#5392](https://gitlab.haskell.org//ghc/ghc/issues/5392)</th>
  <td>Warnings about impossible MPTCs would be nice</td></tr>
  <tr><th>[\#5416](https://gitlab.haskell.org//ghc/ghc/issues/5416)</th>
  <td>Local modules and Template Haskell declaration splices</td></tr>
  <tr><th>[\#5467](https://gitlab.haskell.org//ghc/ghc/issues/5467)</th>
  <td>Template Haskell: support for Haddock comments</td></tr>
  <tr><th>[\#5556](https://gitlab.haskell.org//ghc/ghc/issues/5556)</th>
  <td>Support pin-changing on ByteArray\#s</td></tr>
  <tr><th>[\#5590](https://gitlab.haskell.org//ghc/ghc/issues/5590)</th>
  <td>"guarded instances": instance selection can add extra parameters to the class</td></tr>
  <tr><th>[\#5619](https://gitlab.haskell.org//ghc/ghc/issues/5619)</th>
  <td>Shorter qualified import statements</td></tr>
  <tr><th>[\#5672](https://gitlab.haskell.org//ghc/ghc/issues/5672)</th>
  <td>parBufferWHNF could be less subtle</td></tr>
  <tr><th>[\#5813](https://gitlab.haskell.org//ghc/ghc/issues/5813)</th>
  <td>Offer a compiler warning for failable pattern matches</td></tr>
  <tr><th>[\#5823](https://gitlab.haskell.org//ghc/ghc/issues/5823)</th>
  <td>FFI and CAPI needs {-\# INCLUDE \#-} back?</td></tr>
  <tr><th>[\#5834](https://gitlab.haskell.org//ghc/ghc/issues/5834)</th>
  <td>Allow both INLINE and INLINABLE for the same function</td></tr>
  <tr><th>[\#5910](https://gitlab.haskell.org//ghc/ghc/issues/5910)</th>
  <td>Holes with other constraints</td></tr>
  <tr><th>[\#5918](https://gitlab.haskell.org//ghc/ghc/issues/5918)</th>
  <td>hsc2hs forces wordsize (i.e. -m32 or -m64) to be the choice of GHC instead of allowing a different (or no/default choice)</td></tr>
  <tr><th>[\#5941](https://gitlab.haskell.org//ghc/ghc/issues/5941)</th>
  <td>Add compilation stage plugins</td></tr>
  <tr><th>[\#5972](https://gitlab.haskell.org//ghc/ghc/issues/5972)</th>
  <td>option to suppress (Monomorphic) record selector functions</td></tr>
  <tr><th>[\#5985](https://gitlab.haskell.org//ghc/ghc/issues/5985)</th>
  <td>Type operators are not accepted as variables in contexts</td></tr>
  <tr><th>[\#6024](https://gitlab.haskell.org//ghc/ghc/issues/6024)</th>
  <td>Allow defining kinds alone, without a datatype</td></tr>
  <tr><th>[\#6030](https://gitlab.haskell.org//ghc/ghc/issues/6030)</th>
  <td>Typeclass constraint should pick the OverloadedString type.</td></tr>
  <tr><th>[\#6077](https://gitlab.haskell.org//ghc/ghc/issues/6077)</th>
  <td>Respect XDG_CONFIG_HOME</td></tr>
  <tr><th>[\#6089](https://gitlab.haskell.org//ghc/ghc/issues/6089)</th>
  <td>Allow declaration splices inside declaration brackets</td></tr>
  <tr><th>[\#7048](https://gitlab.haskell.org//ghc/ghc/issues/7048)</th>
  <td>Add the ability to statically define a \`FunPtr\` to a haskell function</td></tr>
  <tr><th>[\#7081](https://gitlab.haskell.org//ghc/ghc/issues/7081)</th>
  <td>arrow analogs of lambda case and multi-way if</td></tr>
  <tr><th>[\#7104](https://gitlab.haskell.org//ghc/ghc/issues/7104)</th>
  <td>Add tryWriteTBQueue to Control.Concurrent.STM.TBQueue</td></tr>
  <tr><th>[\#7140](https://gitlab.haskell.org//ghc/ghc/issues/7140)</th>
  <td>Allow type signature in export list</td></tr>
  <tr><th>[\#7152](https://gitlab.haskell.org//ghc/ghc/issues/7152)</th>
  <td>Add flag to configure that skips overwriting of symlinks on install</td></tr>
  <tr><th>[\#7158](https://gitlab.haskell.org//ghc/ghc/issues/7158)</th>
  <td>GHCi commands case insensitive</td></tr>
  <tr><th>[\#7204](https://gitlab.haskell.org//ghc/ghc/issues/7204)</th>
  <td>Use a class to control FFI marshalling</td></tr>
  <tr><th>[\#7261](https://gitlab.haskell.org//ghc/ghc/issues/7261)</th>
  <td>ghci's :info and :browse break encapsulation</td></tr>
  <tr><th>[\#7275](https://gitlab.haskell.org//ghc/ghc/issues/7275)</th>
  <td>Give more detailed information about PINNED data in a heap profile</td></tr>
  <tr><th>[\#7283](https://gitlab.haskell.org//ghc/ghc/issues/7283)</th>
  <td>Specialise INLINE functions</td></tr>
  <tr><th>[\#7285](https://gitlab.haskell.org//ghc/ghc/issues/7285)</th>
  <td>mkWeakMVar is non-compositional</td></tr>
  <tr><th>[\#7291](https://gitlab.haskell.org//ghc/ghc/issues/7291)</th>
  <td>hp2ps should cope with incomplete data</td></tr>
  <tr><th>[\#7300](https://gitlab.haskell.org//ghc/ghc/issues/7300)</th>
  <td>Allow CAFs kept reachable by FFI to be forcibly made unreachable for GC</td></tr>
  <tr><th>[\#7330](https://gitlab.haskell.org//ghc/ghc/issues/7330)</th>
  <td>Data Parallel Haskell (DPH) isn't usable yet.</td></tr>
  <tr><th>[\#7331](https://gitlab.haskell.org//ghc/ghc/issues/7331)</th>
  <td>Allow the evaluation of declaration splices in GHCi</td></tr>
  <tr><th>[\#7335](https://gitlab.haskell.org//ghc/ghc/issues/7335)</th>
  <td>Need for extra warning pragma for accidental pattern matching in do blocks</td></tr>
  <tr><th>[\#7337](https://gitlab.haskell.org//ghc/ghc/issues/7337)</th>
  <td>GHC does not generate great code for bit-level rotation</td></tr>
  <tr><th>[\#7395](https://gitlab.haskell.org//ghc/ghc/issues/7395)</th>
  <td>DefaultSignatures conflict with default implementations</td></tr>
  <tr><th>[\#7413](https://gitlab.haskell.org//ghc/ghc/issues/7413)</th>
  <td>runghc (runhaskell) should be able to reload code on editing</td></tr>
  <tr><th>[\#7492](https://gitlab.haskell.org//ghc/ghc/issues/7492)</th>
  <td>Generic1 deriving: Can we replace Rec1 f with f :.: Par1?</td></tr>
  <tr><th>[\#7494](https://gitlab.haskell.org//ghc/ghc/issues/7494)</th>
  <td>Allow compatible type synonyms to be the return type of a GADT data constructor.</td></tr>
  <tr><th>[\#7495](https://gitlab.haskell.org//ghc/ghc/issues/7495)</th>
  <td>generalizing overloaded list syntax to Sized Lists, HLists, HRecords, etc</td></tr>
  <tr><th>[\#7606](https://gitlab.haskell.org//ghc/ghc/issues/7606)</th>
  <td>Stride scheduling for Haskell threads with priorities</td></tr>
  <tr><th>[\#7635](https://gitlab.haskell.org//ghc/ghc/issues/7635)</th>
  <td>SafeHaskell implying other options</td></tr>
  <tr><th>[\#7647](https://gitlab.haskell.org//ghc/ghc/issues/7647)</th>
  <td>UNPACK polymorphic fields</td></tr>
  <tr><th>[\#7662](https://gitlab.haskell.org//ghc/ghc/issues/7662)</th>
  <td>Improve GC of mutable objects</td></tr>
  <tr><th>[\#7741](https://gitlab.haskell.org//ghc/ghc/issues/7741)</th>
  <td>Add SIMD support to x86/x86_64 NCG</td></tr>
  <tr><th>[\#7763](https://gitlab.haskell.org//ghc/ghc/issues/7763)</th>
  <td>Resource limits for Haskell</td></tr>
  <tr><th>[\#7808](https://gitlab.haskell.org//ghc/ghc/issues/7808)</th>
  <td>data families and TH names do not mix well (e.g. cannot use TH deriving)</td></tr>
  <tr><th>[\#7845](https://gitlab.haskell.org//ghc/ghc/issues/7845)</th>
  <td>RebindableSyntax should allow rebinding tuples and lists</td></tr>
  <tr><th>[\#7860](https://gitlab.haskell.org//ghc/ghc/issues/7860)</th>
  <td>Add more bit fiddling functions to 'integer-gmp'</td></tr>
  <tr><th>[\#7870](https://gitlab.haskell.org//ghc/ghc/issues/7870)</th>
  <td>Compilation errors break the complexity encapsulation on DSLs, impairs success in industry</td></tr>
  <tr><th>[\#7952](https://gitlab.haskell.org//ghc/ghc/issues/7952)</th>
  <td>Can cost-centre annotations be included in -ddump-simpl?</td></tr>
  <tr><th>[\#7977](https://gitlab.haskell.org//ghc/ghc/issues/7977)</th>
  <td>Optimization: Shift dropped list heads by coeffecient to prevent thunk generation</td></tr>
  <tr><th>[\#8012](https://gitlab.haskell.org//ghc/ghc/issues/8012)</th>
  <td>Warn when using Enum instance for Float or Double</td></tr>
  <tr><th>[\#8015](https://gitlab.haskell.org//ghc/ghc/issues/8015)</th>
  <td>Show warning when file-header pragma is used after module keyword</td></tr>
  <tr><th>[\#8043](https://gitlab.haskell.org//ghc/ghc/issues/8043)</th>
  <td>Feature Request : Qualified module exports</td></tr>
  <tr><th>[\#8045](https://gitlab.haskell.org//ghc/ghc/issues/8045)</th>
  <td>Move I/O manager benchmarks into the GHC tree</td></tr>
  <tr><th>[\#8046](https://gitlab.haskell.org//ghc/ghc/issues/8046)</th>
  <td>Make the timer management scale better across multicore</td></tr>
  <tr><th>[\#8061](https://gitlab.haskell.org//ghc/ghc/issues/8061)</th>
  <td>Support for Complex Double in Foreign Function Interface</td></tr>
  <tr><th>[\#8086](https://gitlab.haskell.org//ghc/ghc/issues/8086)</th>
  <td>Minimal install for GHC</td></tr>
  <tr><th>[\#8099](https://gitlab.haskell.org//ghc/ghc/issues/8099)</th>
  <td>Alternate syntax for indicating when a function is "fully applied" for purposes of inlining</td></tr>
  <tr><th>[\#8107](https://gitlab.haskell.org//ghc/ghc/issues/8107)</th>
  <td>need types to express constant argument  for primop correctness</td></tr>
  <tr><th>[\#8109](https://gitlab.haskell.org//ghc/ghc/issues/8109)</th>
  <td>Type family patterns should support as-patterns.</td></tr>
  <tr><th>[\#8150](https://gitlab.haskell.org//ghc/ghc/issues/8150)</th>
  <td>Foreign export requires redundant type signature</td></tr>
  <tr><th>[\#8161](https://gitlab.haskell.org//ghc/ghc/issues/8161)</th>
  <td>Associated type parameters that are more specific than the instance header</td></tr>
  <tr><th>[\#8171](https://gitlab.haskell.org//ghc/ghc/issues/8171)</th>
  <td>Extending ExtendedDefaultRules</td></tr>
  <tr><th>[\#8199](https://gitlab.haskell.org//ghc/ghc/issues/8199)</th>
  <td>Get rid of HEAP_ALLOCED on Windows (and other non-Linux platforms)</td></tr>
  <tr><th>[\#8206](https://gitlab.haskell.org//ghc/ghc/issues/8206)</th>
  <td>Add support for Portable Native Client</td></tr>
  <tr><th>[\#8220](https://gitlab.haskell.org//ghc/ghc/issues/8220)</th>
  <td>Macros / functions for source location</td></tr>
  <tr><th>[\#8252](https://gitlab.haskell.org//ghc/ghc/issues/8252)</th>
  <td>prefetch\# isn't as general as it should be (currently the general version isn't type safe)</td></tr>
  <tr><th>[\#8288](https://gitlab.haskell.org//ghc/ghc/issues/8288)</th>
  <td>add idris style EDSL support for deep embedding lambdas</td></tr>
  <tr><th>[\#8299](https://gitlab.haskell.org//ghc/ghc/issues/8299)</th>
  <td>Add richer data model address arithmetic: AddrDiff and AddrInt (ie d Int_ptr_diff and Int_ptr_size)</td></tr>
  <tr><th>[\#8304](https://gitlab.haskell.org//ghc/ghc/issues/8304)</th>
  <td>more lenient operator sections (RelaxedSections extension)</td></tr>
  <tr><th>[\#8311](https://gitlab.haskell.org//ghc/ghc/issues/8311)</th>
  <td>suboptimal code generated for even :: Int -\> Bool by NCG (x86, x86_64)</td></tr>
  <tr><th>[\#8325](https://gitlab.haskell.org//ghc/ghc/issues/8325)</th>
  <td>Pattern guards in anonymous functions</td></tr>
  <tr><th>[\#8354](https://gitlab.haskell.org//ghc/ghc/issues/8354)</th>
  <td>Add INLINE (or at least INLINABLE) pragmas for methods of Ord in ghc-prim</td></tr>
  <tr><th>[\#8364](https://gitlab.haskell.org//ghc/ghc/issues/8364)</th>
  <td>equip GHC with an accurate internal model of floating point</td></tr>
  <tr><th>[\#8372](https://gitlab.haskell.org//ghc/ghc/issues/8372)</th>
  <td>enable -dcmm-lint by default for .cmm input files</td></tr>
  <tr><th>[\#8398](https://gitlab.haskell.org//ghc/ghc/issues/8398)</th>
  <td>reify module list in TH</td></tr>
  <tr><th>[\#8400](https://gitlab.haskell.org//ghc/ghc/issues/8400)</th>
  <td>Migrate the RTS to use libuv (or libev, or libevent)</td></tr>
  <tr><th>[\#8404](https://gitlab.haskell.org//ghc/ghc/issues/8404)</th>
  <td>Default to turning on architecture specific optimizations in the codegen</td></tr>
  <tr><th>[\#8423](https://gitlab.haskell.org//ghc/ghc/issues/8423)</th>
  <td>Less conservative compatibility check for closed type families</td></tr>
  <tr><th>[\#8429](https://gitlab.haskell.org//ghc/ghc/issues/8429)</th>
  <td>GHC.Base.{breakpoint, breakpointCond} do nothing</td></tr>
  <tr><th>[\#8441](https://gitlab.haskell.org//ghc/ghc/issues/8441)</th>
  <td>Allow family instances in an hs-boot file</td></tr>
  <tr><th>[\#8460](https://gitlab.haskell.org//ghc/ghc/issues/8460)</th>
  <td>Annotation reification with types in TH</td></tr>
  <tr><th>[\#8477](https://gitlab.haskell.org//ghc/ghc/issues/8477)</th>
  <td>Allow inferring ambiguous types</td></tr>
  <tr><th>[\#8504](https://gitlab.haskell.org//ghc/ghc/issues/8504)</th>
  <td>Provide minor GC residency estimates</td></tr>
  <tr><th>[\#8516](https://gitlab.haskell.org//ghc/ghc/issues/8516)</th>
  <td>Add (-\>) representation and the Invariant class to GHC.Generics</td></tr>
  <tr><th>[\#8560](https://gitlab.haskell.org//ghc/ghc/issues/8560)</th>
  <td>Derive some generic representation for GADTs</td></tr>
  <tr><th>[\#8581](https://gitlab.haskell.org//ghc/ghc/issues/8581)</th>
  <td>Pattern synonym used in an expression context could have different constraints to pattern used in a pattern context</td></tr>
  <tr><th>[\#8583](https://gitlab.haskell.org//ghc/ghc/issues/8583)</th>
  <td>Associated pattern synonyms</td></tr>
  <tr><th>[\#8605](https://gitlab.haskell.org//ghc/ghc/issues/8605)</th>
  <td>Alternative, guard-like syntax for view patterns</td></tr>
  <tr><th>[\#8610](https://gitlab.haskell.org//ghc/ghc/issues/8610)</th>
  <td>Rebuild on a definition-based granularity</td></tr>
  <tr><th>[\#8634](https://gitlab.haskell.org//ghc/ghc/issues/8634)</th>
  <td>Relax functional dependency coherence check ("liberal coverage condition")</td></tr>
  <tr><th>[\#8643](https://gitlab.haskell.org//ghc/ghc/issues/8643)</th>
  <td>Silent name shadowing</td></tr>
  <tr><th>[\#8673](https://gitlab.haskell.org//ghc/ghc/issues/8673)</th>
  <td>GHC could generate GADT record selectors in more cases</td></tr>
  <tr><th>[\#8679](https://gitlab.haskell.org//ghc/ghc/issues/8679)</th>
  <td>Extend FunD data constructor with information about type signature</td></tr>
  <tr><th>[\#8695](https://gitlab.haskell.org//ghc/ghc/issues/8695)</th>
  <td>Arithmetic overflow from (minBound :: Int) \`quot\` (-1)</td></tr>
  <tr><th>[\#8697](https://gitlab.haskell.org//ghc/ghc/issues/8697)</th>
  <td>Type rationals</td></tr>
  <tr><th>[\#8707](https://gitlab.haskell.org//ghc/ghc/issues/8707)</th>
  <td>Kind inference fails in data instance definition</td></tr>
  <tr><th>[\#8751](https://gitlab.haskell.org//ghc/ghc/issues/8751)</th>
  <td>Show parenthesised output of expressions in ghci</td></tr>
  <tr><th>[\#8772](https://gitlab.haskell.org//ghc/ghc/issues/8772)</th>
  <td>ghci should save history more often</td></tr>
  <tr><th>[\#8809](https://gitlab.haskell.org//ghc/ghc/issues/8809)</th>
  <td>Prettier error messages?</td></tr>
  <tr><th>[\#8812](https://gitlab.haskell.org//ghc/ghc/issues/8812)</th>
  <td>Make \*_stub.c files available again</td></tr>
  <tr><th>[\#8816](https://gitlab.haskell.org//ghc/ghc/issues/8816)</th>
  <td>Make SPARC registerised again.</td></tr>
  <tr><th>[\#8828](https://gitlab.haskell.org//ghc/ghc/issues/8828)</th>
  <td>Type pattern synonyms</td></tr>
  <tr><th>[\#8844](https://gitlab.haskell.org//ghc/ghc/issues/8844)</th>
  <td>Pseudo terminal and process-1.2.0.0</td></tr>
  <tr><th>[\#8875](https://gitlab.haskell.org//ghc/ghc/issues/8875)</th>
  <td>Track Exceptions</td></tr>
  <tr><th>[\#8881](https://gitlab.haskell.org//ghc/ghc/issues/8881)</th>
  <td>No way to unsubscribe a bug</td></tr>
  <tr><th>[\#8903](https://gitlab.haskell.org//ghc/ghc/issues/8903)</th>
  <td>Add dead store elimination</td></tr>
  <tr><th>[\#8914](https://gitlab.haskell.org//ghc/ghc/issues/8914)</th>
  <td>Remove unnecessary constraints from MonadComprehensions and ParallelListComp</td></tr>
  <tr><th>[\#8924](https://gitlab.haskell.org//ghc/ghc/issues/8924)</th>
  <td>Text.ParserCombinators.ReadP needs some kind of "commit" or "cut" to force a single parse..</td></tr>
  <tr><th>[\#8927](https://gitlab.haskell.org//ghc/ghc/issues/8927)</th>
  <td>Multiple case match at once</td></tr>
  <tr><th>[\#8930](https://gitlab.haskell.org//ghc/ghc/issues/8930)</th>
  <td>GHC API: List of available Pragmas</td></tr>
  <tr><th>[\#8944](https://gitlab.haskell.org//ghc/ghc/issues/8944)</th>
  <td>Warn instead of stopping on misplaced Haddock comments</td></tr>
  <tr><th>[\#8955](https://gitlab.haskell.org//ghc/ghc/issues/8955)</th>
  <td>Syscall intrinsic</td></tr>
  <tr><th>[\#8967](https://gitlab.haskell.org//ghc/ghc/issues/8967)</th>
  <td>Add syntax for creating finite maps and sets</td></tr>
  <tr><th>[\#8989](https://gitlab.haskell.org//ghc/ghc/issues/8989)</th>
  <td>nofib should record and report more fine-grained binary size information</td></tr>
  <tr><th>[\#8997](https://gitlab.haskell.org//ghc/ghc/issues/8997)</th>
  <td>Warn about unused parameters in recursive definitions</td></tr>
  <tr><th>[\#9030](https://gitlab.haskell.org//ghc/ghc/issues/9030)</th>
  <td>An async exception handler that blocks throwTo until handler finishes running</td></tr>
  <tr><th>[\#9091](https://gitlab.haskell.org//ghc/ghc/issues/9091)</th>
  <td>print and/or apply constraints when showing info for typed holes</td></tr>
  <tr><th>[\#9112](https://gitlab.haskell.org//ghc/ghc/issues/9112)</th>
  <td>support for deriving Vector/MVector instances</td></tr>
  <tr><th>[\#9118](https://gitlab.haskell.org//ghc/ghc/issues/9118)</th>
  <td>Can't eta-reduce representational coercions</td></tr>
  <tr><th>[\#9120](https://gitlab.haskell.org//ghc/ghc/issues/9120)</th>
  <td>Cache intermediate powers</td></tr>
  <tr><th>[\#9137](https://gitlab.haskell.org//ghc/ghc/issues/9137)</th>
  <td>A way to match RULES only for literals</td></tr>
  <tr><th>[\#9143](https://gitlab.haskell.org//ghc/ghc/issues/9143)</th>
  <td>feature request: way to set actual program argv</td></tr>
  <tr><th>[\#9180](https://gitlab.haskell.org//ghc/ghc/issues/9180)</th>
  <td>New magic function \`staticError\`</td></tr>
  <tr><th>[\#9182](https://gitlab.haskell.org//ghc/ghc/issues/9182)</th>
  <td>Empty case analysis for function clauses</td></tr>
  <tr><th>[\#9183](https://gitlab.haskell.org//ghc/ghc/issues/9183)</th>
  <td>GHC shouldn't expand constraint synonyms</td></tr>
  <tr><th>[\#9192](https://gitlab.haskell.org//ghc/ghc/issues/9192)</th>
  <td>Add sameByteArray\#</td></tr>
  <tr><th>[\#9197](https://gitlab.haskell.org//ghc/ghc/issues/9197)</th>
  <td>FFI types should be usable in foreign import decls without revealing representations</td></tr>
  <tr><th>[\#9207](https://gitlab.haskell.org//ghc/ghc/issues/9207)</th>
  <td>Detect obvious cases of infinite recursion.</td></tr>
  <tr><th>[\#9244](https://gitlab.haskell.org//ghc/ghc/issues/9244)</th>
  <td>Compiler could warn about type variable shadowing, and hint about ScopedTypeVariables</td></tr>
  <tr><th>[\#9269](https://gitlab.haskell.org//ghc/ghc/issues/9269)</th>
  <td>Type families returning quantified types</td></tr>
  <tr><th>[\#9289](https://gitlab.haskell.org//ghc/ghc/issues/9289)</th>
  <td>add anyToAddr\# :: (\#a\#)-\> Addr\# primop (inverse of addrToAny\#)</td></tr>
  <tr><th>[\#9319](https://gitlab.haskell.org//ghc/ghc/issues/9319)</th>
  <td>nofib-analyze doesn’t provide per-benchmark compile time/alloc numbers</td></tr>
  <tr><th>[\#9321](https://gitlab.haskell.org//ghc/ghc/issues/9321)</th>
  <td>Support for waiting on multiple MVars</td></tr>
  <tr><th>[\#9328](https://gitlab.haskell.org//ghc/ghc/issues/9328)</th>
  <td>MINIMAL pragma should supprt negation</td></tr>
  <tr><th>[\#9334](https://gitlab.haskell.org//ghc/ghc/issues/9334)</th>
  <td>Implement "instance chains"</td></tr>
  <tr><th>[\#9342](https://gitlab.haskell.org//ghc/ghc/issues/9342)</th>
  <td>Branchless arithmetic operations</td></tr>
  <tr><th>[\#9350](https://gitlab.haskell.org//ghc/ghc/issues/9350)</th>
  <td>Consider using xchg instead of mfence for CS stores</td></tr>
  <tr><th>[\#9351](https://gitlab.haskell.org//ghc/ghc/issues/9351)</th>
  <td>add ability to version symbols .c for packages with C code</td></tr>
  <tr><th>[\#9352](https://gitlab.haskell.org//ghc/ghc/issues/9352)</th>
  <td>Allow \`State\# s\` argument/result types in \`ccall\` FFI imports</td></tr>
  <tr><th>[\#9365](https://gitlab.haskell.org//ghc/ghc/issues/9365)</th>
  <td>Make command key in GHCi configurable</td></tr>
  <tr><th>[\#9376](https://gitlab.haskell.org//ghc/ghc/issues/9376)</th>
  <td>More informative error messages when closed type families fail to simplify</td></tr>
  <tr><th>[\#9392](https://gitlab.haskell.org//ghc/ghc/issues/9392)</th>
  <td>"\\n" is displayed weirdly in error messages</td></tr>
  <tr><th>[\#9394](https://gitlab.haskell.org//ghc/ghc/issues/9394)</th>
  <td>Show data/type family instances with ghci's :info command</td></tr>
  <tr><th>[\#9419](https://gitlab.haskell.org//ghc/ghc/issues/9419)</th>
  <td>Machine-readable output for profiling</td></tr>
  <tr><th>[\#9427](https://gitlab.haskell.org//ghc/ghc/issues/9427)</th>
  <td>Do finer-grained dependency analysis to infer more general kinds on type/class declarations</td></tr>
  <tr><th>[\#9429](https://gitlab.haskell.org//ghc/ghc/issues/9429)</th>
  <td>Alternative to type family Any</td></tr>
  <tr><th>[\#9431](https://gitlab.haskell.org//ghc/ghc/issues/9431)</th>
  <td>integer-gmp small Integer multiplication does two multiplications on x86</td></tr>
  <tr><th>[\#9438](https://gitlab.haskell.org//ghc/ghc/issues/9438)</th>
  <td>Dynamic GHCi doesn't support static libraries</td></tr>
  <tr><th>[\#9447](https://gitlab.haskell.org//ghc/ghc/issues/9447)</th>
  <td>Add support for resizing \`MutableByteArray\#\`s</td></tr>
  <tr><th>[\#9498](https://gitlab.haskell.org//ghc/ghc/issues/9498)</th>
  <td>GHC links against unversioned .so files</td></tr>
  <tr><th>[\#9518](https://gitlab.haskell.org//ghc/ghc/issues/9518)</th>
  <td>Improve error message for unacceptable role annotations</td></tr>
  <tr><th>[\#9522](https://gitlab.haskell.org//ghc/ghc/issues/9522)</th>
  <td>SPECIALISE pragmas for derived instances</td></tr>
  <tr><th>[\#9571](https://gitlab.haskell.org//ghc/ghc/issues/9571)</th>
  <td>nofib should use criterion-style bootstrapping/sampling</td></tr>
  <tr><th>[\#9601](https://gitlab.haskell.org//ghc/ghc/issues/9601)</th>
  <td>Make the rewrite rule system more powerful</td></tr>
  <tr><th>[\#9617](https://gitlab.haskell.org//ghc/ghc/issues/9617)</th>
  <td>Implement \`quot\` and \`rem\` using \`quotRem\`; implement \`div\` and \`mod\` using \`divMod\`</td></tr>
  <tr><th>[\#9622](https://gitlab.haskell.org//ghc/ghc/issues/9622)</th>
  <td>GHCi command to solve a constraint</td></tr>
  <tr><th>[\#9624](https://gitlab.haskell.org//ghc/ghc/issues/9624)</th>
  <td>"Unlikely constraint" recognition</td></tr>
  <tr><th>[\#9642](https://gitlab.haskell.org//ghc/ghc/issues/9642)</th>
  <td>LANGUAGE pragma synonyms</td></tr>
  <tr><th>[\#9645](https://gitlab.haskell.org//ghc/ghc/issues/9645)</th>
  <td>Optimize range checks for primitive types</td></tr>
  <tr><th>[\#9649](https://gitlab.haskell.org//ghc/ghc/issues/9649)</th>
  <td>symbols should/might be type level lists of chars</td></tr>
  <tr><th>[\#9659](https://gitlab.haskell.org//ghc/ghc/issues/9659)</th>
  <td>Offer branchless conditional (CMOV) primop</td></tr>
  <tr><th>[\#9661](https://gitlab.haskell.org//ghc/ghc/issues/9661)</th>
  <td>Branchless ==\# is compiled to branchy code</td></tr>
  <tr><th>[\#9667](https://gitlab.haskell.org//ghc/ghc/issues/9667)</th>
  <td>Type inference is weaker for GADT than analogous Data Family</td></tr>
  <tr><th>[\#9671](https://gitlab.haskell.org//ghc/ghc/issues/9671)</th>
  <td>Allow expressions in patterns</td></tr>
  <tr><th>[\#9685](https://gitlab.haskell.org//ghc/ghc/issues/9685)</th>
  <td>GHC fails to build with mingw32-make on Windows</td></tr>
  <tr><th>[\#9688](https://gitlab.haskell.org//ghc/ghc/issues/9688)</th>
  <td>Improve the interaction between CSE and the join point transformation</td></tr>
  <tr><th>[\#9690](https://gitlab.haskell.org//ghc/ghc/issues/9690)</th>
  <td>in GHCi map \`:editNNN\` to $EDITOR +NNN</td></tr>
  <tr><th>[\#9699](https://gitlab.haskell.org//ghc/ghc/issues/9699)</th>
  <td>TH function to list names in scope</td></tr>
  <tr><th>[\#9700](https://gitlab.haskell.org//ghc/ghc/issues/9700)</th>
  <td>Support C structures in Haskell FFI</td></tr>
  <tr><th>[\#9702](https://gitlab.haskell.org//ghc/ghc/issues/9702)</th>
  <td>Offer a weaker name shadowing warning</td></tr>
  <tr><th>[\#9724](https://gitlab.haskell.org//ghc/ghc/issues/9724)</th>
  <td>reexport IsList class from a trustworthy module</td></tr>
  <tr><th>[\#9731](https://gitlab.haskell.org//ghc/ghc/issues/9731)</th>
  <td>Inductive type definitions on Nat</td></tr>
  <tr><th>[\#9743](https://gitlab.haskell.org//ghc/ghc/issues/9743)</th>
  <td>Expose ghc-bin code as a library</td></tr>
  <tr><th>[\#9748](https://gitlab.haskell.org//ghc/ghc/issues/9748)</th>
  <td>Disambiguate IO actions in GHCi with :set +t</td></tr>
  <tr><th>[\#9756](https://gitlab.haskell.org//ghc/ghc/issues/9756)</th>
  <td>Warn about unnecessary unsafeCoerce</td></tr>
  <tr><th>[\#9784](https://gitlab.haskell.org//ghc/ghc/issues/9784)</th>
  <td>Improve error message for misplaced quote inside promoted qualified type</td></tr>
  <tr><th>[\#9789](https://gitlab.haskell.org//ghc/ghc/issues/9789)</th>
  <td>Make GHC accept .format+lhs as extension for literate haskell files</td></tr>
  <tr><th>[\#9790](https://gitlab.haskell.org//ghc/ghc/issues/9790)</th>
  <td>Produce coercion rules for derived Functor instances</td></tr>
  <tr><th>[\#9795](https://gitlab.haskell.org//ghc/ghc/issues/9795)</th>
  <td>Debug.Trace.trace is too strict</td></tr>
  <tr><th>[\#9819](https://gitlab.haskell.org//ghc/ghc/issues/9819)</th>
  <td>Create typesafe method of obtaining dictionary types from class definitions, and constraint objects from dictionary types</td></tr>
  <tr><th>[\#9835](https://gitlab.haskell.org//ghc/ghc/issues/9835)</th>
  <td>Add bindings for marshaling to/from mpz_t</td></tr>
  <tr><th>[\#9846](https://gitlab.haskell.org//ghc/ghc/issues/9846)</th>
  <td>GHC --make should also look for .hi, not only for .hs</td></tr>
  <tr><th>[\#9864](https://gitlab.haskell.org//ghc/ghc/issues/9864)</th>
  <td>Need realloc/resize feature for mallocForeignPtrBytes allocated memory</td></tr>
  <tr><th>[\#9866](https://gitlab.haskell.org//ghc/ghc/issues/9866)</th>
  <td>ssh pubkey self-mgmt</td></tr>
  <tr><th>[\#9883](https://gitlab.haskell.org//ghc/ghc/issues/9883)</th>
  <td>Make OverloadedLists more usable by splitting the class interface</td></tr>
  <tr><th>[\#9898](https://gitlab.haskell.org//ghc/ghc/issues/9898)</th>
  <td>Wanted: higher-order type-level programming</td></tr>
  <tr><th>[\#9908](https://gitlab.haskell.org//ghc/ghc/issues/9908)</th>
  <td>Improve enumFromX support for OverloadedLists</td></tr>
  <tr><th>[\#9923](https://gitlab.haskell.org//ghc/ghc/issues/9923)</th>
  <td>Offer copy-on-GC sliced arrays</td></tr>
  <tr><th>[\#9931](https://gitlab.haskell.org//ghc/ghc/issues/9931)</th>
  <td>Option to truncate Show output in ghci REPL</td></tr>
  <tr><th>[\#9938](https://gitlab.haskell.org//ghc/ghc/issues/9938)</th>
  <td>GHC's link step needs to be told which packages to link</td></tr>
  <tr><th>[\#9946](https://gitlab.haskell.org//ghc/ghc/issues/9946)</th>
  <td>Expose the source location of template-haskell Names</td></tr>
  <tr><th>[\#9948](https://gitlab.haskell.org//ghc/ghc/issues/9948)</th>
  <td>Recommend class constraint instead of instance constraint</td></tr>
  <tr><th>[\#9974](https://gitlab.haskell.org//ghc/ghc/issues/9974)</th>
  <td>Allow more general structural recursion without UndecidableInstances</td></tr>
  <tr><th>[\#9990](https://gitlab.haskell.org//ghc/ghc/issues/9990)</th>
  <td>Top level module identifiers shadow imported identifiers</td></tr>
  <tr><th>[\#9993](https://gitlab.haskell.org//ghc/ghc/issues/9993)</th>
  <td>PostfixOperators doesn't work for types</td></tr>
  <tr><th>[\#9995](https://gitlab.haskell.org//ghc/ghc/issues/9995)</th>
  <td>:info enhancements</td></tr>
  <tr><th>[\#10016](https://gitlab.haskell.org//ghc/ghc/issues/10016)</th>
  <td>UNPACK support for existentials</td></tr>
  <tr><th>[\#10049](https://gitlab.haskell.org//ghc/ghc/issues/10049)</th>
  <td>Lower level memcpy primop</td></tr>
  <tr><th>[\#10055](https://gitlab.haskell.org//ghc/ghc/issues/10055)</th>
  <td>Offer PolyKinded instances for Data.Fixed.HasResolution</td></tr>
  <tr><th>[\#10063](https://gitlab.haskell.org//ghc/ghc/issues/10063)</th>
  <td>State a law for foldMap</td></tr>
  <tr><th>[\#10071](https://gitlab.haskell.org//ghc/ghc/issues/10071)</th>
  <td>Implement deprecation-warnings for class-methods to non-method transitions</td></tr>
  <tr><th>[\#10076](https://gitlab.haskell.org//ghc/ghc/issues/10076)</th>
  <td>Don't suppress warnings in the presence of errors</td></tr>
  <tr><th>[\#10084](https://gitlab.haskell.org//ghc/ghc/issues/10084)</th>
  <td>Data.List should have a takeLastN function</td></tr>
  <tr><th>[\#10087](https://gitlab.haskell.org//ghc/ghc/issues/10087)</th>
  <td>DefaultSignatures: error message mentions internal name</td></tr>
  <tr><th>[\#10089](https://gitlab.haskell.org//ghc/ghc/issues/10089)</th>
  <td>feature: warn about unused data definitions (with typeclass instances)</td></tr>
  <tr><th>[\#10116](https://gitlab.haskell.org//ghc/ghc/issues/10116)</th>
  <td>Closed type families: Warn if it doesn't handle all cases</td></tr>
  <tr><th>[\#10150](https://gitlab.haskell.org//ghc/ghc/issues/10150)</th>
  <td>Suppress orphan instance warning per instance</td></tr>
  <tr><th>[\#10204](https://gitlab.haskell.org//ghc/ghc/issues/10204)</th>
  <td>Odd interaction between rank-2 types and type families</td></tr>
  <tr><th>[\#10225](https://gitlab.haskell.org//ghc/ghc/issues/10225)</th>
  <td>GHC does not specialize based on type equality</td></tr>
  <tr><th>[\#10235](https://gitlab.haskell.org//ghc/ghc/issues/10235)</th>
  <td>Get profiling info without stopping program</td></tr>
  <tr><th>[\#10324](https://gitlab.haskell.org//ghc/ghc/issues/10324)</th>
  <td>our rts/ghc-prim/base shared library tricks don't work on Android</td></tr>
  <tr><th>[\#10327](https://gitlab.haskell.org//ghc/ghc/issues/10327)</th>
  <td>Devise workaround for how infinite types prevent closed type family reduction</td></tr>
  <tr><th>[\#10331](https://gitlab.haskell.org//ghc/ghc/issues/10331)</th>
  <td>Accept HsSyn in splices and generate it in quotes (ghc-api)</td></tr>
  <tr><th>[\#10336](https://gitlab.haskell.org//ghc/ghc/issues/10336)</th>
  <td>Support qualified self {-\# SOURCE \#-} import</td></tr>
  <tr><th>[\#10350](https://gitlab.haskell.org//ghc/ghc/issues/10350)</th>
  <td>Should be able to specify path for eventlog output.</td></tr>
  <tr><th>[\#10383](https://gitlab.haskell.org//ghc/ghc/issues/10383)</th>
  <td>AArch64: get GHC Calling convention working</td></tr>
  <tr><th>[\#10391](https://gitlab.haskell.org//ghc/ghc/issues/10391)</th>
  <td>Ability to get export list of TH reified module</td></tr>
  <tr><th>[\#10431](https://gitlab.haskell.org//ghc/ghc/issues/10431)</th>
  <td>EqualityConstraints extension?</td></tr>
  <tr><th>[\#10465](https://gitlab.haskell.org//ghc/ghc/issues/10465)</th>
  <td>Make listArray non-strict in structure of argument list</td></tr>
  <tr><th>[\#10478](https://gitlab.haskell.org//ghc/ghc/issues/10478)</th>
  <td>Shorter import syntax</td></tr>
  <tr><th>[\#10505](https://gitlab.haskell.org//ghc/ghc/issues/10505)</th>
  <td>more specific types in the generated \*_stub.h files</td></tr>
  <tr><th>[\#10514](https://gitlab.haskell.org//ghc/ghc/issues/10514)</th>
  <td>Generic for existential types</td></tr>
  <tr><th>[\#10606](https://gitlab.haskell.org//ghc/ghc/issues/10606)</th>
  <td>avoid redundant stores to the stack when examining already-tagged data</td></tr>
  <tr><th>[\#10613](https://gitlab.haskell.org//ghc/ghc/issues/10613)</th>
  <td>Mechanism for checking that we only enter single-entry thunks once</td></tr>
  <tr><th>[\#10621](https://gitlab.haskell.org//ghc/ghc/issues/10621)</th>
  <td>Handle annotations in hsig/boot files</td></tr>
  <tr><th>[\#10652](https://gitlab.haskell.org//ghc/ghc/issues/10652)</th>
  <td>Better cache performance in Array\#</td></tr>
  <tr><th>[\#10674](https://gitlab.haskell.org//ghc/ghc/issues/10674)</th>
  <td>Expose OSThreadID and assorted functions from Haskell</td></tr>
  <tr><th>[\#10681](https://gitlab.haskell.org//ghc/ghc/issues/10681)</th>
  <td>Teach GHC to interpret all hs files as two levels of hs-boot files (abstract types only/full types + values)</td></tr>
  <tr><th>[\#10708](https://gitlab.haskell.org//ghc/ghc/issues/10708)</th>
  <td>Rejection of constant functions defined using conditional pattern matching</td></tr>
  <tr><th>[\#10741](https://gitlab.haskell.org//ghc/ghc/issues/10741)</th>
  <td>add flag to dump module and package dependencies</td></tr>
  <tr><th>[\#10756](https://gitlab.haskell.org//ghc/ghc/issues/10756)</th>
  <td>Allow users to indicate inaccessible patterns</td></tr>
  <tr><th>[\#10776](https://gitlab.haskell.org//ghc/ghc/issues/10776)</th>
  <td>DataKinds promotion of String -\> Symbol and Natural -\> Nat</td></tr>
  <tr><th>[\#10789](https://gitlab.haskell.org//ghc/ghc/issues/10789)</th>
  <td>Notify user when a kind mismatch holds up a type family reduction</td></tr>
  <tr><th>[\#10794](https://gitlab.haskell.org//ghc/ghc/issues/10794)</th>
  <td>Extension request: "where" clauses in type signatures</td></tr>
  <tr><th>[\#10803](https://gitlab.haskell.org//ghc/ghc/issues/10803)</th>
  <td>New SignatureSections extension</td></tr>
  <tr><th>[\#10804](https://gitlab.haskell.org//ghc/ghc/issues/10804)</th>
  <td>Rules conditional on strictess properties</td></tr>
  <tr><th>[\#10809](https://gitlab.haskell.org//ghc/ghc/issues/10809)</th>
  <td>Add prefetch{Small}{Mutable}Array\[0..3\]\#</td></tr>
  <tr><th>[\#10827](https://gitlab.haskell.org//ghc/ghc/issues/10827)</th>
  <td>GHCi should support interpeting multiple packages/units with separate DynFlags</td></tr>
  <tr><th>[\#10832](https://gitlab.haskell.org//ghc/ghc/issues/10832)</th>
  <td>Generalize injective type families</td></tr>
  <tr><th>[\#10833](https://gitlab.haskell.org//ghc/ghc/issues/10833)</th>
  <td>Use injective type families (decomposition) when dealing with givens</td></tr>
  <tr><th>[\#10841](https://gitlab.haskell.org//ghc/ghc/issues/10841)</th>
  <td>Run handler on STM retry</td></tr>
  <tr><th>[\#10842](https://gitlab.haskell.org//ghc/ghc/issues/10842)</th>
  <td>"Reactive" Template Haskell</td></tr>
  <tr><th>[\#10871](https://gitlab.haskell.org//ghc/ghc/issues/10871)</th>
  <td>Implement "fat" interface files which can be directly compiled without source</td></tr>
  <tr><th>[\#10887](https://gitlab.haskell.org//ghc/ghc/issues/10887)</th>
  <td>Please export GhcMake.downsweep and make it return a partial module graph in case of errors</td></tr>
  <tr><th>[\#10893](https://gitlab.haskell.org//ghc/ghc/issues/10893)</th>
  <td>Consistent error message suggestions for using language extensions</td></tr>
  <tr><th>[\#10903](https://gitlab.haskell.org//ghc/ghc/issues/10903)</th>
  <td>Add an option to infer CallStack implicit parameters</td></tr>
  <tr><th>[\#10906](https://gitlab.haskell.org//ghc/ghc/issues/10906)</th>
  <td>\`SPECIALIZE instance\` could be better</td></tr>
  <tr><th>[\#10912](https://gitlab.haskell.org//ghc/ghc/issues/10912)</th>
  <td>Support for out of the box static linking</td></tr>
  <tr><th>[\#10915](https://gitlab.haskell.org//ghc/ghc/issues/10915)</th>
  <td>Statistical profiling support in the RTS</td></tr>
  <tr><th>[\#10925](https://gitlab.haskell.org//ghc/ghc/issues/10925)</th>
  <td>GHC should inform where unknown identifiers originate whenever possible</td></tr>
  <tr><th>[\#10933](https://gitlab.haskell.org//ghc/ghc/issues/10933)</th>
  <td>REMOVED pragma</td></tr>
  <tr><th>[\#10956](https://gitlab.haskell.org//ghc/ghc/issues/10956)</th>
  <td>Allow default keyboard behavior to be easily overriden</td></tr>
  <tr><th>[\#10972](https://gitlab.haskell.org//ghc/ghc/issues/10972)</th>
  <td>Add a :binfo (beginner info) GHCi command</td></tr>
  <tr><th>[\#10976](https://gitlab.haskell.org//ghc/ghc/issues/10976)</th>
  <td>Applicative Comprehensions</td></tr>
  <tr><th>[\#10978](https://gitlab.haskell.org//ghc/ghc/issues/10978)</th>
  <td>Anonymous type instances</td></tr>
  <tr><th>[\#10985](https://gitlab.haskell.org//ghc/ghc/issues/10985)</th>
  <td>When a "non-exhaustive pattern"-error occurs, output the arguments (if possible)</td></tr>
  <tr><th>[\#10986](https://gitlab.haskell.org//ghc/ghc/issues/10986)</th>
  <td>GHC should delete all temporary files it creates in /tmp</td></tr>
  <tr><th>[\#11012](https://gitlab.haskell.org//ghc/ghc/issues/11012)</th>
  <td>Support for unicode primes on identifiers.</td></tr>
  <tr><th>[\#11014](https://gitlab.haskell.org//ghc/ghc/issues/11014)</th>
  <td>re-order GHC type errors for clarity</td></tr>
  <tr><th>[\#11035](https://gitlab.haskell.org//ghc/ghc/issues/11035)</th>
  <td>Add implicit call-stacks to partial functions in base</td></tr>
  <tr><th>[\#11078](https://gitlab.haskell.org//ghc/ghc/issues/11078)</th>
  <td>Access to module renaming with reifyModule, in TemplateHaskell</td></tr>
  <tr><th>[\#11080](https://gitlab.haskell.org//ghc/ghc/issues/11080)</th>
  <td>Open data kinds</td></tr>
  <tr><th>[\#11081](https://gitlab.haskell.org//ghc/ghc/issues/11081)</th>
  <td>Implement Introspective Template Haskell</td></tr>
  <tr><th>[\#11115](https://gitlab.haskell.org//ghc/ghc/issues/11115)</th>
  <td>Indicate missing associated type instances</td></tr>
  <tr><th>[\#11134](https://gitlab.haskell.org//ghc/ghc/issues/11134)</th>
  <td>Limit frequency of idle GCs</td></tr>
  <tr><th>[\#11143](https://gitlab.haskell.org//ghc/ghc/issues/11143)</th>
  <td>Feature request: Add index/read/write primops with byte offset for ByteArray\#</td></tr>
  <tr><th>[\#11169](https://gitlab.haskell.org//ghc/ghc/issues/11169)</th>
  <td>Remove the word "skolem" from user error messages</td></tr>
  <tr><th>[\#11179](https://gitlab.haskell.org//ghc/ghc/issues/11179)</th>
  <td>Allow plugins to access "dead code"</td></tr>
  <tr><th>[\#11186](https://gitlab.haskell.org//ghc/ghc/issues/11186)</th>
  <td>Give strong preference to type variable names in scope when reporting hole contexts</td></tr>
  <tr><th>[\#11191](https://gitlab.haskell.org//ghc/ghc/issues/11191)</th>
  <td>provide \`make uninstall\`</td></tr>
  <tr><th>[\#11243](https://gitlab.haskell.org//ghc/ghc/issues/11243)</th>
  <td>Flag to not expand type families</td></tr>
  <tr><th>[\#11270](https://gitlab.haskell.org//ghc/ghc/issues/11270)</th>
  <td>"Unusable UNPACK pragma" warnings should be printed even without -O</td></tr>
  <tr><th>[\#11281](https://gitlab.haskell.org//ghc/ghc/issues/11281)</th>
  <td>Way to run --make and -M simultaneously</td></tr>
  <tr><th>[\#11286](https://gitlab.haskell.org//ghc/ghc/issues/11286)</th>
  <td>ghc-pkg library</td></tr>
  <tr><th>[\#11309](https://gitlab.haskell.org//ghc/ghc/issues/11309)</th>
  <td>Warn on shady data constructor export</td></tr>
  <tr><th>[\#11342](https://gitlab.haskell.org//ghc/ghc/issues/11342)</th>
  <td>Character kind</td></tr>
  <tr><th>[\#11343](https://gitlab.haskell.org//ghc/ghc/issues/11343)</th>
  <td>Unable to infer type when using DuplicateRecordFields</td></tr>
  <tr><th>[\#11349](https://gitlab.haskell.org//ghc/ghc/issues/11349)</th>
  <td>\[TypeApplications\] Create Proxy-free alternatives of functions in base</td></tr>
  <tr><th>[\#11350](https://gitlab.haskell.org//ghc/ghc/issues/11350)</th>
  <td>Allow visible type application in patterns</td></tr>
  <tr><th>[\#11352](https://gitlab.haskell.org//ghc/ghc/issues/11352)</th>
  <td>Allow applying type to label</td></tr>
  <tr><th>[\#11373](https://gitlab.haskell.org//ghc/ghc/issues/11373)</th>
  <td>GHC should support static archive creation on all systems</td></tr>
  <tr><th>[\#11377](https://gitlab.haskell.org//ghc/ghc/issues/11377)</th>
  <td>Template Haskell only imports</td></tr>
  <tr><th>[\#11378](https://gitlab.haskell.org//ghc/ghc/issues/11378)</th>
  <td>Use the compiler that built ghc for dynamic code loading, for cross-compiling</td></tr>
  <tr><th>[\#11387](https://gitlab.haskell.org//ghc/ghc/issues/11387)</th>
  <td>Typecasting using type application syntax</td></tr>
  <tr><th>[\#11393](https://gitlab.haskell.org//ghc/ghc/issues/11393)</th>
  <td>Ability to define INLINE pragma for all instances of a given typeclass</td></tr>
  <tr><th>[\#11398](https://gitlab.haskell.org//ghc/ghc/issues/11398)</th>
  <td>Type application for operator sections</td></tr>
  <tr><th>[\#11409](https://gitlab.haskell.org//ghc/ghc/issues/11409)</th>
  <td>Cannot instantiate literals using TypeApplications</td></tr>
  <tr><th>[\#11418](https://gitlab.haskell.org//ghc/ghc/issues/11418)</th>
  <td>Suggest correct spelling when module is not found because of typo</td></tr>
  <tr><th>[\#11425](https://gitlab.haskell.org//ghc/ghc/issues/11425)</th>
  <td>The GHC API doesn't provide a good hscTarget option for tooling</td></tr>
  <tr><th>[\#11439](https://gitlab.haskell.org//ghc/ghc/issues/11439)</th>
  <td>Request for comments: Allow duplicate type signatures</td></tr>
  <tr><th>[\#11441](https://gitlab.haskell.org//ghc/ghc/issues/11441)</th>
  <td>RFC: Inline intermediate languages (Core, STG, Cmm, even StrictCore)</td></tr>
  <tr><th>[\#11457](https://gitlab.haskell.org//ghc/ghc/issues/11457)</th>
  <td>Run type-checker plugins before GHC's solver</td></tr>
  <tr><th>[\#11461](https://gitlab.haskell.org//ghc/ghc/issues/11461)</th>
  <td>Allow pattern synonyms to be bundled with type classes?</td></tr>
  <tr><th>[\#11469](https://gitlab.haskell.org//ghc/ghc/issues/11469)</th>
  <td>GHCi should get LANGUAGE extensions/defaulting from the module whose full top-level scope is visible</td></tr>
  <tr><th>[\#11470](https://gitlab.haskell.org//ghc/ghc/issues/11470)</th>
  <td>Support changing cross compiler target at runtime</td></tr>
  <tr><th>[\#11482](https://gitlab.haskell.org//ghc/ghc/issues/11482)</th>
  <td>Turn -fdefer-typed-holes on by default</td></tr>
  <tr><th>[\#11483](https://gitlab.haskell.org//ghc/ghc/issues/11483)</th>
  <td>Ghci should TAB-complete keywords, not only identifiers</td></tr>
  <tr><th>[\#11534](https://gitlab.haskell.org//ghc/ghc/issues/11534)</th>
  <td>Allow class associated types to reference functional dependencies</td></tr>
  <tr><th>[\#11561](https://gitlab.haskell.org//ghc/ghc/issues/11561)</th>
  <td>Have static ghci link against its own copy of its libraries</td></tr>
  <tr><th>[\#11581](https://gitlab.haskell.org//ghc/ghc/issues/11581)</th>
  <td>TypeError requires UndecidableInstances unnecessarily</td></tr>
  <tr><th>[\#11593](https://gitlab.haskell.org//ghc/ghc/issues/11593)</th>
  <td>Template Haskell: Add a way to get names that are neither capturable nor capturing.</td></tr>
  <tr><th>[\#11594](https://gitlab.haskell.org//ghc/ghc/issues/11594)</th>
  <td>closed empty type families  fully applied get reduced lazily when in a constraint tuple and fully applied</td></tr>
  <tr><th>[\#11620](https://gitlab.haskell.org//ghc/ghc/issues/11620)</th>
  <td>RFC: Type-class type signatures (:: Constraint)</td></tr>
  <tr><th>[\#11636](https://gitlab.haskell.org//ghc/ghc/issues/11636)</th>
  <td>Promoting newtype destructor</td></tr>
  <tr><th>[\#11641](https://gitlab.haskell.org//ghc/ghc/issues/11641)</th>
  <td>Allow wildcards for parameters functionally determined (also type synonyms)</td></tr>
  <tr><th>[\#11646](https://gitlab.haskell.org//ghc/ghc/issues/11646)</th>
  <td>Make pattern synonym export type mismatch a warning</td></tr>
  <tr><th>[\#11652](https://gitlab.haskell.org//ghc/ghc/issues/11652)</th>
  <td>Cyclical dependencies aren't reported in pure functions</td></tr>
  <tr><th>[\#11658](https://gitlab.haskell.org//ghc/ghc/issues/11658)</th>
  <td>Type synonym with context in pattern synonym</td></tr>
  <tr><th>[\#11671](https://gitlab.haskell.org//ghc/ghc/issues/11671)</th>
  <td>Allow labels starting with uppercase with OverloadedLabels</td></tr>
  <tr><th>[\#11682](https://gitlab.haskell.org//ghc/ghc/issues/11682)</th>
  <td>:main doesn't use a bound thread</td></tr>
  <tr><th>[\#11686](https://gitlab.haskell.org//ghc/ghc/issues/11686)</th>
  <td>implicit call stacks should provide a way to get the calling function's name</td></tr>
  <tr><th>[\#11693](https://gitlab.haskell.org//ghc/ghc/issues/11693)</th>
  <td>Add \`When\` type family</td></tr>
  <tr><th>[\#11706](https://gitlab.haskell.org//ghc/ghc/issues/11706)</th>
  <td>Increase precedence of lexps (if-then-else, case-of, do, lambda and let-in)</td></tr>
  <tr><th>[\#11713](https://gitlab.haskell.org//ghc/ghc/issues/11713)</th>
  <td>STM Finalizers</td></tr>
  <tr><th>[\#11718](https://gitlab.haskell.org//ghc/ghc/issues/11718)</th>
  <td>Disable the Preview button on trac</td></tr>
  <tr><th>[\#11738](https://gitlab.haskell.org//ghc/ghc/issues/11738)</th>
  <td>A command to remove modules from the target list</td></tr>
  <tr><th>[\#11765](https://gitlab.haskell.org//ghc/ghc/issues/11765)</th>
  <td>Allow documentary type signatures</td></tr>
  <tr><th>[\#11769](https://gitlab.haskell.org//ghc/ghc/issues/11769)</th>
  <td>Support and redistributables for ARM64 (64bit)</td></tr>
  <tr><th>[\#11782](https://gitlab.haskell.org//ghc/ghc/issues/11782)</th>
  <td>Teach ghc-pkg to read multiple registrations from command line</td></tr>
  <tr><th>[\#11796](https://gitlab.haskell.org//ghc/ghc/issues/11796)</th>
  <td>Warn about unwanted instances in a modular way</td></tr>
  <tr><th>[\#11801](https://gitlab.haskell.org//ghc/ghc/issues/11801)</th>
  <td>RFC: Make browse command display everything unqualified</td></tr>
  <tr><th>[\#11807](https://gitlab.haskell.org//ghc/ghc/issues/11807)</th>
  <td>-fforce-relink / -fforce-link option</td></tr>
  <tr><th>[\#11815](https://gitlab.haskell.org//ghc/ghc/issues/11815)</th>
  <td>Data.List: Add a function to get consecutive elements (mapConsecutives)</td></tr>
  <tr><th>[\#11817](https://gitlab.haskell.org//ghc/ghc/issues/11817)</th>
  <td>Add proper support for weak symbols to the runtime linker</td></tr>
  <tr><th>[\#11950](https://gitlab.haskell.org//ghc/ghc/issues/11950)</th>
  <td>Eventlog should include delimiters showing when the process writes to the .eventlog file</td></tr>
  <tr><th>[\#11953](https://gitlab.haskell.org//ghc/ghc/issues/11953)</th>
  <td>Export Word32\#, Word64\# on all architectures</td></tr>
  <tr><th>[\#11962](https://gitlab.haskell.org//ghc/ghc/issues/11962)</th>
  <td>Support induction recursion</td></tr>
  <tr><th>[\#11967](https://gitlab.haskell.org//ghc/ghc/issues/11967)</th>
  <td>Custom message when showing functions, comparing functions, ...</td></tr>
  <tr><th>[\#11971](https://gitlab.haskell.org//ghc/ghc/issues/11971)</th>
  <td>Unify error messages that suggest enabling extensions</td></tr>
  <tr><th>[\#11993](https://gitlab.haskell.org//ghc/ghc/issues/11993)</th>
  <td>RFC, allow local bindings in pattern synonyms</td></tr>
  <tr><th>[\#12001](https://gitlab.haskell.org//ghc/ghc/issues/12001)</th>
  <td>RFC: Add pattern synonyms to base</td></tr>
  <tr><th>[\#12008](https://gitlab.haskell.org//ghc/ghc/issues/12008)</th>
  <td>GHCi autocomplete text following cursor/insertion point</td></tr>
  <tr><th>[\#12014](https://gitlab.haskell.org//ghc/ghc/issues/12014)</th>
  <td>Make it possible to deprecate a method instantiation of a typeclass instance</td></tr>
  <tr><th>[\#12016](https://gitlab.haskell.org//ghc/ghc/issues/12016)</th>
  <td>Allow wildcards in type synonyms and data declarations</td></tr>
  <tr><th>[\#12020](https://gitlab.haskell.org//ghc/ghc/issues/12020)</th>
  <td>Error message on use of != should suggest use of /=</td></tr>
  <tr><th>[\#12044](https://gitlab.haskell.org//ghc/ghc/issues/12044)</th>
  <td>Remove sortWith in favor of sortOn</td></tr>
  <tr><th>[\#12048](https://gitlab.haskell.org//ghc/ghc/issues/12048)</th>
  <td>Allow CustomTypeErrors in type synonyms (+ evaluate nested type family?)</td></tr>
  <tr><th>[\#12049](https://gitlab.haskell.org//ghc/ghc/issues/12049)</th>
  <td>\`OverloadedStrings\` for types</td></tr>
  <tr><th>[\#12053](https://gitlab.haskell.org//ghc/ghc/issues/12053)</th>
  <td>Mode for ghc --make which only compiles the files I pass on command line</td></tr>
  <tr><th>[\#12073](https://gitlab.haskell.org//ghc/ghc/issues/12073)</th>
  <td>Missing instance of MonadFix for Q</td></tr>
  <tr><th>[\#12086](https://gitlab.haskell.org//ghc/ghc/issues/12086)</th>
  <td>Allow omitting type family signature</td></tr>
  <tr><th>[\#12096](https://gitlab.haskell.org//ghc/ghc/issues/12096)</th>
  <td>Attach stacktrace information to SomeException</td></tr>
  <tr><th>[\#12114](https://gitlab.haskell.org//ghc/ghc/issues/12114)</th>
  <td>Make injectivity check less conservative</td></tr>
  <tr><th>[\#12139](https://gitlab.haskell.org//ghc/ghc/issues/12139)</th>
  <td>Add TUI (text-based user interface) for GHCi</td></tr>
  <tr><th>[\#12157](https://gitlab.haskell.org//ghc/ghc/issues/12157)</th>
  <td>Warning priorities (or: report hole warnings first)</td></tr>
  <tr><th>[\#12159](https://gitlab.haskell.org//ghc/ghc/issues/12159)</th>
  <td>Record-like GADTs with repeated fields (of same type) rejected</td></tr>
  <tr><th>[\#12178](https://gitlab.haskell.org//ghc/ghc/issues/12178)</th>
  <td>Allow inline pragmas on pattern synonyms</td></tr>
  <tr><th>[\#12183](https://gitlab.haskell.org//ghc/ghc/issues/12183)</th>
  <td>Do not display global bindings with -fno-max-relevant-binds</td></tr>
  <tr><th>[\#12190](https://gitlab.haskell.org//ghc/ghc/issues/12190)</th>
  <td>Generalize irrefutable patterns (static semantics like let-bindings)</td></tr>
  <tr><th>[\#12203](https://gitlab.haskell.org//ghc/ghc/issues/12203)</th>
  <td>Allow constructors on LHS of (implicit) bidirectional pattern synonym</td></tr>
  <tr><th>[\#12240](https://gitlab.haskell.org//ghc/ghc/issues/12240)</th>
  <td>Common Sense for Type Classes</td></tr>
  <tr><th>[\#12244](https://gitlab.haskell.org//ghc/ghc/issues/12244)</th>
  <td>Idea: Remove unused symbols in link-time for smaller binaries</td></tr>
  <tr><th>[\#12349](https://gitlab.haskell.org//ghc/ghc/issues/12349)</th>
  <td>Parallel make should interleave output if it means we can report an error earlier</td></tr>
  <tr><th>[\#12360](https://gitlab.haskell.org//ghc/ghc/issues/12360)</th>
  <td>Extend support for binding implicit parameters</td></tr>
  <tr><th>[\#12361](https://gitlab.haskell.org//ghc/ghc/issues/12361)</th>
  <td>Add -dppr-ribbon-cols</td></tr>
  <tr><th>[\#12362](https://gitlab.haskell.org//ghc/ghc/issues/12362)</th>
  <td>don't complain about type variable ambiguity when the expression is parametrically polymorphic</td></tr>
  <tr><th>[\#12363](https://gitlab.haskell.org//ghc/ghc/issues/12363)</th>
  <td>Type application for infix</td></tr>
  <tr><th>[\#12376](https://gitlab.haskell.org//ghc/ghc/issues/12376)</th>
  <td>Allow function definitions in record syntax</td></tr>
  <tr><th>[\#12389](https://gitlab.haskell.org//ghc/ghc/issues/12389)</th>
  <td>Limit duplicate export warnings for datatypes</td></tr>
  <tr><th>[\#12397](https://gitlab.haskell.org//ghc/ghc/issues/12397)</th>
  <td>Support for PDB debug information generation</td></tr>
  <tr><th>[\#12400](https://gitlab.haskell.org//ghc/ghc/issues/12400)</th>
  <td>Suggest misspelling if a type signature has similarly named binding</td></tr>
  <tr><th>[\#12422](https://gitlab.haskell.org//ghc/ghc/issues/12422)</th>
  <td>Add decidable equality class</td></tr>
  <tr><th>[\#12428](https://gitlab.haskell.org//ghc/ghc/issues/12428)</th>
  <td>Allow pattern synonyms to optionally carry coerceability</td></tr>
  <tr><th>[\#12448](https://gitlab.haskell.org//ghc/ghc/issues/12448)</th>
  <td>Allow partial application of bidirectional pattern synonyms</td></tr>
  <tr><th>[\#12450](https://gitlab.haskell.org//ghc/ghc/issues/12450)</th>
  <td>Option to suppress GHCi output "Failed, modules loaded"</td></tr>
  <tr><th>[\#12457](https://gitlab.haskell.org//ghc/ghc/issues/12457)</th>
  <td>Deriving should be (more closely) integrated with other metaprogramming methods</td></tr>
  <tr><th>[\#12463](https://gitlab.haskell.org//ghc/ghc/issues/12463)</th>
  <td>SPECIALIZABLE pragma?</td></tr>
  <tr><th>[\#12465](https://gitlab.haskell.org//ghc/ghc/issues/12465)</th>
  <td>Evil idea: Allow empty record field update syntax for types.</td></tr>
  <tr><th>[\#12470](https://gitlab.haskell.org//ghc/ghc/issues/12470)</th>
  <td>Move LLVM code generator to LLVM bitcode format</td></tr>
  <tr><th>[\#12477](https://gitlab.haskell.org//ghc/ghc/issues/12477)</th>
  <td>Allow left sectioning and tuple sectioning of types</td></tr>
  <tr><th>[\#12483](https://gitlab.haskell.org//ghc/ghc/issues/12483)</th>
  <td>Improve parse error message on indentation mistake</td></tr>
  <tr><th>[\#12498](https://gitlab.haskell.org//ghc/ghc/issues/12498)</th>
  <td>Support unconventionally named import libraries</td></tr>
  <tr><th>[\#12505](https://gitlab.haskell.org//ghc/ghc/issues/12505)</th>
  <td>Add foldl1' to Data.Foldable</td></tr>
  <tr><th>[\#12508](https://gitlab.haskell.org//ghc/ghc/issues/12508)</th>
  <td>Show evaluation step-by-step in GHCi</td></tr>
  <tr><th>[\#12515](https://gitlab.haskell.org//ghc/ghc/issues/12515)</th>
  <td>Pattern synonyms with non-conid/consym names give poor error messages</td></tr>
  <tr><th>[\#12518](https://gitlab.haskell.org//ghc/ghc/issues/12518)</th>
  <td>Allow customizing immutable package dbs by stacking</td></tr>
  <tr><th>[\#12541](https://gitlab.haskell.org//ghc/ghc/issues/12541)</th>
  <td>RFC: Implicit parentheses in GHCi</td></tr>
  <tr><th>[\#12543](https://gitlab.haskell.org//ghc/ghc/issues/12543)</th>
  <td>Warning for duplicate language extensions</td></tr>
  <tr><th>[\#12547](https://gitlab.haskell.org//ghc/ghc/issues/12547)</th>
  <td>Concurrent.ForeignPtr needs to access a C-ForeignPtr, but this is already gone</td></tr>
  <tr><th>[\#12551](https://gitlab.haskell.org//ghc/ghc/issues/12551)</th>
  <td>Make type indices take local constraints into account in type instance declaration</td></tr>
  <tr><th>[\#12580](https://gitlab.haskell.org//ghc/ghc/issues/12580)</th>
  <td>Eagerly simplify inherently-coherent instances</td></tr>
  <tr><th>[\#12618](https://gitlab.haskell.org//ghc/ghc/issues/12618)</th>
  <td>Add saturated constructor applications to Core</td></tr>
  <tr><th>[\#12620](https://gitlab.haskell.org//ghc/ghc/issues/12620)</th>
  <td>Allow the user to prevent floating and CSE</td></tr>
  <tr><th>[\#12626](https://gitlab.haskell.org//ghc/ghc/issues/12626)</th>
  <td>Remove redundant type applications in Core</td></tr>
  <tr><th>[\#12627](https://gitlab.haskell.org//ghc/ghc/issues/12627)</th>
  <td>build sytem feature request: persist warnings</td></tr>
  <tr><th>[\#12633](https://gitlab.haskell.org//ghc/ghc/issues/12633)</th>
  <td>Support standard syntax for language pragmas in GHCi</td></tr>
  <tr><th>[\#12639](https://gitlab.haskell.org//ghc/ghc/issues/12639)</th>
  <td>Inconsistent treatment of FlexibleInstances and MPTCs with standard vs. flexible deriving</td></tr>
  <tr><th>[\#12649](https://gitlab.haskell.org//ghc/ghc/issues/12649)</th>
  <td>Allow TypeApplications syntax to be used to instantiate type variables in SPECIALISE pragmas</td></tr>
  <tr><th>[\#12651](https://gitlab.haskell.org//ghc/ghc/issues/12651)</th>
  <td>Test suite should handle stage1 compiler</td></tr>
  <tr><th>[\#12665](https://gitlab.haskell.org//ghc/ghc/issues/12665)</th>
  <td>Make Read instances for Integral types faster, and make them fail fast</td></tr>
  <tr><th>[\#12677](https://gitlab.haskell.org//ghc/ghc/issues/12677)</th>
  <td>Type equality in constraint not used?</td></tr>
  <tr><th>[\#12680](https://gitlab.haskell.org//ghc/ghc/issues/12680)</th>
  <td>Permit type equality instances in signatures</td></tr>
  <tr><th>[\#12683](https://gitlab.haskell.org//ghc/ghc/issues/12683)</th>
  <td>Monad laws in terms of fishes (\>=\>)</td></tr>
  <tr><th>[\#12693](https://gitlab.haskell.org//ghc/ghc/issues/12693)</th>
  <td>Relax qualified import syntax</td></tr>
  <tr><th>[\#12703](https://gitlab.haskell.org//ghc/ghc/issues/12703)</th>
  <td>Expand Backpack's signature matching relation beyond definitional equality</td></tr>
  <tr><th>[\#12708](https://gitlab.haskell.org//ghc/ghc/issues/12708)</th>
  <td>RFC: Representation polymorphic Num</td></tr>
  <tr><th>[\#12710](https://gitlab.haskell.org//ghc/ghc/issues/12710)</th>
  <td>Make some reserved Unicode symbols "specials"</td></tr>
  <tr><th>[\#12717](https://gitlab.haskell.org//ghc/ghc/issues/12717)</th>
  <td>Permit data types in signatures to be implemented with equivalent pattern synonyms (and vice versa)</td></tr>
  <tr><th>[\#12747](https://gitlab.haskell.org//ghc/ghc/issues/12747)</th>
  <td>INLINE vs NOINLINE vs \<nothing\> give three different results; two would be better</td></tr>
  <tr><th>[\#12766](https://gitlab.haskell.org//ghc/ghc/issues/12766)</th>
  <td>Allow runtime-representation polymorphic data families</td></tr>
  <tr><th>[\#12773](https://gitlab.haskell.org//ghc/ghc/issues/12773)</th>
  <td>Data.Functor.Classes instances for ZipList</td></tr>
  <tr><th>[\#12786](https://gitlab.haskell.org//ghc/ghc/issues/12786)</th>
  <td>genericReplicateM and genericReplicateM_</td></tr>
  <tr><th>[\#12823](https://gitlab.haskell.org//ghc/ghc/issues/12823)</th>
  <td>Inconsistency in acceptance of equality constraints in different forms</td></tr>
  <tr><th>[\#12848](https://gitlab.haskell.org//ghc/ghc/issues/12848)</th>
  <td>Reduce long-term memory usage of GHCi</td></tr>
  <tr><th>[\#12857](https://gitlab.haskell.org//ghc/ghc/issues/12857)</th>
  <td>associate pattern synonyms with a type synonym</td></tr>
  <tr><th>[\#12864](https://gitlab.haskell.org//ghc/ghc/issues/12864)</th>
  <td>Produce type errors after looking at whole applications</td></tr>
  <tr><th>[\#12868](https://gitlab.haskell.org//ghc/ghc/issues/12868)</th>
  <td>Add groupOn function to Data.List</td></tr>
  <tr><th>[\#12886](https://gitlab.haskell.org//ghc/ghc/issues/12886)</th>
  <td>Proposal for throwLeft and throwLeftIO in Control.Exception</td></tr>
  <tr><th>[\#12888](https://gitlab.haskell.org//ghc/ghc/issues/12888)</th>
  <td>‘Identity instance’: Outputable SDoc</td></tr>
  <tr><th>[\#12895](https://gitlab.haskell.org//ghc/ghc/issues/12895)</th>
  <td>Lookup rules associated with functions/values in GHCI</td></tr>
  <tr><th>[\#12896](https://gitlab.haskell.org//ghc/ghc/issues/12896)</th>
  <td>Consider using compact regions in GHC itself to reduce GC overhead</td></tr>
  <tr><th>[\#12900](https://gitlab.haskell.org//ghc/ghc/issues/12900)</th>
  <td>Common up identical info tables</td></tr>
  <tr><th>[\#12902](https://gitlab.haskell.org//ghc/ghc/issues/12902)</th>
  <td>Improve handle decoding error messages</td></tr>
  <tr><th>[\#12928](https://gitlab.haskell.org//ghc/ghc/issues/12928)</th>
  <td>Too easy to trigger CUSK condition using TH</td></tr>
  <tr><th>[\#12953](https://gitlab.haskell.org//ghc/ghc/issues/12953)</th>
  <td>Use computed gotos in the interpreter when the compiler supports it</td></tr>
  <tr><th>[\#12982](https://gitlab.haskell.org//ghc/ghc/issues/12982)</th>
  <td>Missed constant folding oportunities</td></tr>
  <tr><th>[\#12986](https://gitlab.haskell.org//ghc/ghc/issues/12986)</th>
  <td>Ignore case when parsing language pragmas</td></tr>
  <tr><th>[\#13013](https://gitlab.haskell.org//ghc/ghc/issues/13013)</th>
  <td>Add readMaybe to Prelude (possibly readEither too), make Haddocks for read more cautionary</td></tr>
  <tr><th>[\#13026](https://gitlab.haskell.org//ghc/ghc/issues/13026)</th>
  <td>RFC functions for sums and products</td></tr>
  <tr><th>[\#13028](https://gitlab.haskell.org//ghc/ghc/issues/13028)</th>
  <td>Compile-time validation of literals in IsString instances</td></tr>
  <tr><th>[\#13034](https://gitlab.haskell.org//ghc/ghc/issues/13034)</th>
  <td>clean memory in GHCi</td></tr>
  <tr><th>[\#13038](https://gitlab.haskell.org//ghc/ghc/issues/13038)</th>
  <td>implementation of Modus ponens and Modus tollens</td></tr>
  <tr><th>[\#13039](https://gitlab.haskell.org//ghc/ghc/issues/13039)</th>
  <td>Add options to select GHCi prompt type errors</td></tr>
  <tr><th>[\#13042](https://gitlab.haskell.org//ghc/ghc/issues/13042)</th>
  <td>Allow type annotations / visible type application in pattern synonyms</td></tr>
  <tr><th>[\#13051](https://gitlab.haskell.org//ghc/ghc/issues/13051)</th>
  <td>Make the Register Allocator Loop-Aware</td></tr>
  <tr><th>[\#13065](https://gitlab.haskell.org//ghc/ghc/issues/13065)</th>
  <td>Prohibit user-defined Generic and Generic1 instances</td></tr>
  <tr><th>[\#13103](https://gitlab.haskell.org//ghc/ghc/issues/13103)</th>
  <td>The RTS loader/linker relies too heavily on file extensions.</td></tr>
  <tr><th>[\#13116](https://gitlab.haskell.org//ghc/ghc/issues/13116)</th>
  <td>Allow Overloaded things in patterns</td></tr>
  <tr><th>[\#13131](https://gitlab.haskell.org//ghc/ghc/issues/13131)</th>
  <td>add effectful version of atomicModifyMutVar\#</td></tr>
  <tr><th>[\#13152](https://gitlab.haskell.org//ghc/ghc/issues/13152)</th>
  <td>Provide a mechanism to notify build system when .hi file is ready</td></tr>
  <tr><th>[\#13164](https://gitlab.haskell.org//ghc/ghc/issues/13164)</th>
  <td>idle time full GCs (idle cpu usage)</td></tr>
  <tr><th>[\#13176](https://gitlab.haskell.org//ghc/ghc/issues/13176)</th>
  <td>Deprecate the realWorld\#</td></tr>
  <tr><th>[\#13177](https://gitlab.haskell.org//ghc/ghc/issues/13177)</th>
  <td>Give Data.Functor.\* its lifted void and unit</td></tr>
  <tr><th>[\#13186](https://gitlab.haskell.org//ghc/ghc/issues/13186)</th>
  <td>Change EvNum to EvNum :: Natural -\> EvLit</td></tr>
  <tr><th>[\#13189](https://gitlab.haskell.org//ghc/ghc/issues/13189)</th>
  <td>Implement same specification as GHC spec file for mingw32</td></tr>
  <tr><th>[\#13229](https://gitlab.haskell.org//ghc/ghc/issues/13229)</th>
  <td>Add -ddump-inlinings-reasoning</td></tr>
  <tr><th>[\#13232](https://gitlab.haskell.org//ghc/ghc/issues/13232)</th>
  <td>Undeflow/overflow warnings for floating-point values</td></tr>
  <tr><th>[\#13241](https://gitlab.haskell.org//ghc/ghc/issues/13241)</th>
  <td>Compile-time flag causing GC to zero evacuated memory</td></tr>
  <tr><th>[\#13248](https://gitlab.haskell.org//ghc/ghc/issues/13248)</th>
  <td>Allow an injective type family RHS to be another injective type family</td></tr>
  <tr><th>[\#13257](https://gitlab.haskell.org//ghc/ghc/issues/13257)</th>
  <td>out-of-range warnings for negative literals, without -XNegativeLiterals</td></tr>
  <tr><th>[\#13262](https://gitlab.haskell.org//ghc/ghc/issues/13262)</th>
  <td>Allow type synonym family application in instance head if it has no free variables</td></tr>
  <tr><th>[\#13276](https://gitlab.haskell.org//ghc/ghc/issues/13276)</th>
  <td>Unboxed sums are not Typeable</td></tr>
  <tr><th>[\#13282](https://gitlab.haskell.org//ghc/ghc/issues/13282)</th>
  <td>Introduce fast path through simplifier for static bindings</td></tr>
  <tr><th>[\#13299](https://gitlab.haskell.org//ghc/ghc/issues/13299)</th>
  <td>Typecheck multiple modules at the same time</td></tr>
  <tr><th>[\#13319](https://gitlab.haskell.org//ghc/ghc/issues/13319)</th>
  <td>Generate makefile dependencies suitable for ghc --make!</td></tr>
  <tr><th>[\#13322](https://gitlab.haskell.org//ghc/ghc/issues/13322)</th>
  <td>Pattern synonyms in hs-boot files</td></tr>
  <tr><th>[\#13334](https://gitlab.haskell.org//ghc/ghc/issues/13334)</th>
  <td>Constant folding for repeated integer operation of unknown value</td></tr>
  <tr><th>[\#13341](https://gitlab.haskell.org//ghc/ghc/issues/13341)</th>
  <td>Lift constraint products</td></tr>
  <tr><th>[\#13358](https://gitlab.haskell.org//ghc/ghc/issues/13358)</th>
  <td>Role ranges (allow decomposition on newtypes)</td></tr>
  <tr><th>[\#13360](https://gitlab.haskell.org//ghc/ghc/issues/13360)</th>
  <td>Add a flag to enable inferring HasCallStack constraints</td></tr>
  <tr><th>[\#13362](https://gitlab.haskell.org//ghc/ghc/issues/13362)</th>
  <td>GHC first generation of GC to be as large as largest cache size by default</td></tr>
  <tr><th>[\#13372](https://gitlab.haskell.org//ghc/ghc/issues/13372)</th>
  <td>Attach CallStack to IOError, add CallStack constraints to relevant functions in base</td></tr>
  <tr><th>[\#13373](https://gitlab.haskell.org//ghc/ghc/issues/13373)</th>
  <td>Handle long file paths on Windows</td></tr>
  <tr><th>[\#13403](https://gitlab.haskell.org//ghc/ghc/issues/13403)</th>
  <td>Derive instances (Applicative, Monad, ...) for structures lifted over functors</td></tr>
  <tr><th>[\#13408](https://gitlab.haskell.org//ghc/ghc/issues/13408)</th>
  <td>Consider inferring a higher-rank kind for type synonyms</td></tr>
  <tr><th>[\#13427](https://gitlab.haskell.org//ghc/ghc/issues/13427)</th>
  <td>Per-function code-generation options</td></tr>
  <tr><th>[\#13436](https://gitlab.haskell.org//ghc/ghc/issues/13436)</th>
  <td>feature for checking files in temp location</td></tr>
  <tr><th>[\#13437](https://gitlab.haskell.org//ghc/ghc/issues/13437)</th>
  <td>Easier editing of multi-line GHCi-commands</td></tr>
  <tr><th>[\#13443](https://gitlab.haskell.org//ghc/ghc/issues/13443)</th>
  <td>Typeclass resolution errors quite puzzling</td></tr>
  <tr><th>[\#13469](https://gitlab.haskell.org//ghc/ghc/issues/13469)</th>
  <td>-fdefer-type-errors for Backpack</td></tr>
  <tr><th>[\#13480](https://gitlab.haskell.org//ghc/ghc/issues/13480)</th>
  <td>GHCi display visible type application</td></tr>
  <tr><th>[\#13496](https://gitlab.haskell.org//ghc/ghc/issues/13496)</th>
  <td>Add Generic instance to GHC.Stack.SrcLoc</td></tr>
  <tr><th>[\#13502](https://gitlab.haskell.org//ghc/ghc/issues/13502)</th>
  <td>Static argument transformation should also run after specialisation</td></tr>
  <tr><th>[\#13505](https://gitlab.haskell.org//ghc/ghc/issues/13505)</th>
  <td>Write the name of the people of the Haskell Committee into the GHC compiler</td></tr>
  <tr><th>[\#13511](https://gitlab.haskell.org//ghc/ghc/issues/13511)</th>
  <td>ApplicativeDo return case doesn't handle lets</td></tr>
  <tr><th>[\#13523](https://gitlab.haskell.org//ghc/ghc/issues/13523)</th>
  <td>Be more explicit in generated case alternatives</td></tr>
  <tr><th>[\#13529](https://gitlab.haskell.org//ghc/ghc/issues/13529)</th>
  <td>eventlog to report more information about stopping threads because of FFI calls</td></tr>
  <tr><th>[\#13533](https://gitlab.haskell.org//ghc/ghc/issues/13533)</th>
  <td>-ddump-warnings</td></tr>
  <tr><th>[\#13551](https://gitlab.haskell.org//ghc/ghc/issues/13551)</th>
  <td>Support DEPRECATED and WARNING pragmas on Template Haskell</td></tr>
  <tr><th>[\#13554](https://gitlab.haskell.org//ghc/ghc/issues/13554)</th>
  <td>Allow the user to provide a C function that is called on each thread the RTS creates before running any Haskell code</td></tr>
  <tr><th>[\#13572](https://gitlab.haskell.org//ghc/ghc/issues/13572)</th>
  <td>Add ArgMin / ArgMax pattern synonyms</td></tr>
  <tr><th>[\#13573](https://gitlab.haskell.org//ghc/ghc/issues/13573)</th>
  <td>Add Foldable1 to base</td></tr>
  <tr><th>[\#13581](https://gitlab.haskell.org//ghc/ghc/issues/13581)</th>
  <td>UNPACK should allow recursion that obviously terminates</td></tr>
  <tr><th>[\#13592](https://gitlab.haskell.org//ghc/ghc/issues/13592)</th>
  <td>Newtype type class with compiler generated instances</td></tr>
  <tr><th>[\#13621](https://gitlab.haskell.org//ghc/ghc/issues/13621)</th>
  <td>Problems with injective type families</td></tr>
  <tr><th>[\#13663](https://gitlab.haskell.org//ghc/ghc/issues/13663)</th>
  <td>Option to disable turning recursive let-bindings to recursive functions</td></tr>
  <tr><th>[\#13686](https://gitlab.haskell.org//ghc/ghc/issues/13686)</th>
  <td>Compile a few modules for profiling unconditionally</td></tr>
  <tr><th>[\#13712](https://gitlab.haskell.org//ghc/ghc/issues/13712)</th>
  <td>Attach size to ForeignPtr</td></tr>
  <tr><th>[\#13723](https://gitlab.haskell.org//ghc/ghc/issues/13723)</th>
  <td>Recover gracefully from simplifier tick exhaustion</td></tr>
  <tr><th>[\#13725](https://gitlab.haskell.org//ghc/ghc/issues/13725)</th>
  <td>Remove false dependency on the destination of the popcnt instruction</td></tr>
  <tr><th>[\#13733](https://gitlab.haskell.org//ghc/ghc/issues/13733)</th>
  <td>Simplify constraints on RULES LHS</td></tr>
  <tr><th>[\#13743](https://gitlab.haskell.org//ghc/ghc/issues/13743)</th>
  <td>Colourise command output</td></tr>
  <tr><th>[\#13753](https://gitlab.haskell.org//ghc/ghc/issues/13753)</th>
  <td>Improve GHC's ghc package environment lookup logic</td></tr>
  <tr><th>[\#13779](https://gitlab.haskell.org//ghc/ghc/issues/13779)</th>
  <td>Add more signature suppression control for dumps</td></tr>
  <tr><th>[\#13801](https://gitlab.haskell.org//ghc/ghc/issues/13801)</th>
  <td>Make -main-is work with {thing} from arbitrary installed packages</td></tr>
  <tr><th>[\#13835](https://gitlab.haskell.org//ghc/ghc/issues/13835)</th>
  <td>ghci with ":set +t" should print type before starting evaluation</td></tr>
  <tr><th>[\#13843](https://gitlab.haskell.org//ghc/ghc/issues/13843)</th>
  <td>Expand type information collected (:set +c), used by :all-types, :type-at, ..</td></tr>
  <tr><th>[\#13852](https://gitlab.haskell.org//ghc/ghc/issues/13852)</th>
  <td>Can we have more SIMD primops, corresponding to the untapped AVX etc. instructions?</td></tr>
  <tr><th>[\#13855](https://gitlab.haskell.org//ghc/ghc/issues/13855)</th>
  <td>Syntactic sugar to write the recursion in GHC</td></tr>
  <tr><th>[\#13861](https://gitlab.haskell.org//ghc/ghc/issues/13861)</th>
  <td>Take more advantage of STG representation invariance (follows up \#9291)</td></tr>
  <tr><th>[\#13868](https://gitlab.haskell.org//ghc/ghc/issues/13868)</th>
  <td>Improved help suggested in the error message about "import".</td></tr>
  <tr><th>[\#13869](https://gitlab.haskell.org//ghc/ghc/issues/13869)</th>
  <td>Improved response from GHCi about ":l" or ":r".</td></tr>
  <tr><th>[\#13884](https://gitlab.haskell.org//ghc/ghc/issues/13884)</th>
  <td>To an automatic compilation of file-header LANGUAGE pragmas in GHC.</td></tr>
  <tr><th>[\#13893](https://gitlab.haskell.org//ghc/ghc/issues/13893)</th>
  <td>Improved help for "import" command at the start of a script when "variable not in scope" in error msg</td></tr>
  <tr><th>[\#13944](https://gitlab.haskell.org//ghc/ghc/issues/13944)</th>
  <td>Introduce synchronized FFI</td></tr>
  <tr><th>[\#13957](https://gitlab.haskell.org//ghc/ghc/issues/13957)</th>
  <td>Allow deriving multiparameter type classes with representationally equal arguments</td></tr>
  <tr><th>[\#13966](https://gitlab.haskell.org//ghc/ghc/issues/13966)</th>
  <td>Skip-less stream fusion: a missed opportunity</td></tr>
  <tr><th>[\#13992](https://gitlab.haskell.org//ghc/ghc/issues/13992)</th>
  <td>Error message, room for improvement (polykinds)</td></tr>
  <tr><th>[\#14003](https://gitlab.haskell.org//ghc/ghc/issues/14003)</th>
  <td>Allow more worker arguments in SpecConstr</td></tr>
  <tr><th>[\#14007](https://gitlab.haskell.org//ghc/ghc/issues/14007)</th>
  <td>CI builds for integer-simple variant of GHC for Windows</td></tr>
  <tr><th>[\#14018](https://gitlab.haskell.org//ghc/ghc/issues/14018)</th>
  <td>Highlight differences of Frontend Plugin vs GHC API</td></tr>
  <tr><th>[\#14039](https://gitlab.haskell.org//ghc/ghc/issues/14039)</th>
  <td>Add ability to install libraries bundled with ghc into separate prefixes</td></tr>
  <tr><th>[\#14046](https://gitlab.haskell.org//ghc/ghc/issues/14046)</th>
  <td>“Illegal type synonym family application in instance” is too strict in the presence of functional dependencies</td></tr>
  <tr><th>[\#14049](https://gitlab.haskell.org//ghc/ghc/issues/14049)</th>
  <td>relax -Wmissing-import-lists to allow one module with implicit imports when using NoImplicitPrelude</td></tr>
  <tr><th>[\#14061](https://gitlab.haskell.org//ghc/ghc/issues/14061)</th>
  <td>reflection</td></tr>
  <tr><th>[\#14124](https://gitlab.haskell.org//ghc/ghc/issues/14124)</th>
  <td>add shrink prim-op for other array type</td></tr>
  <tr><th>[\#14132](https://gitlab.haskell.org//ghc/ghc/issues/14132)</th>
  <td>Report an error for a missing class instance before an error for type family instances of an associated type.</td></tr>
  <tr><th>[\#14134](https://gitlab.haskell.org//ghc/ghc/issues/14134)</th>
  <td>Implement enums for Cmm</td></tr>
  <tr><th>[\#14143](https://gitlab.haskell.org//ghc/ghc/issues/14143)</th>
  <td>forkProcess leaks file descriptors</td></tr>
  <tr><th>[\#14144](https://gitlab.haskell.org//ghc/ghc/issues/14144)</th>
  <td>Standardize binary distribution doc files</td></tr>
  <tr><th>[\#14145](https://gitlab.haskell.org//ghc/ghc/issues/14145)</th>
  <td>I expect \`hp2ps -cd\` to work as \`hp2ps -c -d\` does.</td></tr>
  <tr><th>[\#14146](https://gitlab.haskell.org//ghc/ghc/issues/14146)</th>
  <td>Can GHC propose kind restrictions?</td></tr>
  <tr><th>[\#14196](https://gitlab.haskell.org//ghc/ghc/issues/14196)</th>
  <td>Replace ArrayArray\# with either UnliftedArray\# or Array\#</td></tr>
  <tr><th>[\#14201](https://gitlab.haskell.org//ghc/ghc/issues/14201)</th>
  <td>Implement ideas from "Compiling Pattern Matching to Good Decision Trees"</td></tr>
  <tr><th>[\#14210](https://gitlab.haskell.org//ghc/ghc/issues/14210)</th>
  <td>bkp files cannot find TemplateHaskell symbols (even without Backpack features)</td></tr>
  <tr><th>[\#14219](https://gitlab.haskell.org//ghc/ghc/issues/14219)</th>
  <td>Include source location information in -ddump-rule-rewrites</td></tr>
  <tr><th>[\#14227](https://gitlab.haskell.org//ghc/ghc/issues/14227)</th>
  <td>Add -fdefer-ffi-errors flag</td></tr>
  <tr><th>[\#14239](https://gitlab.haskell.org//ghc/ghc/issues/14239)</th>
  <td>Let -fspecialise-aggressively respect NOINLINE (or NOSPECIALISABLE?)</td></tr>
  <tr><th>[\#14252](https://gitlab.haskell.org//ghc/ghc/issues/14252)</th>
  <td>ApplicativeDo: Add compiler message about irrefutable pattern matches and Monad constraints</td></tr>
  <tr><th>[\#14255](https://gitlab.haskell.org//ghc/ghc/issues/14255)</th>
  <td>Type-indexed type fingerprints</td></tr>
  <tr><th>[\#14259](https://gitlab.haskell.org//ghc/ghc/issues/14259)</th>
  <td>Worker/Wrapper for sum return</td></tr>
  <tr><th>[\#14292](https://gitlab.haskell.org//ghc/ghc/issues/14292)</th>
  <td>Coercing between constraints of newtypes</td></tr>
  <tr><th>[\#14307](https://gitlab.haskell.org//ghc/ghc/issues/14307)</th>
  <td>NamedFieldPuns should allow "ambiguous" field names</td></tr>
  <tr><th>[\#14317](https://gitlab.haskell.org//ghc/ghc/issues/14317)</th>
  <td>Solve Coercible constraints over type constructors</td></tr>
  <tr><th>[\#14349](https://gitlab.haskell.org//ghc/ghc/issues/14349)</th>
  <td>Semigroup/Monoid instances for System.Exit.ExitCode</td></tr>
  <tr><th>[\#14362](https://gitlab.haskell.org//ghc/ghc/issues/14362)</th>
  <td>Allow: Coercing (a:\~:b) to (b:\~:a)</td></tr>
  <tr><th>[\#14372](https://gitlab.haskell.org//ghc/ghc/issues/14372)</th>
  <td>CMM contains a bunch of tail-merging opportunities</td></tr>
  <tr><th>[\#14397](https://gitlab.haskell.org//ghc/ghc/issues/14397)</th>
  <td>For type error involving inferred types, show source of the type</td></tr>
  <tr><th>[\#14400](https://gitlab.haskell.org//ghc/ghc/issues/14400)</th>
  <td>Make :sprint, :print, and :force work with cyclical structures</td></tr>
  <tr><th>[\#14407](https://gitlab.haskell.org//ghc/ghc/issues/14407)</th>
  <td>rts: Threads/caps affinity</td></tr>
  <tr><th>[\#14422](https://gitlab.haskell.org//ghc/ghc/issues/14422)</th>
  <td>{-\# complete \#-} should be able to be at least partially type directed</td></tr>
  <tr><th>[\#14423](https://gitlab.haskell.org//ghc/ghc/issues/14423)</th>
  <td>{-\# complete \#-} should be able to handle \| like {-\# minimal \#-}</td></tr>
  <tr><th>[\#14443](https://gitlab.haskell.org//ghc/ghc/issues/14443)</th>
  <td>Add a plugin phase that allows users to modify HsSyn before the desugarer</td></tr>
  <tr><th>[\#14467](https://gitlab.haskell.org//ghc/ghc/issues/14467)</th>
  <td>Support HasCallStack for calls to panic</td></tr>
  <tr><th>[\#14474](https://gitlab.haskell.org//ghc/ghc/issues/14474)</th>
  <td>reify RHS of "value" variable</td></tr>
  <tr><th>[\#14476](https://gitlab.haskell.org//ghc/ghc/issues/14476)</th>
  <td>Keep source locations in Core (optionally)</td></tr>
  <tr><th>[\#14478](https://gitlab.haskell.org//ghc/ghc/issues/14478)</th>
  <td>Abstract pattern synonyms (for hsig and hs-boot)</td></tr>
  <tr><th>[\#14491](https://gitlab.haskell.org//ghc/ghc/issues/14491)</th>
  <td>Windows build with "--enable-distro-toolchain" fails with "make install"</td></tr>
  <tr><th>[\#14495](https://gitlab.haskell.org//ghc/ghc/issues/14495)</th>
  <td>Relocatable GHC</td></tr>
  <tr><th>[\#14527](https://gitlab.haskell.org//ghc/ghc/issues/14527)</th>
  <td>Warn on recursive bindings</td></tr>
  <tr><th>[\#14553](https://gitlab.haskell.org//ghc/ghc/issues/14553)</th>
  <td>Implement native CPP</td></tr>
  <tr><th>[\#14559](https://gitlab.haskell.org//ghc/ghc/issues/14559)</th>
  <td>When overlapping instances lead to a compile error, show import chains for the instances</td></tr>
  <tr><th>[\#14571](https://gitlab.haskell.org//ghc/ghc/issues/14571)</th>
  <td>RFE: Make template-haskell cabal package require a specific version of GHC</td></tr>
  <tr><th>[\#14572](https://gitlab.haskell.org//ghc/ghc/issues/14572)</th>
  <td>Document for writing GHC-based compiler</td></tr>
  <tr><th>[\#14586](https://gitlab.haskell.org//ghc/ghc/issues/14586)</th>
  <td>ARM and ARM64 NCG</td></tr>
  <tr><th>[\#14592](https://gitlab.haskell.org//ghc/ghc/issues/14592)</th>
  <td>Totality checking</td></tr>
  <tr><th>[\#14604](https://gitlab.haskell.org//ghc/ghc/issues/14604)</th>
  <td>Flag to disable error position description</td></tr>
  <tr><th>[\#14609](https://gitlab.haskell.org//ghc/ghc/issues/14609)</th>
  <td>Per-instance UndecidableInstances</td></tr>
  <tr><th>[\#14622](https://gitlab.haskell.org//ghc/ghc/issues/14622)</th>
  <td>Add \`joinState\# :: State\# s -\> State\# s -\> State\# s\` primop</td></tr>
  <tr><th>[\#14623](https://gitlab.haskell.org//ghc/ghc/issues/14623)</th>
  <td>Allow qAddDependentFile on directories</td></tr>
  <tr><th>[\#14634](https://gitlab.haskell.org//ghc/ghc/issues/14634)</th>
  <td>Add print stacktrace to exception handler in runtime system</td></tr>
  <tr><th>[\#14664](https://gitlab.haskell.org//ghc/ghc/issues/14664)</th>
  <td>"GHC.Integer can't throw exceptions" is wrong</td></tr>
  <tr><th>[\#14685](https://gitlab.haskell.org//ghc/ghc/issues/14685)</th>
  <td>Pragma to reset language extensions in module header</td></tr>
  <tr><th>[\#14711](https://gitlab.haskell.org//ghc/ghc/issues/14711)</th>
  <td>Machine readable output of coverage</td></tr>
  <tr><th>[\#14712](https://gitlab.haskell.org//ghc/ghc/issues/14712)</th>
  <td>After git pull make can't work without make clean</td></tr>
  <tr><th>[\#14716](https://gitlab.haskell.org//ghc/ghc/issues/14716)</th>
  <td>indexM-style accessor for arrays?</td></tr>
  <tr><th>[\#14722](https://gitlab.haskell.org//ghc/ghc/issues/14722)</th>
  <td>Error message points to wrong location</td></tr>
  <tr><th>[\#14755](https://gitlab.haskell.org//ghc/ghc/issues/14755)</th>
  <td>Allow putting SCC annotations on class instance methods</td></tr>
  <tr><th>[\#14756](https://gitlab.haskell.org//ghc/ghc/issues/14756)</th>
  <td>\`ghc -M\` doesn't emit dependencies for header files included either via CPP or CApiFFI</td></tr>
  <tr><th>[\#14770](https://gitlab.haskell.org//ghc/ghc/issues/14770)</th>
  <td>Allow static pointer expressions to have static pointer free variables</td></tr>
  <tr><th>[\#14772](https://gitlab.haskell.org//ghc/ghc/issues/14772)</th>
  <td>Keep Role Annotations in the renamed syntax tree</td></tr>
  <tr><th>[\#14793](https://gitlab.haskell.org//ghc/ghc/issues/14793)</th>
  <td>some executables in the binary distribution are not versioned</td></tr>
  <tr><th>[\#14800](https://gitlab.haskell.org//ghc/ghc/issues/14800)</th>
  <td>-Wincomplete-uni-patterns should not warn about explicitly-marked irrefutable patterns</td></tr>
  <tr><th>[\#14806](https://gitlab.haskell.org//ghc/ghc/issues/14806)</th>
  <td>Officially sanction certain unsafeCoerce applications with unboxed unary tuples</td></tr>
  <tr><th>[\#14809](https://gitlab.haskell.org//ghc/ghc/issues/14809)</th>
  <td>Invocation of ghc with --make flag should be smarter about hs-boot and lhs-boot files</td></tr>
  <tr><th>[\#14812](https://gitlab.haskell.org//ghc/ghc/issues/14812)</th>
  <td>Dot-Notation for Flipped Function Application</td></tr>
  <tr><th>[\#14818](https://gitlab.haskell.org//ghc/ghc/issues/14818)</th>
  <td>Provide highestOneBit function in Data.Bits module</td></tr>
  <tr><th>[\#14824](https://gitlab.haskell.org//ghc/ghc/issues/14824)</th>
  <td>automatically select instance</td></tr>
  <tr><th>[\#14826](https://gitlab.haskell.org//ghc/ghc/issues/14826)</th>
  <td>Flatten data types extending other data types in STG</td></tr>
  <tr><th>[\#14827](https://gitlab.haskell.org//ghc/ghc/issues/14827)</th>
  <td>Recognize when inlining would create a join point</td></tr>
  <tr><th>[\#14864](https://gitlab.haskell.org//ghc/ghc/issues/14864)</th>
  <td>Suggestion: Turn on +RTS -xt by default</td></tr>
  <tr><th>[\#14882](https://gitlab.haskell.org//ghc/ghc/issues/14882)</th>
  <td>memchr\#</td></tr>
  <tr><th>[\#14903](https://gitlab.haskell.org//ghc/ghc/issues/14903)</th>
  <td>RISC-V port</td></tr>
  <tr><th>[\#14911](https://gitlab.haskell.org//ghc/ghc/issues/14911)</th>
  <td>Offer a way to augment call stacks</td></tr>
  <tr><th>[\#14917](https://gitlab.haskell.org//ghc/ghc/issues/14917)</th>
  <td>Allow levity polymorphism in binding position</td></tr>
  <tr><th>[\#14922](https://gitlab.haskell.org//ghc/ghc/issues/14922)</th>
  <td>Add inductively-defined Nat to base</td></tr>
  <tr><th>[\#14937](https://gitlab.haskell.org//ghc/ghc/issues/14937)</th>
  <td>QuantifiedConstraints: Reify implication constraints from terms lacking them</td></tr>
  <tr><th>[\#14976](https://gitlab.haskell.org//ghc/ghc/issues/14976)</th>
  <td>WebAssembly support</td></tr>
  <tr><th>[\#14979](https://gitlab.haskell.org//ghc/ghc/issues/14979)</th>
  <td>Issue warning is -main-is is used in OPTIONS pragma</td></tr>
  <tr><th>[\#15000](https://gitlab.haskell.org//ghc/ghc/issues/15000)</th>
  <td>Add a linter for debug information (-g)</td></tr>
  <tr><th>[\#15003](https://gitlab.haskell.org//ghc/ghc/issues/15003)</th>
  <td>Data.List documentation should list complexities of more functions</td></tr>
  <tr><th>[\#15043](https://gitlab.haskell.org//ghc/ghc/issues/15043)</th>
  <td>A more aggressive version of -fprint-expanded-synonyms that prints all type synonyms</td></tr>
  <tr><th>[\#15044](https://gitlab.haskell.org//ghc/ghc/issues/15044)</th>
  <td>Option to output instance resolution process</td></tr>
  <tr><th>[\#15077](https://gitlab.haskell.org//ghc/ghc/issues/15077)</th>
  <td>Suggest NoMonomorphismRestriction or type signature</td></tr>
  <tr><th>[\#15078](https://gitlab.haskell.org//ghc/ghc/issues/15078)</th>
  <td>base: Customary type class laws (e.g. for Eq) and non-abiding instances (e.g. Float) should be documented</td></tr>
  <tr><th>[\#15080](https://gitlab.haskell.org//ghc/ghc/issues/15080)</th>
  <td>List Operators Sorted by Precedence in GHCi</td></tr>
  <tr><th>[\#15092](https://gitlab.haskell.org//ghc/ghc/issues/15092)</th>
  <td>Optionally bounds-check primops</td></tr>
  <tr><th>[\#15129](https://gitlab.haskell.org//ghc/ghc/issues/15129)</th>
  <td>Expose ghc-pkg internals as a library</td></tr>
  <tr><th>[\#15151](https://gitlab.haskell.org//ghc/ghc/issues/15151)</th>
  <td>Better Interaction Between Specialization and GND</td></tr>
  <tr><th>[\#15156](https://gitlab.haskell.org//ghc/ghc/issues/15156)</th>
  <td>Show instances for types exported from the ghc package</td></tr>
  <tr><th>[\#15182](https://gitlab.haskell.org//ghc/ghc/issues/15182)</th>
  <td>Lazier Semigroup instance for Maybe</td></tr>
  <tr><th>[\#15183](https://gitlab.haskell.org//ghc/ghc/issues/15183)</th>
  <td>Expose the SNat type and the natSing method</td></tr>
  <tr><th>[\#15194](https://gitlab.haskell.org//ghc/ghc/issues/15194)</th>
  <td>Consider a QSem variant that can hold back resources</td></tr>
  <tr><th>[\#15253](https://gitlab.haskell.org//ghc/ghc/issues/15253)</th>
  <td>Add support for type-level integers</td></tr>
  <tr><th>[\#15267](https://gitlab.haskell.org//ghc/ghc/issues/15267)</th>
  <td>Extend TVar/MVar to N capacity / Add primitive channel type</td></tr>
  <tr><th>[\#15272](https://gitlab.haskell.org//ghc/ghc/issues/15272)</th>
  <td>Handle implied flags more intuitively</td></tr>
  <tr><th>[\#15297](https://gitlab.haskell.org//ghc/ghc/issues/15297)</th>
  <td>Add support for insert, extract and broadcast SIMD instructions for native X86 codegen</td></tr>
  <tr><th>[\#15298](https://gitlab.haskell.org//ghc/ghc/issues/15298)</th>
  <td>Support spliced function names in type signatures in TH declaration quotes</td></tr>
  <tr><th>[\#15310](https://gitlab.haskell.org//ghc/ghc/issues/15310)</th>
  <td>Derive Generic1 instances for types of kind (k -\> \*) -\> \* that include applications of the parameter</td></tr>
  <tr><th>[\#15320](https://gitlab.haskell.org//ghc/ghc/issues/15320)</th>
  <td>Save the types in the typechecked syntax tree</td></tr>
  <tr><th>[\#15326](https://gitlab.haskell.org//ghc/ghc/issues/15326)</th>
  <td>Add option to disable error message expression context (the 'In the expression' things)</td></tr>
  <tr><th>[\#15327](https://gitlab.haskell.org//ghc/ghc/issues/15327)</th>
  <td>Optimize remainders by powers of two for Integer and Natural</td></tr>
  <tr><th>[\#15358](https://gitlab.haskell.org//ghc/ghc/issues/15358)</th>
  <td>no way to talk about unpacking sum types / unpacking tuples</td></tr>
  <tr><th>[\#15364](https://gitlab.haskell.org//ghc/ghc/issues/15364)</th>
  <td>Replace the atomicModifyMutVar\# primop</td></tr>
  <tr><th>[\#15392](https://gitlab.haskell.org//ghc/ghc/issues/15392)</th>
  <td>Inconsistency in parsing trailing commas inside import section</td></tr>
  <tr><th>[\#15394](https://gitlab.haskell.org//ghc/ghc/issues/15394)</th>
  <td>GHC doesn't come with dynamic object files/libraries compiled with profiling</td></tr>
  <tr><th>[\#15395](https://gitlab.haskell.org//ghc/ghc/issues/15395)</th>
  <td>Make StaticPtr (more) robust to code changes and recompilation</td></tr>
  <tr><th>[\#15403](https://gitlab.haskell.org//ghc/ghc/issues/15403)</th>
  <td>Compact Regions with Lazy Evaluation</td></tr>
  <tr><th>[\#15424](https://gitlab.haskell.org//ghc/ghc/issues/15424)</th>
  <td>There should be a flag for GHCi that tells it to load as many modules as possible</td></tr>
  <tr><th>[\#15432](https://gitlab.haskell.org//ghc/ghc/issues/15432)</th>
  <td>Referring to current module (or aliasing current module)</td></tr>
  <tr><th>[\#15441](https://gitlab.haskell.org//ghc/ghc/issues/15441)</th>
  <td>Data type with phantoms using TypeInType isn't coercible</td></tr>
  <tr><th>[\#15448](https://gitlab.haskell.org//ghc/ghc/issues/15448)</th>
  <td>Allow execution of stage2 compiler to happen later</td></tr>
  <tr><th>[\#15452](https://gitlab.haskell.org//ghc/ghc/issues/15452)</th>
  <td>\[GHCi\] Add an option to make tab completion case-insensitive</td></tr>
  <tr><th>[\#15454](https://gitlab.haskell.org//ghc/ghc/issues/15454)</th>
  <td>Have GHCi automatically use -fobject-code for modules that use UnboxedTuples</td></tr>
  <tr><th>[\#15461](https://gitlab.haskell.org//ghc/ghc/issues/15461)</th>
  <td>Machine accessible interface to GHCi</td></tr>
  <tr><th>[\#15468](https://gitlab.haskell.org//ghc/ghc/issues/15468)</th>
  <td>add -Wname-shadowing and -Wunused-pattern-binds to the default warnings for ghci</td></tr>
  <tr><th>[\#15470](https://gitlab.haskell.org//ghc/ghc/issues/15470)</th>
  <td>Record projections with ambiguous types</td></tr>
  <tr><th>[\#15483](https://gitlab.haskell.org//ghc/ghc/issues/15483)</th>
  <td>ghc -M requires -dep-suffix for no good reason</td></tr>
  <tr><th>[\#15489](https://gitlab.haskell.org//ghc/ghc/issues/15489)</th>
  <td>TestCoercion should be a superclass of TestEquality</td></tr>
  <tr><th>[\#15493](https://gitlab.haskell.org//ghc/ghc/issues/15493)</th>
  <td>Elide empty dictionaries</td></tr>
  <tr><th>[\#15510](https://gitlab.haskell.org//ghc/ghc/issues/15510)</th>
  <td>Qualified Holes</td></tr>
  <tr><th>[\#15512](https://gitlab.haskell.org//ghc/ghc/issues/15512)</th>
  <td>Rewrite rules should be able to produce custom compiler errors</td></tr>
  <tr><th>[\#15513](https://gitlab.haskell.org//ghc/ghc/issues/15513)</th>
  <td>How to pass "-hide-all-packages" to the GHC API?</td></tr>
  <tr><th>[\#15521](https://gitlab.haskell.org//ghc/ghc/issues/15521)</th>
  <td>Provide a strict version of sum</td></tr>
  <tr><th>[\#15530](https://gitlab.haskell.org//ghc/ghc/issues/15530)</th>
  <td>Type applications in patterns</td></tr>
  <tr><th>[\#15532](https://gitlab.haskell.org//ghc/ghc/issues/15532)</th>
  <td>Relaxing Levity-Polymorphic Binder Check for Lifted vs Unlifted pointers</td></tr>
  <tr><th>[\#15533](https://gitlab.haskell.org//ghc/ghc/issues/15533)</th>
  <td>Access the number of bits in the target machine's Int type at compile time</td></tr>
  <tr><th>[\#15534](https://gitlab.haskell.org//ghc/ghc/issues/15534)</th>
  <td>Allow associated types in Minimal pragmas</td></tr>
  <tr><th>[\#15536](https://gitlab.haskell.org//ghc/ghc/issues/15536)</th>
  <td>Unify unlifted pointer equality primitives</td></tr>
  <tr><th>[\#15546](https://gitlab.haskell.org//ghc/ghc/issues/15546)</th>
  <td>Display coaxiom branch incompatibilities in GHCi</td></tr>
  <tr><th>[\#15547](https://gitlab.haskell.org//ghc/ghc/issues/15547)</th>
  <td>A function \`nat2Word\# :: KnownNat n =\> Proxy\# n -\> Word\#\`</td></tr>
  <tr><th>[\#15548](https://gitlab.haskell.org//ghc/ghc/issues/15548)</th>
  <td>Make TABLES_NEXT_TO_CODE a dynflag</td></tr>
  <tr><th>[\#15557](https://gitlab.haskell.org//ghc/ghc/issues/15557)</th>
  <td>Reduce type families in equations' RHS when testing equation compatibility</td></tr>
  <tr><th>[\#15565](https://gitlab.haskell.org//ghc/ghc/issues/15565)</th>
  <td>ancient ghc release history on web page is incomplete</td></tr>
  <tr><th>[\#15566](https://gitlab.haskell.org//ghc/ghc/issues/15566)</th>
  <td>Implement minimumOn, maximumOn to mirror sortOn</td></tr>
  <tr><th>[\#15596](https://gitlab.haskell.org//ghc/ghc/issues/15596)</th>
  <td>When a type application cannot be applied to an identifier due to the absence of an explicit type signature, let the error just say so!</td></tr>
  <tr><th>[\#15610](https://gitlab.haskell.org//ghc/ghc/issues/15610)</th>
  <td>GHCi command to list instances a (possibly compound) type belongs to</td></tr>
  <tr><th>[\#15613](https://gitlab.haskell.org//ghc/ghc/issues/15613)</th>
  <td>GHCi command, tracing steps of instance resolution for Constraint or expression</td></tr>
  <tr><th>[\#15622](https://gitlab.haskell.org//ghc/ghc/issues/15622)</th>
  <td>Generalize \`E{0,1,2,3,6,9,12}\` from \`Data.Fixed\`</td></tr>
  <tr><th>[\#15640](https://gitlab.haskell.org//ghc/ghc/issues/15640)</th>
  <td>Add "difficulty" field to tickets</td></tr>
  <tr><th>[\#15642](https://gitlab.haskell.org//ghc/ghc/issues/15642)</th>
  <td>Improve the worst case performance of weak pointers</td></tr>
  <tr><th>[\#15650](https://gitlab.haskell.org//ghc/ghc/issues/15650)</th>
  <td>Add (or document if already exist) ability to derive custom typeclasses via source plugins</td></tr>
  <tr><th>[\#15657](https://gitlab.haskell.org//ghc/ghc/issues/15657)</th>
  <td>Support promotion of pattern synonyms to kinds</td></tr>
  <tr><th>[\#15665](https://gitlab.haskell.org//ghc/ghc/issues/15665)</th>
  <td>Break up the stable pointer table</td></tr>
  <tr><th>[\#15674](https://gitlab.haskell.org//ghc/ghc/issues/15674)</th>
  <td>GADT's displayed type is misleading</td></tr>
  <tr><th>[\#15687](https://gitlab.haskell.org//ghc/ghc/issues/15687)</th>
  <td>Type synonym unused binds no warning?</td></tr>
  <tr><th>[\#15690](https://gitlab.haskell.org//ghc/ghc/issues/15690)</th>
  <td>Add Eq1/Ord1/Read1/Show1 instances to newtypes in Data.Semigroup</td></tr>
  <tr><th>[\#15707](https://gitlab.haskell.org//ghc/ghc/issues/15707)</th>
  <td>More liberally kinded coercions for newtypes</td></tr>
  <tr><th>[\#15719](https://gitlab.haskell.org//ghc/ghc/issues/15719)</th>
  <td>Primitive atomic logical operations</td></tr>
  <tr><th>[\#15726](https://gitlab.haskell.org//ghc/ghc/issues/15726)</th>
  <td>Don't include Haskeline (and others) in the global package DB</td></tr>
  <tr><th>[\#15731](https://gitlab.haskell.org//ghc/ghc/issues/15731)</th>
  <td>Add sortOn/coerce rule</td></tr>
  <tr><th>[\#15737](https://gitlab.haskell.org//ghc/ghc/issues/15737)</th>
  <td>Implement sconcat Semigroup Instances for Tuples</td></tr>
  <tr><th>[\#15741](https://gitlab.haskell.org//ghc/ghc/issues/15741)</th>
  <td>Accept GHCRTS=-N1 when not compiled with -threaded</td></tr>
  <tr><th>[\#15745](https://gitlab.haskell.org//ghc/ghc/issues/15745)</th>
  <td>Panicking typechecker plugins</td></tr>
  <tr><th>[\#15762](https://gitlab.haskell.org//ghc/ghc/issues/15762)</th>
  <td>ghci command: report function's inferred visible type applications</td></tr>
  <tr><th>[\#15782](https://gitlab.haskell.org//ghc/ghc/issues/15782)</th>
  <td>Visible type/kind applications in declaration of data/type constructors</td></tr>
  <tr><th>[\#15785](https://gitlab.haskell.org//ghc/ghc/issues/15785)</th>
  <td>Improve/complete the GHCi :doc command</td></tr>
  <tr><th>[\#15786](https://gitlab.haskell.org//ghc/ghc/issues/15786)</th>
  <td>Hi Haddock: Enable haddock to generate docs from .hi-files</td></tr>
  <tr><th>[\#15814](https://gitlab.haskell.org//ghc/ghc/issues/15814)</th>
  <td>Orphan Instance Overlap Error Message</td></tr>
  <tr><th>[\#15838](https://gitlab.haskell.org//ghc/ghc/issues/15838)</th>
  <td>Warn about unused dependencies</td></tr>
  <tr><th>[\#15842](https://gitlab.haskell.org//ghc/ghc/issues/15842)</th>
  <td>Exponentiation needs PrelRules</td></tr>
  <tr><th>[\#15850](https://gitlab.haskell.org//ghc/ghc/issues/15850)</th>
  <td>coercion errors don't always mention that data constructors are not in scope</td></tr>
  <tr><th>[\#15851](https://gitlab.haskell.org//ghc/ghc/issues/15851)</th>
  <td>Error message: "accepting non-standard pattern guards" when there are no actual patterns</td></tr>
  <tr><th>[\#15855](https://gitlab.haskell.org//ghc/ghc/issues/15855)</th>
  <td>Warn about incomplete NamedFieldPuns patterns</td></tr>
  <tr><th>[\#15857](https://gitlab.haskell.org//ghc/ghc/issues/15857)</th>
  <td>Need for better warnings arising from StarIsType extension</td></tr>
  <tr><th>[\#15868](https://gitlab.haskell.org//ghc/ghc/issues/15868)</th>
  <td>Standard deriving should be less conservative when \`UndecidableInstances\` is enabled</td></tr>
  <tr><th>[\#15876](https://gitlab.haskell.org//ghc/ghc/issues/15876)</th>
  <td>Function versioning instead of compilation flags...</td></tr>
  <tr><th>[\#15882](https://gitlab.haskell.org//ghc/ghc/issues/15882)</th>
  <td>:load in ghci should expose the entire namespace of a module</td></tr>
  <tr><th>[\#15890](https://gitlab.haskell.org//ghc/ghc/issues/15890)</th>
  <td>Provide a way for hadrian users to always pass some options to hadrian itself</td></tr>
  <tr><th>[\#15912](https://gitlab.haskell.org//ghc/ghc/issues/15912)</th>
  <td>Output hadrian build data for cabal-helper tool support</td></tr>
  <tr><th>[\#15959](https://gitlab.haskell.org//ghc/ghc/issues/15959)</th>
  <td>If a type signature is too long to read left-to-right then let it read top-to-bottom.</td></tr>
  <tr><th>[\#15965](https://gitlab.haskell.org//ghc/ghc/issues/15965)</th>
  <td>readv and pread support</td></tr>
  <tr><th>[\#15983](https://gitlab.haskell.org//ghc/ghc/issues/15983)</th>
  <td>Built-in support for half-floats</td></tr>
  <tr><th>[\#15993](https://gitlab.haskell.org//ghc/ghc/issues/15993)</th>
  <td>Bitwise-oriented semigroup and monoid newtype wrappers for Data.Bits.Bits instances</td></tr>
  <tr><th>[\#15996](https://gitlab.haskell.org//ghc/ghc/issues/15996)</th>
  <td>Add Unlifted List type to base</td></tr>
  <tr><th>[\#15997](https://gitlab.haskell.org//ghc/ghc/issues/15997)</th>
  <td>EventManager could benefit from Data.Primitive.UnliftedArray</td></tr>
  <tr><th>[\#15998](https://gitlab.haskell.org//ghc/ghc/issues/15998)</th>
  <td>GHC.Event.Thread.eventManager has a lot of indirections</td></tr>
  <tr><th>[\#16009](https://gitlab.haskell.org//ghc/ghc/issues/16009)</th>
  <td>Deprecate \`optional\` from Text.ParserCombinators.ReadP</td></tr>
  <tr><th>[\#16027](https://gitlab.haskell.org//ghc/ghc/issues/16027)</th>
  <td>Cannot use DefaultSignatures for TypeFamiles</td></tr>
  <tr><th>[\#16036](https://gitlab.haskell.org//ghc/ghc/issues/16036)</th>
  <td>expDouble\#\# 0.0\#\# doesn't get complied into 1.0\#\#</td></tr>
  <tr><th>[\#16049](https://gitlab.haskell.org//ghc/ghc/issues/16049)</th>
  <td>Add a warning flag that warns when a datatype could be a newtype</td></tr>
  <tr><th>[\#16050](https://gitlab.haskell.org//ghc/ghc/issues/16050)</th>
  <td>Instance resolution error message unclear, because of missing kind information</td></tr>
  <tr><th>[\#16086](https://gitlab.haskell.org//ghc/ghc/issues/16086)</th>
  <td>Compile countLeadingZeros to lzcnt if -mbmi2 is set</td></tr>
  <tr><th>[\#16099](https://gitlab.haskell.org//ghc/ghc/issues/16099)</th>
  <td>expose st_blksize field from fstat syscall</td></tr>
  <tr><th>[\#16107](https://gitlab.haskell.org//ghc/ghc/issues/16107)</th>
  <td>Update GCC compiler & friends</td></tr>
  <tr><th>[\#16108](https://gitlab.haskell.org//ghc/ghc/issues/16108)</th>
  <td>GHC --version doesn't show arch</td></tr>
  <tr><th>[\#16119](https://gitlab.haskell.org//ghc/ghc/issues/16119)</th>
  <td>Hide the gnarly levity polymorphism stuff in the signatures of \`undefined\`, \`throw\`, etc</td></tr>
  <tr><th>[\#16120](https://gitlab.haskell.org//ghc/ghc/issues/16120)</th>
  <td>Hadrian should use cabal build-tools installed Alex and Happy if necessary</td></tr>
  <tr><th>[\#16128](https://gitlab.haskell.org//ghc/ghc/issues/16128)</th>
  <td>Pattern match checker should shortcut on simple cases</td></tr>
  <tr><th>[\#16144](https://gitlab.haskell.org//ghc/ghc/issues/16144)</th>
  <td>Provide integer-simple builds on Windows.</td></tr>
  <tr><th>[\#16149](https://gitlab.haskell.org//ghc/ghc/issues/16149)</th>
  <td>GHC for NetBSD</td></tr>
  <tr><th>[\#16155](https://gitlab.haskell.org//ghc/ghc/issues/16155)</th>
  <td>Pattern Synonym for Ratio</td></tr>
  <tr><th>[\#16163](https://gitlab.haskell.org//ghc/ghc/issues/16163)</th>
  <td>Don’t throw an error (by default) for use of DEPRECATED symbols under -Werror</td></tr>
  <tr><th>[\#16164](https://gitlab.haskell.org//ghc/ghc/issues/16164)</th>
  <td>Provide bitreverse primop</td></tr>
  <tr><th>[\#16173](https://gitlab.haskell.org//ghc/ghc/issues/16173)</th>
  <td>Move \`Data.Profunctor\` from \`profunctors\` package to \`base\`</td></tr>
  <tr><th>[\#16176](https://gitlab.haskell.org//ghc/ghc/issues/16176)</th>
  <td>Let-insertion for template haskell</td></tr>
  <tr><th>[\#16177](https://gitlab.haskell.org//ghc/ghc/issues/16177)</th>
  <td>Rename Q (TExp a) to Code a</td></tr>
  <tr><th>[\#16178](https://gitlab.haskell.org//ghc/ghc/issues/16178)</th>
  <td>Brackets and splices should be overloaded like the static keyword</td></tr>
  <tr><th>[\#16189](https://gitlab.haskell.org//ghc/ghc/issues/16189)</th>
  <td>ParsedSource (especially module name source span) not available from Source Plugin</td></tr>
  <tr><th>[\#16216](https://gitlab.haskell.org//ghc/ghc/issues/16216)</th>
  <td>readHexRational missing from Numeric</td></tr>
  <tr><th>[\#16220](https://gitlab.haskell.org//ghc/ghc/issues/16220)</th>
  <td>-Wmissing-signatures should warn when top-level signature is partial</td></tr>
  <tr><th>[\#16232](https://gitlab.haskell.org//ghc/ghc/issues/16232)</th>
  <td>Add setField to HasField</td></tr>
  <tr><th>[\#16237](https://gitlab.haskell.org//ghc/ghc/issues/16237)</th>
  <td>Run check-api-annotations and check-ppr on all parseable test code</td></tr>
  <tr><th>[\#16238](https://gitlab.haskell.org//ghc/ghc/issues/16238)</th>
  <td>Hadrian provides no way to accept output of test</td></tr>
  <tr><th>[\#16240](https://gitlab.haskell.org//ghc/ghc/issues/16240)</th>
  <td>Core visible  conditional compilation/static definition/code selection based upon static key values</td></tr>
  <tr><th>[\#16266](https://gitlab.haskell.org//ghc/ghc/issues/16266)</th>
  <td>Allow definition of functions with absurd contexts</td></tr>
  <tr><th>[\#16267](https://gitlab.haskell.org//ghc/ghc/issues/16267)</th>
  <td>EXTRA_HC_OPTS workflow under hadrian.</td></tr>
  <tr><th>[\#16276](https://gitlab.haskell.org//ghc/ghc/issues/16276)</th>
  <td>Feature request: Polymorphic kinds in Data.Functor.Classes</td></tr>
  <tr><th>[\#16290](https://gitlab.haskell.org//ghc/ghc/issues/16290)</th>
  <td>Export \`noinline\` function from \`GHC.Exts\`</td></tr>
  <tr><th>[\#16300](https://gitlab.haskell.org//ghc/ghc/issues/16300)</th>
  <td>Make TH always reify data types with explicit return kinds</td></tr>
  <tr><th>[\#16301](https://gitlab.haskell.org//ghc/ghc/issues/16301)</th>
  <td>Additions to Control.Exception: withException, displaySomeExceptionType</td></tr>
  <tr><th>[\#16309](https://gitlab.haskell.org//ghc/ghc/issues/16309)</th>
  <td>Flag to instruct GHC not to use an environment file</td></tr>
  <tr><th>[\#16314](https://gitlab.haskell.org//ghc/ghc/issues/16314)</th>
  <td>Improve confusing error message with MINIMAL pragma</td></tr>
  <tr><th>[\#16316](https://gitlab.haskell.org//ghc/ghc/issues/16316)</th>
  <td>\`-package-env=\` in OPTIONS_GHC not supported</td></tr>
  <tr><th>[\#16321](https://gitlab.haskell.org//ghc/ghc/issues/16321)</th>
  <td>Suggest -XScopedTypeVariables instead of -XRankNTypes</td></tr>
  <tr><th>[\#16332](https://gitlab.haskell.org//ghc/ghc/issues/16332)</th>
  <td>Feature request: Data.Foldable.asumMap</td></tr>
  <tr><th>[\#16351](https://gitlab.haskell.org//ghc/ghc/issues/16351)</th>
  <td>Extend constant folding operations to bit manipulations</td></tr>
  <tr><th>[\#16363](https://gitlab.haskell.org//ghc/ghc/issues/16363)</th>
  <td>Modular constant folding</td></tr>
  <tr><th>[\#16366](https://gitlab.haskell.org//ghc/ghc/issues/16366)</th>
  <td>Strict version of \`foldlM\`.</td></tr>
  <tr><th>[\#16380](https://gitlab.haskell.org//ghc/ghc/issues/16380)</th>
  <td>HPC's CLI is awkward</td></tr>
  <tr><th>[\#16403](https://gitlab.haskell.org//ghc/ghc/issues/16403)</th>
  <td>Add isBinDigit to Data.Char</td></tr>
  <tr><th>[\#16412](https://gitlab.haskell.org//ghc/ghc/issues/16412)</th>
  <td>Type family signatures in indefinite modules</td></tr></table>

   feature requests, 

  <table><tr><th>[\#602](https://gitlab.haskell.org//ghc/ghc/issues/602)</th>
  <td>Warning Suppression</td></tr>
  <tr><th>[\#605](https://gitlab.haskell.org//ghc/ghc/issues/605)</th>
  <td>Optimisation: strict enumerations</td></tr>
  <tr><th>[\#609](https://gitlab.haskell.org//ghc/ghc/issues/609)</th>
  <td>Useful optimisation for set-cost-centre</td></tr>
  <tr><th>[\#618](https://gitlab.haskell.org//ghc/ghc/issues/618)</th>
  <td>Dependency caching in ghc --make</td></tr>
  <tr><th>[\#624](https://gitlab.haskell.org//ghc/ghc/issues/624)</th>
  <td>Program location for thread error messages</td></tr>
  <tr><th>[\#634](https://gitlab.haskell.org//ghc/ghc/issues/634)</th>
  <td>Implement a more efficient TArray</td></tr>
  <tr><th>[\#701](https://gitlab.haskell.org//ghc/ghc/issues/701)</th>
  <td>Better CSE optimisation</td></tr>
  <tr><th>[\#855](https://gitlab.haskell.org//ghc/ghc/issues/855)</th>
  <td>Improvements to SpecConstr</td></tr>
  <tr><th>[\#888](https://gitlab.haskell.org//ghc/ghc/issues/888)</th>
  <td>Implement the static argument transformation</td></tr>
  <tr><th>[\#932](https://gitlab.haskell.org//ghc/ghc/issues/932)</th>
  <td>Improve inlining</td></tr>
  <tr><th>[\#1016](https://gitlab.haskell.org//ghc/ghc/issues/1016)</th>
  <td>Avoidance of unaligned loads is overly conservative</td></tr>
  <tr><th>[\#1349](https://gitlab.haskell.org//ghc/ghc/issues/1349)</th>
  <td>Generalise the ! and UNPACK mechanism for data types, to unpack function arguments</td></tr>
  <tr><th>[\#1371](https://gitlab.haskell.org//ghc/ghc/issues/1371)</th>
  <td>Add  -O3</td></tr>
  <tr><th>[\#1377](https://gitlab.haskell.org//ghc/ghc/issues/1377)</th>
  <td>GHCi debugger tasks</td></tr>
  <tr><th>[\#1572](https://gitlab.haskell.org//ghc/ghc/issues/1572)</th>
  <td>Make it easy to find documentation for GHC and installed packages</td></tr>
  <tr><th>[\#1574](https://gitlab.haskell.org//ghc/ghc/issues/1574)</th>
  <td>Broken link testing</td></tr>
  <tr><th>[\#1600](https://gitlab.haskell.org//ghc/ghc/issues/1600)</th>
  <td>Optimisation: CPR the results of IO</td></tr>
  <tr><th>[\#1631](https://gitlab.haskell.org//ghc/ghc/issues/1631)</th>
  <td>Make the External Package Table contain ModDetails not ModIface</td></tr>
  <tr><th>[\#2123](https://gitlab.haskell.org//ghc/ghc/issues/2123)</th>
  <td>implement waitForProcess using signals</td></tr>
  <tr><th>[\#2725](https://gitlab.haskell.org//ghc/ghc/issues/2725)</th>
  <td>Remove Hack in compiler/nativeGen/X86/CodeGen.hs</td></tr>
  <tr><th>[\#2968](https://gitlab.haskell.org//ghc/ghc/issues/2968)</th>
  <td>Avoid generating C trigraphs</td></tr>
  <tr><th>[\#2988](https://gitlab.haskell.org//ghc/ghc/issues/2988)</th>
  <td>Improve float-in</td></tr>
  <tr><th>[\#3251](https://gitlab.haskell.org//ghc/ghc/issues/3251)</th>
  <td>split rts headers into public and private</td></tr>
  <tr><th>[\#3355](https://gitlab.haskell.org//ghc/ghc/issues/3355)</th>
  <td>Refactor Template Haskell syntax conversions</td></tr>
  <tr><th>[\#3379](https://gitlab.haskell.org//ghc/ghc/issues/3379)</th>
  <td>GHC should use the standard binary package</td></tr>
  <tr><th>[\#3462](https://gitlab.haskell.org//ghc/ghc/issues/3462)</th>
  <td>New codegen: allocate large objects using allocateLocal()</td></tr>
  <tr><th>[\#3511](https://gitlab.haskell.org//ghc/ghc/issues/3511)</th>
  <td>port GHC to OpenBSD/sparc64 (unregisterised is fine)</td></tr>
  <tr><th>[\#3713](https://gitlab.haskell.org//ghc/ghc/issues/3713)</th>
  <td>Track -dynamic/-fPIC to avoid obscure linker errors</td></tr>
  <tr><th>[\#3755](https://gitlab.haskell.org//ghc/ghc/issues/3755)</th>
  <td>Improve join point inlining</td></tr>
  <tr><th>[\#3946](https://gitlab.haskell.org//ghc/ghc/issues/3946)</th>
  <td>Better diagnostic when entering a GC'd CAF</td></tr>
  <tr><th>[\#4211](https://gitlab.haskell.org//ghc/ghc/issues/4211)</th>
  <td>LLVM: Stack alignment on OSX</td></tr>
  <tr><th>[\#4243](https://gitlab.haskell.org//ghc/ghc/issues/4243)</th>
  <td>Make a proper options parser for the RTS</td></tr>
  <tr><th>[\#4281](https://gitlab.haskell.org//ghc/ghc/issues/4281)</th>
  <td>Make impredicativity work properly</td></tr>
  <tr><th>[\#4295](https://gitlab.haskell.org//ghc/ghc/issues/4295)</th>
  <td>Review higher-rank and impredicative types</td></tr>
  <tr><th>[\#4374](https://gitlab.haskell.org//ghc/ghc/issues/4374)</th>
  <td>Remove in-tree gmp</td></tr>
  <tr><th>[\#4941](https://gitlab.haskell.org//ghc/ghc/issues/4941)</th>
  <td>SpecConstr generates functions that do not use their arguments</td></tr>
  <tr><th>[\#4960](https://gitlab.haskell.org//ghc/ghc/issues/4960)</th>
  <td>Better inlining test in CoreUnfold</td></tr>
  <tr><th>[\#5140](https://gitlab.haskell.org//ghc/ghc/issues/5140)</th>
  <td>Fix LLVM backend for PowerPC</td></tr>
  <tr><th>[\#5143](https://gitlab.haskell.org//ghc/ghc/issues/5143)</th>
  <td>Soft heap limit flag</td></tr>
  <tr><th>[\#5567](https://gitlab.haskell.org//ghc/ghc/issues/5567)</th>
  <td>LLVM: Improve alias analysis / performance</td></tr>
  <tr><th>[\#5791](https://gitlab.haskell.org//ghc/ghc/issues/5791)</th>
  <td>Defer other kinds of errors until runtime, not just type errors</td></tr>
  <tr><th>[\#5793](https://gitlab.haskell.org//ghc/ghc/issues/5793)</th>
  <td>make nofib awesome</td></tr>
  <tr><th>[\#6017](https://gitlab.haskell.org//ghc/ghc/issues/6017)</th>
  <td>Reading ./.ghci files raises security issues</td></tr>
  <tr><th>[\#7790](https://gitlab.haskell.org//ghc/ghc/issues/7790)</th>
  <td>Add dummy undefined symbols to indicate ways</td></tr>
  <tr><th>[\#7829](https://gitlab.haskell.org//ghc/ghc/issues/7829)</th>
  <td>make better/more robust loopbreaker choices</td></tr>
  <tr><th>[\#7917](https://gitlab.haskell.org//ghc/ghc/issues/7917)</th>
  <td>update documentation of InstalledPackageInfo</td></tr>
  <tr><th>[\#8050](https://gitlab.haskell.org//ghc/ghc/issues/8050)</th>
  <td>add a required wrapper around plugin installers</td></tr>
  <tr><th>[\#8096](https://gitlab.haskell.org//ghc/ghc/issues/8096)</th>
  <td>Add fudge-factor for performance tests run on non-validate builds</td></tr>
  <tr><th>[\#8238](https://gitlab.haskell.org//ghc/ghc/issues/8238)</th>
  <td>Implement unloading of shared libraries</td></tr>
  <tr><th>[\#8272](https://gitlab.haskell.org//ghc/ghc/issues/8272)</th>
  <td>testing if SpLim=$rbp and Sp=$rsp changed performance at all</td></tr>
  <tr><th>[\#8287](https://gitlab.haskell.org//ghc/ghc/issues/8287)</th>
  <td>exploring calling convention changes and related engineering for 7.10</td></tr>
  <tr><th>[\#8290](https://gitlab.haskell.org//ghc/ghc/issues/8290)</th>
  <td>lookupSymbol API is unsafe</td></tr>
  <tr><th>[\#8313](https://gitlab.haskell.org//ghc/ghc/issues/8313)</th>
  <td>Poor performance of higher-order functions with unboxing</td></tr>
  <tr><th>[\#8315](https://gitlab.haskell.org//ghc/ghc/issues/8315)</th>
  <td>Improve specialized Hoopl module</td></tr>
  <tr><th>[\#8317](https://gitlab.haskell.org//ghc/ghc/issues/8317)</th>
  <td>Optimize tagToEnum\# at Core level</td></tr>
  <tr><th>[\#8323](https://gitlab.haskell.org//ghc/ghc/issues/8323)</th>
  <td>explore ways to possibly use more tag bits in x86_64 pointers</td></tr>
  <tr><th>[\#8326](https://gitlab.haskell.org//ghc/ghc/issues/8326)</th>
  <td>Place heap checks common in case alternatives before the case</td></tr>
  <tr><th>[\#8488](https://gitlab.haskell.org//ghc/ghc/issues/8488)</th>
  <td>Annotations should not distinguish type and value</td></tr>
  <tr><th>[\#8489](https://gitlab.haskell.org//ghc/ghc/issues/8489)</th>
  <td>clean up dependency and usages handling in interface files</td></tr>
  <tr><th>[\#8578](https://gitlab.haskell.org//ghc/ghc/issues/8578)</th>
  <td>Improvements to SpinLock implementation</td></tr>
  <tr><th>[\#8597](https://gitlab.haskell.org//ghc/ghc/issues/8597)</th>
  <td>Git Hook script to prevent large binary blobs being checked in</td></tr>
  <tr><th>[\#8598](https://gitlab.haskell.org//ghc/ghc/issues/8598)</th>
  <td>IO hack in demand analyzer gets in the way of CPR</td></tr>
  <tr><th>[\#8655](https://gitlab.haskell.org//ghc/ghc/issues/8655)</th>
  <td>Evaluate know-to-terminate-soon thunks</td></tr>
  <tr><th>[\#8767](https://gitlab.haskell.org//ghc/ghc/issues/8767)</th>
  <td>Add rules involving \`coerce\` to the libraries</td></tr>
  <tr><th>[\#8782](https://gitlab.haskell.org//ghc/ghc/issues/8782)</th>
  <td>Using GADT's to maintain invariant in GHC libraries</td></tr>
  <tr><th>[\#8910](https://gitlab.haskell.org//ghc/ghc/issues/8910)</th>
  <td>cross compiling for x86_64 solaris2</td></tr>
  <tr><th>[\#9133](https://gitlab.haskell.org//ghc/ghc/issues/9133)</th>
  <td>Improve parser error reporting in \`ghc-pkg\`</td></tr>
  <tr><th>[\#9251](https://gitlab.haskell.org//ghc/ghc/issues/9251)</th>
  <td>ghc does not expose branchless max/min operations as primops</td></tr>
  <tr><th>[\#9276](https://gitlab.haskell.org//ghc/ghc/issues/9276)</th>
  <td>audit ghc floating point support for IEEE (non)compliance</td></tr>
  <tr><th>[\#9374](https://gitlab.haskell.org//ghc/ghc/issues/9374)</th>
  <td>Investigate Static Argument Transformation</td></tr>
  <tr><th>[\#9403](https://gitlab.haskell.org//ghc/ghc/issues/9403)</th>
  <td>Make --show-iface more human readable</td></tr>
  <tr><th>[\#9496](https://gitlab.haskell.org//ghc/ghc/issues/9496)</th>
  <td>Simplify primitives for short cut fusion</td></tr>
  <tr><th>[\#9505](https://gitlab.haskell.org//ghc/ghc/issues/9505)</th>
  <td>Bounded instance for Word (and possibly others) uses explicitly unboxed literals</td></tr>
  <tr><th>[\#9511](https://gitlab.haskell.org//ghc/ghc/issues/9511)</th>
  <td>Remove deprecated -fglasgow-exts from NoFib suite</td></tr>
  <tr><th>[\#9534](https://gitlab.haskell.org//ghc/ghc/issues/9534)</th>
  <td>IEEE Standard 754 for Binary Floating-Point Arithmetic by Prof. W. Kahan, UCB</td></tr>
  <tr><th>[\#9542](https://gitlab.haskell.org//ghc/ghc/issues/9542)</th>
  <td>GHC-IO-Handle-Text.hPutStr' and writeBlocks look like they need refactoring</td></tr>
  <tr><th>[\#9572](https://gitlab.haskell.org//ghc/ghc/issues/9572)</th>
  <td>nofib target for just building should be part of validate</td></tr>
  <tr><th>[\#9588](https://gitlab.haskell.org//ghc/ghc/issues/9588)</th>
  <td>Add \`MonadPlus (Either e)\` and \`Alternative (Either e)\` instances</td></tr>
  <tr><th>[\#9674](https://gitlab.haskell.org//ghc/ghc/issues/9674)</th>
  <td>Foldable doesn't have any laws</td></tr>
  <tr><th>[\#9716](https://gitlab.haskell.org//ghc/ghc/issues/9716)</th>
  <td>The list modules need a bit of post-BBP shaking</td></tr>
  <tr><th>[\#9718](https://gitlab.haskell.org//ghc/ghc/issues/9718)</th>
  <td>Avoid TidyPgm predicting what CorePrep will do</td></tr>
  <tr><th>[\#9719](https://gitlab.haskell.org//ghc/ghc/issues/9719)</th>
  <td>Improve \`mkInteger\` interface</td></tr>
  <tr><th>[\#9735](https://gitlab.haskell.org//ghc/ghc/issues/9735)</th>
  <td>Template Haskell for cross compilers (port from GHCJS)</td></tr>
  <tr><th>[\#9786](https://gitlab.haskell.org//ghc/ghc/issues/9786)</th>
  <td>Make quot/rem/div/mod with known divisors fast</td></tr>
  <tr><th>[\#9805](https://gitlab.haskell.org//ghc/ghc/issues/9805)</th>
  <td>Use TrieMaps to speed up type class instance lookup</td></tr>
  <tr><th>[\#9837](https://gitlab.haskell.org//ghc/ghc/issues/9837)</th>
  <td>Introduce a logging API to GHC</td></tr>
  <tr><th>[\#10068](https://gitlab.haskell.org//ghc/ghc/issues/10068)</th>
  <td>Make the runtime reflection API for names, modules, locations more systematic</td></tr>
  <tr><th>[\#10074](https://gitlab.haskell.org//ghc/ghc/issues/10074)</th>
  <td>Implement the 'Improved LLVM Backend' proposal</td></tr>
  <tr><th>[\#10143](https://gitlab.haskell.org//ghc/ghc/issues/10143)</th>
  <td>Separate PprFlags (used by Outputable) from DynFlags</td></tr>
  <tr><th>[\#10172](https://gitlab.haskell.org//ghc/ghc/issues/10172)</th>
  <td>Cross-platform sed</td></tr>
  <tr><th>[\#10181](https://gitlab.haskell.org//ghc/ghc/issues/10181)</th>
  <td>Lint check: arity invariant</td></tr>
  <tr><th>[\#10266](https://gitlab.haskell.org//ghc/ghc/issues/10266)</th>
  <td>Split base for Backpack</td></tr>
  <tr><th>[\#10303](https://gitlab.haskell.org//ghc/ghc/issues/10303)</th>
  <td>Make it easier to print stack traces when debugging GHC itself</td></tr>
  <tr><th>[\#10319](https://gitlab.haskell.org//ghc/ghc/issues/10319)</th>
  <td>Eta expand PAPs</td></tr>
  <tr><th>[\#10344](https://gitlab.haskell.org//ghc/ghc/issues/10344)</th>
  <td>Make BranchList simpler</td></tr>
  <tr><th>[\#10352](https://gitlab.haskell.org//ghc/ghc/issues/10352)</th>
  <td>Properly link Haskell shared libs on all systems</td></tr>
  <tr><th>[\#10450](https://gitlab.haskell.org//ghc/ghc/issues/10450)</th>
  <td>Poor type error message when an argument is insufficently polymorphic</td></tr>
  <tr><th>[\#10640](https://gitlab.haskell.org//ghc/ghc/issues/10640)</th>
  <td>Document prim-ops</td></tr>
  <tr><th>[\#10710](https://gitlab.haskell.org//ghc/ghc/issues/10710)</th>
  <td>More self-explanatory pragmas for inlining phase control</td></tr>
  <tr><th>[\#10735](https://gitlab.haskell.org//ghc/ghc/issues/10735)</th>
  <td>Smooth out the differences between \`compiler/utils/Pretty.hs\` and \`libraries/pretty\`</td></tr>
  <tr><th>[\#10739](https://gitlab.haskell.org//ghc/ghc/issues/10739)</th>
  <td>Resuscitate the humble ticky-ticky profiler</td></tr>
  <tr><th>[\#10740](https://gitlab.haskell.org//ghc/ghc/issues/10740)</th>
  <td>Ensure that ticky, profiler, and GC allocation numbers agree</td></tr>
  <tr><th>[\#10766](https://gitlab.haskell.org//ghc/ghc/issues/10766)</th>
  <td>user manual: INLINE's interaction with optimization levels is not clear</td></tr>
  <tr><th>[\#10844](https://gitlab.haskell.org//ghc/ghc/issues/10844)</th>
  <td>CallStack should not be inlined</td></tr>
  <tr><th>[\#10854](https://gitlab.haskell.org//ghc/ghc/issues/10854)</th>
  <td>Remove recursive uses of \`pprParendHsExpr\` from \`HsExpr.ppr_expr\`</td></tr>
  <tr><th>[\#10892](https://gitlab.haskell.org//ghc/ghc/issues/10892)</th>
  <td>ApplicativeDo should use \*\> and \<\*</td></tr>
  <tr><th>[\#10909](https://gitlab.haskell.org//ghc/ghc/issues/10909)</th>
  <td>Test suite: Support non-utf8 locale</td></tr>
  <tr><th>[\#10913](https://gitlab.haskell.org//ghc/ghc/issues/10913)</th>
  <td>deprecate and then remove -fwarn-hi-shadowing</td></tr>
  <tr><th>[\#10918](https://gitlab.haskell.org//ghc/ghc/issues/10918)</th>
  <td>Float once-used let binding into a recursive function</td></tr>
  <tr><th>[\#10941](https://gitlab.haskell.org//ghc/ghc/issues/10941)</th>
  <td>Large heap address space breaks valgrind</td></tr>
  <tr><th>[\#10962](https://gitlab.haskell.org//ghc/ghc/issues/10962)</th>
  <td>Improved arithmetic primops</td></tr>
  <tr><th>[\#11031](https://gitlab.haskell.org//ghc/ghc/issues/11031)</th>
  <td>Record Pattern Synonym Cleanup</td></tr>
  <tr><th>[\#11138](https://gitlab.haskell.org//ghc/ghc/issues/11138)</th>
  <td>Kill the terrible LLVM Mangler</td></tr>
  <tr><th>[\#11149](https://gitlab.haskell.org//ghc/ghc/issues/11149)</th>
  <td>Unify fixity/associativity of \<\>-ish pretty-printing operators</td></tr>
  <tr><th>[\#11238](https://gitlab.haskell.org//ghc/ghc/issues/11238)</th>
  <td>Redesign the dynamic linking on ELF systems</td></tr>
  <tr><th>[\#11295](https://gitlab.haskell.org//ghc/ghc/issues/11295)</th>
  <td>Figure out what LLVM passes are fruitful</td></tr>
  <tr><th>[\#11359](https://gitlab.haskell.org//ghc/ghc/issues/11359)</th>
  <td>Consider removing RelaxedLayout and friends</td></tr>
  <tr><th>[\#11366](https://gitlab.haskell.org//ghc/ghc/issues/11366)</th>
  <td>Control.Exception.assert should perhaps take an implicit call stack</td></tr>
  <tr><th>[\#11394](https://gitlab.haskell.org//ghc/ghc/issues/11394)</th>
  <td>Base should use native Win32 IO on Windows</td></tr>
  <tr><th>[\#11502](https://gitlab.haskell.org//ghc/ghc/issues/11502)</th>
  <td>Scrutinize use of 'long' in rts/</td></tr>
  <tr><th>[\#11528](https://gitlab.haskell.org//ghc/ghc/issues/11528)</th>
  <td>Representation of value set abstractions as trees causes performance issues</td></tr>
  <tr><th>[\#11551](https://gitlab.haskell.org//ghc/ghc/issues/11551)</th>
  <td>Get doctests into testsuite</td></tr>
  <tr><th>[\#11557](https://gitlab.haskell.org//ghc/ghc/issues/11557)</th>
  <td>Unbundle Cabal from GHC</td></tr>
  <tr><th>[\#11609](https://gitlab.haskell.org//ghc/ghc/issues/11609)</th>
  <td>Document unicode report deviations</td></tr>
  <tr><th>[\#11610](https://gitlab.haskell.org//ghc/ghc/issues/11610)</th>
  <td>Remove IEThingAll constructor from IE datatype</td></tr>
  <tr><th>[\#11613](https://gitlab.haskell.org//ghc/ghc/issues/11613)</th>
  <td>Add microlens to testsuite</td></tr>
  <tr><th>[\#11735](https://gitlab.haskell.org//ghc/ghc/issues/11735)</th>
  <td>Optimize coercionKind</td></tr>
  <tr><th>[\#11739](https://gitlab.haskell.org//ghc/ghc/issues/11739)</th>
  <td>Simplify axioms; should be applied to types</td></tr>
  <tr><th>[\#11749](https://gitlab.haskell.org//ghc/ghc/issues/11749)</th>
  <td>Add long forms for multi-character short-form flags and possibly deprecate short forms</td></tr>
  <tr><th>[\#11778](https://gitlab.haskell.org//ghc/ghc/issues/11778)</th>
  <td>Preserve demandInfo on lambda binders in the simpifier</td></tr>
  <tr><th>[\#11958](https://gitlab.haskell.org//ghc/ghc/issues/11958)</th>
  <td>Improved testing of cross-compiler</td></tr>
  <tr><th>[\#12085](https://gitlab.haskell.org//ghc/ghc/issues/12085)</th>
  <td>Premature defaulting and variable not in scope</td></tr>
  <tr><th>[\#12090](https://gitlab.haskell.org//ghc/ghc/issues/12090)</th>
  <td>Document Weverything/Wall/Wextra/Wdefault in user's guide</td></tr>
  <tr><th>[\#12215](https://gitlab.haskell.org//ghc/ghc/issues/12215)</th>
  <td>Debugging herald should be printed before forcing SDoc</td></tr>
  <tr><th>[\#12218](https://gitlab.haskell.org//ghc/ghc/issues/12218)</th>
  <td>Implement -fexternal-interpreter via static linking</td></tr>
  <tr><th>[\#12364](https://gitlab.haskell.org//ghc/ghc/issues/12364)</th>
  <td>Demand analysis for sum types</td></tr>
  <tr><th>[\#12461](https://gitlab.haskell.org//ghc/ghc/issues/12461)</th>
  <td>Document -O3</td></tr>
  <tr><th>[\#12486](https://gitlab.haskell.org//ghc/ghc/issues/12486)</th>
  <td>Investigate removing libGCC symbols from RtsSymbols.c</td></tr>
  <tr><th>[\#12556](https://gitlab.haskell.org//ghc/ghc/issues/12556)</th>
  <td>\`stimes\` adds extra power to Semigroup</td></tr>
  <tr><th>[\#12619](https://gitlab.haskell.org//ghc/ghc/issues/12619)</th>
  <td>Allow users guide to be built independently from GHC</td></tr>
  <tr><th>[\#12635](https://gitlab.haskell.org//ghc/ghc/issues/12635)</th>
  <td>Compress core in interface files</td></tr>
  <tr><th>[\#12647](https://gitlab.haskell.org//ghc/ghc/issues/12647)</th>
  <td>Can't capture improvement of functional dependencies</td></tr>
  <tr><th>[\#12650](https://gitlab.haskell.org//ghc/ghc/issues/12650)</th>
  <td>Too many warnings about consistentCafInfo</td></tr>
  <tr><th>[\#12662](https://gitlab.haskell.org//ghc/ghc/issues/12662)</th>
  <td>Investigate ListSetOps module</td></tr>
  <tr><th>[\#12669](https://gitlab.haskell.org//ghc/ghc/issues/12669)</th>
  <td>Add some weird Kmettian tests to the test suite</td></tr>
  <tr><th>[\#12687](https://gitlab.haskell.org//ghc/ghc/issues/12687)</th>
  <td>Add a flag to control constraint solving trace</td></tr>
  <tr><th>[\#12720](https://gitlab.haskell.org//ghc/ghc/issues/12720)</th>
  <td>Remove ghcii.sh</td></tr>
  <tr><th>[\#12735](https://gitlab.haskell.org//ghc/ghc/issues/12735)</th>
  <td>Evaluate the feasibility of using lld for linking</td></tr>
  <tr><th>[\#12744](https://gitlab.haskell.org//ghc/ghc/issues/12744)</th>
  <td>Register Allocator Unit Tests</td></tr>
  <tr><th>[\#12758](https://gitlab.haskell.org//ghc/ghc/issues/12758)</th>
  <td>Bring sanity to our performance testsuite</td></tr>
  <tr><th>[\#12765](https://gitlab.haskell.org//ghc/ghc/issues/12765)</th>
  <td>Don't optimize coercions with -O0</td></tr>
  <tr><th>[\#12774](https://gitlab.haskell.org//ghc/ghc/issues/12774)</th>
  <td>bkpcabal02 test fails on OS X</td></tr>
  <tr><th>[\#12812](https://gitlab.haskell.org//ghc/ghc/issues/12812)</th>
  <td>Debugging functions should throw warnings</td></tr>
  <tr><th>[\#12822](https://gitlab.haskell.org//ghc/ghc/issues/12822)</th>
  <td>Cleanup GHC verbosity flags</td></tr>
  <tr><th>[\#12887](https://gitlab.haskell.org//ghc/ghc/issues/12887)</th>
  <td>Make each RTS header self-contained</td></tr>
  <tr><th>[\#12891](https://gitlab.haskell.org//ghc/ghc/issues/12891)</th>
  <td>Automate symbols inclusion in RtsSymbols.c from Rts.h</td></tr>
  <tr><th>[\#12892](https://gitlab.haskell.org//ghc/ghc/issues/12892)</th>
  <td>Improve type safety in the RTS</td></tr>
  <tr><th>[\#12913](https://gitlab.haskell.org//ghc/ghc/issues/12913)</th>
  <td>Port SplitSections to Windows</td></tr>
  <tr><th>[\#12941](https://gitlab.haskell.org//ghc/ghc/issues/12941)</th>
  <td>Extend nofib with benchmarks focused on compiler performance</td></tr>
  <tr><th>[\#12943](https://gitlab.haskell.org//ghc/ghc/issues/12943)</th>
  <td>Adjust AST to capture additional parens around a context</td></tr>
  <tr><th>[\#12948](https://gitlab.haskell.org//ghc/ghc/issues/12948)</th>
  <td>Implement unwind rules for non-Intel architectures</td></tr>
  <tr><th>[\#12961](https://gitlab.haskell.org//ghc/ghc/issues/12961)</th>
  <td>Duplicate exported functions?</td></tr>
  <tr><th>[\#12995](https://gitlab.haskell.org//ghc/ghc/issues/12995)</th>
  <td>interpetBCO doesn't get stripped from executables</td></tr>
  <tr><th>[\#13009](https://gitlab.haskell.org//ghc/ghc/issues/13009)</th>
  <td>Hierarchical Module Structure for GHC</td></tr>
  <tr><th>[\#13015](https://gitlab.haskell.org//ghc/ghc/issues/13015)</th>
  <td>Remove as much Haskell code as we can from Lexer.x</td></tr>
  <tr><th>[\#13020](https://gitlab.haskell.org//ghc/ghc/issues/13020)</th>
  <td>Use a fixed SDK for Mac OS X builds</td></tr>
  <tr><th>[\#13044](https://gitlab.haskell.org//ghc/ghc/issues/13044)</th>
  <td>make it possible to apply GHC rewrite rules to class methods</td></tr>
  <tr><th>[\#13072](https://gitlab.haskell.org//ghc/ghc/issues/13072)</th>
  <td>Move large tuples to a separate module in base</td></tr>
  <tr><th>[\#13101](https://gitlab.haskell.org//ghc/ghc/issues/13101)</th>
  <td>Enable GHC to be loaded into GHCi</td></tr>
  <tr><th>[\#13122](https://gitlab.haskell.org//ghc/ghc/issues/13122)</th>
  <td>Investigate reporting build errors with harbormaster.sendmessage</td></tr>
  <tr><th>[\#13128](https://gitlab.haskell.org//ghc/ghc/issues/13128)</th>
  <td>Refactor AvailInfo to be more sensible</td></tr>
  <tr><th>[\#13134](https://gitlab.haskell.org//ghc/ghc/issues/13134)</th>
  <td>Simplifier ticks exhausted</td></tr>
  <tr><th>[\#13138](https://gitlab.haskell.org//ghc/ghc/issues/13138)</th>
  <td>Clean up ghc-pkg validation warnings in build log</td></tr>
  <tr><th>[\#13149](https://gitlab.haskell.org//ghc/ghc/issues/13149)</th>
  <td>Giving Backpack a Promotion</td></tr>
  <tr><th>[\#13151](https://gitlab.haskell.org//ghc/ghc/issues/13151)</th>
  <td>Make all never-exported IfaceDecls implicit</td></tr>
  <tr><th>[\#13179](https://gitlab.haskell.org//ghc/ghc/issues/13179)</th>
  <td>Levity-polymorphic data types</td></tr>
  <tr><th>[\#13182](https://gitlab.haskell.org//ghc/ghc/issues/13182)</th>
  <td>Rethinking dataToTag\#</td></tr>
  <tr><th>[\#13213](https://gitlab.haskell.org//ghc/ghc/issues/13213)</th>
  <td>Lifting thunks out of thunks to reduce their size.</td></tr>
  <tr><th>[\#13217](https://gitlab.haskell.org//ghc/ghc/issues/13217)</th>
  <td>configure script uses different CFLAGS from the actual build</td></tr>
  <tr><th>[\#13238](https://gitlab.haskell.org//ghc/ghc/issues/13238)</th>
  <td>Harmonise pretty printing of parens in hsSyn</td></tr>
  <tr><th>[\#13252](https://gitlab.haskell.org//ghc/ghc/issues/13252)</th>
  <td>Perf: Make dep_finsts a map from type family Name to Module</td></tr>
  <tr><th>[\#13261](https://gitlab.haskell.org//ghc/ghc/issues/13261)</th>
  <td>Consider moving Typeable evidence generation wholly back to solver</td></tr>
  <tr><th>[\#13270](https://gitlab.haskell.org//ghc/ghc/issues/13270)</th>
  <td>Make Core Lint faster</td></tr>
  <tr><th>[\#13280](https://gitlab.haskell.org//ghc/ghc/issues/13280)</th>
  <td>Consider deriving more Foldable methods</td></tr>
  <tr><th>[\#13309](https://gitlab.haskell.org//ghc/ghc/issues/13309)</th>
  <td>Use liftA2 in ApplicativeDo</td></tr>
  <tr><th>[\#13336](https://gitlab.haskell.org//ghc/ghc/issues/13336)</th>
  <td>Improve or remove the glomming warning</td></tr>
  <tr><th>[\#13346](https://gitlab.haskell.org//ghc/ghc/issues/13346)</th>
  <td>Run nofib with -fspec-constr-keen</td></tr>
  <tr><th>[\#13351](https://gitlab.haskell.org//ghc/ghc/issues/13351)</th>
  <td>Investigate a foldr rule for short static lists</td></tr>
  <tr><th>[\#13357](https://gitlab.haskell.org//ghc/ghc/issues/13357)</th>
  <td>Check demand signatures for catchRetry\# and catchSTM\#</td></tr>
  <tr><th>[\#13374](https://gitlab.haskell.org//ghc/ghc/issues/13374)</th>
  <td>Let exprSize ignore case analysis</td></tr>
  <tr><th>[\#13412](https://gitlab.haskell.org//ghc/ghc/issues/13412)</th>
  <td>Centralize the definition for GHC's libdir on Windows</td></tr>
  <tr><th>[\#13448](https://gitlab.haskell.org//ghc/ghc/issues/13448)</th>
  <td>Make HPC use an RTS option to select the tix file</td></tr>
  <tr><th>[\#13460](https://gitlab.haskell.org//ghc/ghc/issues/13460)</th>
  <td>StandardTargets wiki page is out of date</td></tr>
  <tr><th>[\#13521](https://gitlab.haskell.org//ghc/ghc/issues/13521)</th>
  <td>Remove comments about API annotations</td></tr>
  <tr><th>[\#13532](https://gitlab.haskell.org//ghc/ghc/issues/13532)</th>
  <td>Work out better way of interacting with boot library upstreams</td></tr>
  <tr><th>[\#13564](https://gitlab.haskell.org//ghc/ghc/issues/13564)</th>
  <td>Why does memory usage increase so much during CoreTidy?</td></tr>
  <tr><th>[\#13565](https://gitlab.haskell.org//ghc/ghc/issues/13565)</th>
  <td>Compiler allocations on sched in nofib regressed by 10% between 091333313 and 1883afb2</td></tr>
  <tr><th>[\#13569](https://gitlab.haskell.org//ghc/ghc/issues/13569)</th>
  <td>Optimise code-gen for field-updates of large products</td></tr>
  <tr><th>[\#13593](https://gitlab.haskell.org//ghc/ghc/issues/13593)</th>
  <td>Unexpected behavior from Data.List.groupBy</td></tr>
  <tr><th>[\#13596](https://gitlab.haskell.org//ghc/ghc/issues/13596)</th>
  <td>Try disabling inlining of DynFlags combinators</td></tr>
  <tr><th>[\#13650](https://gitlab.haskell.org//ghc/ghc/issues/13650)</th>
  <td>Implement KPush in types</td></tr>
  <tr><th>[\#13681](https://gitlab.haskell.org//ghc/ghc/issues/13681)</th>
  <td>Remove deprecated ForceSpecConstr</td></tr>
  <tr><th>[\#13698](https://gitlab.haskell.org//ghc/ghc/issues/13698)</th>
  <td>Add a more complete example for the special SPEC argument to the user guide</td></tr>
  <tr><th>[\#13737](https://gitlab.haskell.org//ghc/ghc/issues/13737)</th>
  <td>Have typechecking produce HsType Typechecked instead of Type</td></tr>
  <tr><th>[\#13745](https://gitlab.haskell.org//ghc/ghc/issues/13745)</th>
  <td>Investigate compile-time regressions in regex-tdfa-1.2.2</td></tr>
  <tr><th>[\#13892](https://gitlab.haskell.org//ghc/ghc/issues/13892)</th>
  <td>Add some benchmarks to nofib from Andras Kovac's Eff benchmarks</td></tr>
  <tr><th>[\#13897](https://gitlab.haskell.org//ghc/ghc/issues/13897)</th>
  <td>Ship check-ppr in bindist and compile during testsuite run</td></tr>
  <tr><th>[\#13898](https://gitlab.haskell.org//ghc/ghc/issues/13898)</th>
  <td>Consolidate treatment of strictness in parser</td></tr>
  <tr><th>[\#13923](https://gitlab.haskell.org//ghc/ghc/issues/13923)</th>
  <td>Add a suppression flag to stop Typeable bindings being emitted with -ddump-simpl</td></tr>
  <tr><th>[\#13939](https://gitlab.haskell.org//ghc/ghc/issues/13939)</th>
  <td>Fix and enable SplitSections on 32bit Windows</td></tr>
  <tr><th>[\#14005](https://gitlab.haskell.org//ghc/ghc/issues/14005)</th>
  <td>Explore moving from pretty to prettyprinter</td></tr>
  <tr><th>[\#14023](https://gitlab.haskell.org//ghc/ghc/issues/14023)</th>
  <td>Split up glasgow_exts.rst</td></tr>
  <tr><th>[\#14030](https://gitlab.haskell.org//ghc/ghc/issues/14030)</th>
  <td>Implement the "Derive Lift instances for data types in template-haskell" proposal</td></tr>
  <tr><th>[\#14057](https://gitlab.haskell.org//ghc/ghc/issues/14057)</th>
  <td>Upstream Alpine Linux distribution patches</td></tr>
  <tr><th>[\#14067](https://gitlab.haskell.org//ghc/ghc/issues/14067)</th>
  <td>Static Argument Transformation for tail-recursive functions</td></tr>
  <tr><th>[\#14095](https://gitlab.haskell.org//ghc/ghc/issues/14095)</th>
  <td>Improve parallelism in --make mode</td></tr>
  <tr><th>[\#14099](https://gitlab.haskell.org//ghc/ghc/issues/14099)</th>
  <td>Document fundeps</td></tr>
  <tr><th>[\#14119](https://gitlab.haskell.org//ghc/ghc/issues/14119)</th>
  <td>Refactor type patterns</td></tr>
  <tr><th>[\#14123](https://gitlab.haskell.org//ghc/ghc/issues/14123)</th>
  <td>Figure out invariants surrounding ticks in Core</td></tr>
  <tr><th>[\#14200](https://gitlab.haskell.org//ghc/ghc/issues/14200)</th>
  <td>Deprecate WrappedMonad</td></tr>
  <tr><th>[\#14216](https://gitlab.haskell.org//ghc/ghc/issues/14216)</th>
  <td>Compare compiler performance of GHC 8.0.2 and 8.2.1 on Stackage</td></tr>
  <tr><th>[\#14223](https://gitlab.haskell.org//ghc/ghc/issues/14223)</th>
  <td>Casts get in the way of join points</td></tr>
  <tr><th>[\#14233](https://gitlab.haskell.org//ghc/ghc/issues/14233)</th>
  <td>Remove old GetTickCount() code path for Windows</td></tr>
  <tr><th>[\#14234](https://gitlab.haskell.org//ghc/ghc/issues/14234)</th>
  <td>Enable -Wconversion for GHC's C code</td></tr>
  <tr><th>[\#14283](https://gitlab.haskell.org//ghc/ghc/issues/14283)</th>
  <td>Remove the special case for tagToEnum\# in the code generator?</td></tr>
  <tr><th>[\#14309](https://gitlab.haskell.org//ghc/ghc/issues/14309)</th>
  <td>Expand comment in hPutStrLn</td></tr>
  <tr><th>[\#14314](https://gitlab.haskell.org//ghc/ghc/issues/14314)</th>
  <td>Consider changing CC detection strategy</td></tr>
  <tr><th>[\#14340](https://gitlab.haskell.org//ghc/ghc/issues/14340)</th>
  <td>Rename AND typecheck types before values</td></tr>
  <tr><th>[\#14370](https://gitlab.haskell.org//ghc/ghc/issues/14370)</th>
  <td>improve documentation of -fdefer-typed-holes for naked expressions in ghci</td></tr>
  <tr><th>[\#14376](https://gitlab.haskell.org//ghc/ghc/issues/14376)</th>
  <td>Testsuite contains "ddump-cps-cmm"</td></tr>
  <tr><th>[\#14401](https://gitlab.haskell.org//ghc/ghc/issues/14401)</th>
  <td>Add a test ensuring that TypeReps can be stored in compact regions</td></tr>
  <tr><th>[\#14405](https://gitlab.haskell.org//ghc/ghc/issues/14405)</th>
  <td>Remove core-spec.pdf from repository</td></tr>
  <tr><th>[\#14409](https://gitlab.haskell.org//ghc/ghc/issues/14409)</th>
  <td>Split GHC(-API) into multiple packages</td></tr>
  <tr><th>[\#14416](https://gitlab.haskell.org//ghc/ghc/issues/14416)</th>
  <td>CI with CircleCI</td></tr>
  <tr><th>[\#14428](https://gitlab.haskell.org//ghc/ghc/issues/14428)</th>
  <td>Rework HsValBindsLR</td></tr>
  <tr><th>[\#14429](https://gitlab.haskell.org//ghc/ghc/issues/14429)</th>
  <td>Remove constraint types from HsExtension, post TTG</td></tr>
  <tr><th>[\#14461](https://gitlab.haskell.org//ghc/ghc/issues/14461)</th>
  <td>Reuse free variable lists through nested closures</td></tr>
  <tr><th>[\#14475](https://gitlab.haskell.org//ghc/ghc/issues/14475)</th>
  <td>Upload documentation dumps</td></tr>
  <tr><th>[\#14480](https://gitlab.haskell.org//ghc/ghc/issues/14480)</th>
  <td>Clean up tyConTYPE</td></tr>
  <tr><th>[\#14502](https://gitlab.haskell.org//ghc/ghc/issues/14502)</th>
  <td>Build Alpine Linux binary distributions</td></tr>
  <tr><th>[\#14508](https://gitlab.haskell.org//ghc/ghc/issues/14508)</th>
  <td>Bring up Appveyor for Windows CI</td></tr>
  <tr><th>[\#14509](https://gitlab.haskell.org//ghc/ghc/issues/14509)</th>
  <td>Consider adding new stg_ap_\* functions</td></tr>
  <tr><th>[\#14550](https://gitlab.haskell.org//ghc/ghc/issues/14550)</th>
  <td>Report the provenance of TyThings</td></tr>
  <tr><th>[\#14558](https://gitlab.haskell.org//ghc/ghc/issues/14558)</th>
  <td>Unable to parse integer-gmp's Cabal file</td></tr>
  <tr><th>[\#14583](https://gitlab.haskell.org//ghc/ghc/issues/14583)</th>
  <td>Don’t use \`String\` when generating code</td></tr>
  <tr><th>[\#14585](https://gitlab.haskell.org//ghc/ghc/issues/14585)</th>
  <td>Consider doing a CPS→SSA conversion in the backend</td></tr>
  <tr><th>[\#14596](https://gitlab.haskell.org//ghc/ghc/issues/14596)</th>
  <td>Remove uses of unsafeGlobalDynFlags for state hack</td></tr>
  <tr><th>[\#14597](https://gitlab.haskell.org//ghc/ghc/issues/14597)</th>
  <td>Consider removing uses of unsafeGlobalDynFlags for -dppr-debug</td></tr>
  <tr><th>[\#14602](https://gitlab.haskell.org//ghc/ghc/issues/14602)</th>
  <td>Implement the pattern synonym construction function signatures proposal</td></tr>
  <tr><th>[\#14672](https://gitlab.haskell.org//ghc/ghc/issues/14672)</th>
  <td>Make likelyhood of branches/conditions available throughout the compiler.</td></tr>
  <tr><th>[\#14687](https://gitlab.haskell.org//ghc/ghc/issues/14687)</th>
  <td>Investigate differences in Int-In/Outlining</td></tr>
  <tr><th>[\#14701](https://gitlab.haskell.org//ghc/ghc/issues/14701)</th>
  <td>Investigate the performance impact of code alignment</td></tr>
  <tr><th>[\#14726](https://gitlab.haskell.org//ghc/ghc/issues/14726)</th>
  <td>Add AnnTypeAt to distinguish between '@' for type application</td></tr>
  <tr><th>[\#14731](https://gitlab.haskell.org//ghc/ghc/issues/14731)</th>
  <td>Document alignment & underlying size invariants for array types in GHC.Prim</td></tr>
  <tr><th>[\#14791](https://gitlab.haskell.org//ghc/ghc/issues/14791)</th>
  <td>Move stack checks out of code paths that don't use the stack.</td></tr>
  <tr><th>[\#14830](https://gitlab.haskell.org//ghc/ghc/issues/14830)</th>
  <td>Use test instead of cmp for comparison against zero.</td></tr>
  <tr><th>[\#14844](https://gitlab.haskell.org//ghc/ghc/issues/14844)</th>
  <td>SpecConstr also non-recursive function</td></tr>
  <tr><th>[\#14852](https://gitlab.haskell.org//ghc/ghc/issues/14852)</th>
  <td>Implement the "Eq1, Ord1, Read1, Show1 instances in GHC.Generics" proposal</td></tr>
  <tr><th>[\#14853](https://gitlab.haskell.org//ghc/ghc/issues/14853)</th>
  <td>Implement the "Semigroup and Monoid instances in Data.Functor.Product and friends" proposal</td></tr>
  <tr><th>[\#14909](https://gitlab.haskell.org//ghc/ghc/issues/14909)</th>
  <td>Change default armhf target to a newer architecture</td></tr>
  <tr><th>[\#14914](https://gitlab.haskell.org//ghc/ghc/issues/14914)</th>
  <td>Only turn suitable targets into a fallthrough in CmmContFlowOpt.</td></tr>
  <tr><th>[\#14951](https://gitlab.haskell.org//ghc/ghc/issues/14951)</th>
  <td>SpecConstr needs two runs when one should suffice</td></tr>
  <tr><th>[\#14964](https://gitlab.haskell.org//ghc/ghc/issues/14964)</th>
  <td>performance regressions from 8.0.2 to 8.4.1</td></tr>
  <tr><th>[\#14971](https://gitlab.haskell.org//ghc/ghc/issues/14971)</th>
  <td>Use appropriatly sized comparison instruction for small values.</td></tr>
  <tr><th>[\#14983](https://gitlab.haskell.org//ghc/ghc/issues/14983)</th>
  <td>Have custom type errors imply Void</td></tr>
  <tr><th>[\#15011](https://gitlab.haskell.org//ghc/ghc/issues/15011)</th>
  <td>Automate update of VersionHistory table</td></tr>
  <tr><th>[\#15028](https://gitlab.haskell.org//ghc/ghc/issues/15028)</th>
  <td>Deprecate and Remove Data.Semigroup.Option and Data.Monoid.First/Last</td></tr>
  <tr><th>[\#15048](https://gitlab.haskell.org//ghc/ghc/issues/15048)</th>
  <td>Fix mirroring of terminfo and haskeline</td></tr>
  <tr><th>[\#15072](https://gitlab.haskell.org//ghc/ghc/issues/15072)</th>
  <td>Teach the testsuite driver about response files</td></tr>
  <tr><th>[\#15089](https://gitlab.haskell.org//ghc/ghc/issues/15089)</th>
  <td>Compiling stage2 with -g causes spurious test suite failures due to stderr mismatches</td></tr>
  <tr><th>[\#15090](https://gitlab.haskell.org//ghc/ghc/issues/15090)</th>
  <td>Do more coercion optimisation on the fly</td></tr>
  <tr><th>[\#15091](https://gitlab.haskell.org//ghc/ghc/issues/15091)</th>
  <td>Better occurrence analysis for join points</td></tr>
  <tr><th>[\#15106](https://gitlab.haskell.org//ghc/ghc/issues/15106)</th>
  <td>Explore using pure unifier instead of monadic one</td></tr>
  <tr><th>[\#15117](https://gitlab.haskell.org//ghc/ghc/issues/15117)</th>
  <td>Add linting checks for DWARF unwind information</td></tr>
  <tr><th>[\#15124](https://gitlab.haskell.org//ghc/ghc/issues/15124)</th>
  <td>Improve block layout for the NCG</td></tr>
  <tr><th>[\#15126](https://gitlab.haskell.org//ghc/ghc/issues/15126)</th>
  <td>Opportunity to compress common info table representation.</td></tr>
  <tr><th>[\#15137](https://gitlab.haskell.org//ghc/ghc/issues/15137)</th>
  <td>Install the "primitive" package on CI before running tests</td></tr>
  <tr><th>[\#15148](https://gitlab.haskell.org//ghc/ghc/issues/15148)</th>
  <td>Allow setting of custom alignments</td></tr>
  <tr><th>[\#15192](https://gitlab.haskell.org//ghc/ghc/issues/15192)</th>
  <td>Refactor of Coercion</td></tr>
  <tr><th>[\#15210](https://gitlab.haskell.org//ghc/ghc/issues/15210)</th>
  <td>Use ByteStrings for strings that don't use FastString's features</td></tr>
  <tr><th>[\#15219](https://gitlab.haskell.org//ghc/ghc/issues/15219)</th>
  <td>Implement UnliftedNewtypes proposal</td></tr>
  <tr><th>[\#15247](https://gitlab.haskell.org//ghc/ghc/issues/15247)</th>
  <td>Use empty types for TTG extension constructors</td></tr>
  <tr><th>[\#15249](https://gitlab.haskell.org//ghc/ghc/issues/15249)</th>
  <td>Add support for cmpeq and cmpgt MMX intrinsics</td></tr>
  <tr><th>[\#15258](https://gitlab.haskell.org//ghc/ghc/issues/15258)</th>
  <td>Implement CMOV support.</td></tr>
  <tr><th>[\#15277](https://gitlab.haskell.org//ghc/ghc/issues/15277)</th>
  <td>Move field name resolution to the type-checker</td></tr>
  <tr><th>[\#15283](https://gitlab.haskell.org//ghc/ghc/issues/15283)</th>
  <td>Locale issue in the testsuite</td></tr>
  <tr><th>[\#15340](https://gitlab.haskell.org//ghc/ghc/issues/15340)</th>
  <td>Investigate using Ward on RTS</td></tr>
  <tr><th>[\#15345](https://gitlab.haskell.org//ghc/ghc/issues/15345)</th>
  <td>Duplication between \`CoreSubst\` and \`SimplEnv\` is very unfortunate</td></tr>
  <tr><th>[\#15378](https://gitlab.haskell.org//ghc/ghc/issues/15378)</th>
  <td>Use TcLevels to decide about floating out of implications</td></tr>
  <tr><th>[\#15381](https://gitlab.haskell.org//ghc/ghc/issues/15381)</th>
  <td>Write documentation for iserv-proxy</td></tr>
  <tr><th>[\#15384](https://gitlab.haskell.org//ghc/ghc/issues/15384)</th>
  <td>Every implication should bump the TcLevel exactly once</td></tr>
  <tr><th>[\#15414](https://gitlab.haskell.org//ghc/ghc/issues/15414)</th>
  <td>Why both gen and gen_no in bdescr?</td></tr>
  <tr><th>[\#15429](https://gitlab.haskell.org//ghc/ghc/issues/15429)</th>
  <td>The docs for Unsafe.Coerce should recommend using Data.Coerce instead</td></tr>
  <tr><th>[\#15479](https://gitlab.haskell.org//ghc/ghc/issues/15479)</th>
  <td>Return HsType GhcTc from tc_hs_type</td></tr>
  <tr><th>[\#15495](https://gitlab.haskell.org//ghc/ghc/issues/15495)</th>
  <td>Handling Source Locations via TTG</td></tr>
  <tr><th>[\#15506](https://gitlab.haskell.org//ghc/ghc/issues/15506)</th>
  <td>CI setup should build and run nofib</td></tr>
  <tr><th>[\#15573](https://gitlab.haskell.org//ghc/ghc/issues/15573)</th>
  <td>Make bindings with multiple occurrences a join point instead of duplicating code during inlining.</td></tr>
  <tr><th>[\#15575](https://gitlab.haskell.org//ghc/ghc/issues/15575)</th>
  <td>Investigate Haskell rewrite of testsuite driver</td></tr>
  <tr><th>[\#15580](https://gitlab.haskell.org//ghc/ghc/issues/15580)</th>
  <td>Specialize min/max functions for GHC provided instances.</td></tr>
  <tr><th>[\#15614](https://gitlab.haskell.org//ghc/ghc/issues/15614)</th>
  <td>Test suite should show reason for skipping a test</td></tr>
  <tr><th>[\#15651](https://gitlab.haskell.org//ghc/ghc/issues/15651)</th>
  <td>Check if some auto apply code is dead and remove if appropriate.</td></tr>
  <tr><th>[\#15654](https://gitlab.haskell.org//ghc/ghc/issues/15654)</th>
  <td>Use deriving Functor in the codebase</td></tr>
  <tr><th>[\#15656](https://gitlab.haskell.org//ghc/ghc/issues/15656)</th>
  <td>Extend -Wall with incomplete-uni-patterns and incomplete-record-updates</td></tr>
  <tr><th>[\#15698](https://gitlab.haskell.org//ghc/ghc/issues/15698)</th>
  <td>SingleEntry update flag for Stg bindings is not used</td></tr>
  <tr><th>[\#15706](https://gitlab.haskell.org//ghc/ghc/issues/15706)</th>
  <td>Refactor NewHsTypeX to DerivedCoreTy</td></tr>
  <tr><th>[\#15733](https://gitlab.haskell.org//ghc/ghc/issues/15733)</th>
  <td>Several links in GHC.Exts.Heap documentation are broken</td></tr>
  <tr><th>[\#15752](https://gitlab.haskell.org//ghc/ghc/issues/15752)</th>
  <td>Renaming \`Pat\` in \`HsSyn\` to \`HsPat\` for Consistency</td></tr>
  <tr><th>[\#15756](https://gitlab.haskell.org//ghc/ghc/issues/15756)</th>
  <td>The "-freduction-depth=N" GHC flag is not documented</td></tr>
  <tr><th>[\#15771](https://gitlab.haskell.org//ghc/ghc/issues/15771)</th>
  <td>Could there be further refactoring to the \`seq\` typing rule</td></tr>
  <tr><th>[\#15773](https://gitlab.haskell.org//ghc/ghc/issues/15773)</th>
  <td>User Guide contains confusing information about \`-rtsopts\` modes.</td></tr>
  <tr><th>[\#15779](https://gitlab.haskell.org//ghc/ghc/issues/15779)</th>
  <td>Follow-ups to D5169</td></tr>
  <tr><th>[\#15821](https://gitlab.haskell.org//ghc/ghc/issues/15821)</th>
  <td>Implement more constant folding for Naturals</td></tr>
  <tr><th>[\#15880](https://gitlab.haskell.org//ghc/ghc/issues/15880)</th>
  <td>GHC.Stats: Add info on amount of pinned memory</td></tr>
  <tr><th>[\#15885](https://gitlab.haskell.org//ghc/ghc/issues/15885)</th>
  <td>Enhancing COMPLETE pragma to support pattern synonyms with polymorphic (output) types</td></tr>
  <tr><th>[\#15896](https://gitlab.haskell.org//ghc/ghc/issues/15896)</th>
  <td>GHC API: add function to allow looking up Name for Located RdrName</td></tr>
  <tr><th>[\#15901](https://gitlab.haskell.org//ghc/ghc/issues/15901)</th>
  <td>Assert and record that code generation requires distinct uiques for let-binders</td></tr>
  <tr><th>[\#15919](https://gitlab.haskell.org//ghc/ghc/issues/15919)</th>
  <td>Deprecate split objects</td></tr>
  <tr><th>[\#15929](https://gitlab.haskell.org//ghc/ghc/issues/15929)</th>
  <td>Explore whether adding XRay attributes to LLVM IR is worthwhile</td></tr>
  <tr><th>[\#15977](https://gitlab.haskell.org//ghc/ghc/issues/15977)</th>
  <td>Restructure typechecking modules</td></tr>
  <tr><th>[\#15981](https://gitlab.haskell.org//ghc/ghc/issues/15981)</th>
  <td>Implement the Linear Types proposal</td></tr>
  <tr><th>[\#15988](https://gitlab.haskell.org//ghc/ghc/issues/15988)</th>
  <td>Remove Text.Printf and System.Console.GetOpt from base</td></tr>
  <tr><th>[\#16007](https://gitlab.haskell.org//ghc/ghc/issues/16007)</th>
  <td>Implement \`-Os\`</td></tr>
  <tr><th>[\#16014](https://gitlab.haskell.org//ghc/ghc/issues/16014)</th>
  <td>Avoid unnecessary code duplication from String Literals.</td></tr>
  <tr><th>[\#16041](https://gitlab.haskell.org//ghc/ghc/issues/16041)</th>
  <td>Address warnings reported by -Wincomplete-uni-patterns and -Wincomplete-record-updates</td></tr>
  <tr><th>[\#16044](https://gitlab.haskell.org//ghc/ghc/issues/16044)</th>
  <td>Transition to C11 memory model</td></tr>
  <tr><th>[\#16052](https://gitlab.haskell.org//ghc/ghc/issues/16052)</th>
  <td>Core optimizations for memset on a small range</td></tr>
  <tr><th>[\#16055](https://gitlab.haskell.org//ghc/ghc/issues/16055)</th>
  <td>libffi build logic is quite spread out</td></tr>
  <tr><th>[\#16062](https://gitlab.haskell.org//ghc/ghc/issues/16062)</th>
  <td>Improve -dynamic-too progress messages</td></tr>
  <tr><th>[\#16064](https://gitlab.haskell.org//ghc/ghc/issues/16064)</th>
  <td>Improving Placement of Heap Checks - Avoiding Slowdowns in Hot Code</td></tr>
  <tr><th>[\#16090](https://gitlab.haskell.org//ghc/ghc/issues/16090)</th>
  <td>Typeset Big-O complexities with TeX-style notation in libraries/base</td></tr>
  <tr><th>[\#16098](https://gitlab.haskell.org//ghc/ghc/issues/16098)</th>
  <td>More efficient implementation plan for primops with continuation arguments</td></tr>
  <tr><th>[\#16126](https://gitlab.haskell.org//ghc/ghc/issues/16126)</th>
  <td>Make -threaded the default</td></tr>
  <tr><th>[\#16165](https://gitlab.haskell.org//ghc/ghc/issues/16165)</th>
  <td>Move Hadrian (github) wiki information to in-tree docs</td></tr>
  <tr><th>[\#16215](https://gitlab.haskell.org//ghc/ghc/issues/16215)</th>
  <td>Refactor getProgramContext in Hadrian</td></tr>
  <tr><th>[\#16217](https://gitlab.haskell.org//ghc/ghc/issues/16217)</th>
  <td>check-api-annotations should check that an annotation does not precede its span</td></tr>
  <tr><th>[\#16243](https://gitlab.haskell.org//ghc/ghc/issues/16243)</th>
  <td>Improve fregs-graph.</td></tr>
  <tr><th>[\#16253](https://gitlab.haskell.org//ghc/ghc/issues/16253)</th>
  <td>Offer a shorthand for \`--skip=_build/stage$n/compiler/.dependencies.mk\`</td></tr>
  <tr><th>[\#16277](https://gitlab.haskell.org//ghc/ghc/issues/16277)</th>
  <td>Make JUnit report stdout/stderr in more cases</td></tr>
  <tr><th>[\#16280](https://gitlab.haskell.org//ghc/ghc/issues/16280)</th>
  <td>Update Latest page of User's Guide to GHC 8.6.3</td></tr>
  <tr><th>[\#16284](https://gitlab.haskell.org//ghc/ghc/issues/16284)</th>
  <td>Abortion of fixed-point iteration in Demand Analyser discards sound results</td></tr>
  <tr><th>[\#16304](https://gitlab.haskell.org//ghc/ghc/issues/16304)</th>
  <td>Hadrian: refactor libffi and rts rules.</td></tr>
  <tr><th>[\#16311](https://gitlab.haskell.org//ghc/ghc/issues/16311)</th>
  <td>Suggest -XExistentialQuantification for 'forall' in data declarations</td></tr>
  <tr><th>[\#16327](https://gitlab.haskell.org//ghc/ghc/issues/16327)</th>
  <td>NCG: Don't double adjust SP before calls on Windows.</td></tr>
  <tr><th>[\#16333](https://gitlab.haskell.org//ghc/ghc/issues/16333)</th>
  <td>Implement Loop-invariant code motion / Hoisting for Cmm</td></tr>
  <tr><th>[\#16335](https://gitlab.haskell.org//ghc/ghc/issues/16335)</th>
  <td>Make CPR Analysis more aggressive for inductive cases</td></tr>
  <tr><th>[\#16337](https://gitlab.haskell.org//ghc/ghc/issues/16337)</th>
  <td>Investigate using Gitlab "Review Apps" to review documentation</td></tr>
  <tr><th>[\#16338](https://gitlab.haskell.org//ghc/ghc/issues/16338)</th>
  <td>Update CI images to 8.6.\* and cabal-2.4.\*</td></tr>
  <tr><th>[\#16340](https://gitlab.haskell.org//ghc/ghc/issues/16340)</th>
  <td>Improve properFraction for Ratio</td></tr>
  <tr><th>[\#16355](https://gitlab.haskell.org//ghc/ghc/issues/16355)</th>
  <td>Save CI performance metrics on windows jobs</td></tr>
  <tr><th>[\#16369](https://gitlab.haskell.org//ghc/ghc/issues/16369)</th>
  <td>Implement testing of stage1 in ./validate --hadrian</td></tr>
  <tr><th>[\#16393](https://gitlab.haskell.org//ghc/ghc/issues/16393)</th>
  <td>Implement the "Explicit specificity in type variable binders" proposal</td></tr></table>

   tasks) 
  The May 2015 Status Report. 
  [ Harbormaster](https://phabricator.haskell.org/diffusion/GHC/history/), the [ nightly build bots](http://haskell.inf.elte.hu/builders/) and Travis [](https://travis-ci.org/ghc/ghc.svg) are keeping an eye on our code. See the [Status page](status) for more information. 
  NoticesVolunteer your machine as a [GHC builder](builder). This will help us find and fix issues with GHC on your platform. 
  [Internships](internships) at Microsoft Research, Cambridge, working on Haskell and GHC. 
  Developer Documentation[Building and Porting GHC](building)[Working on GHC ](working-conventions)[The GHC Commentary](commentary)
  Documentation on the design, architecture, and implementation of GHC itself, with references to the actual source code. 
  [Debugging GHC](debugging)
  All aspects of debugging, testing, and benchmarking GHC. 
  Download in other formats:[Plain Text](/trac/ghc/wiki/WikiStart?version=6&format=txt)[](http://trac.edgewall.org/)Powered by [Trac 1.2.2](/trac/ghc/about)

          By [Edgewall Software](http://www.edgewall.org/).Visit the Trac open source project at
  [http://trac.edgewall.org/](http://trac.edgewall.org/)