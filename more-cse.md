# Notes on common sub-expression elimination (CSE)

## Tickets


The CSE pass is pretty simple at the moment.  Here are tickets that identify currently-missed opportunities.


Use Keyword = `CSE` to ensure that a ticket ends up on these lists.

**Open Tickets:**

<table><tr><th>[\#149](https://gitlab.haskell.org//ghc/ghc/issues/149)</th>
<td>missed CSE opportunity</td></tr>
<tr><th>[\#701](https://gitlab.haskell.org//ghc/ghc/issues/701)</th>
<td>Better CSE optimisation</td></tr>
<tr><th>[\#947](https://gitlab.haskell.org//ghc/ghc/issues/947)</th>
<td>ghc -O space leak: CSE between different CAFs</td></tr>
<tr><th>[\#1158](https://gitlab.haskell.org//ghc/ghc/issues/1158)</th>
<td>Problem with GADTs and explicit type signatures</td></tr>
<tr><th>[\#2189](https://gitlab.haskell.org//ghc/ghc/issues/2189)</th>
<td>hSetBuffering stdin NoBuffering doesn't work on Windows</td></tr>
<tr><th>[\#2968](https://gitlab.haskell.org//ghc/ghc/issues/2968)</th>
<td>Avoid generating C trigraphs</td></tr>
<tr><th>[\#4052](https://gitlab.haskell.org//ghc/ghc/issues/4052)</th>
<td>Two sided sections</td></tr>
<tr><th>[\#4505](https://gitlab.haskell.org//ghc/ghc/issues/4505)</th>
<td>Segmentation fault on long input (list of pairs)</td></tr>
<tr><th>[\#4959](https://gitlab.haskell.org//ghc/ghc/issues/4959)</th>
<td>Warning about variables with leading underscore that are used anyway</td></tr>
<tr><th>[\#5344](https://gitlab.haskell.org//ghc/ghc/issues/5344)</th>
<td>CSE should look through coercions</td></tr>
<tr><th>[\#5928](https://gitlab.haskell.org//ghc/ghc/issues/5928)</th>
<td>INLINABLE fails to specialize in presence of simple wrapper</td></tr>
<tr><th>[\#7080](https://gitlab.haskell.org//ghc/ghc/issues/7080)</th>
<td>Make RULES and SPECIALISE more consistent</td></tr>
<tr><th>[\#7411](https://gitlab.haskell.org//ghc/ghc/issues/7411)</th>
<td>Exceptions are optimized away in certain situations</td></tr>
<tr><th>[\#7596](https://gitlab.haskell.org//ghc/ghc/issues/7596)</th>
<td>Opportunity to improve CSE</td></tr>
<tr><th>[\#8015](https://gitlab.haskell.org//ghc/ghc/issues/8015)</th>
<td>Show warning when file-header pragma is used after module keyword</td></tr>
<tr><th>[\#8144](https://gitlab.haskell.org//ghc/ghc/issues/8144)</th>
<td>Interface hashes include time stamp of dependent files (UsageFile mtime)</td></tr>
<tr><th>[\#8927](https://gitlab.haskell.org//ghc/ghc/issues/8927)</th>
<td>Multiple case match at once</td></tr>
<tr><th>[\#9688](https://gitlab.haskell.org//ghc/ghc/issues/9688)</th>
<td>Improve the interaction between CSE and the join point transformation</td></tr>
<tr><th>[\#10732](https://gitlab.haskell.org//ghc/ghc/issues/10732)</th>
<td>Legal Bang Patterns cannot parse</td></tr>
<tr><th>[\#11523](https://gitlab.haskell.org//ghc/ghc/issues/11523)</th>
<td>Infinite Loop when mixing UndecidableSuperClasses and the class/instance constraint synonym trick.</td></tr>
<tr><th>[\#12131](https://gitlab.haskell.org//ghc/ghc/issues/12131)</th>
<td>Can't solve constraints with UndecidableSuperClasses but can infer kind (+ undesired order of kinds)</td></tr>
<tr><th>[\#12436](https://gitlab.haskell.org//ghc/ghc/issues/12436)</th>
<td>Too many nested forkProcess's eventually cause SIGSEGV in the child</td></tr>
<tr><th>[\#12620](https://gitlab.haskell.org//ghc/ghc/issues/12620)</th>
<td>Allow the user to prevent floating and CSE</td></tr>
<tr><th>[\#12657](https://gitlab.haskell.org//ghc/ghc/issues/12657)</th>
<td>GHC and GHCi: RWX mmap denied by GrSec, results in a segfault</td></tr>
<tr><th>[\#13219](https://gitlab.haskell.org//ghc/ghc/issues/13219)</th>
<td>CSE for join points</td></tr>
<tr><th>[\#13303](https://gitlab.haskell.org//ghc/ghc/issues/13303)</th>
<td>Bad pretty printer for let bindings in a do-notation with braces</td></tr>
<tr><th>[\#13582](https://gitlab.haskell.org//ghc/ghc/issues/13582)</th>
<td>Confusing error message with multiparameter type classes.</td></tr>
<tr><th>[\#13584](https://gitlab.haskell.org//ghc/ghc/issues/13584)</th>
<td>ghci parse error on operator info</td></tr>
<tr><th>[\#13589](https://gitlab.haskell.org//ghc/ghc/issues/13589)</th>
<td>Possible inconsistency in CSE's treatment of NOINLINE</td></tr>
<tr><th>[\#13694](https://gitlab.haskell.org//ghc/ghc/issues/13694)</th>
<td>CSE runs before SpecConstr</td></tr>
<tr><th>[\#13851](https://gitlab.haskell.org//ghc/ghc/issues/13851)</th>
<td>Change in specialisation(?) behaviour since 8.0.2 causes 6x slowdown</td></tr>
<tr><th>[\#13873](https://gitlab.haskell.org//ghc/ghc/issues/13873)</th>
<td>Adding a SPECIALIZE at a callsite in Main.hs is causing a regression</td></tr>
<tr><th>[\#14010](https://gitlab.haskell.org//ghc/ghc/issues/14010)</th>
<td>UndecidableSuperClasses - Could not deduce (Category d)</td></tr>
<tr><th>[\#14186](https://gitlab.haskell.org//ghc/ghc/issues/14186)</th>
<td>CSE fails to CSE two identical large top-level functions</td></tr>
<tr><th>[\#14222](https://gitlab.haskell.org//ghc/ghc/issues/14222)</th>
<td>Simple text fusion example results in rather duplicative code</td></tr>
<tr><th>[\#14684](https://gitlab.haskell.org//ghc/ghc/issues/14684)</th>
<td>combineIdenticalAlts is only partially implemented</td></tr>
<tr><th>[\#14839](https://gitlab.haskell.org//ghc/ghc/issues/14839)</th>
<td>Bits typeclass law for LSB</td></tr>
<tr><th>[\#14922](https://gitlab.haskell.org//ghc/ghc/issues/14922)</th>
<td>Add inductively-defined Nat to base</td></tr>
<tr><th>[\#14976](https://gitlab.haskell.org//ghc/ghc/issues/14976)</th>
<td>WebAssembly support</td></tr>
<tr><th>[\#14980](https://gitlab.haskell.org//ghc/ghc/issues/14980)</th>
<td>Runtime performance regression with binary operations on vectors</td></tr>
<tr><th>[\#15028](https://gitlab.haskell.org//ghc/ghc/issues/15028)</th>
<td>Deprecate and Remove Data.Semigroup.Option and Data.Monoid.First/Last</td></tr>
<tr><th>[\#15044](https://gitlab.haskell.org//ghc/ghc/issues/15044)</th>
<td>Option to output instance resolution process</td></tr>
<tr><th>[\#15253](https://gitlab.haskell.org//ghc/ghc/issues/15253)</th>
<td>Add support for type-level integers</td></tr>
<tr><th>[\#15622](https://gitlab.haskell.org//ghc/ghc/issues/15622)</th>
<td>Generalize \`E{0,1,2,3,6,9,12}\` from \`Data.Fixed\`</td></tr>
<tr><th>[\#15773](https://gitlab.haskell.org//ghc/ghc/issues/15773)</th>
<td>User Guide contains confusing information about \`-rtsopts\` modes.</td></tr>
<tr><th>[\#16009](https://gitlab.haskell.org//ghc/ghc/issues/16009)</th>
<td>Deprecate \`optional\` from Text.ParserCombinators.ReadP</td></tr>
<tr><th>[\#16173](https://gitlab.haskell.org//ghc/ghc/issues/16173)</th>
<td>Move \`Data.Profunctor\` from \`profunctors\` package to \`base\`</td></tr></table>

**Closed Tickets:**

<table><tr><th>[\#17](https://gitlab.haskell.org//ghc/ghc/issues/17)</th>
<td>Separate warnings for unused local and top-level bindings</td></tr>
<tr><th>[\#667](https://gitlab.haskell.org//ghc/ghc/issues/667)</th>
<td>Efficient Map \<-\> Set conversions</td></tr>
<tr><th>[\#738](https://gitlab.haskell.org//ghc/ghc/issues/738)</th>
<td>ghc can't load files with selinux Enforcing</td></tr>
<tr><th>[\#907](https://gitlab.haskell.org//ghc/ghc/issues/907)</th>
<td>Case sensitive ghci commands</td></tr>
<tr><th>[\#984](https://gitlab.haskell.org//ghc/ghc/issues/984)</th>
<td>Syntax error shows in the wrong position</td></tr>
<tr><th>[\#1029](https://gitlab.haskell.org//ghc/ghc/issues/1029)</th>
<td>ghc fails to find installed user package even though "ghc-pkg list" shows it</td></tr>
<tr><th>[\#1102](https://gitlab.haskell.org//ghc/ghc/issues/1102)</th>
<td>Lambda unicode character lex</td></tr>
<tr><th>[\#1103](https://gitlab.haskell.org//ghc/ghc/issues/1103)</th>
<td>Japanese Unicode</td></tr>
<tr><th>[\#1148](https://gitlab.haskell.org//ghc/ghc/issues/1148)</th>
<td>Bad warnings about unused imports that are in fact needed</td></tr>
<tr><th>[\#1258](https://gitlab.haskell.org//ghc/ghc/issues/1258)</th>
<td>Insecure temporary file creation</td></tr>
<tr><th>[\#1448](https://gitlab.haskell.org//ghc/ghc/issues/1448)</th>
<td>Segmentation fault with readline Module</td></tr>
<tr><th>[\#1527](https://gitlab.haskell.org//ghc/ghc/issues/1527)</th>
<td>parsing error message that could be improved</td></tr>
<tr><th>[\#1570](https://gitlab.haskell.org//ghc/ghc/issues/1570)</th>
<td>ghc panic when compiling with -fPIC</td></tr>
<tr><th>[\#1762](https://gitlab.haskell.org//ghc/ghc/issues/1762)</th>
<td>wrong result of isProperSubsetOf</td></tr>
<tr><th>[\#1905](https://gitlab.haskell.org//ghc/ghc/issues/1905)</th>
<td>runProcess: misbehaving exception on nonexistent working directory</td></tr>
<tr><th>[\#1968](https://gitlab.haskell.org//ghc/ghc/issues/1968)</th>
<td>data family + GADT: not implemented yet</td></tr>
<tr><th>[\#2047](https://gitlab.haskell.org//ghc/ghc/issues/2047)</th>
<td>ghc compiled program crashes with segfault when using -M and/or -c</td></tr>
<tr><th>[\#2109](https://gitlab.haskell.org//ghc/ghc/issues/2109)</th>
<td>hIsEOF blocks when applied to a non-closed socket with no input available</td></tr>
<tr><th>[\#2118](https://gitlab.haskell.org//ghc/ghc/issues/2118)</th>
<td>deriving for GRose</td></tr>
<tr><th>[\#2172](https://gitlab.haskell.org//ghc/ghc/issues/2172)</th>
<td>ghci / ghc -e outputs terminal escapes even when redirected to file/pipe</td></tr>
<tr><th>[\#2199](https://gitlab.haskell.org//ghc/ghc/issues/2199)</th>
<td>th32SnapEnumProcesses in System.Win32.Process prints debugging information to stdout</td></tr>
<tr><th>[\#2431](https://gitlab.haskell.org//ghc/ghc/issues/2431)</th>
<td>Allow empty case analysis</td></tr>
<tr><th>[\#2587](https://gitlab.haskell.org//ghc/ghc/issues/2587)</th>
<td>Optimiser bug with extistentials and GADT's</td></tr>
<tr><th>[\#2714](https://gitlab.haskell.org//ghc/ghc/issues/2714)</th>
<td>No match in record selector Var.tcTyVarDetails</td></tr>
<tr><th>[\#2868](https://gitlab.haskell.org//ghc/ghc/issues/2868)</th>
<td>\`par\` \`pseq\` does not work as expected</td></tr>
<tr><th>[\#2940](https://gitlab.haskell.org//ghc/ghc/issues/2940)</th>
<td>Do CSE after CorePrep</td></tr>
<tr><th>[\#2983](https://gitlab.haskell.org//ghc/ghc/issues/2983)</th>
<td>Segmentation fault in ray tracer with certain options</td></tr>
<tr><th>[\#3212](https://gitlab.haskell.org//ghc/ghc/issues/3212)</th>
<td>getOptions'.parseLanguage(2) went past eof token</td></tr>
<tr><th>[\#3749](https://gitlab.haskell.org//ghc/ghc/issues/3749)</th>
<td>unexpected parse errors with \`do\` and \`let\`</td></tr>
<tr><th>[\#3752](https://gitlab.haskell.org//ghc/ghc/issues/3752)</th>
<td>6.12.1 release notes have bad url for Haskell Platrofm</td></tr>
<tr><th>[\#3822](https://gitlab.haskell.org//ghc/ghc/issues/3822)</th>
<td>guards in arrow notation (Arrows extension) case statement cause compiler panic</td></tr>
<tr><th>[\#3974](https://gitlab.haskell.org//ghc/ghc/issues/3974)</th>
<td>Duplicate bug: (see \#3975) filepath: normalise trailing dot</td></tr>
<tr><th>[\#3975](https://gitlab.haskell.org//ghc/ghc/issues/3975)</th>
<td>filepath: normalise trailing dot</td></tr>
<tr><th>[\#4038](https://gitlab.haskell.org//ghc/ghc/issues/4038)</th>
<td>segmentation fault between GHC-6.12.2 and gtk2hs</td></tr>
<tr><th>[\#4039](https://gitlab.haskell.org//ghc/ghc/issues/4039)</th>
<td>problems with semaphores in ghc-6.10.4 unix-2.3.2.0</td></tr>
<tr><th>[\#4209](https://gitlab.haskell.org//ghc/ghc/issues/4209)</th>
<td>LLVM: Vector code segfaults under OSX</td></tr>
<tr><th>[\#4219](https://gitlab.haskell.org//ghc/ghc/issues/4219)</th>
<td>sequence is not tail recursive, doesn't work with large inputs in strict monads</td></tr>
<tr><th>[\#4282](https://gitlab.haskell.org//ghc/ghc/issues/4282)</th>
<td>Proposal: make Data.List.intersperse and intercalate less strict</td></tr>
<tr><th>[\#4312](https://gitlab.haskell.org//ghc/ghc/issues/4312)</th>
<td>Proposal: Further performance improvements of Data.Set</td></tr>
<tr><th>[\#4333](https://gitlab.haskell.org//ghc/ghc/issues/4333)</th>
<td>Proposal: Improve Data.IntSet</td></tr>
<tr><th>[\#4397](https://gitlab.haskell.org//ghc/ghc/issues/4397)</th>
<td>RULES for Class ops don't fire in HEAD</td></tr>
<tr><th>[\#4805](https://gitlab.haskell.org//ghc/ghc/issues/4805)</th>
<td>segfault in Data.HashTable, triggered by long Agda runs</td></tr>
<tr><th>[\#4808](https://gitlab.haskell.org//ghc/ghc/issues/4808)</th>
<td>File descriptor reuse causes Handle to point to a different stream</td></tr>
<tr><th>[\#5007](https://gitlab.haskell.org//ghc/ghc/issues/5007)</th>
<td>"deriving" seems to ignore class context for a type family</td></tr>
<tr><th>[\#5031](https://gitlab.haskell.org//ghc/ghc/issues/5031)</th>
<td>GHC API (7.0.2) + runhaskell causes bus error in Mac OS X 10.6</td></tr>
<tr><th>[\#5129](https://gitlab.haskell.org//ghc/ghc/issues/5129)</th>
<td>"evaluate" optimized away</td></tr>
<tr><th>[\#5162](https://gitlab.haskell.org//ghc/ghc/issues/5162)</th>
<td>Add Int index functions to Data.Set</td></tr>
<tr><th>[\#5216](https://gitlab.haskell.org//ghc/ghc/issues/5216)</th>
<td>Haskell Platform 2011.2.0.1 Setup</td></tr>
<tr><th>[\#5280](https://gitlab.haskell.org//ghc/ghc/issues/5280)</th>
<td>System.Random commits (rand \`mod\` base) error.</td></tr>
<tr><th>[\#5465](https://gitlab.haskell.org//ghc/ghc/issues/5465)</th>
<td>deepseq missing NFData instances for (-\>), Data.Fixed and Data.Version</td></tr>
<tr><th>[\#5468](https://gitlab.haskell.org//ghc/ghc/issues/5468)</th>
<td>Implement deepseq-\>{array,containers} dependancy reversal proposal</td></tr>
<tr><th>[\#5497](https://gitlab.haskell.org//ghc/ghc/issues/5497)</th>
<td>Relax haskell98 build-dep on time package to allow time-1.4</td></tr>
<tr><th>[\#5557](https://gitlab.haskell.org//ghc/ghc/issues/5557)</th>
<td>Code using seq has wrong strictness (too lazy)</td></tr>
<tr><th>[\#5587](https://gitlab.haskell.org//ghc/ghc/issues/5587)</th>
<td>Code using seq has wrong strictness (too lazy) when optimised</td></tr>
<tr><th>[\#5625](https://gitlab.haskell.org//ghc/ghc/issues/5625)</th>
<td>Code using seq has wrong strictness when unoptimised (too lazy)</td></tr>
<tr><th>[\#5691](https://gitlab.haskell.org//ghc/ghc/issues/5691)</th>
<td>Crash: strange interaction of ScopedTypeVariables extension with type class name resolution</td></tr>
<tr><th>[\#5723](https://gitlab.haskell.org//ghc/ghc/issues/5723)</th>
<td>GHCi (7.4.0.20111219) sometimes segfaults when reading .ghci</td></tr>
<tr><th>[\#5915](https://gitlab.haskell.org//ghc/ghc/issues/5915)</th>
<td>Code using seq has wrong strictness when unoptimised (too strict)</td></tr>
<tr><th>[\#6059](https://gitlab.haskell.org//ghc/ghc/issues/6059)</th>
<td>FFI: segfault when jumping to code buffer (under certain conditions)</td></tr>
<tr><th>[\#6140](https://gitlab.haskell.org//ghc/ghc/issues/6140)</th>
<td>segfault in OS X GHCi when dealing with infinite integers</td></tr>
<tr><th>[\#7043](https://gitlab.haskell.org//ghc/ghc/issues/7043)</th>
<td>32-bit GHC ceiling of negative float SEGFAULT: 11</td></tr>
<tr><th>[\#7087](https://gitlab.haskell.org//ghc/ghc/issues/7087)</th>
<td>'select' fails for very large arguments to 'threadDelay'</td></tr>
<tr><th>[\#7171](https://gitlab.haskell.org//ghc/ghc/issues/7171)</th>
<td>erroneous overlapping instances reported with FunDeps</td></tr>
<tr><th>[\#7317](https://gitlab.haskell.org//ghc/ghc/issues/7317)</th>
<td>Segmentation fault in RTS' STM code on git master</td></tr>
<tr><th>[\#7472](https://gitlab.haskell.org//ghc/ghc/issues/7472)</th>
<td>Build on FreeBSD fails with ncurses</td></tr>
<tr><th>[\#7475](https://gitlab.haskell.org//ghc/ghc/issues/7475)</th>
<td>Mysterious Data.Word Segmentation Fault in GHCi</td></tr>
<tr><th>[\#7509](https://gitlab.haskell.org//ghc/ghc/issues/7509)</th>
<td>Changing the second prompt. :{ modules\| No setting available to change.</td></tr>
<tr><th>[\#7588](https://gitlab.haskell.org//ghc/ghc/issues/7588)</th>
<td>GHC HEAD built with LLVM on Mac OS X miscompiles RTS: SIGSEGV in stg_PAP_apply</td></tr>
<tr><th>[\#7599](https://gitlab.haskell.org//ghc/ghc/issues/7599)</th>
<td>timeout does not behave as expected</td></tr>
<tr><th>[\#7616](https://gitlab.haskell.org//ghc/ghc/issues/7616)</th>
<td>ghci loading package base linking crash</td></tr>
<tr><th>[\#7629](https://gitlab.haskell.org//ghc/ghc/issues/7629)</th>
<td>segmentation fault in compiled program, involves gtk, selinux</td></tr>
<tr><th>[\#7669](https://gitlab.haskell.org//ghc/ghc/issues/7669)</th>
<td>Empty case causes warning</td></tr>
<tr><th>[\#7777](https://gitlab.haskell.org//ghc/ghc/issues/7777)</th>
<td>ghc panic: varargs + sets</td></tr>
<tr><th>[\#7838](https://gitlab.haskell.org//ghc/ghc/issues/7838)</th>
<td>ghc(i) crashes on instance declaration</td></tr>
<tr><th>[\#7956](https://gitlab.haskell.org//ghc/ghc/issues/7956)</th>
<td>ghci segfaults with -vN command-line options</td></tr>
<tr><th>[\#8016](https://gitlab.haskell.org//ghc/ghc/issues/8016)</th>
<td>case expression with mixed use of Num instances cause spurious overlap warning</td></tr>
<tr><th>[\#8216](https://gitlab.haskell.org//ghc/ghc/issues/8216)</th>
<td>Segment fault using TH</td></tr>
<tr><th>[\#8746](https://gitlab.haskell.org//ghc/ghc/issues/8746)</th>
<td>Crosscompiling</td></tr>
<tr><th>[\#9441](https://gitlab.haskell.org//ghc/ghc/issues/9441)</th>
<td>CSE should deal with letrec</td></tr>
<tr><th>[\#9465](https://gitlab.haskell.org//ghc/ghc/issues/9465)</th>
<td>configure: sed: illegal option -- r</td></tr>
<tr><th>[\#9684](https://gitlab.haskell.org//ghc/ghc/issues/9684)</th>
<td>Broken build on OS X (incompatible pthread_setname_np API)</td></tr>
<tr><th>[\#9818](https://gitlab.haskell.org//ghc/ghc/issues/9818)</th>
<td>Add \`Natural\` number type to \`base\`</td></tr>
<tr><th>[\#9822](https://gitlab.haskell.org//ghc/ghc/issues/9822)</th>
<td>Add displayException to Exception typeclass</td></tr>
<tr><th>[\#10894](https://gitlab.haskell.org//ghc/ghc/issues/10894)</th>
<td>In users_guide/profiling.xml : -prof flag not to be used with cabal/stack</td></tr>
<tr><th>[\#10910](https://gitlab.haskell.org//ghc/ghc/issues/10910)</th>
<td>Data families seem not to be "surely apart" from anything</td></tr>
<tr><th>[\#10960](https://gitlab.haskell.org//ghc/ghc/issues/10960)</th>
<td>Closed type families don't reduce on data family instances</td></tr>
<tr><th>[\#11083](https://gitlab.haskell.org//ghc/ghc/issues/11083)</th>
<td>Documentation clarification for Data.List.isSubsequenceOf</td></tr>
<tr><th>[\#11480](https://gitlab.haskell.org//ghc/ghc/issues/11480)</th>
<td>UndecidableSuperClasses causes the compiler to spin with UndecidableInstances</td></tr>
<tr><th>[\#11525](https://gitlab.haskell.org//ghc/ghc/issues/11525)</th>
<td>Using a dummy typechecker plugin causes an ambiguity check error</td></tr>
<tr><th>[\#11703](https://gitlab.haskell.org//ghc/ghc/issues/11703)</th>
<td>Segmentation fault/internal error: evacuate: strange closure type 15 with GHC.Generics</td></tr>
<tr><th>[\#12736](https://gitlab.haskell.org//ghc/ghc/issues/12736)</th>
<td>Calling a complex Haskell function (obtained via FFI wrapper function) from MSVC 64-bit C code (passed in as FunPtr) can leave SSE2 registers in the XMM6-XMM15 range modified</td></tr>
<tr><th>[\#13062](https://gitlab.haskell.org//ghc/ghc/issues/13062)</th>
<td>\`opt' failed in phase \`LLVM Optimiser'. (Exit code: -11)</td></tr>
<tr><th>[\#13302](https://gitlab.haskell.org//ghc/ghc/issues/13302)</th>
<td>Let in do-notation with braces does not parse</td></tr>
<tr><th>[\#13832](https://gitlab.haskell.org//ghc/ghc/issues/13832)</th>
<td>No parameter-validation in Control.Concurrent.setNumCapabilities</td></tr>
<tr><th>[\#13888](https://gitlab.haskell.org//ghc/ghc/issues/13888)</th>
<td>GHC 8.0.2 panics when trying a simple snippet involving Parsec</td></tr>
<tr><th>[\#13889](https://gitlab.haskell.org//ghc/ghc/issues/13889)</th>
<td>GHC 8.0.2 panics when trying a simple snippet involving Parsec</td></tr>
<tr><th>[\#13990](https://gitlab.haskell.org//ghc/ghc/issues/13990)</th>
<td>Core Lint error on empty case</td></tr>
<tr><th>[\#14187](https://gitlab.haskell.org//ghc/ghc/issues/14187)</th>
<td>Transpose hangs on infinite by finite lists</td></tr>
<tr><th>[\#14542](https://gitlab.haskell.org//ghc/ghc/issues/14542)</th>
<td>Renamer / typechecker hang (UndecidableSuperClasses)</td></tr>
<tr><th>[\#14718](https://gitlab.haskell.org//ghc/ghc/issues/14718)</th>
<td>Add Boolean type families to 'base' package</td></tr>
<tr><th>[\#14767](https://gitlab.haskell.org//ghc/ghc/issues/14767)</th>
<td>Move Data.Functor.Contravariant into base</td></tr>
<tr><th>[\#14837](https://gitlab.haskell.org//ghc/ghc/issues/14837)</th>
<td>Semigroup and Monoid instances for ST</td></tr>
<tr><th>[\#14857](https://gitlab.haskell.org//ghc/ghc/issues/14857)</th>
<td>GHC-8.4.1 release notes don't mention 'Div', 'Mod' and other type families</td></tr>
<tr><th>[\#15027](https://gitlab.haskell.org//ghc/ghc/issues/15027)</th>
<td>Remove WrappedMonad from Control.Applicative</td></tr>
<tr><th>[\#15339](https://gitlab.haskell.org//ghc/ghc/issues/15339)</th>
<td>Add function equality instance for finite functions to base</td></tr>
<tr><th>[\#15500](https://gitlab.haskell.org//ghc/ghc/issues/15500)</th>
<td>internal error: Unable to commit 1048576 bytes of memory. Deepseq</td></tr>
<tr><th>[\#16089](https://gitlab.haskell.org//ghc/ghc/issues/16089)</th>
<td>seq is not cooperating with :sprint in GHCi as expected</td></tr></table>

## More aggressive CSE


Joachim did some experiments trying to achieve more CSE, but could not achieve a uniform win in the benchmarks. This page holds some of the notes of what has been tried, and what happened. Some of this has also been noted at [\#7596](https://gitlab.haskell.org//ghc/ghc/issues/7596). This is more a list of anecdotal insights; full insights would have led to a patch to master... :-)


The main idea was to make the float out phase flout out more stuff, so that later on the CSE pass sees more possibilities to CSE expressions up. In itself, this works as expected, and solves the motivating example from [\#7596](https://gitlab.haskell.org//ghc/ghc/issues/7596), but for various reasons the results were not as good as hoped-for.


Some reasons why more CSE could hurt:

- When one very aggressively, this will float things like `GHC.Classes.<= @ a sc`, which is of no use.
- Sharing across case branches. ([ticket:7596\#comment:3](https://gitlab.haskell.org//ghc/ghc/issues/7596)) The code in question has multiple case branches, all of which called `reverse rev`. Floating this expression out and sharing it does not gain anything in terms of saved work or allocations, but can increase allocation, i.e. if the value is not used in all branches.
- Different arguments to a function  behave similar as case branches, if the function may only evaluate one of them. Floating out subexpression of the arguments can then increase allocation.
- An additional CSE pass as the end, even without further floating, made `puzzle` worse: The thunks were `OnceL` before, so after CSE the whole thunk-updating-machinery was added. Even worse: The second occurrence of the expression was dead code (which the compiler cannot know).


There were also effects that I did not fully understand:

- In `kahan`, the two calls to `newArray` were shared (as functions, of course, as they are `ST` actions). This (or something else) caused the `inner` loop to be moved inside the `outer` loop, causing many allocations of that loop’s function.
- Even only floating saturated constructor applications (which had only a little effect for most benchmarks) made things works; the cause is hidden somewhere in `GHC.Show` and I did not find out why.


Summary:


More floating and/or CSE is tricky, as there are a few way this can make things worse. Also, it is difficult to find out what goes wrong, as the Core tend to change a lot even with small adjustments of the code.


It might be worth trying to implement a very careful floating/CSE pass that will make sure that only expressions that are going to be allocated in every case (i.e. because they are in a strict context, or arguments to functions in strict context) can be CSEd. There might be some small gains to achieve without too many regressions.
