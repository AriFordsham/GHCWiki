# Jan Stolarek's internship notes

# Wise people say…


Geoffrey:

- In the past, LLVM could not recognize all loops output by the LLVm back end as loops. Perhaps that has changed.
- Answering the question "What does loopification do that isn't already being done?" would still be useful
- So figuring out how to make LLVM recognize more loops would be good.
- if you write a simple, tight loop in Haskell of the sort a C compiler would vectorize, will LLVm vectorize it? If not, why?


Austin Seipp:

- i took the time to implement a half-assed almost-working loopification pass a few months ago. the sinking pass by Simon is what really does a huge amount of the optimizations Kryzsztof's thesis attacked differently. but i think doing loopification could maybe lead to identifying things like loop invariant expressions. it can't bootstrap the compiler with it (JS: it = Austin's patch). i think whenever i tie the knot in the new graph, i don't abandon parts of the old CmmNode, which then causes dead labels to hang around
- oh, yeah, and as i noted in the commit message, you have to be careful when ordering those optimizations around. this is obviously only a valid transform pre-CPS. also you have to run a block elimination passes, otherwise things can happen where work can get duplicated into empty blocks
-  i think another problem is that the new codegen doesn't always discard empty basic blocks which pisses off the native code generator (see [\#7574](https://gitlab.haskell.org//ghc/ghc/issues/7574) ) so we need a little refactoring to handle that correctly, too, by being able to do SCC passes from any particular node
- i think that the fix for [\#7574](https://gitlab.haskell.org//ghc/ghc/issues/7574) is probably pretty easily actually, it just requires shuffling things around. oh, and to be clear it's not \*empty\* basic blocks, it's \*unreachable\* basic blocks that make the codegen mad


Simon Marlow:

- CmmSink removes dead assignments (though not in loops), which is why it's commented out.  A single removeDeadAssigments pass costs about 5% of compilation time, and in the vast majority of code does nothing over what CmmSink already does.
- **PLEASE make sure that you're carefully measuring compilation time when making changes to the code generator**.  Expensive optimisations need to go in -O2 (at least).

# Back-end notes

## Various notes to self

- Does it make sense to create a separate flag for every Cmm optimisation I add? After all they are designed to work together
- I need to remember to cerfully choose at which optimization levels my Cmm passes are enabled
- Here's an interesting bit from `CoreToStg.lhs`: "a dead variable's stack slot (if it has one): should be stubbed to avoid space leaks"

## Loopification

- tests that fail with panic on `29b6db7` (branch `js-loopification-v4`, run with `make EXTRA_HC_OPTS='-fcmm-loopify -fcmm-copy-propagation' WAY=normal`):

```wiki
   ../../libraries/array/tests            largeArray [bad stdout] (normal)
   ../../libraries/base/tests             data-fixed-show-read [bad exit code] (normal)
   ../../libraries/base/tests             dynamic003 [bad exit code] (normal)
   ../../libraries/base/tests             enum01 [bad exit code] (normal)
   ../../libraries/base/tests             enumRatio [bad exit code] (normal)
   ../../libraries/base/tests             exceptionsrun001 [bad exit code] (normal)
   ../../libraries/base/tests             fixed [bad exit code] (normal)
   ../../libraries/base/tests             ioref001 [bad exit code] (normal)
   ../../libraries/base/tests             memo001 [bad exit code] 
   ../../libraries/base/tests             memo002 [bad stdout] (normal)
   ../../libraries/base/tests             rand001 [bad exit code] (normal)
   ../../libraries/base/tests             tempfiles [bad stdout] (normal)
   ../../libraries/base/tests             text001 [bad exit code] (normal)
   ../../libraries/base/tests             tup001 [bad exit code] (normal)
   ../../libraries/base/tests             unicode001 [bad stdout] (normal)
   ../../libraries/base/tests             unicode002 [bad stdout] (normal)
   ../../libraries/base/tests/Concurrent  ThreadDelay001 [bad exit code] (normal)
   ../../libraries/base/tests/IO          T4144 [bad exit code] (normal)
   ../../libraries/base/tests/IO          T7853 [bad stdout] (normal)
   ../../libraries/base/tests/IO          finalization001 [bad stdout] (normal)
   ../../libraries/base/tests/IO          hClose001 [bad exit code] (normal)
   ../../libraries/base/tests/IO          hClose002 [bad exit code] (normal)
   ../../libraries/base/tests/IO          hClose003 [bad stdout] (normal)
   ../../libraries/base/tests/IO          hDuplicateTo001 [bad exit code] (normal)
   ../../libraries/base/tests/IO          hGetBuf001 [bad exit code] (normal)
   ../../libraries/base/tests/IO          hGetBuffering001 [bad exit code] (normal)
   ../../libraries/base/tests/IO          hGetChar001 [bad exit code] (normal)
   ../../libraries/base/tests/IO          hGetPosn001 [bad exit code] (normal)
   ../../libraries/base/tests/IO          hSeek001 [bad exit code] (normal)
   ../../libraries/base/tests/IO          hSeek003 [bad exit code] (normal)
   ../../libraries/base/tests/IO          hSetBuffering003 [bad stdout] (normal)
   ../../libraries/base/tests/IO          ioeGetErrorString001 [bad stdout] (normal)
   ../../libraries/base/tests/IO          ioeGetFileName001 [bad stdout] (normal)
   ../../libraries/base/tests/IO          ioeGetHandle001 [bad stdout] (normal)
   ../../libraries/base/tests/IO          openFile001 [bad exit code] (normal)
   ../../libraries/base/tests/IO          readwrite002 [bad exit code] (normal)
   ../../libraries/base/tests/Numeric     num007 [bad exit code] (normal)
   ../../libraries/base/tests/Numeric     num009 [bad stdout] (normal)
   ../../libraries/base/tests/Numeric     num010 [bad exit code] (normal)
   ../../libraries/base/tests/System      getEnv001 [bad exit code] (normal)
   ../../libraries/base/tests/System      system001 [bad exit code] (normal)
   ../../libraries/directory/tests        currentDirectory001 [bad exit code] (normal)
   ../../libraries/hpc/tests/fork         hpc_fork [bad stdout] (normal)
   ../../libraries/hpc/tests/function     tough [bad stdout] (normal)
   ../../libraries/hpc/tests/function2    tough2 [bad stdout] (normal)
   ../../libraries/hpc/tests/simple/tixs  hpc_markup_001 [bad stdout] (normal)
   ../../libraries/old-time/tests         time002 [bad stdout] (normal)
   ../../libraries/old-time/tests         time003 [bad exit code] (normal)
   ../../libraries/process/tests          process005 [bad exit code] (normal)
   ../../libraries/process/tests          process007 [bad exit code] (normal)
   ../../libraries/process/tests          process009 [bad exit code] (normal)
   ../../libraries/random/tests           random1283 [bad exit code] (normal)
   ../../libraries/random/tests           rangeTest [bad exit code] (normal)
   ../../libraries/stm/tests              stm049 [bad exit code] (normal)
   ../../libraries/stm/tests              stm050 [bad exit code] (normal)
   ../../libraries/stm/tests              stm052 [bad exit code] (normal)
   ../../libraries/unix/tests             resourceLimit [bad exit code] (normal)
   ../../libraries/unix/tests             user001 [bad stdout] (normal)
   ../../libraries/unix/tests/libposix    posix006 [bad stdout] (normal)
   ../../libraries/unix/tests/libposix    posix010 [bad stdout] (normal)
   array/should_run                       arr005 [bad exit code] (normal)
   array/should_run                       arr010 [bad stdout] (normal)
   array/should_run                       arr011 [bad stdout] (normal)
   array/should_run                       arr012 [bad exit code] (normal)
   array/should_run                       arr013 [bad exit code] (normal)
   array/should_run                       arr016 [bad exit code] (normal)
   array/should_run                       arr017 [bad stdout] (normal)
   array/should_run                       arr018 [bad exit code] (normal)
   array/should_run                       arr019 [bad stdout] 
   array/should_run                       arr020 [bad exit code] (normal)
   arrows/should_run                      T3822 [bad exit code] (normal)
   arrows/should_run                      arrowrun001 [bad exit code] (normal)
   arrows/should_run                      arrowrun002 [bad exit code] (normal)
   arrows/should_run                      arrowrun003 [bad exit code] (normal)
   arrows/should_run                      arrowrun004 [bad exit code] (normal)
   codeGen/should_run                     T1852 [bad exit code] (normal)
   codeGen/should_run                     T2838 [bad exit code] (normal)
   codeGen/should_run                     T5149 [bad exit code] (normal)
   codeGen/should_run                     T7361 [bad stdout] (normal)
   codeGen/should_run                     T7600 [bad exit code] (normal)
   codeGen/should_run                     cgrun008 [bad stdout] (normal)
   codeGen/should_run                     cgrun011 [bad exit code] (normal)
   codeGen/should_run                     cgrun013 [bad stdout] (normal)
   codeGen/should_run                     cgrun015 [bad exit code] (normal)
   codeGen/should_run                     cgrun016 [bad stderr] (normal)
   codeGen/should_run                     cgrun017 [bad exit code] (normal)
   codeGen/should_run                     cgrun018 [bad exit code] (normal)
   codeGen/should_run                     cgrun021 [bad exit code] (normal)
   codeGen/should_run                     cgrun022 [bad stdout] (normal)
   codeGen/should_run                     cgrun026 [bad exit code] (normal)
   codeGen/should_run                     cgrun028 [bad exit code] (normal)
   codeGen/should_run                     cgrun031 [bad exit code] (normal)
   codeGen/should_run                     cgrun033 [bad exit code] (normal)
   codeGen/should_run                     cgrun034 [bad exit code] (normal)
   codeGen/should_run                     cgrun035 [bad exit code] (normal)
   codeGen/should_run                     cgrun045 [bad stderr] (normal)
   codeGen/should_run                     cgrun046 [bad exit code] (normal)
   codeGen/should_run                     cgrun047 [bad exit code] (normal)
   codeGen/should_run                     cgrun050 [bad exit code] (normal)
   codeGen/should_run                     cgrun051 [bad stderr] (normal)
   codeGen/should_run                     cgrun054 [bad exit code] (normal)
   codeGen/should_run                     cgrun055 [bad exit code] (normal)
   codeGen/should_run                     cgrun059 [bad exit code] (normal)
   codeGen/should_run                     cgrun064 [bad exit code] (normal)
   codeGen/should_run                     cgrun065 [bad exit code] (normal)
   codeGen/should_run                     cgrun066 [bad exit code] (normal)
   codeGen/should_run                     cgrun067 [bad exit code] (normal)
   codeGen/should_run                     cgrun068 [bad exit code] (normal)
   codeGen/should_run                     cgrun069 [bad exit code] (normal)
   codeGen/should_run                     cgrun070 [bad exit code] (normal)
   codeGen/should_run                     cgrun071 [bad exit code] (normal)
   codeGen/should_run                     cgrun072 [bad exit code] (normal)
   codeGen/should_run                     setByteArray [bad exit code] (normal)
   concurrent/T2317                       T2317 [bad stdout] (normal)
   concurrent/prog003                     concprog003 [bad exit code] (normal)
   concurrent/should_run                  T1980 [bad exit code] (normal)
   concurrent/should_run                  T3279 [bad exit code] (normal)
   concurrent/should_run                  T367 [bad exit code] (normal)
   concurrent/should_run                  T4030 [bad exit code] (normal)
   concurrent/should_run                  conc003 [bad stdout] (normal)
   concurrent/should_run                  conc004 [bad exit code] (normal)
   concurrent/should_run                  conc021 [bad stderr] (normal)
   concurrent/should_run                  conc032 [bad exit code] (normal)
   concurrent/should_run                  conc045 [bad exit code] (normal)
   concurrent/should_run                  conc051 [bad exit code] (normal)
   concurrent/should_run                  readMVar1 [bad exit code] (normal)
   concurrent/should_run                  throwto001 [bad exit code] (normal)
   deSugar/should_run                     DsLambdaCase [bad stdout] (normal)
   deSugar/should_run                     DsMultiWayIf [bad exit code] 
   deSugar/should_run                     T3126 [bad exit code] (normal)
   deSugar/should_run                     T5742 [bad exit code] (normal)
   deSugar/should_run                     dsrun001 [bad exit code] (normal)
   deSugar/should_run                     dsrun005 [bad exit code] (normal)
   deSugar/should_run                     dsrun009 [bad exit code] (normal)
   deSugar/should_run                     dsrun012 [bad exit code] (normal)
   deSugar/should_run                     dsrun013 [bad exit code] (normal)
   deSugar/should_run                     dsrun014 [bad exit code] (normal)
   deSugar/should_run                     dsrun015 [bad exit code] (normal)
   deSugar/should_run                     dsrun016 [bad exit code] (normal)
   deSugar/should_run                     dsrun017 [bad exit code] (normal)
   deSugar/should_run                     dsrun018 [bad exit code] (normal)
   deSugar/should_run                     dsrun019 [bad exit code] (normal)
   deSugar/should_run                     dsrun020 [bad exit code] (normal)
   deSugar/should_run                     dsrun021 [bad exit code] (normal)
   deSugar/should_run                     dsrun022 [bad exit code] (normal)
   deSugar/should_run                     dsrun023 [bad exit code] (normal)
   deSugar/should_run                     mc01 [bad exit code] (normal)
   deSugar/should_run                     mc02 [bad exit code] (normal)
   deSugar/should_run                     mc03 [bad exit code] (normal)
   deSugar/should_run                     mc04 [bad exit code] (normal)
   deSugar/should_run                     mc05 [bad exit code] (normal)
   deSugar/should_run                     mc06 [bad exit code] (normal)
   deSugar/should_run                     mc07 [bad exit code] (normal)
   deSugar/should_run                     mc08 [bad exit code] (normal)
   deriving/should_run                    T2529 [bad exit code] (normal)
   deriving/should_run                    T4528a [bad stdout] (normal)
   deriving/should_run                    T5628 [bad stderr] (normal)
   deriving/should_run                    drvrun-functor1 [bad exit code] (normal)
   deriving/should_run                    drvrun002 [bad exit code] (normal)
   deriving/should_run                    drvrun003 [bad stdout] (normal)
   deriving/should_run                    drvrun004 [bad exit code] (normal)
   deriving/should_run                    drvrun006 [bad exit code] (normal)
   deriving/should_run                    drvrun009 [bad exit code] (normal)
   deriving/should_run                    drvrun010 [bad exit code] (normal)
   deriving/should_run                    drvrun011 [bad exit code] (normal)
   deriving/should_run                    drvrun012 [bad exit code] (normal)
   deriving/should_run                    drvrun015 [bad exit code] (normal)
   deriving/should_run                    drvrun018 [bad exit code] (normal)
   deriving/should_run                    drvrun019 [bad exit code] (normal)
   deriving/should_run                    drvrun020 [bad exit code] (normal)
   driver                                 T4437 [bad stdout] (normal)
   driver/T1959                           T1959 [bad exit code] (normal)
   driver/T7835                           T7835 [bad stdout] (normal)
   driver/recomp011                       recomp011 [bad exit code] (normal)
   ffi/should_run                         Capi_Ctype_001 [bad exit code] (normal)
   ffi/should_run                         Capi_Ctype_002 [bad exit code] (normal)
   ffi/should_run                         T1679 [bad exit code] (normal)
   ffi/should_run                         T2469 [bad exit code] (normal)
   ffi/should_run                         T2594 [bad exit code] (normal)
   ffi/should_run                         T2917a [bad stdout] (normal)
   ffi/should_run                         T4012 [bad stdout] (normal)
   ffi/should_run                         T4038 [bad exit code] (normal)
   ffi/should_run                         T4221 [bad exit code] (normal)
   ffi/should_run                         capi_value [bad exit code] (normal)
   ffi/should_run                         fed001 [bad exit code] (normal)
   ffi/should_run                         ffi002 [bad exit code] (normal)
   ffi/should_run                         ffi003 [bad exit code] (normal)
   ffi/should_run                         ffi006 [bad exit code] (normal)
   ffi/should_run                         ffi008 [bad stderr]
   ffi/should_run                         ffi009 [bad exit code] (normal)
   ffi/should_run                         ffi011 [bad exit code] (normal)
   ffi/should_run                         ffi013 [bad exit code] (normal)
   ffi/should_run                         ffi016 [bad exit code] (normal)
   ffi/should_run                         ffi019 [bad exit code] (normal)
   ffi/should_run                         ffi021 [bad exit code] (normal)
   ffi/should_run                         ffi022 [bad exit code] (normal)
   ffi/should_run                         ffi_parsing_001 [bad exit code] (normal)
   gadt                                   Session [bad stdout] (normal)
   gadt                                   gadt2 [bad exit code] (normal)
   gadt                                   gadt4 [bad exit code] (normal)
   gadt                                   gadt5 [bad exit code] (normal)
   gadt                                   tc [bad exit code] (normal)
   gadt                                   type-rep [bad exit code] (normal)
   gadt                                   ubx-records [bad exit code] (normal)
   gadt                                   while [bad exit code] (normal)
   generics/GEq                           GEq1 [bad exit code] (normal)
   generics/GEq                           GEq2 [bad exit code] (normal)
   generics/GFunctor                      GFunctor1 [bad exit code] (normal)
   generics/GMap                          GMap1 [bad exit code] (normal)
   generics/GShow                         GShow1 [bad exit code] (normal)
   generics/Uniplate                      GUniplate1 [bad exit code] (normal)
   ghc-api                                T6145 [bad exit code] (normal)
   ghc-api/T4891                          T4891 [bad exit code] (normal)
   ghc-api/T7478                          T7478 [bad exit code] (normal)
   ghc-api/apirecomp001                   apirecomp001 [bad exit code] (normal)
   ghci/linking                           ghcilink002 [bad exit code] (normal)
   ghci/linking                           ghcilink005 [bad exit code] (normal)
   ghci/scripts                           ghci024 [bad stdout] (normal)
   indexed-types/should_run               GMapAssoc [bad exit code] (normal)
   indexed-types/should_run               GMapTop [bad exit code] (normal)
   indexed-types/should_run               T4235 [bad exit code] (normal)
   lib/integer                            gcdInteger [bad stdout] (normal)
   mdo/should_compile                     mdo002 [bad exit code] (normal)
   mdo/should_compile                     mdo003 [bad exit code] (normal)
   mdo/should_compile                     mdo004 [bad exit code] (normal)
   mdo/should_run                         mdorun001 [bad exit code] (normal)
   mdo/should_run                         mdorun002 [bad exit code] (normal)
   numeric/should_compile                 T7116 [bad stdout] (normal)
   numeric/should_run                     T7233 [bad stdout] (normal)
   numeric/should_run                     T7689 [bad exit code] (normal)
   numeric/should_run                     add2 [bad stdout] (normal)
   numeric/should_run                     arith001 [bad exit code] (normal)
   numeric/should_run                     arith002 [bad exit code] (normal)
   numeric/should_run                     arith003 [bad stdout] (normal)
   numeric/should_run                     arith005 [bad exit code] (normal)
   numeric/should_run                     arith008 [bad stdout] (normal)
   numeric/should_run                     arith011 [bad exit code] (normal)
   numeric/should_run                     arith012 [bad stdout] (normal)
   numeric/should_run                     arith013 [bad exit code] (normal)
   numeric/should_run                     arith016 [bad exit code] (normal)
   numeric/should_run                     mul2 [bad stdout] (normal)
   numeric/should_run                     numrun012 [bad exit code] (normal)
   numeric/should_run                     quotRem2 [bad stdout] (normal)
   overloadedlists/should_run             overloadedlistsrun04 [bad stdout] (normal)
   parser/should_run                      operator [bad exit code] (normal)
   parser/should_run                      operator2 [bad exit code] (normal)
   parser/should_run                      readRun001 [bad exit code] (normal)
   parser/should_run                      readRun002 [bad exit code] (normal)
   parser/should_run                      readRun004 [bad exit code]
   perf/compiler                          T1969 [stat not good enough] (normal)
   perf/compiler                          T3064 [stat not good enough] (normal)
   perf/compiler                          T3294 [stat not good enough] (normal)
   perf/compiler                          T4801 [stat not good enough] (normal)
   perf/compiler                          T5030 [stat not good enough] (normal)
   perf/compiler                          T5321FD [stat not good enough] (normal)
   perf/compiler                          T5321Fun [stat not good enough] (normal)
   perf/compiler                          T5631 [stat not good enough] (normal)
   perf/compiler                          T5642 [stat not good enough] (normal)
   perf/compiler                          T5837 [stat not good enough] (normal)
   perf/compiler                          T783 [stat not good enough] (normal)
   perf/compiler                          parsing001 [stat not good enough] (normal)
   perf/should_run                        Conversions [exit code non-0] (normal)
   perf/should_run                        MethSharing [exit code non-0] (normal)
   perf/should_run                        T3245 [bad exit code] (normal)
   perf/should_run                        T3586 [exit code non-0] (normal)
   perf/should_run                        T3736 [bad stderr] (normal)
   perf/should_run                        T3738 [bad exit code] (normal)
   perf/should_run                        T4321 [exit code non-0] (normal)
   perf/should_run                        T4474a [bad exit code] (normal)
   perf/should_run                        T4474b [bad exit code] (normal)
   perf/should_run                        T4474c [bad exit code] (normal)
   perf/should_run                        T4830 [bad exit code] (normal)
   perf/should_run                        T4978 [bad exit code] (normal)
   perf/should_run                        T5113 [bad exit code] (normal)
   perf/should_run                        T5237 [bad stdout] (normal)
   perf/should_run                        T5549 [bad exit code] (normal)
   perf/should_run                        T7257 [bad exit code] (normal)
   perf/should_run                        T7436 [stat too good] (normal)
   perf/should_run                        T7507 [bad exit code] (normal)
   perf/should_run                        T7797 [bad exit code] (normal)
   perf/should_run                        T7850 [bad exit code] (normal)
   perf/should_run                        T7954 [exit code non-0] (normal)
   perf/should_run                        T876 [bad stdout] (normal)
   perf/should_run                        lazy-bs-alloc [stat too good] (normal)
   perf/space_leaks                       T2762 [bad exit code] (normal)
   perf/space_leaks                       T4334 [bad exit code] (normal)
   polykinds                              Freeman [bad stdout] (normal)
   polykinds                              MonoidsFD [bad exit code] (normal)
   polykinds                              MonoidsTF [bad exit code] (normal)
   polykinds                              PolyKinds09 [bad exit code] (normal)
   polykinds                              PolyKinds10 [bad exit code] (normal)
   programs/10queens                      10queens [bad stdout] (normal)
   programs/Queens                        queens [bad exit code] (normal)
   programs/andre_monad                   andre_monad [bad exit code] (normal)
   programs/andy_cherry                   andy_cherry [bad exit code] (normal)
   programs/barton-mangler-bug            barton-mangler-bug [bad stdout] (normal)
   programs/cholewo-eval                  cholewo-eval [bad exit code] (normal)
   programs/cvh_unboxing                  cvh_unboxing [bad exit code] (normal)
   programs/fast2haskell                  fast2haskell [bad stdout] (normal)
   programs/jl_defaults                   jl_defaults [bad exit code] (normal)
   programs/joao-circular                 joao-circular [bad exit code] (normal)
   programs/jq_readsPrec                  jq_readsPrec [bad stdout] (normal)
   programs/jtod_circint                  jtod_circint [bad exit code] (normal)
   programs/jules_xref                    jules_xref [bad stdout] (normal)
   programs/jules_xref2                   jules_xref2 [bad stdout] (normal)
   programs/launchbury                    launchbury [bad exit code] (normal)
   programs/lex                           lex [bad stdout] (normal)
   programs/life_space_leak               life_space_leak [bad stdout] (normal)
   programs/north_array                   north_array [bad exit code] (normal)
   programs/record_upd                    record_upd [bad exit code] (normal)
   programs/rittri                        rittri [bad exit code] (normal)
   programs/sanders_array                 sanders_array [bad stdout] (normal)
   programs/seward-space-leak             seward-space-leak [bad exit code] (normal)
   programs/strict_anns                   strict_anns [bad exit code] (normal)
   programs/thurston-modular-arith        thurston-modular-arith [bad exit code] (normal)
   rebindable                             rebindable2 [bad exit code] (normal)
   rebindable                             rebindable3 [bad exit code] (normal)
   rebindable                             rebindable4 [bad exit code] (normal)
   rebindable                             rebindable7 [bad exit code] (normal)
   rts                                    T2047 [bad exit code] (normal)
   rts                                    T4059 [bad exit code] (normal)
   rts                                    T4850 [bad stderr] (normal)
   rts                                    T5423 [bad exit code] (normal)
   rts                                    T7919 [exit code non-0] (normal)
   rts                                    bug1010 [bad stdout] (normal)
   rts                                    exec_signals [bad exit code] (normal)
   rts                                    outofmem2 [bad stderr] (normal)
   rts                                    stack003 [bad exit code] (normal)
   safeHaskell/safeLanguage               SafeLang04 [bad exit code] (normal)
   safeHaskell/safeLanguage               SafeLang05 [bad exit code] (normal)
   safeHaskell/safeLanguage               SafeLang06 [bad exit code] (normal)
   safeHaskell/safeLanguage               SafeLang09 [bad stderr] (normal)
   safeHaskell/safeLanguage               SafeLang11 [exit code non-0] (normal)
   safeHaskell/safeLanguage               SafeLang13 [bad exit code] (normal)
   safeHaskell/safeLanguage               SafeLang15 [bad exit code] (normal)
   safeHaskell/unsafeLibs                 BadImport02 [bad exit code] (normal)
   simplCore/should_run                   T5587 [bad stderr] (normal)
   simplCore/should_run                   T5915 [bad exit code] (normal)
   simplCore/should_run                   T5920 [bad exit code] (normal)
   th                                     T3600 [exit code non-0] (normal)
   th                                     TH_repE2 [bad exit code] (normal)
   th                                     TH_repGuardOutput [bad exit code] (normal)
   th                                     TH_spliceDecl3 [exit code non-0] (normal)
   th                                     TH_unresolvedInfix [bad stdout] (normal)
   typecheck/should_run                   T1735 [bad exit code] (normal)
   typecheck/should_run                   T3731 [bad exit code] (normal)
   typecheck/should_run                   T3731-short [bad exit code] (normal)
   typecheck/should_run                   T5573a [bad stdout] (normal)
   typecheck/should_run                   T5751 [bad exit code] (normal)
   typecheck/should_run                   T5759 [bad exit code] (normal)
   typecheck/should_run                   T5913 [bad exit code] (normal)
   typecheck/should_run                   T6117 [bad exit code] (normal)
   typecheck/should_run                   church [bad exit code] (normal)
   typecheck/should_run                   tcrun002 [bad stdout] (normal)
   typecheck/should_run                   tcrun003 [bad exit code] (normal)
   typecheck/should_run                   tcrun009 [bad stdout] (normal)
   typecheck/should_run                   tcrun010 [bad exit code] (normal)
   typecheck/should_run                   tcrun013 [bad stdout] (normal)
   typecheck/should_run                   tcrun019 [bad exit code] (normal)
   typecheck/should_run                   tcrun023 [bad exit code] (normal)
   typecheck/should_run                   tcrun025 [bad exit code] (normal)
   typecheck/should_run                   tcrun028 [bad exit code] (normal)
   typecheck/should_run                   tcrun033 [bad exit code] (normal)
   typecheck/should_run                   tcrun042 [bad exit code] (normal)
   typecheck/should_run                   tcrun044 [bad stdout] (normal)
   typecheck/should_run                   tcrun045 [bad exit code] (normal)
   typecheck/should_run                   tcrun049 [bad exit code] (normal)
   typecheck/should_run                   tcrun050 [bad exit code] (normal)

```


None of these seem to be directly related to loopification, except maybe for performance ones.

## Let-no-escape notes

- Code generation for let-no-escape: `cgLneBinds` in `codeGen/StgCmmExpr.hs`
- Heap checking in let-no-escape: see `Note [Heap checks]` in `codeGen/StgCmmHeap.hs`
- From `codeGen/StgCmmMonad.hs`:

  ```wiki
  data CgLoc
    = CmmLoc CmmExpr        -- A stable CmmExpr; that is, one not mentioning
                          -- Hp, so that it remains valid across calls

    | LneLoc BlockId [LocalReg]             -- A join point
          -- A join point (= let-no-escape) should only.
          -- be tail-called, and in a saturated way.
          -- To tail-call it, assign to these locals,.
          -- and branch to the block id
  ```
- Simon Marlow says: "\[let-no-escape\] catches  more cases than just join points.  Any function or variable binding that does not escape is turned into let-no-escape."

## Some interesting tickets

- [\#605](https://gitlab.haskell.org//ghc/ghc/issues/605) - Optimisation: strict enumerations
- [\#1498](https://gitlab.haskell.org//ghc/ghc/issues/1498) - Optimisation: eliminate unnecessary heap check in recursive function. 
- [\#1600](https://gitlab.haskell.org//ghc/ghc/issues/1600) - Optimisation: CPR the results of IO
- [\#2289](https://gitlab.haskell.org//ghc/ghc/issues/2289) - Needless reboxing of values when returning from a tight loop
- [\#2387](https://gitlab.haskell.org//ghc/ghc/issues/2387) - Optimizer misses unboxing opportunity
- [\#4470](https://gitlab.haskell.org//ghc/ghc/issues/4470) - Loop optimization: identical counters
- [\#4937](https://gitlab.haskell.org//ghc/ghc/issues/4937) - Remove indirections caused by sum types, such as Maybe
- [\#5567](https://gitlab.haskell.org//ghc/ghc/issues/5567) - LLVM: Improve alias analysis / performance [BackEndNotes](back-end-notes#heap/stack-checks) page has some discussion of this.
- [\#7198](https://gitlab.haskell.org//ghc/ghc/issues/7198) - New codegen more than doubles compile time of T3294
- [\#7574](https://gitlab.haskell.org//ghc/ghc/issues/7574) - Register allocator chokes on certain branches with literals (bug can be triggered with `./inplace/bin/ghc-stage2 -c -no-hs-main -fasm -O2 ./testsuite/tests/llvm/should_compile/T7571.cmm`)
- [\#8048](https://gitlab.haskell.org//ghc/ghc/issues/8048) - Register spilling produces ineffecient/highly contending code

## Notes on the wiki

- [Commentary/Compiler/NewCodeGen/Cleanup](commentary/compiler/new-code-gen/cleanup)
- [Commentary/Compiler/NewCodeGenStupidity](commentary/compiler/new-code-gen-stupidity)
- [ cmm-notes](http://darcs.haskell.org/ghc/compiler/cmm/cmm-notes)

# Various clean-up tasks

## Cmm clean-up

- remove unused CmmRewriteAssignments
- `cmm/CmmLive.hs:106`. This function is not used:

```wiki
removeDeadAssignments :: DynFlags -> CmmGraph
                      -> UniqSM (CmmGraph, BlockEnv CmmLocalLive)
```


It is however referenced in some of the comments. I might be able to use it for my dead assignment removal. Simon PJ notes: ", we want to eliminate dead assignments to stack locations too, so the liveness info need to be augmented with stack areas.
"

- Cmm dumping could be improved. Right now it dumps all optimisation passes for one fragment of Cmm code, then for next fragment and so on. It would be more convinient to dump whole Cmm code after each pass. I'm not sure if that's possible with the current pipeline design. It seems that Stg-\>Cmm pass is intentionally design to produce Cmm code incrementally (via Stream) and I suspect that this might be the reason why the code is processed incrementally.
- Simon M. says: The CmmSink pass before stack layout is disabled because I never got around to measuring it to determine whether it is a good idea or not. By all means do that!

## Cleaning up the STG -\>Cmm pass


When generating Cmm from STG there is some [SRT information](commentary/rts/storage/gc/ca-fs) being generated. It is not used and has to be rebuilt anyway after converting to CPS Cmm. Below are some random notes and pieces of code that might related to this:
  

- Cmm conversions in the compiler pipeline: `main/HscMain.hs` has `tryNewCodeGen` (l. 1300), which first calls `StgCmm.codegen` and then passes the generated Cmm to `cmmPipeline` function from `cmm/CmmPipeline.hs`. According to Austin Seipp `cpsTop` in `cmm/CmmPipeline.hs` takes care of converting to CPS: "yeah, CmmPipeline does take care of it. it's partially cpsTop that does it, and doSRTs elaborates the top-level info tables and stuff beyond that but mostly cpsTop. i think your general turning point is after the stack layout and stack pointer manifestation". 


This code in `cmm/Cmm.hs` that might be relevant (or not):

```wiki
-- (line 141 and onwards)
-- | Info table as a haskell data type
data CmmInfoTable
  = CmmInfoTable {
      cit_lbl  :: CLabel, -- Info table label
      cit_rep  :: SMRep,
      cit_prof :: ProfilingInfo,
      cit_srt  :: C_SRT
    }

data ProfilingInfo
  = NoProfilingInfo
  | ProfilingInfo [Word8] [Word8] -- closure_type, closure_desc

-- C_SRT is what StgSyn.SRT gets translated to...
-- we add a label for the table, and expect only the 'offset/length' form

data C_SRT = NoC_SRT
           | C_SRT !CLabel !WordOff !StgHalfWord {-bitmap or escape-}
           deriving (Eq)

needsSRT :: C_SRT -> Bool
needsSRT NoC_SRT       = False
needsSRT (C_SRT _ _ _) = True
```

## Random code

- `main/HscMain.lhs:1300`\`. Is:

```wiki
| otherwise
  = {-# SCC "cmmPipeline" #-}
    let initTopSRT = initUs_ us emptySRT in

    let run_pipeline topSRT cmmgroup = do
          (topSRT, cmmgroup) <- cmmPipeline hsc_env topSRT cmmgroup
          return (topSRT,cmmgroup)

    in do topSRT <- Stream.mapAccumL run_pipeline initTopSRT ppr_stream1
          Stream.yield (srtToData topSRT)
```


The `<- / return` sequence in the definition of `run_pipeline` can be eliminated, which allows to remove the `do` notation, which allows to do eta-reduction, which (finally) allows to remove the `run_pipeline` binding and using `(cmmPipeline hsc_env)` instead:

```wiki
| otherwise
  = {-# SCC "cmmPipeline" #-}
    let initTopSRT = initUs_ us emptySRT
    in do topSRT <- Stream.mapAccumL (cmmPipeline hsc_env) initTopSRT ppr_stream1
          Stream.yield (srtToData topSRT)
```

- `cmm/CmmUtils.hs`, function `toBlockListEntryFirst` - perhaps it would be safer to return a tuple in this case? This would probably make the invariant more explicit.

## Wiki

- [NewCodeGenPipeline](commentary/compiler/new-code-gen-pipeline) has some outdated sections in the Cmm pipeline description: Add spill/reload, Rewrite assignments. So far I only marked them as OUTDATED
- [NewCodeGenModules](commentary/compiler/new-code-gen-modules) - mostly outdated. Mentioned data types and modules no longer exist.

# Various stuff


Tickets that I could potentially look into:

- [\#3070](https://gitlab.haskell.org//ghc/ghc/issues/3070) - floor(0/0) should not be defined
- [\#3676](https://gitlab.haskell.org//ghc/ghc/issues/3676) - realToFrac doesn't sanely convert between floating types
- [\#3744](https://gitlab.haskell.org//ghc/ghc/issues/3744) - Comparisons against minBound/maxBound not optimised
- [\#4101](https://gitlab.haskell.org//ghc/ghc/issues/4101) - Primitive constant unfolding
- [\#5615](https://gitlab.haskell.org//ghc/ghc/issues/5615) - ghc produces poor code for `div` with constant powers of 2.
- [\#7116](https://gitlab.haskell.org//ghc/ghc/issues/7116) - Missing optimisation: strength reduction of floating-point multiplication
- [\#7858](https://gitlab.haskell.org//ghc/ghc/issues/7858) - Fix definitions of abs/signum for Floats/Doubles.
- [\#8072](https://gitlab.haskell.org//ghc/ghc/issues/8072) - Optimizations change result of div for Word


Other things to do:

- investigate opportunities for improving heap checks. An idea: if a worker knows its heap requirements it could pass them to the caller, thus avoiding the heap check. A question: how much time do we really spend on heap checks?


Some LLVM notes that may be useful:

- [LLVM Alias Notes](commentary/compiler/backends/llvm/alias)
- [ David Terei's LLVM blog post](http://blog.davidterei.com/2011/09/ghc-project-for-all.html)
- [ Max Bolingbroke's LLVM blog entry](http://blog.omega-prime.co.uk/?p=135)
- [ Implementation of various LLVM optimisations using Hoopl](https://github.com/mlite/HsLlvm) - **this seems very relevant**

# Github repos

- [ GHC](https://github.com/jstolarek/ghc)
- [ testsuite](https://github.com/jstolarek/testsuite)
- [ packages-array](https://github.com/jstolarek/packages-array)
- [ packages-base](https://github.com/jstolarek/packages-base)
- [ packages-ghc-prim](https://github.com/jstolarek/packages-ghc-prim)
- [ packages-integer-gmp](https://github.com/jstolarek/packages-integer-gmp)
- [ packages-integer-simple](https://github.com/jstolarek/packages-integer-simple)
- [ packages-primitive](https://github.com/jstolarek/packages-primitive)

- [ test programs](https://github.com/jstolarek/ghc-tests)


Unboxed Booleans ([\#6135](https://gitlab.haskell.org//ghc/ghc/issues/6135)) work is in all 8 repos on branch `bool-primops-vX`, where `X` is a number. `X` is increased after rebasing on top of new HEAD (I'm doing this to avoid upstream rebasing).


Loopification work is in main GHC repo on branch `js-loopification-vX`.
