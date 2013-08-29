
Notes about running demand analysis a second time, late in the pipeline.


Commit [c080f727ba5f83921b842fcff71e9066adbdc250](/trac/ghc/changeset/c080f727ba5f83921b842fcff71e9066adbdc250/ghc)


The numbers quoted on this wiki page were using [ef017944600cf4e153aad686a6a78bfb48dea67a](/trac/ghc/changeset/ef017944600cf4e153aad686a6a78bfb48dea67a/ghc) as the base commit — after measuring, I rebased my patch to apply it to [33c880b43ed72d77f6b1d95d5ccefbd376c78c78](/trac/ghc/changeset/33c880b43ed72d77f6b1d95d5ccefbd376c78c78/ghc)


The corresponding testsuite commit is a7920ef6eefa5578c89b7cda0d6be207ee38c502

## Commit notes


The -flate-dmd-anal flag runs the demand analysis a second time just before CorePrep. It's not on by default yet, but we hope -O2 will eventually imply it, perhaps even for the GHC 7.8 release.


The bulk of this patch merely simplifies the treatment of wrappers in interface files.

## TODO

- Update the documentation to explain -flate-dmd-anal.

- Ask the community for help in determining if we should make -O2 imply -flate-dmd-anal.

## Removing the clever .hi files scheme


Running the demand analyzer twice breaks some expectations of the .hi file format. Prior to this commit, GHC regenerated the wrapper's body from the its strictness signature and worker id. Now, instead, the body is simply encoded just like any other InlineStable.


This change…

1. simplifies a special case; there's plenty of knock-on code elimination from no longer having ids in UnfoldingSource,
1. increases the size of .hi files (see below),
1. accordingly increases compile time a bit (eg \~ +1% over nofib),
1. accommodates the late demand analysis (see below)
1. similarly accommodates the -ffun-to-thunk flag


Simplifying the .hi scheme was the easiest way to enable `-flate-dmd-anal` and make `-ffun-to-thunk` safe to use. **It is possible to revert back to the clever .hi scheme**. It will however require some care in order to safely interoperate with `-flate-dmd-anal`, `-ffun-to-thunk`, and any future work that similarly effects the accuracy of the clever .hi file scheme's regeneration phase.

### Effect on .hi file size


Removing the clever .hi file scheme for wrappers results as expected in an increase of .hi file size.


In $TOPDIR/libraries, there's an extra 569,509 bytes of .hi file.


Here's the files with a growth \>10K.

```wiki
(bytes growth,file)
(11103,"base/dist-install/build/GHC/Arr.hi")
(12479,"template-haskell/dist-install/build/Language/Haskell/TH/Lib.hi")
(12756,"binary/dist-install/build/Data/Binary/Class.hi")
(15727,"random/dist-install/build/System/Random.hi")
(29348,"base/dist-install/build/Data/Data.hi")
(30497,"template-haskell/dist-install/build/Language/Haskell/TH/Syntax.hi")
(37081,"Cabal/Cabal/dist-install/build/Distribution/PackageDescription.hi")
(64200,"ghc-prim/dist-install/build/GHC/Classes.hi")
```


Here's the files with a growth \>10%.

```wiki
(0.10163132137030995,"Cabal/Cabal/dist-install/build/Distribution/Simple/Bench.hi")
(0.1067165410638649,"hoopl/dist-install/build/Compiler/Hoopl/XUtil.hi")
(0.11125552378476736,"base/dist-install/build/Control/Monad.hi")
(0.11311653959856854,"time/dist-install/build/Data/Time/Calendar/Private.hi")
(0.12166183143643532,"transformers/dist-install/build/Data/Functor/Compose.hi")
(0.1584435579816642,"hoopl/dist-install/build/Compiler/Hoopl/Combinators.hi")
(0.21422422135168143,"ghc-prim/dist-install/build/GHC/Classes.hi")
```

### Accommodation of -flate-dmd-anal and -ffun-to-thunk --


The clever .hi scheme caused CoreLint errors when combined with -flate-dmd-anal. I irresponsibly cannot remember the recipe for this bug. It was triggered in one of three ways: building GHC, running nofib, or running ./validate.


Similar to -flate-dmd-anal, abandoning the clever .hi scheme lets us safely import code compiled with/without -ffun-to-thunk from a module compiled without/with -ffun-to-thunk. I can explain this one.

- Compile A.hs with -ffun-to-thunk
- Compile a file B.hs that imports A.hs without -ffun-to-thunk


If demand analysis removes all the value arguments from a function f in A.hs and B.hs uses that function, compilation of B.hs will crash. The problem is that the regeneration of the body of f in B will attempt to apply f to a `realWorld#` argument because there is no -ffun-to-thunk flag. However, f no longer accepts any arguments, since it was compiled with -ffun-to-thunk. Boom.


(The -flate-dmd-anal bug was similar, but more involved.)

## -flate-dmd-anal


-flate-dmd-anal adds a second demand analysis with a subsequent invocation of the simplifier just before CorePrep. Cf [\#7782](https://gitlab.haskell.org//ghc/ghc/issues/7782)

### Effect on .hi file size and .a file size


The second demand analysis generates more worker/wrapper splits, so it also generates larger .hi files and larger .o files. The numbers in this section measure the difference between `-O2 -flate-dmd-anal` and `-O2 -fno-late-dmd-anal`. This is on my 64 bit Mac OS X.


It's based on the size of the .hi and .a files in $TOPDIR/libraries.

<table><tr><th></th>
<th>.hi bytes</th>
<th>.a bytes
</th></tr>
<tr><th>no late-dmd</th>
<th></th>
<th></th></tr>
<tr><th>     late-dmd</th>
<th></th>
<th></th></tr>
<tr><th>  difference  </th>
<th> +552,057   </th>
<th> +684,696 
</th></tr></table>


These are the big .hi changes over 10K.

```wiki
(growth bytes,  module)
(35807,"base/dist-install/build/Data/Data.hi")
(54562,"template-haskell/dist-install/build/Language/Haskell/TH/Syntax.hi")
(59000,"Cabal/Cabal/dist-install/build/Distribution/PackageDescription.hi")
(69900,"template-haskell/dist-install/build/Language/Haskell/TH/Lib.hi")
```


These are the big .hi changes over 10%.

```wiki
(growth%,  module)
(0.10158001494608733,"haskeline/dist-install/build/System/Console/Haskeline/Command.hi")
(0.10499966324675034,"hoopl/dist-install/build/Compiler/Hoopl/MkGraph.hi")
(0.11207246180884142,"haskeline/dist-install/build/System/Console/Haskeline/Command/Undo.hi")
(0.11254620966637761,"transformers/dist-install/build/Control/Applicative/Lift.hi")
(0.11394046020649104,"base/dist-install/build/GHC/Event/Thread.hi")
(0.11417453220731909,"dph/dph-lifted-base/dist-install/build/Data/Array/Parallel/PArray/Reference.hi")
(0.11493796526054591,"hoopl/dist-install/build/Compiler/Hoopl/XUtil.hi")
(0.11842105263157894,"dph/dph-prim-seq/dist-install/build/Data/Array/Parallel/Unlifted/Sequential/Extracts.hi")
(0.1252496671105193,"base/dist-install/build/Control/Concurrent/QSemN.hi")
(0.13623208379272325,"base/dist-install/build/Numeric.hi")
(0.174892616905746,"haskeline/dist-install/build/System/Console/Haskeline/Backend/DumbTerm.hi")
(0.17564356435643563,"base/dist-install/build/Data/Ratio.hi")
(0.1764402762032361,"base/dist-install/build/Control/Concurrent/QSem.hi")
(0.2952895972676818,"dph/dph-lifted-copy/dist-install/build/Data/Array/Parallel/Lifted/TH/Repr.hi")
(0.3762859126952084,"template-haskell/dist-install/build/Language/Haskell/TH/Lib.hi")
```


These are the big .a changes over 10K.

<table><tr><th> growth bytes </th>
<th> module
</th></tr>
<tr><th>-19408</th>
<th>libHSdph-prim-par-0.8.0.1.a 
</th></tr>
<tr><th>-16976</th>
<th>libHSdph-prim-seq-0.8.0.1.a 
</th></tr>
<tr><th>10440</th>
<th>libHShoopl-3.10.0.0.a 
</th></tr>
<tr><th>11120</th>
<th>libHStransformers-0.3.0.0.a 
</th></tr>
<tr><th>11472</th>
<th>libHSold-time-1.1.0.1.a 
</th></tr>
<tr><th>22584</th>
<th>libHStime-1.4.0.2.a 
</th></tr>
<tr><th>30168</th>
<th>libHSdph-lifted-copy-0.8.0.1.a 
</th></tr>
<tr><th>35224</th>
<th>libHSvector-0.9.1.a 
</th></tr>
<tr><th>44448</th>
<th>libHScontainers-0.5.0.0.a 
</th></tr>
<tr><th>48408</th>
<th>libHShaskeline-0.7.0.4.a 
</th></tr>
<tr><th>115104</th>
<th>libHStemplate-haskell-2.9.0.0.a 
</th></tr>
<tr><th>120936</th>
<th>libHSbase-4.7.0.0.a 
</th></tr>
<tr><th>237088</th>
<th>libHSCabal-1.17.0.a 
</th></tr></table>