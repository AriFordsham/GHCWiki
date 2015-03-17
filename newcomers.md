# Resources for newcomers to GHC


This page is intended to serve as the first stop for those people who say, "I want to contribute to GHC, but I don't know quite where to begin." Begin here. While the [building guide](building), [working conventions page](working-conventions), [commentary](commentary) and [debugging](debugging) pages (always linked from the left sidebar) have great information that can come in handy while you're working on your first, or first several patches, this page is intended to have the details you will need to get rolling.

## First steps

- To orient yourself to the general architecture of GHC, [ this article](http://www.aosabook.org/en/ghc.html), written by two of the chief architects of GHC, Simon Marlow and Simon Peyton-Jones, is excellent and current (2012).

- While you're reading that article, download and build the sources. [Prepare](building/preparation) your machine, [download](building/getting-the-sources) the source, and [build](building/hacking). For the short, short version, which may or may not work for your machine, you can try this:

```
# needed only once, URL rewrite rule is persisted in ${HOME}/.gitconfig
git config --global url."git://github.com/ghc/packages-".insteadOf git://github.com/ghc/packages/ 

# clone GHC's main Git repository (creates './ghc' folder in CWD)
git clone --recursive git://github.com/ghc/ghc
cd ghc/
git clone git://github.com/ghc/ghc-tarballs.git  # Windows only
# configure build
cd mk
cp build.mk.sample build.mk
## edit build.mk to remove the comment marker # on the line "BuildFlavour = quick"
cd ..
perl boot
./configure

# build GHC
make -j8 # parallelize to at most 8 parallel jobs; adapt to actual number of cpu cores
## edit build.mk to remove the comment marker # on the line stage=2
```


replace `git://` by `http://` or `https://` in the instructions above if you're behind a firewall blocking port 9418. For more details see also [Building/GettingTheSources](building/getting-the-sources).

>
> If your machine has all the prerequisites, this might just work. Expect it all to take roughly an hour.

- After a successful build, you should have your brand new compiler in `ghc/inplace/bin/ghc-stage2`. (GHCi is launched with `ghc/inplace/bin/ghc-stage2 --interactive`). Try it out.

- The final edit of `build.mk` makes sure that only the stage-2 compiler will be build after this (see [here](building/architecture/idiom/stages) about stages). This will be much faster, and usually what you want. If, for some reason, you're working on the stage-1 compiler, you can undo that change and use `make 1`, but you must be in the compiler subdirectory, not the ghc subdirectory.

- A good first sanity check is to twiddle some error message in the code, just to see that changed error message pop up when you compile a file. Write some Haskell code with an error in it, and look at the error message. Search through the ghc code for that error message (almost all the relevant code is in the `compiler/` subdirectory of `ghc`). Change the message, and then rebuild (run `make` in the `ghc` subdirectory of `ghc` -- that is, `ghc/ghc`). If you see the changed message, you're good to go.

- If you've made it this far, you're well on your way to becoming a GHC developer. You should subscribe to the [ ghc-devs](http://www.haskell.org/mailman/listinfo/ghc-devs) mailing list.

## Fixing a bug or implementing a new feature


Below is a list of tickets that appear to be "low-hanging fruit" -- things that might be reasonable for a newcomer to GHC hacking. Of course, we can't ever be sure of how hard a task is before doing it, so apologies if one of these is too hard.


Bugs:

<table><tr><th>[\#8316](https://gitlab.haskell.org//ghc/ghc/issues/8316)</th>
<td>GHCi debugger panics when trying force a certain variable</td></tr>
<tr><th>[\#10346](https://gitlab.haskell.org//ghc/ghc/issues/10346)</th>
<td>Cross-module SpecConstr</td></tr>
<tr><th>[\#11068](https://gitlab.haskell.org//ghc/ghc/issues/11068)</th>
<td>Make Generic/Generic1 methods inlinable</td></tr>
<tr><th>[\#12488](https://gitlab.haskell.org//ghc/ghc/issues/12488)</th>
<td>Explicit namespaces doesn't enforce namespaces</td></tr>
<tr><th>[\#12576](https://gitlab.haskell.org//ghc/ghc/issues/12576)</th>
<td>Large Address space is not supported on Windows</td></tr>
<tr><th>[\#12636](https://gitlab.haskell.org//ghc/ghc/issues/12636)</th>
<td>ProfHeap's printf modifiers are incorrect</td></tr>
<tr><th>[\#13165](https://gitlab.haskell.org//ghc/ghc/issues/13165)</th>
<td>Speed up the RTS hash table</td></tr>
<tr><th>[\#13193](https://gitlab.haskell.org//ghc/ghc/issues/13193)</th>
<td>Integer (gmp) performance regression?</td></tr>
<tr><th>[\#13452](https://gitlab.haskell.org//ghc/ghc/issues/13452)</th>
<td>Lock .tix file</td></tr>
<tr><th>[\#13795](https://gitlab.haskell.org//ghc/ghc/issues/13795)</th>
<td>:kind! is not expanding type synonyms anymore</td></tr>
<tr><th>[\#14899](https://gitlab.haskell.org//ghc/ghc/issues/14899)</th>
<td>Significant compilation time regression between 8.4 and HEAD due to coverage checking</td></tr>
<tr><th>[\#15252](https://gitlab.haskell.org//ghc/ghc/issues/15252)</th>
<td>syn_arg_wraps and syn_res_wrap are only populated after typechecking</td></tr>
<tr><th>[\#15402](https://gitlab.haskell.org//ghc/ghc/issues/15402)</th>
<td>The settings and behaviour of idle GC are very confusing</td></tr>
<tr><th>[\#15540](https://gitlab.haskell.org//ghc/ghc/issues/15540)</th>
<td>GHCi does not follow the XDG Base Directory Specification</td></tr>
<tr><th>[\#15603](https://gitlab.haskell.org//ghc/ghc/issues/15603)</th>
<td>ref6 example from StaticPointers documentation doesn't type check</td></tr>
<tr><th>[\#15660](https://gitlab.haskell.org//ghc/ghc/issues/15660)</th>
<td>source file modify race leads to inconsistent error message</td></tr>
<tr><th>[\#15784](https://gitlab.haskell.org//ghc/ghc/issues/15784)</th>
<td>:doc shouldn't report \<has no documentation\> for a data constructor when it can show docs for the type constructor of the same name and type</td></tr>
<tr><th>[\#15820](https://gitlab.haskell.org//ghc/ghc/issues/15820)</th>
<td>Document the proposals process in the GHC manual</td></tr>
<tr><th>[\#15836](https://gitlab.haskell.org//ghc/ghc/issues/15836)</th>
<td>ghc-in-ghci script fails when there is a Main.hs in the top-level directory</td></tr>
<tr><th>[\#15839](https://gitlab.haskell.org//ghc/ghc/issues/15839)</th>
<td>DerivingStrategies defaulting warning has no associated enable/suppress flag</td></tr>
<tr><th>[\#15843](https://gitlab.haskell.org//ghc/ghc/issues/15843)</th>
<td>Tuple sections can't be quoted</td></tr>
<tr><th>[\#15932](https://gitlab.haskell.org//ghc/ghc/issues/15932)</th>
<td>DeriveFunctor and GeneralizedNewtypeDeriving instances never reporting as covered</td></tr>
<tr><th>[\#15935](https://gitlab.haskell.org//ghc/ghc/issues/15935)</th>
<td>TYPE is not generated by genprimops</td></tr>
<tr><th>[\#15963](https://gitlab.haskell.org//ghc/ghc/issues/15963)</th>
<td>Test suite should report timeouts as timeouts</td></tr>
<tr><th>[\#15973](https://gitlab.haskell.org//ghc/ghc/issues/15973)</th>
<td>Int used to represent target integer literals</td></tr>
<tr><th>[\#16167](https://gitlab.haskell.org//ghc/ghc/issues/16167)</th>
<td>-ddump-json doesn't work with -e</td></tr>
<tr><th>[\#16196](https://gitlab.haskell.org//ghc/ghc/issues/16196)</th>
<td>Update README.md to reflect gitlab</td></tr>
<tr><th>[\#16235](https://gitlab.haskell.org//ghc/ghc/issues/16235)</th>
<td>Hadrian devel2 builds Haddock</td></tr>
<tr><th>[\#16273](https://gitlab.haskell.org//ghc/ghc/issues/16273)</th>
<td>Hadrian turns on \`-Wno-unused-imports\` for text when using integer-simple</td></tr>
<tr><th>[\#16282](https://gitlab.haskell.org//ghc/ghc/issues/16282)</th>
<td>all-missed-specializations doesn't suggest warning</td></tr>
<tr><th>[\#16360](https://gitlab.haskell.org//ghc/ghc/issues/16360)</th>
<td>GHC fails when GHC_PACKAGE_PATH contains trailing slash</td></tr></table>


Feature requests:

<table><tr><th>[\#7275](https://gitlab.haskell.org//ghc/ghc/issues/7275)</th>
<td>Give more detailed information about PINNED data in a heap profile</td></tr>
<tr><th>[\#8109](https://gitlab.haskell.org//ghc/ghc/issues/8109)</th>
<td>Type family patterns should support as-patterns.</td></tr>
<tr><th>[\#12178](https://gitlab.haskell.org//ghc/ghc/issues/12178)</th>
<td>Allow inline pragmas on pattern synonyms</td></tr>
<tr><th>[\#12982](https://gitlab.haskell.org//ghc/ghc/issues/12982)</th>
<td>Missed constant folding oportunities</td></tr>
<tr><th>[\#15461](https://gitlab.haskell.org//ghc/ghc/issues/15461)</th>
<td>Machine accessible interface to GHCi</td></tr>
<tr><th>[\#15483](https://gitlab.haskell.org//ghc/ghc/issues/15483)</th>
<td>ghc -M requires -dep-suffix for no good reason</td></tr>
<tr><th>[\#16155](https://gitlab.haskell.org//ghc/ghc/issues/16155)</th>
<td>Pattern Synonym for Ratio</td></tr>
<tr><th>[\#16164](https://gitlab.haskell.org//ghc/ghc/issues/16164)</th>
<td>Provide bitreverse primop</td></tr></table>


Once you fix the bug or implement the new feature, make sure to write a test-case proving that you've done what you said. Then take some time to get to know and submit a code review to [Phabricator](phabricator). If the patch looks good, one of the committers will put it into the GHC codebase. Then, tackle another bug!

## Practical advice

- [This page](working-conventions/fixing-bugs) describes in more detail workflow for fixing bugs.

- Learn about our [git working conventions](working-conventions/git) and [git submodules](working-conventions/git/submodules).

- I (Richard E) use emacs to edit the code, and I have a hotkey dedicated to searching the ghc codebase, and another one dedicated to compiling ghc. This makes work on ghc much more interactive. See [the Emacs page](emacs) for more info.

## Less practical advice

- Don't get scared. GHC is a big codebase, but it makes sense when you stare at it long enough!

- Be forewarned that many pages on the GHC wiki are somewhat out-of-date. Always check the last modification date. Email if you're not sure.

## Need help?


You can email the [ ghc-devs](http://www.haskell.org/mailman/listinfo/ghc-devs) list, or Richard Eisenberg (`eir at cis . upenn . edu`), a newish GHC developer himself who would be happy to foster more participation and answer your emails.


Happy hacking!
