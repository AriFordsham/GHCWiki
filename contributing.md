# Contributing to GHC


GHC is a BSD-licensed open-source project, and we welcome your help in making it better. This page and the side bar on the left have pointers to information you'll need.

- [Information for newcomers](contributing#newcomers-to-ghc). This the first stop for those people who say, "I want to contribute to GHC, but I don't know quite where to begin." Begin here.

- [How to contribute a patch to GHC](working-conventions/fixing-bugs). For [adding features](working-conventions/adding-features), there are a few extra steps to follow.

- [How to propose a change to the libraries](http://haskell.org/haskellwiki/Library_submissions)

## Working conventions

- **Using Git**: read [how to use git with GHC](working-conventions/git). Information about our submodule setup is in [WorkingConventions/Git/Submodules](working-conventions/git/submodules), and some useful Git tricks are in [WorkingConventions/Git/Tricks](working-conventions/git/tricks).

- **Using Phabricator**: we used to use Phabricator as a code review tool; here are [our Phabricator guidance notes](phabricator).

- **Releases and branches**: Our conventions for making releases and how the branches are managed: [Releases](working-conventions/releases)

- **Useful tools**: [Various tools](working-conventions/useful-tools) which exist to make working on GHC more pleasant.

- **Using the Bug Tracker**: see [Using the Bug Tracker](working-conventions/bug-tracker)

- **Coding style**: When you are editing GHC's source code, please follow our coding guidelines:

  - [Coding style in the compiler](commentary/coding-style)
  - [Coding style in the runtime system](commentary/rts/conventions)

- **Licensing**: make sure you are familiar with GHC's [Licensing](licensing).  Unless you say otherwise, we will assume that if you submit a contribution to GHC, then you intend to supply it to us under the same license as the existing code. However, we do not ask for copyright attribution; you retain copyright on any contributions you make, so feel free to add your copyright to the top of any file in which you make non-trivial changes.

- **For boot libraries**: GHC ships with a number of [boot libraries](commentary/libraries/version-history) maintained outside of GHC itself. Maintainers of such libraries should read [WorkingConventions/BootLibraries](working-conventions/boot-libraries) for guidance on how to maintain their libraries.

## Tips and Tricks

- [Loading GHC into GHCi](building/in-ghci) can provide a more iterative development experience. 

- To have an easier time looking up tickets and searching trac, use [the browser tips page](browser-tips) to make your search and lookups for Trac tickets substantially easier.

- If you use Emacs, see [Emacs](emacs) for some useful stuff to put in your `.emacs` file.

- If you have lots of Haskell installations, you may find Edsko's blog post [Comprehensive Haskell Sandboxes](http://www.edsko.net/2013/02/10/comprehensive-haskell-sandboxes/) useful.

## Newcomers to GHC


While the [building guide](building), [working conventions](working-conventions), [commentary](commentary) and [debugging](debugging) pages (always linked from the left sidebar) have great information that can come in handy while you're working on your first, or first several patches, this section is intended to have the details you will need to get rolling.


If you have any questions along the way don't hesitate to reach out to the community. There are people on the [mailing lists and IRC](mailing-lists-and-irc) who will gladly help you (although you may need to be patient). Don't forget that all GHC developers are still learning; your question is never too silly to ask.

### First steps

- See [Building/QuickStart](building/quick-start) to get started building GHC. Expect it all to take roughly between 30 minutes and a couple of hours, depending on your CPU speed, and the number of jobs you can run in parallel. Note that [building older versions of GHC may require having an older version of GHC on your path](Building/Preparation/Tools).

- While you are waiting for your build to finish, orient yourself to the general architecture of GHC. This [article](http://www.aosabook.org/en/ghc.html) is written by two of the chief architects of GHC, Simon Marlow and Simon Peyton-Jones, is excellent and current (2012).

- After a successful build, you should have your brand new compiler in `./inplace/bin/ghc-stage2`. (GHCi is launched with `./inplace/bin/ghc-stage2 --interactive`). Try it out.

### Finding a ticket


Now that you can build GHC, let's get hacking. But first, you'll need to identify a goal. GHC's Trac tickets are a great place to find starting points. You are encouraged to ask for a starting point on IRC or the `ghc-devs`[mailing list](mailing-lists-and-irc). There someone familiar with the process can help you find a ticket that matches your expertise and help you when you get stuck.


If you want to get a taste for possible starting tasks, below is a list of tickets that appear to be "low-hanging fruit" -- things that might be reasonable for a newcomer to GHC hacking. Of course, we can't ever be sure of how hard a task is before doing it, so apologies if one of these is too hard.


You can add tickets to this list by giving them the `newcomer` Trac keyword.

**Bugs:**

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

**Feature requests:**

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

**Tasks:**

<table><tr><th>[\#4960](https://gitlab.haskell.org//ghc/ghc/issues/4960)</th>
<td>Better inlining test in CoreUnfold</td></tr>
<tr><th>[\#10068](https://gitlab.haskell.org//ghc/ghc/issues/10068)</th>
<td>Make the runtime reflection API for names, modules, locations more systematic</td></tr>
<tr><th>[\#11610](https://gitlab.haskell.org//ghc/ghc/issues/11610)</th>
<td>Remove IEThingAll constructor from IE datatype</td></tr>
<tr><th>[\#12619](https://gitlab.haskell.org//ghc/ghc/issues/12619)</th>
<td>Allow users guide to be built independently from GHC</td></tr>
<tr><th>[\#12687](https://gitlab.haskell.org//ghc/ghc/issues/12687)</th>
<td>Add a flag to control constraint solving trace</td></tr>
<tr><th>[\#12822](https://gitlab.haskell.org//ghc/ghc/issues/12822)</th>
<td>Cleanup GHC verbosity flags</td></tr>
<tr><th>[\#13698](https://gitlab.haskell.org//ghc/ghc/issues/13698)</th>
<td>Add a more complete example for the special SPEC argument to the user guide</td></tr>
<tr><th>[\#13892](https://gitlab.haskell.org//ghc/ghc/issues/13892)</th>
<td>Add some benchmarks to nofib from Andras Kovac's Eff benchmarks</td></tr>
<tr><th>[\#13923](https://gitlab.haskell.org//ghc/ghc/issues/13923)</th>
<td>Add a suppression flag to stop Typeable bindings being emitted with -ddump-simpl</td></tr>
<tr><th>[\#14023](https://gitlab.haskell.org//ghc/ghc/issues/14023)</th>
<td>Split up glasgow_exts.rst</td></tr>
<tr><th>[\#14099](https://gitlab.haskell.org//ghc/ghc/issues/14099)</th>
<td>Document fundeps</td></tr>
<tr><th>[\#15651](https://gitlab.haskell.org//ghc/ghc/issues/15651)</th>
<td>Check if some auto apply code is dead and remove if appropriate.</td></tr>
<tr><th>[\#15821](https://gitlab.haskell.org//ghc/ghc/issues/15821)</th>
<td>Implement more constant folding for Naturals</td></tr>
<tr><th>[\#15929](https://gitlab.haskell.org//ghc/ghc/issues/15929)</th>
<td>Explore whether adding XRay attributes to LLVM IR is worthwhile</td></tr>
<tr><th>[\#16052](https://gitlab.haskell.org//ghc/ghc/issues/16052)</th>
<td>Core optimizations for memset on a small range</td></tr>
<tr><th>[\#16062](https://gitlab.haskell.org//ghc/ghc/issues/16062)</th>
<td>Improve -dynamic-too progress messages</td></tr>
<tr><th>[\#16126](https://gitlab.haskell.org//ghc/ghc/issues/16126)</th>
<td>Make -threaded the default</td></tr>
<tr><th>[\#16277](https://gitlab.haskell.org//ghc/ghc/issues/16277)</th>
<td>Make JUnit report stdout/stderr in more cases</td></tr></table>

### Advice

- Read up on the steps you are expected to take for [contributing a patch to GHC](working-conventions/fixing-bugs).

- Need help? You can email the [ghc-devs](http://www.haskell.org/mailman/listinfo/ghc-devs) list, or ask on IRC in `#ghc`.

- Don't get scared. GHC is a big codebase, but it makes sense when you stare at it long enough!

- Don't hesitate to ask questions. We have all been beginners at some point and understand that diving in to GHC can be a challenge. Asking questions will help you make better use of your hacking time.

- Be forewarned that many pages on the GHC Wiki are somewhat out-of-date. Always check the last modification date. Email `ghc-devs` if you're not sure.

- You may want to look at these "how it went for me" blog posts.

  - [Hacking on GHC (is not that hard)](http://rawgit.com/gibiansky/4c54f767bf21a6954b23/raw/67c62c5555f40c6fb67b124307725df168201361/exp.html) by Andrew Gibiansky
  - [Contributing to GHC](http://anniecherkaev.com/projects/contributing-to-ghc) by Annie Cherkaev
  - [Contributing to GHC via Phabricator](https://medium.com/@zw3rk/contributing-to-ghc-290653b63147) by Moritz Angermann

- There is a blog post series by Stephen Diehl that provides an overview of many important data structures and contains links to other sources of information: [Dive into GHC](http://www.stephendiehl.com/posts/ghc_01.html)


Happy hacking!
