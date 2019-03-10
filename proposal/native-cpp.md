# Native `{-# LANGUAGE CPP #-}`

## Problem Statement


Currently, GHC relies on the system-installed [ C-preprocessor](http://en.wikipedia.org/wiki/C_preprocessor) (lateron referred to as system-`cpp`) accompanying the C compiler for implementing `{-# LANGUAGE CPP #-}`. However, this has several drawbacks:

- We already have a couple of tickets filed w/ the `cpp` keyword: 

  <table><tr><th>[\#860](https://gitlab.haskell.org//ghc/ghc/issues/860)</th>
  <td>CPP fails when a macro is used on a line containing a single quote character</td></tr>
  <tr><th>[\#1290](https://gitlab.haskell.org//ghc/ghc/issues/1290)</th>
  <td>ghc runs preprocessor too much</td></tr>
  <tr><th>[\#6132](https://gitlab.haskell.org//ghc/ghc/issues/6132)</th>
  <td>Can't use both shebang line and \#ifdef declarations in the same file.</td></tr>
  <tr><th>[\#8444](https://gitlab.haskell.org//ghc/ghc/issues/8444)</th>
  <td>Fix CPP issue with Xcode5 in integer-simple</td></tr>
  <tr><th>[\#8445](https://gitlab.haskell.org//ghc/ghc/issues/8445)</th>
  <td>Fix Xcode5 CPP issue with compiler/deSugar/DsBinds.lhs and compiler/utils/FastString.lhs</td></tr>
  <tr><th>[\#8493](https://gitlab.haskell.org//ghc/ghc/issues/8493)</th>
  <td>Can't compile happy + ghc with clang's CPP</td></tr>
  <tr><th>[\#9399](https://gitlab.haskell.org//ghc/ghc/issues/9399)</th>
  <td>CPP does not process test case enum01.hs correctly</td></tr>
  <tr><th>[\#9978](https://gitlab.haskell.org//ghc/ghc/issues/9978)</th>
  <td>DEBUG is always replaced as 1 when CPP pragma is on</td></tr>
  <tr><th>[\#10044](https://gitlab.haskell.org//ghc/ghc/issues/10044)</th>
  <td>Wrong line number reported with CPP and line beginning with \#</td></tr>
  <tr><th>[\#10146](https://gitlab.haskell.org//ghc/ghc/issues/10146)</th>
  <td>Clang CPP adds extra newline character</td></tr>
  <tr><th>[\#10230](https://gitlab.haskell.org//ghc/ghc/issues/10230)</th>
  <td>multiline literals doesn't work with CPP extension.</td></tr>
  <tr><th>[\#10543](https://gitlab.haskell.org//ghc/ghc/issues/10543)</th>
  <td>MacOS: validate fails on \\u</td></tr>
  <tr><th>[\#12391](https://gitlab.haskell.org//ghc/ghc/issues/12391)</th>
  <td>LANGUAGE CPP messes up parsing when backslash like \\\\ is at end of line (eol)</td></tr>
  <tr><th>[\#12516](https://gitlab.haskell.org//ghc/ghc/issues/12516)</th>
  <td>Preprocessing: no way to portably use stringize and string concatenation</td></tr>
  <tr><th>[\#12628](https://gitlab.haskell.org//ghc/ghc/issues/12628)</th>
  <td>__GLASGOW_HASKELL_LLVM__ is no longer an Int</td></tr>
  <tr><th>[\#14113](https://gitlab.haskell.org//ghc/ghc/issues/14113)</th>
  <td>Error message carets point at the wrong places in the presence of CPP macros</td></tr>
  <tr><th>[\#14756](https://gitlab.haskell.org//ghc/ghc/issues/14756)</th>
  <td>\`ghc -M\` doesn't emit dependencies for header files included either via CPP or CApiFFI</td></tr>
  <tr><th>[\#14757](https://gitlab.haskell.org//ghc/ghc/issues/14757)</th>
  <td>ghc recompilation check doesn't take into account headers directly included by CApiFFI</td></tr>
  <tr><th>[\#15279](https://gitlab.haskell.org//ghc/ghc/issues/15279)</th>
  <td>CPP \#includes may result in nonsensical SrcSpans</td></tr>
  <tr><th>[\#15328](https://gitlab.haskell.org//ghc/ghc/issues/15328)</th>
  <td>cpphs: internal error: evacuate(static): strange closure type 8440</td></tr>
  <tr><th>[\#15775](https://gitlab.haskell.org//ghc/ghc/issues/15775)</th>
  <td>Interpreter is treating a comment character as an identifier character.</td></tr></table>

  Fragile semantics, as the "traditional mode" in `cpp` GHC relies on is not well-specified, and therefore implementations disagree in subtle but annoying ways
  Consider all the Clang-issues GHC experienced when Apple switched from the GCC toolchain to the Clang toolchain
  Packages using `-XCPP` only tested with one system-`cpp` variant may not work with another system-`cpp` which either means more testing-cost and/or support-costs
  As system-`cpp` is designed to handle mostly C-code, it conflicts with Haskell's tokenization/syntax, specifically:
  Haskell-multi-line string literals can't be used anymore with `-XCPP` (c.f. [ SO Question](http://stackoverflow.com/questions/2549167/cpp-extension-and-multiline-literals-in-haskell) and/or [\#10230](https://gitlab.haskell.org//ghc/ghc/issues/10230))
  Haddock comments get mangled as system-`cpp` isn't aware of Haskell comments
  system-`cpp` may get confused about "unterminated" `'`s even though in Haskell they are not always used for quoting character literals. For example, Haskell allows variable names like `x'` or even `x'y`.  Another practical example from the [ int-cast](http://hackage.haskell.org/package/int-cast) package, in the following code
  \#if defined(WORD_SIZE_IN_BITS)typeinstanceIntBaseTypeInt='FixedIntTagWORD_SIZE_IN_BITStypeinstanceIntBaseTypeWord='FixedWordTagWORD_SIZE_IN_BITS\#else\#errorCannot determine bit-size of'Int'/'Word'type\#endif
  GNU `cpp` fails to macro-expand `WORD_SIZE_IN_BITS` due to the unterminated `'`-quote
  Valid Haskell operators such as `/*`, `*/` or `//` are misinterpreted by system-`cpp` as comment-starters
  Unix She-bang (`#!/usr/bin/env runghc`) Haskell scripts can't be used with `-XCPP` (c.f. [ SO Q](http://stackoverflow.com/questions/8177950/how-can-i-load-a-runhaskell-script-without-a-hs-extension-with-ghci))
  Lack of ability to extend/evolve `-XCPP` as we have no control over system-`cpp`Possible Course of ActionsPlan 0: No change (i.e. keep using relying on system-`cpp`)
  Nothing is gained, but since the issue remains unsolved, we may risk to become pressed for time (and/or cause GHC release delays) if the circumstances change suddenly and force us to act (e.g. if GCC's or Clang's `cpp` change in an incompatible way for GHC).
  Plan 1: Use custom fixed `cpp` implementation bundled with GHCOne candidate would be the C-implemented `tradcpp` (see [ http://www.freshports.org/devel/tradcpp/](http://www.freshports.org/devel/tradcpp/))
  Clang's `cpp` could be another candidate (as suggested [ here](http://permalink.gmane.org/gmane.comp.lang.haskell.cafe/116403)). Needs more investigation
  Probably not as easy to extend/evolve to be more Haskell-syntax-aware
  Plan 2: Embed Malcom's `cpphs` into GHC**Advantages**`cpphs` has been widely used, hence it's proven code
  It's already more Haskell-aware than system-`cpp``cpphs` is actively maintained
  no more `fork(2)/exec(2)`**Disadvantages**`cpphs` is licensed as "LGPLv2 w/ static linking exception" (see below)
  GHC's total licence agreement getting extended (TODO show concrete change)
  The `ghc` package would be tainted by this license augmentation. (But no more tainted than it is already, by the LGPL integer-gmp package.)
  Plan 3: Write native BSD-licenced Haskell implementation from scratch**Advantages**no more `fork(2)/exec(2)`Tailored to GHC's needs
  **Disadvantages**Requires menpower and time
  Additional long-term maintenance effort for GHC-HQ
  Plan 4: Bundle `cpphs`-based executable with GHC
  This is a variant of plan 2 where `cpphs` code remains in a separate executable.
  **Advantages**`cpphs` has been widely used, hence it's proven code
  It's already more Haskell-aware than system-`cpp``cpphs` is actively maintained
  ~~no more `fork(2)/exec(2)`~~**Disadvantages**`cpphs` is licensed as "LGPLv2 w/ static linking exception" (see below)
  GHC's total licence agreement getting extended (TODO show concrete change)
  ~~The `ghc` package would be tainted by this license augmentation~~`cpphs`'s licence in more detailThe main intent behind `cpphs`'s current licensing is to have modifications/improvements of redistributed `cpphs` binaries made publicly available to recipients of the binaries (so that they can be e.g. merged upstream if useful). This is a concern the BSD3 licence doesn't address.
  The library portion of the `cpphs` is dual-licensed (see [ http://code.haskell.org/cpphs/COPYRIGHT](http://code.haskell.org/cpphs/COPYRIGHT)):
  [ LGPL v2.1](https://www.gnu.org/licenses/lgpl-2.1.html) with static linking exception

  As a relaxation of clause 6 of the LGPL, the copyright holders of this library give permission to use, copy, link, modify, and distribute, binary-only object-code versions of an executable linked with the original unmodified Library, without requiring the supply of any mechanism to modify or replace the Library and relink (clauses 6a, 6b, 6c, 6d, 6e), provided that all the other terms of clause 6 are complied with.
  for binary distributions only: [ http://code.haskell.org/cpphs/LICENCE-commercial](http://code.haskell.org/cpphs/LICENCE-commercial) (doesn't seem useful for GHC)
  As a practical consequence of the *LGPL with static-linking-exception* (LGPL+SLE), **if no modifications are made to the `cpphs`-parts** (i.e. the LGPL+SLE covered modules) of the GHC code-base, **then there is no requirement to ship (or make available) any source code** together with the binaries, even if other parts of the GHC code-base were modified.
  LGPL w/ static linking exception is sometimes used: cf. ZeroMQ [ http://zeromq.org/area:licensing](http://zeromq.org/area:licensing)[ http://programmers.stackexchange.com/questions/179084/is-there-a-modified-lgpl-license-that-allows-static-linking](http://programmers.stackexchange.com/questions/179084/is-there-a-modified-lgpl-license-that-allows-static-linking)[ wikipedia:GPL_linking_exception](http://en.wikipedia.org/wiki/GPL_linking_exception)`ghc` package's current license
  The `ghc` package which can be linked into programs currently depends on the packages
  `array`, `base`, `binary`, `bin-package-db`, `bytestring`, `containers`, `deepseq`, `directory`, `filepath`, `ghc-prim`, `hoopl`, `hpc`, `integer-gmp`, `pretty`, `process`, `rts`, `template-haskell`, `time`, `transformers`, and `unix` whose collated `LICENSE` have been pasted as [ http://lpaste.net/131294](http://lpaste.net/131294)
          jQuery.loadStyleSheet("/trac/ghc/pygments/trac.css", "text/css");
      Download in other formats:[Plain Text](/trac/ghc/wiki/Proposal/NativeCpp?version=15&format=txt)[](http://trac.edgewall.org/)Powered by [Trac 1.2.2](/trac/ghc/about)

          By [Edgewall Software](http://www.edgewall.org/).Visit the Trac open source project at
  [http://trac.edgewall.org/](http://trac.edgewall.org/)