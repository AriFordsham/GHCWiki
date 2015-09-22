# GHC plans for 7.10.3


We have not yet decided when, or even whether, to release GHC 7.10.3.  We will do so if (but only if!) we have documented cases of "show-stoppers" in 7.10.2.  Namely, cases from users where
 

- You are unable to use 7.10.2 because of some bug
- There is no reasonable workaround, so you are truly stuck
- We know how to fix it
- The fix is not too disruptive; i.e. does not risk introducing a raft of new bugs


So please tell us if your problem falls in to this category, saying a bit about why it is important to you, and why you can't work around it.  It's worth attaching the details to the relevant ticket, add it to the manual list just below, and send a heads-up email to ghc-devs to draw attention to it.


Here's a list of the tickets that appear to fall into this show-stopping category:

- [\#10528](https://gitlab.haskell.org//ghc/ghc/issues/10528), [\#10829](https://gitlab.haskell.org//ghc/ghc/issues/10829), [\#10745](https://gitlab.haskell.org//ghc/ghc/issues/10745) (simplification on LHS and RHS of rules)
- [\#10726](https://gitlab.haskell.org//ghc/ghc/issues/10726), [\#10795](https://gitlab.haskell.org//ghc/ghc/issues/10795) ([ Ruben Moor email](https://mail.haskell.org/pipermail/glasgow-haskell-users/2015-September/026035.html))
- [\#10568](https://gitlab.haskell.org//ghc/ghc/issues/10568), [\#10672](https://gitlab.haskell.org//ghc/ghc/issues/10672) ([ Luke Iannini email](https://mail.haskell.org/pipermail/ghc-devs/2015-September/009973.html))
- [\#10726](https://gitlab.haskell.org//ghc/ghc/issues/10726) and [\#10777](https://gitlab.haskell.org//ghc/ghc/issues/10777) (Windows): some pressure to get these patches into 7.10.3


Possible (not world ending, but probably not easy to workaround and breaks):

- [\#9238](https://gitlab.haskell.org//ghc/ghc/issues/9238) (Levent Erkok): mentioned [ reddit](https://www.reddit.com/r/haskell/comments/3kwsu4/ghcdev_anyone_need_ghc_7103) as a [ bug for SBV](https://github.com/LeventErkok/sbv/issues/138#issuecomment-139804285)


See [milestone:7.10.3](/trac/ghc/milestone/7.10.3) and [ Active tickets](https://ghc.haskell.org/trac/ghc/query?status=infoneeded&status=merge&status=new&status=patch&group=status&milestone=7.10.3) for more.

## Tickets slated for 7.10.3

## Status: closed (60 matches)

<table><tr><th>Ticket (Ticket query: milestone: 7.10.3, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: id)</th>
<th>Type (Ticket query: milestone: 7.10.3, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: type)</th>
<th>Summary (Ticket query: milestone: 7.10.3, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: summary)</th>
<th>Priority (Ticket query: milestone: 7.10.3, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, desc: 1, order: priority)</th>
<th>Owner (Ticket query: milestone: 7.10.3, group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: owner)</th></tr>
<tr><th>[\#7830](https://gitlab.haskell.org//ghc/ghc/issues/7830)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Error: operand out of range](https://gitlab.haskell.org//ghc/ghc/issues/7830)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
                    </th>
<th></th></tr>
<tr><th>[\#10489](https://gitlab.haskell.org//ghc/ghc/issues/10489)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Panic in TcEvidence due to wrong role](https://gitlab.haskell.org//ghc/ghc/issues/10489)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
                    </th>
<th></th></tr>
<tr><th>[\#10826](https://gitlab.haskell.org//ghc/ghc/issues/10826)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[\[Security\] Safe Haskell can be bypassed via annotations](https://gitlab.haskell.org//ghc/ghc/issues/10826)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
                    </th>
<th>kanetw</th></tr>
<tr><th>[\#11061](https://gitlab.haskell.org//ghc/ghc/issues/11061)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GHC 7.10.3 RC1: build broken on OS X](https://gitlab.haskell.org//ghc/ghc/issues/11061)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      highest
                    </th>
<th></th></tr>
<tr><th>[\#3242](https://gitlab.haskell.org//ghc/ghc/issues/3242)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GHCi linker does not correctly locate static libraries under Windows](https://gitlab.haskell.org//ghc/ghc/issues/3242)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th>Phyx-</th></tr>
<tr><th>[\#9297](https://gitlab.haskell.org//ghc/ghc/issues/9297)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Packages linked against certain Windows .dll files give warnings at runtime](https://gitlab.haskell.org//ghc/ghc/issues/9297)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th>simonmar</th></tr>
<tr><th>[\#10375](https://gitlab.haskell.org//ghc/ghc/issues/10375)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[arm: ghci hits an illegal instruction](https://gitlab.haskell.org//ghc/ghc/issues/10375)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#10438](https://gitlab.haskell.org//ghc/ghc/issues/10438)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GHC 7.10.1 panic due to PartialTypeSignatures, TypeFamilies, and local bindings](https://gitlab.haskell.org//ghc/ghc/issues/10438)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#10516](https://gitlab.haskell.org//ghc/ghc/issues/10516)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[PolyKinds results in incorrect reporting of type synonym parameter count](https://gitlab.haskell.org//ghc/ghc/issues/10516)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#10528](https://gitlab.haskell.org//ghc/ghc/issues/10528)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[compile time performance regression with OverloadedStrings and Text](https://gitlab.haskell.org//ghc/ghc/issues/10528)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#10549](https://gitlab.haskell.org//ghc/ghc/issues/10549)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[floatExpr tick break\<2\>](https://gitlab.haskell.org//ghc/ghc/issues/10549)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th>bgamari</th></tr>
<tr><th>[\#10672](https://gitlab.haskell.org//ghc/ghc/issues/10672)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GHCi linker does not understand C++ exception tables on Windows](https://gitlab.haskell.org//ghc/ghc/issues/10672)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th>Phyx-</th></tr>
<tr><th>[\#10689](https://gitlab.haskell.org//ghc/ghc/issues/10689)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[compiling singletons-1.1.2.1 as -O1 -fspec-constr fails as 'Template variable unbound in rewrite rule'](https://gitlab.haskell.org//ghc/ghc/issues/10689)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#10747](https://gitlab.haskell.org//ghc/ghc/issues/10747)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Infix pattern synonyms fail to parse (regression)](https://gitlab.haskell.org//ghc/ghc/issues/10747)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th>mpickering</th></tr>
<tr><th>[\#10829](https://gitlab.haskell.org//ghc/ghc/issues/10829)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Simplification in the RHS of rules](https://gitlab.haskell.org//ghc/ghc/issues/10829)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#10934](https://gitlab.haskell.org//ghc/ghc/issues/10934)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Iface type variable out of scope](https://gitlab.haskell.org//ghc/ghc/issues/10934)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#10997](https://gitlab.haskell.org//ghc/ghc/issues/10997)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Pattern synonym causes Iface error.](https://gitlab.haskell.org//ghc/ghc/issues/10997)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#11003](https://gitlab.haskell.org//ghc/ghc/issues/11003)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Suggested fix for incorrect directory permissions is wrong](https://gitlab.haskell.org//ghc/ghc/issues/11003)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#11127](https://gitlab.haskell.org//ghc/ghc/issues/11127)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Update cabal submodule to 1.22.5](https://gitlab.haskell.org//ghc/ghc/issues/11127)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#1407](https://gitlab.haskell.org//ghc/ghc/issues/1407)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Add the ability to :set -l{foo} in .ghci files](https://gitlab.haskell.org//ghc/ghc/issues/1407)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>archblob</th></tr>
<tr><th>[\#6037](https://gitlab.haskell.org//ghc/ghc/issues/6037)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Compile-time crash with sources with non-representable unicode characters](https://gitlab.haskell.org//ghc/ghc/issues/6037)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>snoyberg</th></tr>
<tr><th>[\#8652](https://gitlab.haskell.org//ghc/ghc/issues/8652)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Cross-compiling broken for ARM/Linux target](https://gitlab.haskell.org//ghc/ghc/issues/8652)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9238](https://gitlab.haskell.org//ghc/ghc/issues/9238)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Negative zero broken](https://gitlab.haskell.org//ghc/ghc/issues/9238)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9861](https://gitlab.haskell.org//ghc/ghc/issues/9861)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[ghc  readme provides out of date git clone directions](https://gitlab.haskell.org//ghc/ghc/issues/9861)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9878](https://gitlab.haskell.org//ghc/ghc/issues/9878)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Static pointers in GHCi cause panic](https://gitlab.haskell.org//ghc/ghc/issues/9878)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>Phyx-</th></tr>
<tr><th>[\#9907](https://gitlab.haskell.org//ghc/ghc/issues/9907)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>["Unknown PEi386 section name \`.text$printf'" error in GHCi on Windows](https://gitlab.haskell.org//ghc/ghc/issues/9907)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>Phyx-</th></tr>
<tr><th>[\#9970](https://gitlab.haskell.org//ghc/ghc/issues/9970)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      feature request
                    </th>
<th>[Export more types in GHC.RTS.Flags](https://gitlab.haskell.org//ghc/ghc/issues/9970)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>RyanGlScott</th></tr>
<tr><th>[\#10409](https://gitlab.haskell.org//ghc/ghc/issues/10409)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Binary compiled with ghc-7.10 amd64/linux to aarch64/linux cross compiler segfaults.](https://gitlab.haskell.org//ghc/ghc/issues/10409)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>erikd</th></tr>
<tr><th>[\#10435](https://gitlab.haskell.org//ghc/ghc/issues/10435)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[catastrophic exception-handling disablement on Windows Server 2008 R2](https://gitlab.haskell.org//ghc/ghc/issues/10435)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>simonmar</th></tr>
<tr><th>[\#10495](https://gitlab.haskell.org//ghc/ghc/issues/10495)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Poor error message for Coercible constraint unsatisfiability](https://gitlab.haskell.org//ghc/ghc/issues/10495)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>goldfire</th></tr>
<tr><th>[\#10498](https://gitlab.haskell.org//ghc/ghc/issues/10498)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>["if ... then \\case -\> else ..." causes a "missing else clause" error](https://gitlab.haskell.org//ghc/ghc/issues/10498)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#10563](https://gitlab.haskell.org//ghc/ghc/issues/10563)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GHC 7.10.1 Win7 x86_64 crash when building reflex-dom-0.1.1](https://gitlab.haskell.org//ghc/ghc/issues/10563)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#10568](https://gitlab.haskell.org//ghc/ghc/issues/10568)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Regression from 7.8.4, loading GLUT into GHCI fails on the Mac](https://gitlab.haskell.org//ghc/ghc/issues/10568)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>darchon</th></tr>
<tr><th>[\#10590](https://gitlab.haskell.org//ghc/ghc/issues/10590)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[RTS failing with removeThreadFromDeQueue: not found message](https://gitlab.haskell.org//ghc/ghc/issues/10590)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>slyfox</th></tr>
<tr><th>[\#10596](https://gitlab.haskell.org//ghc/ghc/issues/10596)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Template Haskell : getQ and putQ doesn't work](https://gitlab.haskell.org//ghc/ghc/issues/10596)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#10660](https://gitlab.haskell.org//ghc/ghc/issues/10660)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[.dyn_o isn't generated for .hsig files with -dynamic-too](https://gitlab.haskell.org//ghc/ghc/issues/10660)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>spinda</th></tr>
<tr><th>[\#10665](https://gitlab.haskell.org//ghc/ghc/issues/10665)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[INLINE breaks rewrite rules when '-g' is used](https://gitlab.haskell.org//ghc/ghc/issues/10665)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#10667](https://gitlab.haskell.org//ghc/ghc/issues/10667)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>['-g' option generates invalid assembly when '\*/\*' operator is used](https://gitlab.haskell.org//ghc/ghc/issues/10667)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#10668](https://gitlab.haskell.org//ghc/ghc/issues/10668)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Missing brackets in import hint with TypeOperators](https://gitlab.haskell.org//ghc/ghc/issues/10668)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>thomasw</th></tr>
<tr><th>[\#10700](https://gitlab.haskell.org//ghc/ghc/issues/10700)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[include/stg/Prim.h isn't C++ compatible](https://gitlab.haskell.org//ghc/ghc/issues/10700)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>rasen</th></tr>
<tr><th>[\#10713](https://gitlab.haskell.org//ghc/ghc/issues/10713)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Type family not reducing over data family](https://gitlab.haskell.org//ghc/ghc/issues/10713)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>goldfire</th></tr>
<tr><th>[\#10715](https://gitlab.haskell.org//ghc/ghc/issues/10715)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Possible regression in Coercible a (X a) between 7.8 and 7.10](https://gitlab.haskell.org//ghc/ghc/issues/10715)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>goldfire</th></tr>
<tr><th>[\#10745](https://gitlab.haskell.org//ghc/ghc/issues/10745)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Error in fusion when compiling Data.Yaml](https://gitlab.haskell.org//ghc/ghc/issues/10745)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#10772](https://gitlab.haskell.org//ghc/ghc/issues/10772)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Type operator variable in prefix notation fails](https://gitlab.haskell.org//ghc/ghc/issues/10772)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#10795](https://gitlab.haskell.org//ghc/ghc/issues/10795)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Upgrade gcc in 7.10](https://gitlab.haskell.org//ghc/ghc/issues/10795)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#10810](https://gitlab.haskell.org//ghc/ghc/issues/10810)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Data constructor operators mis-printed in Template Haskell](https://gitlab.haskell.org//ghc/ghc/issues/10810)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#10817](https://gitlab.haskell.org//ghc/ghc/issues/10817)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Looping default associated type family without UndecidableInstances](https://gitlab.haskell.org//ghc/ghc/issues/10817)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#10855](https://gitlab.haskell.org//ghc/ghc/issues/10855)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GHC rejects code that Haskell 2010 report accepts](https://gitlab.haskell.org//ghc/ghc/issues/10855)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#10870](https://gitlab.haskell.org//ghc/ghc/issues/10870)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[PPC.Ppr: Shift by 32 bits is not allowed.](https://gitlab.haskell.org//ghc/ghc/issues/10870)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#10879](https://gitlab.haskell.org//ghc/ghc/issues/10879)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[base is not included in the haddock index](https://gitlab.haskell.org//ghc/ghc/issues/10879)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#10882](https://gitlab.haskell.org//ghc/ghc/issues/10882)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Fix target triple for Arm](https://gitlab.haskell.org//ghc/ghc/issues/10882)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#10899](https://gitlab.haskell.org//ghc/ghc/issues/10899)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Polytype accepted in RHS of default associated type](https://gitlab.haskell.org//ghc/ghc/issues/10899)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>goldfire</th></tr>
<tr><th>[\#10904](https://gitlab.haskell.org//ghc/ghc/issues/10904)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[C finalizer may be called on re-used memory](https://gitlab.haskell.org//ghc/ghc/issues/10904)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#10924](https://gitlab.haskell.org//ghc/ghc/issues/10924)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Template variable unbound in rewrite rule](https://gitlab.haskell.org//ghc/ghc/issues/10924)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#11055](https://gitlab.haskell.org//ghc/ghc/issues/11055)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[GHC 7.8.4 crash on ARM while building Stack 0.1.7](https://gitlab.haskell.org//ghc/ghc/issues/11055)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#11064](https://gitlab.haskell.org//ghc/ghc/issues/11064)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Call Arity has the trivial application case wrong](https://gitlab.haskell.org//ghc/ghc/issues/11064)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>nomeata</th></tr>
<tr><th>[\#11076](https://gitlab.haskell.org//ghc/ghc/issues/11076)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Demand information of foreign calls is wrong](https://gitlab.haskell.org//ghc/ghc/issues/11076)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#10390](https://gitlab.haskell.org//ghc/ghc/issues/10390)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Constraint order must match with RankNTypes](https://gitlab.haskell.org//ghc/ghc/issues/10390)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th></th></tr>
<tr><th>[\#10413](https://gitlab.haskell.org//ghc/ghc/issues/10413)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      task
                    </th>
<th>[Incorrect offsets for array size indexing](https://gitlab.haskell.org//ghc/ghc/issues/10413)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th>fryguybob</th></tr>
<tr><th>[\#10476](https://gitlab.haskell.org//ghc/ghc/issues/10476)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      bug
                    </th>
<th>[Wrong ar during cross-compilation](https://gitlab.haskell.org//ghc/ghc/issues/10476)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th>thomie</th></tr></table>

## Tickets marked merge with no milestone

<table><tr><th>Ticket (Ticket query: status: merge, milestone: , group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, desc: 1, order: id)</th>
<th>Type (Ticket query: status: merge, milestone: , group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: type)</th>
<th>Summary (Ticket query: status: merge, milestone: , group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: summary)</th>
<th>Priority (Ticket query: status: merge, milestone: , group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: priority)</th>
<th>Owner (Ticket query: status: merge, milestone: , group: status, max: 0, col: id, col: type, col: summary, col: priority, col: owner, order: owner)</th></tr>
<tr><th>
            No tickets found
          </th>
<th></th>
<th></th>
<th></th>
<th></th></tr></table>