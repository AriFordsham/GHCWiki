# Internships on Haskell and GHC, at Microsoft Research, Cambridge

[ Microsoft Research Cambridge](http://research.microsoft.com/aboutmsr/labs/cambridge/) now runs a year-round [ internship programme](http://research.microsoft.com/aboutmsr/jobs/internships/).  You can apply for an internship slot in any area covered by the lab, but Simon and I are of course keen to attract strong applications from people interested in functional programming, Haskell, and GHC.

## What is an internship?


An internship is a paid post, for three months (occasionally up to 6), at Microsoft Research in Cambridge (MSRC).  You get to work on a project agreed, usually in advance, with your MSRC sponsor.  Both parties benefit.  You get to work in a leading-edge research lab, with fantastic people floating around all the time.  We get the benefit of your hard work, and perhaps the start of a collaborative relationship.


In principle, Microsoft owns any intellectual property you generate, but this isn't relevant for an open-source project like GHC.  You're also strongly encouraged to publish your work as a paper, often written jointly with your sponsor.

## Who is eligible?


Internships are aimed primarily at students currently studying for a PhD.  That's the norm, but it's not a cast-iron requirement.

## Internships to work on Haskell and GHC


Simon and I always have a zillion projects that we'd like to see done, but do not have time to do.  An internship is a chance for you to work closely with us --- we usually meet with interns daily, and we never have more than one at a time --- on one of these projects.  (Or you can suggest a project of your own.)


To give you some idea, here are some past projects

- Kevin Donnelly is changing GHC's intermediate language to support equality constraints.
- Roshan James is writing a parallel garbage collection
- Geoff Washburn made the first implementation of GADTs in GHC
- Dimitrios Vitytonis worked on type inference for impredicative polymorphism
- Krasimir Angelov completed and released Visual Haskell. 


Here is a list of some possible future projects we have in mind:

- Update/improve [ Visual Haskell](http://www.haskell.org/visualhaskell)
- Work on the [ GHCi debugger](http://www.haskell.org/haskellwiki/Ghci/Debugger)
- Refactor GHC's code generator into two phases: (a) generate `C--` with native procedure calls,
  (b) perform CPS conversion, as a `C--` to `C--` conversion, to generate code ready 
  for the existing native code generators.
- Improve code generation; see suggestions in [BackEndNotes](back-end-notes)
- Implement [ John Meacham's class alias proposal](http://repetae.net/john/recent/out/classalias.html)
- Implement semi-tagging or other optimisation improvements in GHC
- Experiment with multiprocessor Haskell and/or STM by building and measuring applications, investigate improvements
- Build a Windows-native version of GHC (using MS tools instead of gcc).


MSRC has lots of other interesting work on programming languages too: F\#, security, etc.  Check it out [ here](http://research.microsoft.com/aboutmsr/labs/cambridge/).

## When can I apply?


You can apply ANY TIME.  Internships are not just the summer months. Contact one of us (simonpj@…, simonmar@…) in the first instance (or another sponsor if they are more suitable).


To apply, follow the instructions at [ http://research.microsoft.com/aboutmsr/jobs/internships/](http://research.microsoft.com/aboutmsr/jobs/internships/), **and** email one of us to say that you have done so.


Simon Peyton Jones and Simon Marlow
