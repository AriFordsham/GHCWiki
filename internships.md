# Internships on Haskell and GHC, at Microsoft Research, Cambridge

[ Microsoft Research Cambridge](http://research.microsoft.com/aboutmsr/labs/cambridge/) now runs a year-round [ internship programme](http://research.microsoft.com/aboutmsr/jobs/internships/).  You can apply for an internship slot in any area covered by the lab, but Simon and I are of course keen to attract strong applications from people interested in functional programming, Haskell, and GHC.


The next available slots will be for January 2009 or thereabouts.  We'll consider applications for January in the summer of 2008.

## What is an internship?


An internship is a paid post, for three months (occasionally up to 6), at Microsoft Research in Cambridge (MSRC).  You get to work on a project agreed, usually in advance, with your MSRC sponsor.  Both parties benefit.  You get to work in a leading-edge research lab, with fantastic people floating around all the time.  We get the benefit of your hard work, and perhaps the start of a collaborative relationship.


Almost all interns are in the middle of their PhD.  This is not an absolute requirement; for example, post-PhD is certainly possible.  However, if you are, say, an undergraduate you'd need to be exceptional to beat out the PhD applicants.  


In principle, Microsoft owns any intellectual property you generate, but this isn't relevant for an open-source project like GHC.  You're also strongly encouraged to publish your work as a paper, often written jointly with your sponsor.

## Who is eligible?


Internships are aimed primarily at students currently studying for a PhD.  It's not a cast-iron requirement, but if you are pre-PhD you'd need to argue that you were rather exceptional in some way.

## Internships to work on Haskell and GHC


Simon and I always have a zillion projects that we'd like to see done, but do not have time to do.  An internship is a chance for you to work closely with us --- we usually meet with interns daily, and we never have more than one at a time --- on one of these projects.  (Or you can suggest a project of your own.)


Internship projects should have some research content, and ideally lead to a paper.  It's not just hacking.


To give you some idea, here are some past projects (in no particular order):

- Kevin Donnelly changed GHC's intermediate language to support equality constraints
- Roshan James wrote a parallel garbage collector
- Geoff Washburn made the first implementation of GADTs in GHC
- Dimitrios Vitytonis worked on type inference for impredicative polymorphism
- Krasimir Angelov completed and released Visual Haskell
- Michael Adams worked on refactoring the code generator
- Ben Lippmeier implemented a new register allocator
- Bernie Pope implemented the GHCi debugger

## Current projects


Here is a list of some possible future projects we have in mind.  But feel free to suggest your own!

### Programming environment and tools

- Update/improve [ Visual Haskell](http://www.haskell.org/visualhaskell) to use the (free) [ Visual Studio Shell](http://msdn2.microsoft.com/en-us/vsx2008/products/bb933751.aspx).

- Work on the [ GHCi debugger](http://www.haskell.org/haskellwiki/Ghci/Debugger)

- Make GHC work with [ GCSpy](http://research.sun.com/projects/gcspy/), a generic heap visualiser tool.

- Maintaining an explicit call stack [ExplicitCallStack](explicit-call-stack)

### Turning GHC into a platform


Projects aimed at making GHC into a user-extensible plug-in platform, and less of a monolithic compiler.

- **Allow much finer and more modular control over the way in which rewrite rules and inlining directives are ordered**.  See this [ email thread](http://www.haskell.org/pipermail/haskell-cafe/2008-January/038196.html)

- **Support dynamically-linked Core-to-Core plug-ins**, so that people can add passes simply by writing a Core-to-Core function, and dynamically linking it to GHC.  This would need to be supported by an extensible mechanism like ``attributes`` in mainstream OO languages, so that programmers can add declarative information to the source program that guides the transformation pass.  Likewise the pass might want to construct information that is accessible later.  This mechanism could obviously be used for optimisations, but also for program verifiers, and perhaps also for domain-specific code generation (the pass generates a GPU file, say, replacing the Core code with a foreign call to the GPU program). See [Plugins](plugins) for some early thoughts on this.

- **Improve the GHC API**, whereby you can import GHC as a library.  We make improvements now and then, but it would benefit from some sustained attention.  A particular project would be to port the Haskell refactorer [ HaRE](http://www.cs.kent.ac.uk/projects/refactor-fp/hare.html) to use the GHC API.

### Types

- **Allow unboxed tuples as function arguments**.   Currently unboxed tuples are second class; fixing this would be a nice simplification.

- **Implement overlap and exhaustiveness checking for pattern matching**.  GHC's current overlap and exhaustiveness checker is old and inadequate.  Furthermore, it takes no account of GADTs and type families. 

- **Extend kinds beyond \* and k1-\>k2**.  With GADTs etc we clearly want to have kinds like `Nat`, so that advanced hackery at the type level can be done in a typed language; currently it's all effectively untyped.  A neat approach would be to re-use any data type declaration as a kind declaration.

- **Extensible constraint domains**.  Andrew Kennedy shows how to incorporate [ dimensional analysis](http://research.microsoft.com/~akenn/units/index.html) into an ML-like type system.  Maybe we could do an extensible version of this, so that it wasn't restricted to dimensions.  Integer arithmetic is another obvious domain.  

- Implement [ John Meacham's class alias proposal](http://repetae.net/john/recent/out/classalias.html)

### Parallel stuff

- Experiment with multiprocessor Haskell and/or STM by building and measuring applications, investigate improvements
- Continue work on parallel GC: particularly independent minor-generation collections.

### Other stuff

- Back end and code generation.  This is an active area at the moment, but there is sure to be more to do.  See suggestions in [BackEndNotes](back-end-notes)

### Build system

- Build a Windows-native version of GHC (using MS tools instead of gcc).

## How to apply


You can apply ANY TIME.  Internships are not just the summer months. Contact one of us (simonpj@…, marlowsd@…) in the first instance (or another sponsor if they are more suitable).


To apply, follow the instructions at [ http://research.microsoft.com/aboutmsr/jobs/internships/](http://research.microsoft.com/aboutmsr/jobs/internships/), **and email one of us to say that you have done so**. Do not omit the latter step; across MSR there are hundreds of applicants, and if we don't hear from you personally we may miss your application.  If you don't get a reply to your email, try again - the spam filter may have caught it.


A CV is typically a boring, dry kind of thing, consisting entirely of data (education, employment, publications, talks etc etc).  Yes, we need that, but **please also include a final section entitled "Personal research statement"**.  This should address the following questions:

- What really excites you and fills you with wild enthusiasm?
- What you are proud of in your existing track record?
- What you are working on now?
- What sort of thing you would like to do if you came here?
- (Brutal, but frank.)  Why should we pick you?


Typically your personal research statement will be a couple of pages long, but it's really up to you.  You don't need to address the above questions in the order given, and you can range more widely if you want.  (You should find the same writeup useful for other purposes.)  Write in the first person, and try to convey something of yourself rather than just data. 


MSRC has lots of other interesting work on programming languages too: F\#, security, etc.  Check it out [ here](http://research.microsoft.com/aboutmsr/labs/cambridge/).

## Timescale


So long as we have enough budget, we run the following four internship "slots":

- Jan-Mar
- Apr-Jun
- July-Sept
- Oct-Dec


We usually take decisions about a particular slot three months before it begins (e.g. December, for the Apr-Jun slot).


Simon Peyton Jones and Simon Marlow
