# The 2006 GHC hackathon: 14,15 September 2006, Portland


GHC is used by lots of people, but its implementation is rather over-centralised, even though GHC is a BSD-licensed, open-source project.  The biggest obstacle to more people getting involved is that GHC is a big, and hence intimidating, system.  The purpose of the GHC hackathon is to give a tutorial in GHC’s innards, plus time to work on some projects in a context where there are plenty of people around to help.  The objective is to substantially broaden (more numbers) and deepen (more confidence) the community of people who feel they know enough to fix and enhance GHC.  


These days, GHC is more than just a batch compiler:

- It's an interactive interpreter
- It's a (Haskell) library


The latter, in particular, opens up new possiblities, such as using GHC to parse and type-check Haskell, before analysing or transforming it with your own program.  However, GHC-as-a-library has a pretty big interface that uses a lot of data types, which will be among the things we'll describe at the hackathon.


We'll suggest some projects, but you're welcome to come along with your own wacky ideas or itches that you want to scratch, and work on them in an environment with a high-bandwidth connection to developers who really know the innards of GHC.   You could work on something individual, or in small groups.  We anticipate all being in one room, or in a tight group of rooms, so there’d be lots of informal interaction.


Also there's a good chance it'll be fun :-)
 

## Date and programme


The hackathon will be held in Portland, Oregon, on Thursday September 14th and Friday 15th, immediately before the [ International Conference on Functional Programming (ICFP'06)](http://icfp06.cs.uchicago.edu/) in Portland. There are various interesting ICFP workshops on Sat 16th ([ Generic programming](http://www.informatik.uni-bonn.de/~ralf/wgp2006.html), [ ML](http://www.cl.cam.ac.uk/ml2006/), [ Erlang](http://www.erlang.se/workshop/2006/)), but if people are enthusiastic to continue on Saturday, the space will be available.


We have in mind the following.  

- One full day of tutorial from Simon PJ & Simon M about GHC’s glorious innards (14th)

- Then one or two days of hacking on projects (15th, 16th)  


We expect that some of you will come with GHC-related projects that you want to work on, but we'll also offer a [list of possible projects](hackathon-projects), so you can come without anything in mind and choose on the day.  The list is a wiki; please add your own suggestions to it, even if you can't come!

### Venue


The Hackathon will is hosted by Galois and PSU, and will be held on the PSU campus, in the Fourth Avenue Building, 1900 SW 4th Avenue, Room 10,
near the other ICFP activities. (Note this is changed from the original planned location, near the Galois offices in Beaverton).


The room has desks, a projector, and wireless access. Bring your own laptop
(capable of building GHC). Galois may also provide some wired access, perhaps to a server with
a recent version version of GHC.

## Time


The Hackathon will be open

- 9 till 5 - Thur 14th
- 9 till 5 - Fri 15th 
- 9 till (TBD) - Sat 16th 

### Registration


If you want to come, we strongly encourage you to tell us, in two ways:

- Email jodee@…, stating you wish to attend the GHC Hackathon. 
- Add your name to the list of participants below (this page is a Wiki, so you can do that yourself).


Registration is not absolutely required, but any Pizza, manuals, and workspace will be allocated on a first come, first served basis, so registration is a good plan! 

### Cost


Zero.  But you may have to feed yourself if you don't register.

## History of Hackathons


Wikipedia has an [ article about Hackathons](http://en.wikipedia.org/wiki/Hackathon)

---

## Participants


If you want to come, please add your name to the list below.  Add a sentence or two about yourself and why you want to come. If you are working on a project to do with GHC, tell us about that too.

- [ Simon Marlow](http://www.haskell.org/~simonmar) One of the main authors of GHC, particularly the back end, run-time system, and libraries.
- [ Simon Peyton Jones](http://research.microsoft.com/~simonpj) One of the main authors of GHC, particularly the type system and Core intermediate language.

### Participant List

- Andy Adams-Moran, interested in making cross-compiling with GHC easier, and in learning more about the innards of the compiler (versus the RTS)

- [ Clifford Beshers](http://wiki.freespire.org/index.php/Freespire_Technology_Board_Home), Linspire, particularly interested in GHC as a library.
- Mathieu Boespflug Interested in implementing John Meacham's [ class alias proposal](http://repetae.net/john/recent/out/classalias.html).
- [ Björn Bringert](http://www.cs.chalmers.se/~bringert/), Chalmers University of Technology and Göteborg University, want to learn about the internals of GHC for future projects, possibly implement instance deriving for GADTs

- [ Karl Crary](http://www.cs.cmu.edu/~crary), CMU, working on a Haskell-driven typesetting system
- [ Iavor S. Diatchki](http://www.csee.ogi.edu/~diatchki), interested in Haskell for systems programming/type checking (general improvement rules)

- [ Atze Dijkstra](http://www.cs.uu.nl/wiki/Atze/WebHome), Universiteit Utrecht, working on [ EHC](http://www.cs.uu.nl/wiki/Ehc/WebHome), interested to see what can be reused/learned/shared/etc.
- Thomas DuBuisson, generally interested in GHC RTS
- Edsko de Vries, interested in type systems (I work on uniqueness typing, a substructural type system), and generic programming

- [ Kathleen Fisher](http://www.research.att.com/info/kfisher), ATT, interested in adding support for data description to Haskell

- [ Jeremy Gibbons](http://www.comlab.ox.ac.uk/jeremy.gibbons/), University of Oxford, generally curious about GHC internals - especially advanced aspects of typing
- [ Andy Gill](http://www.gill-warbington.com/home/andy), Galois, Previous contributor to GHC, wanting tools for high assurance Haskell.
- [ Paul Graunke](http://www.galois.com/), Galois, interested in formal methods (Isabelle) and GHC intermediate representations

- [ Thomas Hallgren](http://www.cse.ogi.edu/~hallgren/), dedicated Haskell programmer, interesting in redoing hOp with GHC 6.6 and have the changes included in the main source repository.

- [ Bryn Keller](http://www.xoltar.org), interested bystander. Interested in generating code for unusual targets (e.g. JavaScript, Flash, .Net, etc.).

- Markus Lauer, want to understand better how GHC works
- [ Brett Letner](http://www.galois.com/), Galois, interested in code coverage tool for Haskell.
- [ Andres Löh](http://www.iai.uni-bonn.de~/loeh/), University of Bonn, interested in type systems and generic programming
- Jake Luck, interested in interactive debugging and accessing GHC internals from embedded systems.
- Ian Lynagh, interested in all parts.

- John Matthews, Galois, interested in connecting GHC to the Isabelle theorem prover.
- Luke Maurer, recent graduate of Carleton College, just looking to get my hands dirty
- Mathew Mills, language enthusiast relatively new to Haskell.
- Brandon Moore, Yahoo. Interested in types, syntax extension.

- Ravi Nanavati, Bluespec. Interested in being able to fix GHC bugs before they burn me.

- Andrew Pimlott, Planning Systems Inc., interested in the dark corners we stumble into during application development, and space profiling.

- [ Norman Ramsey](http://www.eecs.harvard.edu/~nr), interested in either code generation or refactoring some part of the run-time system (and would really like to learn more about how the run-time system is put together, even if it's too big for a weekend hack).

- [ Chad Scherrer](http://www.pnl.gov), Pacific Northwest National Laboratory, interested in computational math and statistics
- [ Doaitse Swierstra](http://www.cs.uu.nl/wiki/Swierstra/WebHome), Universiteit Utrecht, working on [ EHC](http://www.cs.uu.nl/wiki/Ehc/WebHome), interested to see what can be reused/learned/shared/etc.

- Aaron Tomb, University of California, Santa Cruz, interested in formal verification of Haskell programs, advanced type systems, and learning about the internals of GHC
- [ Mark Tullsen](http://www.galois.com/), Galois, interested in tools for Haskell and high assurance run-time systems.

- [ Malcolm Wallace](http://www.cs.york.ac.uk/~malcolm), nhc98-hacker, wanting to steal ideas :-), particularly from the type-system.
- [ Geoffrey Washburn](http://www.cis.upenn.edu/~geoffw/), UPenn. Previous GHC contributor (original implementation of GADTs in Core). Wide variety of language interests involving expressiveness, security, and practical programming.
- [ Peng Li](http://www.cis.upenn.edu/~lipeng/), UPenn.  Interested in multiprocessor GHC, STM, RTS and I/O libraries.

- Jeffery Zhang, interested in type systems and metaprogramming.
