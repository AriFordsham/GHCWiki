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


The Hackathon will be held at the [ Kingstad Center](http://www.kingstad.com), near the Galois offices
on the 14th and 15th. Any additional hacking on the 16th will be held at the Galois offices.


The Kingstand venue has a large room, with desks, a projector, and wireless access. Bring your own laptop
(capable of building GHC). Galois may also provide some wired access, perhaps to a server with
a recent version version of GHC.


On Saturday, we will use the Galois offices, which has several conference rooms and wireless access for
registered GHC Hackathon attendees.


Both the Kingstad and the Galois offices is located right next to a light-rail line, allowing easy commuting from
down town and the ICFP venue hotel. 

[ Directions to the Kingstad Center](http://www.kingstad.com/locations/beaverton_fac.html)

<table><tr><th> Thur 14th, Fri 15th </th>
<th>  Address: </th>
<th>  15450 SW Millikan Way
</th></tr>
<tr><th></th>
<th></th>
<th> Beaverton, OR 97007
</th></tr></table>

[ Directions to Galois](http://www.galois.com/files/Directions.pdf)

<table><tr><th>   Sat 16th </th>
<th>  Address: </th>
<th> 12725 SW Millikan Way
</th></tr>
<tr><th></th>
<th></th>
<th> Suite 290
</th></tr>
<tr><th></th>
<th></th>
<th> Beaverton, OR 97005
</th></tr></table>


Note that the Kingstad center and the Galois offices are actually at different Max stops!

## Time


The Hackathon will be open

- 9 till 5 - Thur 14th - Kingstad center
- 9 till 5 - Fri 15th - Kingstad center
- 9 till (TBD) - Sat 16th - Galois offices

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

- [ Andy Gill](http://www.gill-warbington.com/home/andy), Galois, Previous contributor to GHC, wanting tools for high assurance Haskell.
- [ Malcolm Wallace](http://www.cs.york.ac.uk/~malcolm), nhc98-hacker, wanting to steal ideas :-), particularly from the type-system.
- [ Clifford Beshers](http://wiki.freespire.org/index.php/Freespire_Technology_Board_Home), Linspire, particularly interested in GHC as a library.
- [ Paul Graunke](http://www.galois.com/), Galois, interested in formal methods (Isabelle) and GHC intermediate representations
- [ Kathleen Fisher](http://www.research.att.com/info/kfisher), ATT, interested in adding support for data description to Haskell
- Jake Luck, interested in interactive debugging and accessing GHC internals from embedded systems.
- [ Geoffrey Washburn](http://www.cis.upenn.edu/~geoffw/), UPenn. Previous GHC contributor (original implementation of GADTs in Core). Wide variety of language interests involving expressiveness, security, and practical programming.
- [ Mark Tullsen](http://www.galois.com/), Galois, interested in tools for Haskell and high assurance run-time systems.
- [ Jeremy Gibbons](http://www.comlab.ox.ac.uk/jeremy.gibbons/), University of Oxford, generally curious about GHC internals - especially advanced aspects of typing
- [ Brett Letner](http://www.galois.com/), Galois, interested in code coverage tool for Haskell.
- [ Chad Scherrer](http://www.pnl.gov), Pacific Northwest National Laboratory, interested in computational math and statistics
- [ Karl Crary](http://www.cs.cmu.edu/~crary), CMU, working on a Haskell-driven typesetting system
- Luke Maurer, recent graduate of Carleton College, just looking to get my hands dirty
- [ Atze Dijkstra](http://www.cs.uu.nl/wiki/Atze/WebHome), Universiteit Utrecht, working on [ EHC](http://www.cs.uu.nl/wiki/Ehc/WebHome), interested to see what can be reused/learned/shared/etc.
- [ Doaitse Swierstra](http://www.cs.uu.nl/wiki/Swierstra/WebHome), Universiteit Utrecht, working on [ EHC](http://www.cs.uu.nl/wiki/Ehc/WebHome), interested to see what can be reused/learned/shared/etc.
- [ Norman Ramsey](http://www.eecs.harvard.edu/~nr), interested in either code generation or refactoring some part of the run-time system (and would really like to learn more about how the run-time system is put together, even if it's too big for a weekend hack).
- [ Iavor S. Diatchki](http://www.csee.ogi.edu/~diatchki), interested in Haskell for systems programming/type checking (general improvement rules)
- Edsko de Vries, interested in type systems (I work on uniqueness typing, a substructural type system), and generic programming
- Mathieu Boespflug Interested in implementing John Meacham's [ class alias proposal](http://repetae.net/john/recent/out/classalias.html).
- Andy Adams-Moran, interested in making cross-compiling with GHC easier, and in learning more about the innards of the compiler (versus the RTS)
- Aaron Tomb, University of California, Santa Cruz, interested in formal verification of Haskell programs, advanced type systems, and learning about the internals of GHC
- [ Björn Bringert](http://www.cs.chalmers.se/~bringert/), Chalmers University of Technology and Göteborg University, want to learn about the internals of GHC for future projects, possibly implement instance deriving for GADTs
- [ Andres Löh](http://www.iai.uni-bonn.de~/loeh/), University of Bonn, interested in type systems and generic programming
- [ Bryn Keller](http://www.xoltar.org), interested bystander. Interested in generating code for unusual targets (e.g. JavaScript, Flash, .Net, etc.).
- Ian Lynagh, interested in all parts.
- Brandon Moore, Yahoo. Interested in types, syntax extension.
- Ravi Nanavati, Bluespec. Interested in being able to fix GHC bugs before they burn me.
- Jeffery Zhang, interested in type systems and metaprogramming.
- John Matthews, Galois, interested in connecting GHC to the Isabelle theorem prover.
