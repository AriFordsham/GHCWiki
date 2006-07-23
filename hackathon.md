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


Galois will be hosting the meeting.  Their offices are in Beaverton, a suburb of Portland.

- Galois will provide a large room, with desks, a projector, and wireless access.
- We will also have other rooms that can be used for side-projects.
- Bring your own laptop (capable of building GHC).

<table><tr><th>  Address: </th>
<th> 12725 SW Millikan Way
</th></tr>
<tr><th></th>
<th> Suite 290
</th></tr>
<tr><th></th>
<th> Beaverton, OR 97005
</th></tr></table>


More detailed directions will be provided before the hackathon.

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
