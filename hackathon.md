# The 2006 GHC hackathon


At the 2005 Haskell workshop we were asked to consider running a "GHC hackathon" around ICFP 2006.  A surprisingly large number of people in the room indicated that they'd be interested in coming to such a thing.  This Wiki page says what we have in mind, and asks you to indicate whether you'd be likely to come, and what sort of meeting you'd like.  What we do will depend on what you say.

### Purpose


GHC is used by lots of people, but its implementation is rather over-centralised, even though GHC is a BSD-licensed, open-source project.  The biggest obstacle to more people getting involved is that GHC is a big, and hence intimidating, system.  The purpose of the GHC hackathon is to give a tutorial in GHC’s innards, plus time to work on some projects in a context where there are plenty of people around to help.  The objective is to substantially broaden (more numbers) and deepen (more confidence) the community of people who feel they know enough to fix and enhance GHC.  


We'll suggest some projects, but you're welcome to come along with your own wacky ideas or itches that you want to scratch, and work on them in an environment with a high-bandwidth connection to developers who really know the innards of GHC.   You could work on something individual, or in small groups.  We anticipate all being in one room, or in a tight group of rooms, so there’d be lots of informal interaction.


Also there's a good chance it'll be fun :-)
 

### Venue


Galois has very kindly agreed to host the meeting.

### Cost


zero.  But you’ll have to feed yourself.  And you may have to bring a laptop (capable of building GHC).

### Programme


We had in mind the following.  

- One full day of tutorial from Simon & Simon about GHC’s glorious innards. 

- Then one or two days of hacking on projects.  


Timing: The Haskell workshop is on Sunday 17 Sept; ICFP starts the next day.  On Saturday is the ML and Erlang workshops.  We propose to run the GHC hackathon something like Thurs-Sat 14-16 Sept, so that people who want to go to the ML workshop still can; but those who don’t won’t have a blank day.

## Questions

Poll(Would you come?; Yes; (no need to vote if you're not coming!))?

Poll(How long would you prefer?; One day; Two days; Three days)?

---

## Suggestions


Add your suggestions for the hackathon below...

- Hack to allow RTS to integrate with an external event loop (eg to give us ideal threading when using Gtk+)
- Add a `ghc --clean` that just executes `find -name '*.o' -o -name '*.hi' -exec rm {} \;`, perhaps in a more portable fashion.
