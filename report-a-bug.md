# Reporting bugs in GHC


Glasgow Haskell is a changing system so there are sure to be bugs in it.


To report a bug, either:

- Preferred: Create a [ new bug](http://hackage.haskell.org/trac/ghc/newticket?type=bug), and enter your bug report. You can also search the bug database here to make sure your bug hasn't already been reported (if it has, it might still help to add information from your experience to the existing report).
- Bug reports can also be emailed to \<glasgow-haskell-bugs@…\>. 

## How do I tell if I should report my bug?


Take a look at the [ FAQ](http://haskell.org/haskellwiki/GHC/FAQ) and [Chapter 9, What to do when something goes wrong](http://www.haskell.org/ghc/docs/latest/html/users_guide/wrong.html), which will give you some guidance as to whether the behaviour you're seeing is really a bug or not.


If it is a bug, then it might have been reported before: try searching on the [ bug tracker](http://hackage.haskell.org/trac/ghc), and failing that, try [ Google](http://www.google.com/).


If in doubt, just report it.

## What to put in a bug report


The name of the bug-reporting game is: facts, facts, facts. Don't omit them because "Oh, they won't be interested…"

1. What kind of machine are you running on, and exactly what version of the operating system are you using? (on a Unix system, `uname -a` or `cat /etc/motd` will show the desired information.) In the bug tracker, this information can be given in the "Architecture" and "Operating system" fields.
1. What version of GCC are you using? `gcc -v` will tell you.
1. Run the sequence of compiles/runs that caused the offending behaviour, cut-and-paste the whole session into the bug report. We'd prefer to see the whole thing.
1. Add the `-v` flag when running GHC, so we can see exactly what was run, what versions of things you have, etc.
1. What is the program behaviour that is wrong, in your opinion?
1. If practical, please attach or send enough source files for us to duplicate the problem.
1. If you are a Hero and track down the problem in the compilation-system sources, please send us patches (either `darcs send`, plain patches, or just whole files if you prefer).
