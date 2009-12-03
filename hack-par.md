# Parallel Haskell Hackathon

## Organisation


Dates : 10-12 December


Location : St Andrews

## Participants


Vladimir Janjic (Host), Hans-Wolfgang Loidl, Kevin Hammond, Mustafa Aswad, Henrique Ferreiro Garcia, Philip W Trinder, Patrick Maier, Abyd Al Zain, Mischa Dieterle, Thomas Horstmeyer, Jost Berthold, Simon Marlow (virtual),

## Source code


Important: if you can, please prepare a setup of the latest sources in advance.
A darcs repository has been set up on the server in Marburg, please read here how to get a working setup: [ Repository Briefing](http://james.mathematik.uni-marburg.de:8080/EdenWiki/DarcsRepoCheatSheet).


You should contact Mischa (dieterle\@mathematik...) in order to get access, or check out read-only via http (which is of course not what we want later).
If you have questions or problems, edit the page or mail Jost (berthold\@mathematik...)

## Agenda


Topics to cover in the Hackathon:

- Short overview sessions on implementation internals (KH: these should not all be on the first afternoon!):

  - GUM (Mustafa)
  - Eden (Jost)
  - Globus (Abyd)
  - Migration/Load Balancing (Vladimir)
  - ghc HEAD developments (Simon)
  - GUM/Eden Compilation and Debugging (Vladimir?)
- GUM-6 port
- Merging GUM and Eden implementations

  - Packing code
  - Scheduler
  - GC interface
  - Tagging
- Usage of new tracing infrastructure
- Development infrastructure

  - Unified revision control (darcs)
  - Debugging infrastructure
  - Packaging
  - Setup for automatic tests
- Parallel nofib-suite
- Planned extensions and applications

  - Integration with GHC/SMP
  - Integrating migration
  - Globus interface
  - pre-SCSCP GAP interface
  - Micro-kernel/substrate approach to the RTS
- Nominating people in charge of sub-projects

  - parallel nofib suite
  - Eden/GUM code maintenance
  - testing framework
- Prize for the most awkward bug fixed in the Hackaton. <sub>~~Jost: I am non-competitive in this contest, since my task is to introduce them ;)~~</sub><sub>~~HWL: Clearly you are at an disadvantage: you first have to introduce the bug; we don't have that problem;-)~~</sub>

## Expected Outcomes

- Unified repository, containing Eden and GUM code
- Common test platform
- List of people in charge of sub-projects

## Agenda proposals


JB: I suggest a more informal format than real talks. 

- Practical Code Knowledge: One person gives informative overview and points to relevant source code, followed/interrupted by time slots to look up and read the code. 
- Brainstorm: One person presents ideas (slides, black/white board, ???) about future features and development, and is chairing a discussion round afterwards. After discussion, results have to be fixed in keywords, within 5 minutes.

<table><tr><th>**Time**</th>
<th>**Topic**</th>
<th>**Chair/Presenter**</th></tr>
<tr><th></th>
<th></th>
<th></th></tr>
<tr><th> Thu, ???  </th>
<th> Build setup, Compilation, Debugging GHC </th>
<th> ?Vladimir? 
</th></tr>
<tr><th></th>
<th></th>
<th></th></tr>
<tr><th> Thu, noon </th>
<th> Lunch break </th>
<th></th></tr>
<tr><th> Thu, 2pm </th>
<th> Overview of the Implementation Structure, Eden parts </th>
<th> Jost 
</th></tr>
<tr><th> Thu, 2:30 </th>
<th> Heap Closures, Pointer tagging for Dummies </th>
<th> ?Simon?
</th></tr>
<tr><th></th>
<th></th>
<th></th></tr>
<tr><th> Fri, 9am </th>
<th> Breakfast </th>
<th></th></tr>
<tr><th></th>
<th></th>
<th></th></tr>
<tr><th></th>
<th></th>
<th></th></tr>
<tr><th> Fri, 1pm </th>
<th> Lunch break </th>
<th></th></tr>
<tr><th></th>
<th></th>
<th></th></tr>
<tr><th></th>
<th></th>
<th></th></tr>
<tr><th></th>
<th></th>
<th></th></tr>
<tr><th> Sat, 9am </th>
<th> Breakfast </th>
<th></th></tr>
<tr><th></th>
<th></th>
<th></th></tr>
<tr><th></th>
<th></th>
<th></th></tr>
<tr><th> Sat, 1pm </th>
<th> Lunch break </th>
<th></th></tr>
<tr><th></th>
<th></th>
<th></th></tr>
<tr><th></th>
<th></th>
<th></th></tr></table>

<sub>Please maintain this table and the list above together, striking out in the list what is already here.</sub>