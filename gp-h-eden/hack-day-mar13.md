# Parallel Haskell HackDay

## Organisation


Dates : 19.3.2013, starting 10:30


Location : Heriot-Watt University, Edinburgh, Earl Mountbatten Buildings, EM 1.70


It's building 2 on [ http://www.macs.hw.ac.uk/\~hwloidl/hw_map_colour.pdf this map](http://www.macs.hw.ac.uk/~hwloidl/hw_map_colour.pdf this map), with entrance from Boundary Road North.
 

## Participants


So far: Hans-Wolfgang Loidl (Host), Jost Berthold, Vladimir Janjic, Vladimir Komendantsky, Malak Aljabri, Robert Stewart, Evgenij Belikov, Prabhat Totoo

## Agenda

<table><tr><th> 10:30 </th>
<th> Welcome and Setup 
</th></tr>
<tr><th> 10:45 </th>
<th> Exchange of ideas and brainstorm 
</th></tr>
<tr><th> 11:00 </th>
<th> Pair programming on selected topics 
</th></tr>
<tr><th> 13:00 </th>
<th> Lunch 
</th></tr>
<tr><th> 14:00 </th>
<th> Short round of summaries and discussion 
</th></tr>
<tr><th> 14:15 </th>
<th> TBC (pair programming, short talks...) 
</th></tr>
<tr><th> X:00  </th>
<th> Summary Reports and Future Work
</th></tr></table>

## Potential Topics

- GHC parallel RTS: general house-keeping and fixes

  - Cleaning up non-main PEs in parcp way
  - Behaviour of primops in non-parallel ways
  - Start script optimisations
  - eliminating start script in parcp way (i.e. in-RTS log archiving)
  - change PrimOps.cmm to work with LLVM-way
- Eden/GpH programming (nbody, paraffins etc)

  - nbody using Eden and GpH skeletons
  - move forward wrt. test suite
- Serialisation:

  - Factor out from PARALLEL_RTS
  - make good use of it 
- Eventlog and visualisation
