# GHC Kind level


This page gives the theory and implementation overview and details about GHC's kind level.  This work is related to [ Conor's SHE system](http://personal.cis.strath.ac.uk/~conor/pub/she/) and will be related to Iavor's work on [TypeNats](type-nats) to deal with primitive types.

## Theory


We use the mechanism of promotion to lift a data type to the kind level.  This gives access at the type level to the data constructors, and at the kind level to the type constructor.  All data types cannot be promoted.  For examples GADTs or data types with higher-order kinds.

## Implementation


The branch is called `ghc-kinds`.  Its current state is:

<table><tr><th></th>
<th> ADT promotion </th>
<th> Primitives </th>
<th> Kind polymorphism 
</th></tr>
<tr><th> Parser      </th>
<th>      Yes      </th>
<th>    Yes     </th>
<th>       Yes         
</th></tr>
<tr><th> Renamer     </th>
<th>  In progress  </th>
<th>    Yes     </th>
<th></th></tr>
<tr><th> Typechecker </th>
<th></th>
<th></th>
<th></th></tr></table>