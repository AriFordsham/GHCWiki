# GHC Kind level


This page gives the theory and implementation overview and details about GHC's kind level.  This work is related to [ Conor's SHE system](http://personal.cis.strath.ac.uk/~conor/pub/she/) and will be related to Iavor's work on [TypeNats](type-nats) to deal with primitive types (promoted `Int` and `Char`).

## Theory


We use the mechanism of promotion to lift a data type to the kind level.  This gives access at the type level to the data constructors, and at the kind level to the type constructor.  All data types cannot be promoted.  For examples GADTs or data types with higher-order kinds.  We add kind polymorphism to allow promotion of polymorphic data constructors (like `Nil` or `Cons`).


More details can be found on the attached pdf.

## Examples


Examples of reimplementation of existing Haskell librairies can be found in the examples pdf.

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


The stage1 compiler does not work, since there is some `undefined`s in the typechecker.  So you won't be able to build a stage2 or even run validate.  This is the first priority.
