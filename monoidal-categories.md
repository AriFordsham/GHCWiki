## Design of the monoidal category classes and the new Arrow/proc desugaring story

### What are monoidal categories and why are they useful?


Monoidal (and related) categories capture the basic notion of *process*. This is most easily noticed by recognising a tool for using monoidal categories called [ string diagrams](http://ncatlab.org/nlab/show/string+diagram). Examples of string diagrams include circuit diagrams, flow charts, signal flow diagrams, Feynman diagrams, proof nets, etc. In that way, monoidal categories are like Arrows (indeed, every Arrow is a monoidal category with an additional structure which allows reification of functions, and application of one input to another) - except much more general. For example, Megacz's work on Generalized Arrows can be entirely subsumed by monoidal categories (indeed, I even based the initial design off of parts of his thesis)

### Overview


The new class hierarchy is based on monoidal categories, and monoidal categories with additional structure. What is a monoidal category? [ From ncatlab:](http://ncatlab.org/nlab/show/monoidal+category)

<table><tr><th>Definition</th>
<th>Explaination
</th></tr>
<tr><th>⊗:M×M→M</th>
<th>Bifunctor called the tensor product
</th></tr>
<tr><th>1::M</th>
<th>An object called the unit
</th></tr>
<tr><th>a<sub>x,y,z</sub>:(x⊗y)⊗z→x⊗(y⊗z)</th>
<th>The tensor product is associative both ways (isomorphism)
</th></tr>
<tr><th>λ<sub>x</sub>:1⊗x→x</th>
<th>An elimination and introduction rule for removing the left unit from a tensor (isomorphism)
</th></tr>
<tr><th>ρ<sub>x</sub>:x⊗1→x</th>
<th>An elimination and introduction rule for removing the right unit from a tensor (isomorphism)
</th></tr></table>


It also obeys two laws known as the Pentagon equation and the Triangle equation.

## Plan

1. Write monoidal category class heirarchy in GHC.Arrows.Experimental and provide instances for Arrow and several other basic types
1. Convert or write a small FRP library to be based on the new classes to test the design
1. Provide optimisation RULES pragmas as much as possible; for example, the work on Causal Commutative Arrows can be converted relatively straightforwardly to plain monoidal categories
1. Convert the proc/arrow desugarer in GHC to emit instances of the new classes; since all Arrows are monoidal categories, this change will be backwards compatible with user code
1. Change GHC.Arrows.Experimental to GHC.MonoidalCats before the milestone it is merged for


I will be updating this page to document/brainstorm the design as I go along

[ Trac ticket](https://ghc.haskell.org/trac/ghc/ticket/9596)

[ Phabricator differential](https://phabricator.haskell.org/D212)