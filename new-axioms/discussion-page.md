# Discussion: Pattern-matching axioms


At Pedro's invitation, comment/suggestions/requests for clarification/alternative solutions, to explore the design space.

- Organised into sub-headings: as and when these grow, split into sub-pages.

## Requests for Clarification

## Suggestions

## Alternative Solutions


According to \[Bulatz as of 2007\] [ http://www.haskell.org/haskellwiki/GADTs_for_dummies](http://www.haskell.org/haskellwiki/GADTs_for_dummies):

>
> "In many other cases \[of overlap\] this automatic \[instance\] selection is not powerful enough and we are forced to use some artificial tricks or complain to the language developers. The two most well-known language extensions proposed to solve such problems are instance priorities, which allow us to explicitly specify instance selection order, and '/=' constraints, which can be used to explicitly prohibit unwanted matches."

- **Instance Priorities/Selection order** is essentially this Pattern-matching axioms approach.

- **Disequality***constraints* (probably better called **Guards** or **Restraints** to avoid confusion with Class and type equality Constraints) date from at least **A Theory of Overloading**. Sulzmann & Stuckey 2002. Section 8.2 **Overlapping Definitions**.

> >
> > This is using Constraint Handling Rules (implemented through Chameleon) to guide type inference. Example of instances using guards:

```wiki
type instance Equal a a = True                      -- regular instance
type instance Equal a b   | a /~ b  = False         -- guarded instance, using type dis-equality
```

> > **Note:**

- Instances do not have to appear in any particular order; do not have to be defined together (nor even in the same module).

- The syntax mimics pattern guards for function bindings, using /\~ per type equality constraints.

- But the guards are not constraints: they control whether the instance is selected (whereas constraints validate the types after the instance has been selected).

- Instances must not overlap (after taking the disequalities into account), so we can't crete unsound FC coercions.

## Comments

## Example Applications/Uses for Instance Overlap

- **Strongly typed heterogeneous collections** (colloquially known as **HList**s). Kiselyov, Lammel, Schupke 2004.

> >
> > Section 6 **Ended up in murky water** with overlaps. Section 9 discusses approaches to make overlapping less fragile (and specifically **TTypeable** - Type-level Type representations.

- **Data Types a la Carte**. Swierstra 2008. An approach to Wadler's *Expression Problem*.

> >
> > Section 4 discusses limitations caused by overlaps. [ http://wadler.blogspot.co.nz/2008/02/data-types-la-carte.html](http://wadler.blogspot.co.nz/2008/02/data-types-la-carte.html) discusses those limitations, with some solutions suggested.

- **Monad Transformers/Library**. ??

> >
> > (I believe) Monad Transformers originally used overlaps extensively, and were then re-engineered to avoid difficulties. What difficulties? How/why re-engineered? Would the earlier approach be workable under Pattern-matching? Would it be more user-friendly than the later approach?

## References and Links