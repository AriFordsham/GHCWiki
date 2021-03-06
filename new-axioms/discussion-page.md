**Note: This page is out of date.** The final implementation is described [here](new-axioms). This page is kept for historical reasons.

# Discussion: Pattern-matching axioms


At Pedro's invitation, comment/suggestions/requests for clarification/alternative solutions, to explore the design space.

- Organised into sub-headings: as and when these grow, split into sub-pages.

- Please add further comment/detail. And correct anything I've mis-represented \[AntC\].

## Suggestions

- **Instance match fail:** There are use cases where we want to make the existence of a more specific match a type-level failure. (Compare this [https://gitlab.haskell.org/trac/ghc/wiki/TypeFunctions/TotalFamilies\#Definingtotalfamilies](https://gitlab.haskell.org/trac/ghc/wiki/TypeFunctions/TotalFamilies#Definingtotalfamilies) from Chak 2008, using VOID for the same purpose.) Currently a 'dead end' needs fudging with fake instances and constraints, leading to mystifying messages. The example is HList Lacks constraint.

```wiki
hCons :: (Lacks e l) => e -> l -> HCons e l          -- smart constructor, validate that l doesn't already contain e
hCons = HCons                                        -- then we can do type-indexed lookup on the HList

class Lacks e l                                      -- constraint only, no methods
instance Lacks e HNil                                -- got to the end of the HList, not found an element type e
instance (NoneSuch e) => Lacks e (HCons e l')        -- make this a fail by imposing an unfulfillable constraint
instance (Lacks e l') => Lacks e (HCons e' l')       -- this element doesn't match, recurse on the tail

-- possible instance grouping approach:

hCons :: (Lacks e l ~ True) => e -> l -> HCons e l   -- what error reporting does this give when e found in l?

type family Lacks e l :: Bool
type instance Lacks where
    Lacks e HNil = True
    Lacks e (HCons e l') = False
    Lacks e (HCons e' l') = Lacks e l'

-- disequality guards seem to show the intent more clearly:
type instance Lacks e HNil = True
type instance Lacks e (HCons e' l')  | e /~ e'   = Lacks e l'   -- no instance for the equality
```

- **Idiom of a total function** (this would apply to all the HList examples). We only need one instance group for the whole family; then putting both decls seems superfluous. Perhaps we could conflate them:

  ```wiki
      type family Equal a b :: Bool where
          Equal a a = True
          Equal a b = False

      type family HasMember a (b :: '[]) :: Bool where
          HasMember a '[] = False                                  -- (not overlapping)
          HasMember a ( a ': bs ) = True
          HasMember a ( b ': bs ) = HasMember a bs
  ```

## Alternative Solutions



According to \[Bulatz as of 2007\] [http://www.haskell.org/haskellwiki/GADTs_for_dummies](http://www.haskell.org/haskellwiki/GADTs_for_dummies):


>
>
> "In many other cases \[of overlap\] this automatic \[instance\] selection is not powerful enough and we are forced to use some artificial tricks or complain to the language developers. The two most well-known language extensions proposed to solve such problems are instance priorities, which allow us to explicitly specify instance selection order, and '/=' constraints, which can be used to explicitly prohibit unwanted matches."
>
>

#### Instance Priorities/Selection order



Is essentially this Pattern-matching axioms approach.


>
>
> **Instance Chains: Type Class Programming Without Overlapping Instances**. Morris & Mark P.Jones 2010. contains some similar ideas, but in context of Functional Dependencies. (It also supports Class constraints being used to select patterns, and provides a *fail* outcome that triggers backtracking search for a better-matching instance.)
>
>

#### Disequality *constraints*



(Probably better called **Guards** or **Restraints** to avoid confusion with Class and type equality Constraints) date from at least **A Theory of Overloading**. Sulzmann & Stuckey 2002. Section 8.2 **Overlapping Definitions**.


>
>
> This is using Constraint Handling Rules (implemented through Chameleon) to guide type inference. Example of instances using guards:
>
>

```wiki
type instance Equal a a = True                      -- regular instance
type instance Equal a b   | a /~ b  = False         -- guarded instance, using type dis-equality
```

>
>
> **Note:**
>
>

- Instances do not have to appear in any particular order; do not have to be defined together (nor even in the same module).

- The syntax mimics pattern guards for function bindings, using /\~ per type equality constraints.

- But the guards are not constraints: they control whether the instance is selected (whereas constraints validate the types after the instance has been selected).

- Instances must not overlap (after taking the disequalities into account), so we can't create unsound FC coercions.

#### Type-level Type Representations (TTypeable)



Oleg Kiselyov 2004 (part of the HList work, Section 9 of the paper)


>
>
> Translates every type in your code to a cannonical type representation (based on type-level naturals), then you can compare the representaions for equality (and indeed induce an ordering).
>
>

- One downside is that you have to provide a translation instance for each user-defined type, and make sure the representation doesn't clash with any other type. Template Haskell helps, compiler support would be better.

>
>
> (By the way, arguably the whole TTypeable project might have been unnecessary. Oleg built the approach because of persistent trouble around overlaps. But you can't do overlaps without fundeps (in any yet availabe version of GHC -- and Hugs is far worse.) Perhaps the trouble was really because of fundeps interfering with overlap? [http://www.haskell.org/pipermail/haskell-prime/2012-May/003688.html](http://www.haskell.org/pipermail/haskell-prime/2012-May/003688.html) As and when matching coercions are available, we'll be able to experiment.)
>
>

## Comparisons to other approaches with overlaps

- [https://gitlab.haskell.org/trac/ghc/wiki/TFvsFD](https://gitlab.haskell.org/trac/ghc/wiki/TFvsFD) \[Thank you Etienne, and there's a very helpful **See also**.\] Several examples where Type Functions don't seem to be as powerful as Type Classes/Constraints/FunDeps. Some of these examples need overlaps, but not all. Equality constraints seem to make type refinement more 'eager' than under TF's. 

- [http://okmij.org/ftp/Haskell/PeanoArithm.lhs](http://okmij.org/ftp/Haskell/PeanoArithm.lhs) \[referenced from the TF vs FD discussion\] Exploits a bi-directional FunDep technique, to get subtraction of type-level Nats using only a definition of add.

## Comments

- **Instance validation** for type families is 'eager' -- that is, each instance is validated for overlap at point of declaration.

  - Contrast that instance validation (in GHC) for classes is 'negligent' (or 'relaxed', or 'generous' depending on your point of view: can't use the word 'lazy'): you can declare overlaps that compile OK, but then GHC complains at the use site that it has irresolvable overlaps. (The use site might be GHCi.)
  - GHC behaves like that because your code's usages might all be resolvable, so it's trying to be generous.
  - Or GHC might select different instances for what seem like the same uses.
  - Compiler flag IncoherentInstances is a good way to make this effect worse.
  - BTW Hugs' validation for overlaps is eager.

- **Sudden and Silent Overlap:** a newly-imported module or package might declare an instance (for a type class, especially a library class) that is more specific than any you've been working with. The program's behaviour may suddenly change for no (apparent) reason.

>
> >
> >
> > (??The original Monad Transformers approach suffered from this. The design deliberately had a most general instance declared with the class. Application modules overrode it. See **Type Classes: Exploring the design space** Jones/Jones/Meijer 1997, section 3.6.2)
> >
> >
>

- **Undecidability:** presumably the UndecidableInstances option is still applicable, with all the issues around termination and coverage conditions. Are there any additional considerations raised by overlaps/matching coercions?

- **Partial vs. Total Overlap**

>
> >
> >
> > Total overlap means that any substitution that fits the narrower pattern will necessarily fit the wider. Partial overlap between two patterns means that some substitutions will fit both; some only the one; some only t'other. Examples:
> >
> >
>

```wiki
    type instance F Int Bool = ...         -- (1) is totally overlapped by (2), (3) and (4)
    type instance F Int b    = ...         -- (2) partially overlaps (3)
    type instance F a Bool   = ...         -- (3)
    type instance F a b      = ...         -- (4) totally overlaps (1), (2) and (3)
```

>
> >
> >
> > (For reasons I don't understand) when GHC introduced Associated Types (Families), they seemed particularly concerned about partial overlaps (and confluence thereof [http://www.haskell.org/haskellwiki/GHC/Indexed_types](http://www.haskell.org/haskellwiki/GHC/Indexed_types) section 6.2.2 Overlap). Compare that Hugs handles partial overlaps badly, so it's usually better to 'factor' into total overlaps. (That is: if you have instances like (2) and (3), add an instance like (1).)
> >
> >
>

>
> >
> >
> > If you allow only total overlaps, validation of the instances seems easier to understand, and the rules for selecting the instance at the use site would be more coherent (perhaps?). Is it the possibility of partial overlap that led to GHC's validation being more 'generous'/less eager?
> >
> >
>

- **Generalisation and Unification of instance groups** and disequality guards

>
> >
> >
> > Presumably common-or-garden type instances can be generalised to instance groups, and different instance groups can be unified providing their patterns don't overlap. Like this:
> >
> >
>

```wiki
    type instance G (c, d) b   = c               -- generalise to
    type instance G where ...
    type instance G Bool   b   = b               -- generalise likewise

    -- now unify:
    type instance G where
        G (c, d) b   = c
        G Bool   b   = b                         -- no overlap

    -- take an instance with (putative) disequality guards:
    type instance G a b | a /~ (_, _), a /~ Bool = a
    -- generalise:
    type instance G where
       G (_, _) b    = VOID
       G Bool   b    = VOID
       G a      b    = a

    -- unify with the group above (observing overlaps):
    type instance G where
       G (c, d) b    = c
       G Bool   b    = b
       G a      b    = a
    
```

## Example Applications/Uses for Instance Overlap


- **Strongly typed heterogeneous collections** (colloquially known as **HList**s). Kiselyov, Lammel, Schupke 2004.

>
> >
> >
> > Section 6 **Ended up in murky water** with overlaps. Section 9 discusses approaches to make overlapping less fragile (and specifically **TTypeable** - Type-level Type representations.
> >
> >
>

- **Data Types a la Carte**. Swierstra 2008. An approach to Wadler's *Expression Problem*.

>
> >
> >
> > Section 4 discusses limitations caused by overlaps. [http://wadler.blogspot.co.nz/2008/02/data-types-la-carte.html](http://wadler.blogspot.co.nz/2008/02/data-types-la-carte.html) discusses those limitations, with some solutions suggested.
> >
> >
>

- **Monad Transformers/Library**. ??

>
> >
> >
> > (I believe) Monad Transformers originally used overlaps extensively, and were then re-engineered to avoid difficulties. What difficulties? How/why re-engineered? Would the earlier approach be workable under Pattern-matching? Would it be more user-friendly than the later approach?
> >
> >
>

## References and Links


>
>
> See references scattered through the above discussions.
>
>

- [https://gitlab.haskell.org/trac/ghc/wiki/TFvsFD\#SeeAlso2](https://gitlab.haskell.org/trac/ghc/wiki/TFvsFD#SeeAlso2)
- [http://www.haskell.org/haskellwiki/Functional_dependencies_vs._type_families](http://www.haskell.org/haskellwiki/Functional_dependencies_vs._type_families)


\[More to do ...\]


Surprisingly few wiki pages discuss overlaps.

- [http://www.haskell.org/haskellwiki/GHC/AdvancedOverlap](http://www.haskell.org/haskellwiki/GHC/AdvancedOverlap)
- [http://www.haskell.org/haskellwiki/Research_papers/Type_systems](http://www.haskell.org/haskellwiki/Research_papers/Type_systems)


Link to GHC flags on OverlappingInstances, IncoherentInstances, (and possibly UndecidableInstances).


See Haskell-cafe and Haskell-prime mailing lists anon.

- [http://www.haskell.org/pipermail/haskell-prime/2011-July/003491.html](http://www.haskell.org/pipermail/haskell-prime/2011-July/003491.html) -- and discussions this points to
- [http://www.haskell.org/pipermail/haskell-prime/2012-May/003687.html](http://www.haskell.org/pipermail/haskell-prime/2012-May/003687.html)

## Requests for Clarification


>
>
> (Moved to the end: SPJ has now (10 June 2012) clarified these on the main page.)
>
>

- Is it possible to have more than one grouping for a given type function? (Possibly declared in separate modules.) Example:

```wiki
module M1 where
    type instance H Int b  where
        ... -- sequence of patterns to dis-overlap on b

module M2 where
    type instance H Char b where
        ... -- sequence of unrelated patterns on b
```

>
> >
> >
> > (Section 3.3 Transltion of the draft design/Law 3.6 (Consistent instance groups) seems to be saying multiple groupings are OK. An example would make it clearer.)
> >
> >
>

- Are there any rules/validation around the sequence of patterns within a grouping?

>
> >
> >
> > If a later pattern is more specific than an earlier (so could never get selected) is that pattern flagged, or just (in effect) silently ignored?
> >
> >
>

- Must there be a most general pattern? (By most general, I mean exactly matching the instance head. Presumably it would appear last in the group.)

>
> >
> >
> > If not, presumably at a use site type inference could fail to find a match. For example:
> >
> >
>

```wiki
type instance G a b where
    G (c, d) Int = c
    G Bool b     = b
-- stop there

g :: G Int Int               -- what happens now?
```

- Can inference get 'stuck'? (I'm guessing so. See the groundedness issues in the **HList** paper, at beginning of Section 9.)

>
> >
> >
> > For example, at a use site for `F a b`, we can infer `a ~ Int` and `b ~ (Num b0) => b0`, but we can't refine `b` any further. So we don't have sufficient evidence to match pattern `F a a = True`; but neither can we move on to pattern `F a b = False`.
> >
> >
>


SCW: For these last two, it would be consistent with current treatment (and with multiple groups) to allow 'stuck' type families. Perhaps GHC could flag a few more bugs if the user could specify when a type family was expected to be fully covered, but I don't think that failing to do this check will jeopardize type soundness.  

- Are the examples for the multi-type instance declarations quite as intended? The heads have no head, as it were. \[That is as SPJ intended\] Is this allowed?

  ```wiki
      type instance F [a] where ...

      type instance F (a, b) where ...
  ```

  (From a documentation point of view, this shows that the instance groups are non-overlapping.)
