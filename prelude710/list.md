# The 7.10 Prelude should remain list based


As per [Prelude710](prelude710), there is debate over whether the list functions in the GHC 7.10 should be generalized to Foldable/Traversable. This page attempts to itemize some of the concerns about the generalization, and also some alternative approaches to getting some of the benefits of generalization. There is more information at:

- [BurningBridgesSlowly](burning-bridges-slowly)
- [ http://neilmitchell.blogspot.co.uk/2014/10/how-to-rewrite-prelude.html](http://neilmitchell.blogspot.co.uk/2014/10/how-to-rewrite-prelude.html)
- [ http://neilmitchell.blogspot.co.uk/2014/10/why-traversablefoldable-should-not-be.html](http://neilmitchell.blogspot.co.uk/2014/10/why-traversablefoldable-should-not-be.html)

## Concerns with the generalization


There are a number of concerns with the generalization as proposed for GHC 7.10. Some of these could be ignored, but the volume of concerns is in itself somewhat concerning.

- Foldable seems too big. It has gained a number of methods, and seems to have no natural bound on its eventual size.

- Traversable seem to be twice as big as it should be, with both Monad and Applicative variants.

- Given Foldable and Traversable could do with further refinement, dragging them into Prelude seems too soon.

- Teaching beginners what sequence means in its full generality is going to be a challenge.

- While teaching beginners who end up on \#haskell IRC might be possible, this is likely to increase the "bounce" rate, people who see Haskell, play around, and run away scared. I think Haskell probably has a higher bounce rate than most other languages, making it worse would be bad.

- Data.List is now methods not on list, but on Foldable, which is weird just from a naming perspective.

- There are lots of places that are Monad that could be Applicative. Given we're generalising List to Foldable, that now seems a bit weird. Similarly things like length vs genericLength now look very weird, given the structure is generalised but the number isn't.

- I worry that while all the operations now work on lists and things like Vector, they don't work on things like ByteString or Text, which I find myself using far more than other non-list containers.

- The functions in List which are generalised vs those which aren't is a bit surprising. isPrefixOf is not generalised, why not? I don't see any good reason for Data.List having a single list in an argument position in the new world.

- Where should we stop? Certainly you can write any transformation, e.g. sort/reverse, on Traversable. Should we? Of course, you can't write reverse on Traversable as efficiently without adding a new method to Traversable - are we going to not do that?

## Approaches to the generalization


The primary motivation behind the generalization seems to be to avoid name clashes. There are a number of approaches to fix the name clashes without generalizing Prelude. None of these approaches are fully worked through, and would not be ready for GHC 7.10, but could be adapted for GHC 7.12.

- A language pragma could be used select alternative Preludes.

- We could support restricting type signatures in export lists, so that when both a specific and general version were imported they did not clash.

- A module with only the non-Foldable overlapping bits of Data.List could be created.
