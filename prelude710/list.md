# The 7.10 Prelude should remain list based


As per [Prelude710](prelude710), there is debate over whether the list functions in the GHC 7.10 should be generalized to Foldable/Traversable. This page attempts to itemize some of the concerns about the generalization, and also some alternative approaches to getting some of the benefits of generalization. There is more information at:

- [BurningBridgesSlowly](burning-bridges-slowly)
- [http://neilmitchell.blogspot.co.uk/2014/10/how-to-rewrite-prelude.html](http://neilmitchell.blogspot.co.uk/2014/10/how-to-rewrite-prelude.html)
- [http://neilmitchell.blogspot.co.uk/2014/10/why-traversablefoldable-should-not-be.html](http://neilmitchell.blogspot.co.uk/2014/10/why-traversablefoldable-should-not-be.html)


A brief summary of both this plan and the alternative being considered at this point is available at [Prelude710](prelude710), while the details of the Plan Foldable counter-proposal are available at [Prelude710/FTP](prelude710/ftp).

## Concerns with the reasons and motivations


There isn't clear agreement that generalization should be done at all.

- One motivation behind the generalization is to modernize the Prelude and standardize on the Foldable/Traversable. But the Prelude, though far from perfect, has withstood years as a workable base for Haskell. Developers of all sorts routinely build more customized basic sets of libraries to import into their code, either as alternative Preludes, or as a common import. This mechanism works and allows diversity of opinion on what should commonly be in scope.

- Another motivation behind the generalization is to allow the generic functions exported by Data.Foldable and Data.Traversable, such as foldr and sequence, to be in scope unqualified without clashing with the same named functions in the Prelude (which are mostly from Data.List). But Haskell has existing facilities for handling this situation. The common way would be to Import Data.Foldable qualified, perhaps under a short name like F. In this way, foldr is the Prelude list based version, and F.foldr is the generic version. This is the approach used for many modules that have common, short named functions such as Data.Map and Data.Text.

## Concerns with the generalization


There are a number of concerns with the generalizations proposed for GHC 7.10. Some of these could be ignored, but the volume of concerns is in itself somewhat concerning.

- Foldable has lots of class members. While the minimal definition is foldMap, in GHC 7.8 it contains 8 fold functions. In the Foldable/Traversable extension many additional members have been added, including sum, product, maximum, minimum. There is no obvious bound on the number of specialized folds that could be added.

- Traversable contains both Monad and Applicative variants for each function, and following the Applicative-Monad proposal, the Monad variants (mapM and sequence) are now redundant. As a consequence, the derived functions forM and for are also duplicates.

- Given Foldable and Traversable may benefit from further refinement, dragging them into Prelude seems premature.

- Data.List now has many functions that don't mention list in their type signature, for example find, length and null. Having such functions in the Data.List module is awkward from a naming perspective. In contrast, modules like Data.Map export many functions from Data.Foldable, specialized to Map.

- There are lots of functions that could be generalized further, but are not. For example, mapM, forM and sequence could all be expressed in terms of Applicative instead of Monad. Similarly things like length could be generalized to Num, making length and genericLength equivalent.

- While the Prelude operations (e.g. foldr) will now work on containers such as Vector, they still won't work on things like ByteString or Text, which in some code is used far more than other non-list containers.

- Some functions in Data.List could be generalized to Foldable, but have not been. For example, isPrefixOf and isInfixOf can be generalised. More generally, anything with a list in an argument position can be generalized.

- Some functions in Data.List could be generalised to Traversable, but have not been. For example, sort and reverse can be generalized. However, such generalizations are likely to add a performance penalty.

- Given that lots of functions could be generalized, it seems we should either generalize everything, or have a good story for where to stop. For example, isPrefixOf can be generalized, but the related function stripPrefix can only be partly generalized, so should isPrefixOf be generalized?

- The IsList class is an alternative generalization that could be made for some functions, and would work for ByteString and Text. Neither Foldable nor IsList is strictly more general, so both are potential alternatives.

- It should also be noted that `Traversable` can be added to `Prelude` without adding `Foldable`.  Today `Foldable` is a superclass of `Traversable`, but there is no real need for that.  (E.g., building the lens package and all its dependencies only requires 9 trivial changes when `Foldable` is no longer a superclass.)

- Containers with a constrained element type (e.g. `StorableVector`, `Vector.Storable` and `Vector.Unboxed`) cannot be made instances of the `Foldable` class.

- Neither `null` nor `length` are inherently limited to `Foldable` containers, making this, perhaps, an insufficient generalization. Adding them to `Foldable` makes it harder to put them somewhere else later. This does not seem terribly likely, but it should be considered.

- The `Foldable` abstraction is a bit "leaky". Different `Monoid` and `Foldable` instance can interact in very different ways. The usual assumption is that `Foldable` instances are more likely to be like lists than like snoc-lists with regard to laziness and performance, but there is no law to justify this assumption.

- Sums and products may depend for efficiency and precision on both container and element type. For example, one might want to sum floating point numbers in a certain order for greatest precision. There is no way for the `Foldable` instance to behave differently for different monoids without `RULES` hacks.

## Concerns for the ecosystem


The base libraries, and especially the Prelude, are foundations of the larger ecosystem around Haskell. How such a change alters that ecosystem should be taken into account.

- While most code compiles and works as intended with the Foldable changes in, not all does. For very large code bases (think 1M lines or more), moving up to a generalized Prelude would take considerable engineering effort. Most such installations would probably delay adopting such a Prelude, perhaps for years.

- The existing corpus of books, tutorials, syllabi, and the like usually have a significant portion of the text dedicated to these very Prelude functions - and they would all need significant revision. 

- Teaching beginners what sequence means in its full generality is going to be a challenge.

- While teaching beginners who end up on \#haskell IRC might be possible, this is likely to increase the "bounce" rate, people who see Haskell, play around, and run away scared. I think Haskell probably has a higher bounce rate than most other languages, making it worse would be bad.

## Alternatives to the generalization


There are a number of alternative approaches to how the generalization could be introduced into the code base. None of these approaches are fully worked through, and would not be ready for GHC 7.10, but could be adapted for GHC 7.12.

- A language pragma could be used select alternative Preludes.

- We could support restricting type signatures in export lists, so that when both a specific and general version are imported they do not clash.

- A module with only the non-Foldable overlapping bits of Data.List could be created, allowing users who wanted Foldable plus some list functions to avoid name clashes.

- The functions in Data.Foldable and Data.Traversable could be renamed not to clash. It is very common for generalized versions of functions to have a different name than their non-generalized counter parts: fmap and map, mappend and (++).  If the members of Data.Foldable were minimized (see above), then it might be reasonable to rename its foldr something like ffoldr.
