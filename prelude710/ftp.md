# The 7.10 Prelude should be generalized


In 2013, two proposals passed through the libraries@ process. Unlike most proposals before them, these proposals affect types in the Prelude. These are the Applicative/Monad Proposal (AMP) and the Foldable/Traversable Proposal (FTP) (also sometimes referred to as the "Burning Bridges Proposal" based on the title of the original thread).


It has recently been highlighted that as these changes affect the Prelude, and thus affect what users of Haskell see out of the box, they should be held to a higher bar than the usual libraries@ traffic. In particular, there was concern that while the `Applicative`/`Monad` Proposal was warned about extensively in GHC 7.8, the `Foldable`/`Traversable` Proposal was not nearly as well broadcast.


However, there are many good reasons to do both the AMP and FTP generalizations at this time.

- By generalizing the `Prelude`, the `Prelude` will no longer cause name collisions anywhere within `base`. This is a very simple rule to state; it is a very simple rule to understand.

- `Foldable` and `Traversable` are abstractions that have seen long use in the Haskell community, predating even the existence of `Applicative`, dating back into the early 2000s. We know and have tested these abstractions, and they have deep explanatory power.

- `Traversable` in particular has given us insight into the nature of finitary traversals and have been the subject of many papers since Jeremy Gibbons wrote "The Essence of the Iterator Pattern" each of which has managed to narrow the set of laws until we're left with only the obvious generalization of the `Functor` laws that allows for `Applicative` effects. 

- Despite broad explanatory power and the ability to derive connections between a large chunk of the combinators that came before these abstractions were even named, they remain relegated to modules off to the side that contain combinators that are harder to use for the simple reason that using them requires qualified imports or massive amounts of manual hiding.

- `Foldable` shows us a fairly deep connection between 33+ seemingly unrelated operations from the `Prelude`. `Traversable` adds connections between several more to that mix.

- Once we make `Applicative` a superclass of `Monad`, `mapM` is basically \*never\* the right combinator for, well, anything. It requires too tight a constraint (`Monad` rather than `Applicative) on one hand. Going the 'wrong' direction and adding a monomorphized `traverse\` breaks far more code, and makes the scoping problems worse.

- One thing that we were very careful to avoid during this entire wave of generalization is an impact to performance on either asymptotic or constant factor grounds. The continuous performance testing that is done on GHC has done a lot to keep us honest in this regard, but has definitely complicated development.

## FAQ

- Foldable has lots of class members. Why is it so complicated?


The first variant of `Foldable` that people tend to propose is to reduce it to something like

```wiki
class Foldable f where
  toList :: f a -> [a]
```


But this requires us to be able to fully re-associate all of the elements of the structure `f` to the right to make a list out of them.


To repair that we need to switch to something like

```wiki
class Foldable f where
  foldMap :: Monoid m => (a -> m) -> f a -> m
```


This gives us a version where folding over the container relies strictly on the monoid laws of `m`, but doesn't require us to use the ability to reassociate an infinite number of times.


So, why isn't `Foldable` just this?


It turns out that `foldr` implemented in terms of `foldMap` builds up a gigantic function chain before invoking it. This can leak space, and runs slower, and some variants of this can even blow the stack. Allowing versions of these combinators to be supplied for specific containers can avoid such overhead, so several years ago when the class was first concocted a number of specialized folds were added to address such concerns.


Some combinators in `Foldable` compute their answers by different means than their Prelude counterparts, even just by exploiting the `Monoid` laws. e.g. `sum = getSum . foldMap Sum`, for some containers can compute in exponentially faster time than the traditional `Prelude`'s `sum = foldl (+) 0` definition.


There are at least 3 different camps out there for what the proper definition of `sum` should be:

```wiki
sum = foldl (+) 0
sum = getSum . foldMap Sum
sum = foldl' (+) 0
```


The first group is those who believe sum should follow the existing `Prelude` behavior at all costs.


The second group is concerned with ensuring the correct asymptotics for the operation.


The third group notes that the `foldl` definition is sadly naïve, and requires stack frames for every entry in the list, especially if the numeric type involved isn't known and so the compiler can't optimize away to something closer to the third representation based on strictness information for `(+)`.


By adding some of these methods where the `Prelude` behavior and `Data.Foldable` behavior to the class, we are able to ensure that the existing semantics hold for existing programs that were using the `Prelude` combinators without compromising on the asymptotic behavior of code that is written with `Foldable` today.


Finally, in a proposal back in September, David Feuer and Reid Barton proposed adding several more members to `Foldable` to enable specific containers to offer asymptotically even more efficient versions of many combinators.


The class is nearly as small as it can be without either changing the semantics of existing programs on their authors, or compromising the asymptotic performance of `Foldable` operations.

- Traversable contains both Monad and Applicative variants for each function, and following the Applicative-Monad proposal, the Monad variants (mapM and sequence) are now redundant.


We cannot remove `mapM` from the `Traversable` class without a deprecation cycle. Users have defined \*many\* instances of this class in the wild.


But there is another concern. It turns out that there exists a form of container that you can write for which `traverse` will blow the stack when `mapM` will not!

- Given Foldable and Traversable may benefit from further refinement, dragging them into Prelude seems premature.


The combinators will remain, on the other hand whether we go through a smooth deprecation cycle to remove some from the class and move them out to top level definitions when and if we can find ways to implement them without suffering an asymptotic or large constant factor hit is the major concern.


We are proactively seeking ways to resolve this issue. Ticket [\#10071](prelude710/ftp#)([ https://ghc.haskell.org/trac/ghc/ticket/10071](https://ghc.haskell.org/trac/ghc/ticket/10071)) explores adding the ability to deprecate class member redefinition. This gives us the ability to move things out of the class over a pair of release cycles, should we find something we can improve in this manner.

- Data.List now has many functions that don't mention list in their type signature. Having such functions in the Data.List module is awkward from a naming perspective. In contrast, modules like Data.Map export many functions from Data.Foldable, specialized to Map.


From a data-driven perspective, the vast majority of users of `Data.List` import it unqualified to get at other combinators, such as `sort`, which aren't mentioned in the `Prelude`. Let's call this "group A". Having such an import break dozens of unrelated combinators seems like a bad idea.


By exporting generalized versions rather than removing them, we are able to support group A, but also the second most common scenario, where users of Data.List follow the qualified import pattern of Data.Map, let us call this "group B".


However, it is an ugly intermediate state at best.


There are two clear paths for how to evolve Data.List from here.


The first is to deprecate the re-export of the methods from the Prelude in 7.12 and to remove them entirely in 7.14. This ensures that group A never feels any pain at all, and that group B gets a deprecation window of warnings notifying them that they don't have to use the combinators qualified any more. The cost of this approach is that we'd have no place in `base` to house monomorphic versions of these combinators.


A second alternative that may be viable is to concoct some form of WEAK pragma or enable users to export type restricted versions of another combinator and then apply this pragma to these members of `Data.List`. This could revert those combinators to monomorphic form, but requires a more controversial language extension that has some potentially thorny implementation issues to work through, and we've elected not to presume they can be resolved.


By adopting the intermediate state we enable the maximal amount of existing code to continue to work, without committing to either one of these plans at this time without broader community discussion.

- There are lots of functions that could be generalized further, but are not. For example, mapM, forM and sequence could all be expressed in terms of Applicative instead of Monad. 


We can't remove `mapM` for the reasons mentioned above at this time: It is an existing member of the class and can't be removed without a deprecation cycle, but it also has the concrete counter-example where `mapM` can avoid blowing the stack where `traverse` must. `sequence` is similarly an existing member of the class, and can't be removed without a deprecation cycle. 

- Similarly things like length could be generalized to Num, making length and genericLength equivalent.

`length` is left ungeneralized with regards to the numeric type primarily because `genericLength` has absolutely abysmal performance. One of the pragmatic guidelines we followed is that we can't make code slower.

- While the Prelude operations (e.g. foldr) will now work on containers such as Vector, they still won't work on things like ByteString or Text, which in some code is used far more than other non-list containers.


The trend of API duplication for monomorphic containers cannot be entirely reversed without accepting a lot of limitations, moving to a library that lies outside of what is standardizable, and simultaneously giving up a lot of the nice theoretical properties that motivate Foldable and Traversable.

- Some functions in Data.List could be generalized to Foldable, but have not been. For example, isPrefixOf and isInfixOf can be generalised. More generally, anything with a list in an argument position can be generalized.


There are two lists involved in the signatures of `isPrefixOf` and `isInfixOf`. Which gets generalized? Both? Adding `toList` to the class to avoid destroying sharing on the list case with `foldr (:) []` enables us to match the existing performance of these operations even with generalized signatures. However, they aren't in the `Prelude`, and they aren't historically in `Data.Foldable`. There isn't a strong argument _against_ generalizing them except for the fact that the generalized forms would have no good place to live. The changes we've made enable such general code to be written efficiently by users who want them.

- Some functions in Data.List could be generalised to Traversable, but have not been. For example, sort and reverse can be generalized. However, such generalizations are likely to add a performance penalty.


The runs afoul of the "first do no harm" principle from the standpoint of performance.

- Given that lots of functions could be generalized, it seems we should either generalize everything, or have a good story for where to stop. For example, isPrefixOf can be generalized, but the related function stripPrefix can only be partly generalized, so should isPrefixOf be generalized?

- The IsList class is an alternative generalization that could be made for some functions, and would work for ByteString and Text. Neither Foldable nor IsList is strictly more general, so both are potential alternatives.


It also gives up a number of existing inhabitants, or gives up the ability to reason polymorphically about the existing inhabitants such as Data.Map. There are _many_ Foldable containers that contain more than just their elements. IsList requires both the ability to consume the elements of a structure but the ability to construct the structure as well.

- It should also be noted that `Traversable` can be added to `Prelude` without adding `Foldable`.  Today `Foldable` is a superclass of `Traversable`, but there is no real need for that.  (E.g., building the lens package and all its dependencies only requires 9 trivial changes when `Foldable` is no longer a superclass.)


Removing Foldable as a superclass of Traversable destroys a relationship between the Traversable class and 33+ operations with well known and well understood semantics.

- The existing corpus of books, tutorials, syllabi, and the like usually have a significant portion of the text dedicated to these very Prelude functions - and they would all need significant revision. 


At least two books already teach the Foldable and Traversable abstractions: "Learn You a Haskell" and "Beginning Haskell". Moreover, other texts such as "Real World Haskell" introduce the monad operations by telling the user to pretend the monad operations are specific to IO. "Read m as IO when you see it".

- Teaching beginners what sequence means in its full generality is going to be a challenge.


This somehow has never been a problem for Python. They don't even have the types to help guide them.

## Alternatives to the generalization


There are a number of alternative approaches to how the generalization could be introduced into the code base. None of these approaches are fully worked through, and would not be ready for GHC 7.10, but could be adapted for GHC 7.12.

- A language pragma could be used select alternative Preludes.


This fails on many grounds. 

`Control.Monad` re-exports `mapM`, `mapM_`, `sequence`, `sequence_`, `forM` and `forM_`. Which version do users get?

`Data.List` re-exports large numbers of combinators from the Prelude as well.


The `mtl` and many other libraries based around monads and monad transformers re-exports `Control.Monad`, so you get a whole ecosystem where where folks some times have 15+ imports at the top of a module, and every one of them would have to agree on which variant of the Prelude they were re-exporting or you'd start getting conflicts.

- We could support restricting type signatures in export lists, so that when both a specific and general version are imported they do not clash.


When and if such a proposal was implemented we could use this to resolve the intermediate Data.List ugliness, yes.

- A module with only the non-Foldable overlapping bits of Data.List could be created, allowing users who wanted Foldable plus some list functions to avoid name clashes.


This requires a proactive change on the behalf of a rather large segment of users.

- The functions in Data.Foldable and Data.Traversable could be renamed not to clash. It is very common for generalized versions of functions to have a different name than their non-generalized counter parts: fmap and map, mappend and (++).  If the members of Data.Foldable were minimized (see above), then it might be reasonable to rename its foldr something like ffoldr.


This ignores the fact that a very very large segment of the community is already using those combinators.
