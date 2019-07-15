# How to change GHC's documentation

GHC runs on GitLab: https://gitlab.haskell.org/ghc/ghc

Small changes only change comments, not code. Limited to one file.

The continuous integration testing also has slow turnaround. Make sure not to add trailing whitespace :) but don't be afraid to submit fixes. We appreciate your contribution.

## Small changes

The simplest way to contribute patches is using the GitLab web interface. To write a patch for [`Control.Monad`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Monad.html) you would go to [`libraries/base/Control/Monad.hs`](https://gitlab.haskell.org/ghc/ghc/blob/master/libraries/base/Control/Monad.hs) where the **Edit** button takes you to an online editor:

![Screenshot_from_2019-07-15_14-17-52](uploads/8da929b4c0df53ced9d952d2fa6748b1/Screenshot_from_2019-07-15_14-17-52.png)

Change the documentation, write a *descriptive commit message* and *Commit changes*. 

This takes you to **New Merge Request**: Follow the instructions and finally **Submit merge request**.

That's it!

## Big changes

Updating [GHC's documentation](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/) is easy, and can benefit newcomers greatly!

GHC's documentation is included within in its source code, using [Haddock's markup](http://haskell-haddock.readthedocs.io/en/latest/markup.html).

Here are the steps to modify it:

1. **Create a [Merge Request](https://gitlab.haskell.org/ghc/ghc/merge_requests)** on [GitLab](https://gitlab.haskell.org/ghc/ghc/). 
1. **Wait** for the reviewers to look at your patches. If this takes more than a week, complain! 
1. **Wait** for bgamari to publish the documentation. (As of Nov 2017, he runs the publish script manually, about once a month.)  If this takes more than a month, complain on the `#ghc` channel on [FreeNode](http://freenode.net/) !  


Loathe github? You can update documentation using [Phabricator](phabricator) too!


Then have a beer on us.  We are truly grateful.
