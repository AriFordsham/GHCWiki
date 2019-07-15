# How to change GHC's documentation

GHC runs on GitLab: https://gitlab.haskell.org/ghc/ghc

## Small change

The simplest way to contribute patches is using the GitLab web interface. If you find a file under the

* [*libraries*](https://gitlab.haskell.org/ghc/ghc/tree/master/libraries)
* [*users guide*](https://gitlab.haskell.org/ghc/ghc/tree/master/docs/users_guide)

like [`libraries/base/Control/Monad.hs`](https://gitlab.haskell.org/ghc/ghc/blob/master/libraries/base/Control/Monad.hs) you can click **Edit** which takes you to an online editor:

![Screenshot_from_2019-07-15_14-17-52](uploads/8da929b4c0df53ced9d952d2fa6748b1/Screenshot_from_2019-07-15_14-17-52.png)

Change the documentation, write a *descriptive commit message* and *Commit changes*. That's it!

## Big change


Updating [GHC's documentation](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/) is easy, and can benefit new comers greatly !


GHC's documentation is included within in its source code, using [Haddock's markup](http://haskell-haddock.readthedocs.io/en/latest/markup.html).


Here are the steps to modify it:

1. **Create a [Pull Request](https://github.com/ghc/ghc/pulls?utf8=%E2%9C%93&q=is%3Apr)** on [ GitHub](https://github.com/ghc/ghc/). 
1. **Wait** for the reviewers to look at your patches. If this takes more than a week, complain! 
1. **Wait** for bgamari to publish the documentation. (As of Nov 2017, he runs the publish script manually, about once a month.)  If this takes more than a month, complain on the `#ghc` channel on [FreeNode](http://freenode.net/) !  


Loathe github? You can update documentation using [Phabricator](phabricator) too!


Then have a beer on us.  We are truly grateful.
