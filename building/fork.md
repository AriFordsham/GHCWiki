# Getting ready to contribute (GHC fork)

Follow the following steps to get ready to contribute to GHC:

1. create an account on the [Gitlab instance](https://gitlab.haskell.org) of GHC
2. create your own fork of GHC there: after logging in, you can create a fork of GHC by clicking the fork button at [https://gitlab.haskell.org/ghc/ghc](https://gitlab.haskell.org/ghc/ghc).
3. get the GHC sources. **Do NOT clone your fork directly** as this will cause issues with relative submodule paths. Instead clone the official GHC repository as described in [Getting the sources](building/getting-the-sources).
4. add your fork of GHC as a new Git remote with the following command:

```
git remote add myFork git@gitlab.haskell.org:JohnDoe/ghc.git
```

"myFork" can be any name you choose. The exact url is given on your fork's gitlab page.

Now you can e.g. fetch and checkout a branch from your fork:

```
git fetch myFork
git checkout myFork/myBranch
```

Or push a branch into your fork to run the CI on it and to submit pull requests:
```
git push myFork myBranch
```

Note: if you really must clone your fork directly, you can use the python script .gitlab-ci/fix-submodules.py which tweaks the git submodule paths to point to gitlab.haskell.org/ghc/ghc.