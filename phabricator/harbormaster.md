# Harbormaster: continuous integration & patch building for GHC


When **you use Arcanist to submit a diff**, Phabricator will automatically trigger a build rule using an application called "Harbormaster". This application causes a build machine in the background to apply your patch and run `sh ./validate`. Afterwords, your diff will be updated with a status from Harbormaster about the build, including links to build logs and testsuite results.


The current status of the build is at the top. [ Phab:D162](https://phabricator.haskell.org/D162) is a good example:

[](https://i.imgur.com/spVHWUT.png)


Click on the link posted by Harbormaster (in this case, "Harbormaster completed building B467: Diff 379") to the build to go to the Buildable, like [ Phab:B467](https://phabricator.haskell.org/B467) in this case. There you can follow links to individual builds, containing stdout/stderr logs, like [ https://phabricator.haskell.org/harbormaster/build/468/](https://phabricator.haskell.org/harbormaster/build/468/)


Note that every time you run `arc diff` and update an existing review or create a new one, you'll trigger a build.


Harbormaster also builds GHC commits that have been pushed. You can see more on the [ Harbormaster](https://phabricator.haskell.org/harbormaster) application.

**Note**: You can use the word "\[ci skip\]" by itself in the commit when you submit a diff or push something to skip builds. You may want to do this if you know your build will break, but you just want to post code.

## Navigating the application UI


However, it's also useful to understand the application UI as well, so you can look through (or search for) previous builds.


First, go to [ https://phabricator.haskell.org/harbormaster](https://phabricator.haskell.org/harbormaster), the top-level application. Here, you can see a list of all the latest builds; for example -

[](https://i.imgur.com/MZJBSWa.jpg)