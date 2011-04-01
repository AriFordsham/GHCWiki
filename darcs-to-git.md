# Migrating patches from darcs to git


Suppose `ghc/darcs` is a darcs GHC tree containing patches that need to be migrated to git, and `ghc/git` is a git GHC tree.
We're not going to try to preserve the history; just get the code into the repo.


This assumes that all your changes are in the ghc repo itself. Otherwise, if you have made changes to other repos (e.g. `libraries/base`) you'll need to do a similar procedure for those repos too.


First, make a backup of the darcs repo in case something goes wrong:

```wiki
cp -a ghc/darcs ghc/backup-darcs
```


Now make sure `ghc/darcs` is fully up-to-date:

```wiki
cd ghc/darcs
./darcs-all pull -a
cd ../..
```


Next we checkout the git repo as it was at the darcs branch point:

```wiki
git clone ghc/git ghc/migrate
cd ghc/migrate
git checkout -b "some-descriptive-name" ghc-darcs-git-switchover
cd ../..
```


Now we put the git meta-data into our darcs repo, record the changes with git and merge them back to master:

```wiki
cd ghc/darcs
mv ../migrate/.git .
git commit -a
git checkout master
git merge "some-descriptive-name"
cd ../..
```


Finally, we pull the changes into our real git repo:

```wiki
cd ghc/git
git pull ../../ghc/darcs master
```


The `ghc/migrate` directory is no longer needed and can be removed.
