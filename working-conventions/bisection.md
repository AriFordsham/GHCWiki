# Bisection to find bad commits


Say you have a program that fails to build or a test suite that fails at some point between
GHC 8.2.2 and 8.4.1. It can often be useful to know which commit introduced the regression. [Bisection](https://en.wikipedia.org/wiki/Bisection_%28software_engineering%29) is an efficient way of determining this, requiring at most `log_2(N)` commits to find the culprit among `N` commits.

Bisection can be done manually or with a git command.

## Preparation

With either bisection method, the work required to build GHC can be minimised by setting `BuildFlavour=quick`
in `mk/build.mk` if using a `make` build.

## Manual Bisection

Start by finding out which released GHC versions pass and fail your test case.
The tool [ghcup](https://www.haskell.org/ghcup/) can be used to switch between GHC versions. Let's say we've done this already and found out something has gone awry between GHC 8.2.2 and 8.4.1. To get the commit hashes between those release versions:

```
$ git show-ref -s ghc-8.2.2-release
aa3ffbdaffb1cdfc08720ebd3a8d3663ee3293f9
$ git show-ref -s ghc-8.4.1-release
985d8979a02fe297d0ccf121d3207983b8de3661
```

Next grab the range of commit hashes to manually bisect:

```
$ git log aa3ffb..985d89 --format=format:"%H"
f31c40efdc918bc9da8a325327ba5a472bd6ea9e
6540b7113aea04ef7feb3b849861fd4be7c38f1f
c760ae373d47a16170dab0b9ed6f1680a75d4263
...
```

## Git Bisect Command

This approach is especially appealing as git provides convenient support in the form of [\`git bisect\`](https://www.kernel.org/pub/software/scm/git/docs/git-bisect.html). `git bisect` coupled with a reliable test case and the script below (with appropriate modifications) turns the task of bisection into a relatively painless exercise.


**Note:** Bisecting revisions before the switch to submodules (i.e. more than a couple of months prior to the GHC 7.10.1 release) is quite difficult and is generally not worth the effort. The script below probably won't work in this regime.


Download the script below and edit it to reflect your test-case then begin the bisection:

```
$ git bisect start
$ git bisect good ghc-8.2.2-release   # we know the testcase worked here
$ git bisect bad ghc-8.4.1-release    # but it fails here
$ git bisect run ghc-bisect.sh
```


This will run the script for each point in the bisection, skipping commits which are unbuildable. Hopefully this will end with a message informing you of the first bad commit. A log of this procedure will be place in `$logs`: `$logs/all` gives a nice high-level overview and the remaining files record each step of the bisection.



By default the script will clean the tree for every commit. While this is likely to give correct results, it performs a number of potentially redundant rebuilds. The process can be made faster by setting `ALWAYS_CLEAN=0`, which will only clean the tree when a commit fails to build.


## ghc-bisect.sh


```bash
#!/bin/bash

logs=/mnt/work/ghc/tickets/T13930/logs
make_opts="-j9"
ghc=`pwd`/inplace/bin/ghc-stage2

mkdir -p $logs
rev=$(git rev-parse HEAD)

# Bisection step return codes
function skip_commit() { exit 125; }
function commit_good() { exit 0; }
function commit_bad() { exit 1; }
function stop_bisection() { exit 255; }

function log() {
    echo "$@" | tee -a $logs/all
}

function do_it() {
    step=$1
    shift
    log "Commit $rev: $step = $@"
    $@ 2>&1 | tee  $logs/$rev-$step.log
    ret="${PIPESTATUS[0]}"
    log "Commit $rev: $step = $ret"
    return $ret
}

function build_ghc() {
    do_it submodules git submodule update || skip_commit
    # We run `make` twice as sometimes it will spuriously fail with -j
    if [ -z "$ALWAYS_CLEAN" -o "x$ALWAYS_CLEAN" == "x0" ]; then
        # First try building without cleaning, if that fails then clean and try again
        do_it ghc1 make $make_opts || \
          do_it ghc2 make $make_opts || \
          do_it clean make clean && \
          do_it ghc3 make $make_opts || \
          do_it ghc4 make $make_opts || \
          skip_commit
    else
        do_it clean make clean || log "clean failed"
        do_it ghc1 make $make_opts || do_it ghc2 make $make_opts || skip_commit
    fi
}

# This is the actual testcase
# Note that this particular case depended upon the `cabal`
# library, which is checked out in $tree
function run_test() {
    tree=$HOME/trees/cabal
    cd $tree
    #do_it "clean-test" rm -R dist-newstyle
    do_it "build-test" cabal new-build cabal-install --disable-library-profiling --allow-newer=time --with-compiler=$ghc
    do_it "make-links" /home/ben/.env/bin/mk-cabal-bin.sh
    do_it "run-test" timeout 10 bin/cabal configure

    # The test has succeeded if the rule fired 
    if [ "x$?" = "x124" ]; then
        log "Commit $rev: failed"
        commit_bad
    else
        log "Commit $rev: passed"
        commit_good
    fi
}

if [ -z "$@" ]; then
    build_ghc
    run_test
else
    $@
fi
```

## Gotchas

### HTTPS

With an HTTPS clone, it is going to get tiring quickly to reenter credentials when
prompted given the number of submodules GHC has. Better to use SSH for the clone:

```diff
-- git clone https://gitlab.haskell.org/ghc/ghc.git
++ git clone git@gitlab.haskell.org:ghc/ghc.git
```

### Forks

If you're working off a fork of GHC then submodules are not going to work with the
script. To use the bisect script, clone of from `gitlab.haskell.org` instead:

```
$ git bisect run ./bisect.sh                                                      
running ./bisect.sh
Commit 2b5b9dc69e5d0af20b6e7be31638c2e3a1bb765f: submodules = git submodule update
Cloning into '/Users/.../ghc/.arc-linters/arcanist-external-json-linter'...
remote:
remote: ========================================================================
remote:
remote: The project you were looking for could not be found.
remote:
remote: ========================================================================
remote:
fatal: Could not read from remote repository.

Please make sure you have the correct access rights and the repository exists.
fatal: clone of
'git@gitlab.haskell.org:philderbeast/arcanist-external-json-linter.git' into
submodule path '/Users/.../ghc/.arc-linters/arcanist-external-json-linter' failed
Failed to clone '.arc-linters/arcanist-external-json-linter'. Retry scheduled
```

### Pre-8.2


If you are on Linux and see errors of the form,

```wiki
/usr/bin/ld: -r and -pie may not be used together
collect2: error: ld returned 1 exit status
```


You are seeing #12759 and need to cherry-pick d421a7e2.
