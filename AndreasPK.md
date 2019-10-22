This is a scrap page, so things might be duplicated and those kinds of things.

Tickets I'm interested in:

https://gitlab.haskell.org/ghc/ghc/issues/16977 - Possibly a quick fix.

https://gitlab.haskell.org/ghc/ghc/issues/9660 - After/During tag inference



## Current agenda:


#16354 - LLVM broken on windows - TODO

Also include it in CI:
AndreasK, Ideally we would handle LLVM toolchain the same way we handle GHC and xcabal-install
3:59 PM see .gitlab/win32-init.sh

!1990 - Waiting on CI
!1636 - Waiting on CI
!1742 - Waiting on gabor
#1990 - Use dataToTag - needs work
#16970 - Tag inference work  - First do #17004
#16977 - Check simons suggestion - Done by osa - verify it solved the issue. 
#17092 - Implement stack check elision  - TODO
#7741 - NCG-SIMD  - TODO

## Things to do in the near future:

* #16977
* SIMD
* Branch likelyhood: #14672
* Register allocator improvements: #7063, #8048, #12232, #13051, #16243
* Avoid certain stack checks: https://gitlab.haskell.org/ghc/ghc/issues/17092

* #9342 - Branchless Int operations
* #17237 Cmm FloatOut - Put shared cmm statements into the common path
* #17238 Generate lookup instead of jump tables
* #17242 Use dataToTag# for Eq instances
* #17240 Code duplication for code evaluating arguments.

Most things [runtime performance related](https://gitlab.haskell.org/ghc/ghc/issues?scope=all&utf8=%E2%9C%93&state=opened&label_name[]=runtime%20perf)

### Out of the above these have likely bounded time requirements

* #17242 Use dataToTag# for Eq instances
* #9342 - Branchless Int operations
* #17238 Generate lookup instead of jump tables

### Regressions worth looking into

https://gitlab.haskell.org/ghc/ghc/issues/17133

## Pointer tagging in GHC

See also the "pointer tagging" tag: [issues](https://gitlab.haskell.org/ghc/ghc/issues?scope=all&utf8=%E2%9C%93&state=opened&label_name[]=pointer%20tagging)

### #14373 Pointer tagging for large families

Instead of tagging large families with a flag indicating evaluation, tag them with the tag if possible and the flag only for constructors not fitting in the tag.

This is a project of Gabor Greif and he indicated he still want's to finish this recently (@ICFP 2019)

He stated this is blocked on #15155 in the past but I did not look into why/how.

### #17004 Export LFInfo for bindings

This patch exposes the backends view of exported ids in interface files.

The benefits are that we can:
* Tag certain bindings which do not have an unfolding otherwise. 
* Get accurate information about arity *after all transformations have been applied*

### Untagged pointers find their way into strict fields.

There are two tickets about this currently:

* #15155 How untagged pointers sneak into banged fields
* #14677 Code generator does not correctly tag a pointer

Both are related to #16970, implementing #16970 will also fix both of these.

### Other smaller issues:

See the issues tagged with [`pointer tagging` on the tracker](https://gitlab.haskell.org/ghc/ghc/issues?scope=all&utf8=%E2%9C%93&state=opened&label_name[]=pointer%20tagging).