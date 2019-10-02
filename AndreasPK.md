This is a scrap page, so things might be duplicated and those kinds of things.

Tickets I'm interested in:

https://gitlab.haskell.org/ghc/ghc/issues/16977 - Possibly a quick fix.

https://gitlab.haskell.org/ghc/ghc/issues/9660 - After/During tag inference



## Current agenda:

#16970 - Tag inference work  
#16977 - Check simons suggestion  
#17092 - Implement stack check elision  
#7741 - NCG-SIMD  

## Things to do in the near future:

* #16977
* SIMD
* Branch likelyhood: #14672
* Register allocator improvements: #7063, #8048, #12232, #13051, #16243
* Avoid certain stack checks: https://gitlab.haskell.org/ghc/ghc/issues/17092

* #17238 Cmm Lookup tables
* #9342 - Branchless Int operations
* #17237 Cmm FloatOut - Put shared cmm statements into the common path
* #17238 Generate lookup instead of jump tables


Most things [runtime performance related](https://gitlab.haskell.org/ghc/ghc/issues?scope=all&utf8=%E2%9C%93&state=opened&label_name[]=runtime%20perf)

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