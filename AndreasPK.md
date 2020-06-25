This is a scrap page, so things might be duplicated and those kinds of things.

Tickets I'm interested in:

https://gitlab.haskell.org/ghc/ghc/issues/9660 - After/During tag inference


## Current agenda:

IO Manager: https://gitlab.haskell.org/ghc/ghc/merge_requests/1224/diffs  
This is currently my main focus. Things below are on my list but not neccesarily listed by priority.

#16354 - LLVM broken on windows - TODO

Also include it in CI:
AndreasK, Ideally we would handle LLVM toolchain the same way we handle GHC and xcabal-install
3:59 PM see .gitlab/win32-init.sh

Pointer tagging in GHC (various tickets, see below).


* #16970 - Tag inference work
* #18285 - GHC should keep unfoldings for recursive functions which are suitable for specialization.
* #17238 - Generate lookup tables instead of jump tables where possible.
* !1990 - Use dataToTag - needs work
* #17092 - Implement stack check elision  - TODO
* #7741 - NCG-SIMD  - TODO

## Things to do in the near future:

* SIMD
* Branch likelyhood: #14672
* Register allocator improvements: #7063, #8048, #12232, #13051, #16243
* Avoid certain stack checks: https://gitlab.haskell.org/ghc/ghc/issues/17092

* #17784 lookupIdSubst fails because bindings are not in dep order.
* #13535 Simplifier choking on vector test suite
* !2580 - Update RULES for elem
* !2575 - MR to enable fdicts-strict by default. Seems to be correct, needs perf analysis.
* !2574- Refactor DataCon codegen - needs review
* #17759 investigate base functions which currently fail to specialize



* #9342 - Branchless Int operations
* #17237 Cmm FloatOut - Put shared cmm statements into the common path
* #17238 Generate lookup instead of jump tables
* #17242 Use dataToTag# for Eq instances
* #17240 Code duplication for code evaluating arguments.
* #1257 - Support unboxed tuples in GHCi/bytecode.
* #13763 - Runtime (register alloc?) regression

#### Maybe eventually list

* #15808 GHC Linker woes
* #16351 Constant folding for bit operations



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

This is a project of [Gabor Greif](GaborGreif) and he indicated he still want's to finish this recently (@ICFP 2019)

He stated this is blocked on #15155 in the past but I did not look into why/how.

### #17004 Export LFInfo for bindings

This patch exposes the backends view of exported ids in interface files.

The benefits are that we can:
* Tag certain bindings which do not have an unfolding otherwise. 
* Get accurate information about arity *after all transformations have been applied*

The benefits of this are:
* We can omit code to enter closures if we know statically they are evaluated constructors.
* We avoid entering closures since more closures will be appropriately tagged
* We can omit entry code for all constructors. If we can ensure all references to constructors are tagged there is never a reason to enter them.
* We can use a more efficient calling convention in some places, as LFInfo allows us to replace slow calls with more efficient variants.

### Untagged pointers find their way into strict fields.

There are two tickets about this currently:

* #15155 How untagged pointers sneak into banged fields
* #14677 Code generator does not correctly tag a pointer

Both are related to #16970, implementing #16970 will also fix both of these.

### Other smaller issues:

See the issues tagged with [`pointer tagging` on the tracker](https://gitlab.haskell.org/ghc/ghc/issues?scope=all&utf8=%E2%9C%93&state=opened&label_name[]=pointer%20tagging).

## GHC API

### Using the API to compile a String to Core

https://gist.github.com/AndreasPK/0c7ec8a6f01974c92271e54133a7aad0