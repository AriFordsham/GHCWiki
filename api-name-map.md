## Name Map for use with the GHC API


The `ParsedSource`, together with [ApiAnnotations](api-annotations) provide an accurate view of the source code for a module.  Together with supporting libraries such as `ghc-exactprint`, this allows tool writers to make changes to the source, by manipulating the `ParsedSource`, and then using `ghc-exactprint` to generate haskell source code reflecting the changes made, but with nothing else changed. So it preserves layout, comments and so on.


Note: `ParsedSource` is defined, in module `GHC` thus

```wiki
type ParsedSource      = Located (HsModule GhcPs)
```


The `ParsedSource` serves as input to the renamer, generating a different AST, which has most names resolved (except the ones for overloaded record fields).  But in the process, it is changed sufficiently that it can no longer be used as an accurate reflection of the original source code, in terms of layout, comments and so on.

*SPJ: would one possibility to ensure that it can be used as an "accurate reflection of the original source code"?   How hard would that be?  It's very close isn't it?  Parentheses are maintained.  Things like `HsWrap` and `AbsBinds` can easily be discarded.*


So the problem a tool writer faces is that they have the `ParsedSource` which accurately represents the original source, and should be changed to modify the original source, and the ASTs from the renamer and typechecker which fully capture the `Name` information for every identifier in the module.


Many manipulations require access to this later stage information, but as things stand now (GHC 8.6.2) there is no simple way to tie up a `RdrName` from the `ParsedSource` with a `Name` in the `RenamedSource`.

*SPJ: I don't understand clearly enough what you mean.*  Perhaps you mean: 

- given a particular `RdrName` occurring in the `ParsedSource`  --- perhaps at a binding site, perhaps at an occurrence site -- find the `Name` that the reamer makes for it.


But 

- The `Name` alone may not be much use.  Don't you want the `Id` or `TyCon` or `Class` or whatever?
- Moreover, it's possible that the renamer may re-use the same `Name` more than once.  (It doesn't do this much if at all, I agree, but still.)
- Do you intend this for nested situations; e.g

  ```wiki
  f x = let y = x+1
        in y+x
  ```

  and point to the `RdrName` occurrence of `y` in `y+x`?


Perhaps you intend this only for top-level stuff?
*End SPJ*

### Possible Solutions

#### A. Use `GlobalRdrEnv`.


This is a mapping from an `OccName` to the various `Name`s it represents in the source, each represented in a `GlobalRdrElt`.


The problem with this is that the index to the `GlobalRdrEnv` table is a hash of the text of the `OccName`, and so has no location information. i.e. we cannot distinguish between a variable in different occurrences. And nothing in the `GlobalRdrElt` identifies any of the original usage locations.

#### B. Extend the `GlobalRdrEnv` mapping to include the missing information.


Since every `RdrName` of interest is `Located`, it would be enough to add a list of occurrence `SrcSpan`s in the `GlobalRdrElt`.

#### C. Create a separate table mapping each `RdrName` occurrence `SrcSpan` to the corresponding `Name`.


This is the approach currently used in `HaRe`, and is presented for inclusion in the GHC API as [https://phabricator.haskell.org/D5330](https://phabricator.haskell.org/D5330)

#### D. Modify the `RenamedSource` AST so that it also fully represents the parsed source file, when used with the API Annotations.

### Discussion


AZ opinions:


Option A is not viable. Option B could work, but would have to be subject to a `DynFlag` otherwise the space penalty would be prohibitive for normal usage.
Option C is a "bolt-on", but is known to do the job.


Option D would be the best from a tooling perspective, but would be a massive engineering effort, and hard to justify given the use case.
 
