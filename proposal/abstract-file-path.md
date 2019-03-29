# Abstract `FilePath` Proposal (AFPP)

*Herbert Valerio Riedel, Neil Mitchell & Michael Snoyman*

## Motivation



The [Haskell2010](https://www.haskell.org/onlinereport/haskell2010/) `Prelude` defines


```
type FilePath = String
```


This representation has several shortcomings:

- Inefficient in both time and space due to `[Char]`:

  - On 64bit GHC it typically allocates 24 bytes per `FilePath`-character.
  - Heap fragmentation as each character is represented by a separate heap object.
  - Manipulating `FilePath` values involves lots of costly pointer chasing.

- Requires conversions/allocations for each system-call
  (e.g. POSIX this requires conversion to/from `CString`) as well as
  validations.

- Round-tripping issues, where files have their underlying encoding lost on conversion to `String`.

- Not type-safe by being a synonym of `String`.

- Different types of representation in packages such as
  [hackage:posix-paths](http://hackage.haskell.org/package/posix-paths) and
  the [hackage:unix](http://hackage.haskell.org/package/unix) package (e.g. `System.Posix.*.ByteString`)


One approach would be to create a new `FilePath` type, while leaving
`type FilePath = String` in place.  However, since the
`Prelude.FilePath` type is ubiquitously used in many APIs, this would
either amount to duplicate APIs or require conversions between the old
and the new `FilePath` type.

## Proposal



Instead, we propose to phase out `type FilePath = String` in 3 phases:


<table><tr><th><b>Phase 1</b></th>
<td>
Add type forward-compat conversion functions <tt>toFilePath</tt> &amp; <tt>fromFilePath</tt> to the standard <tt>System.IO</tt> module, as that module is the official home of the <tt>FilePath</tt> type in the Haskell Report.
</td></tr></table>


>
>
> Also add these conversion functions for
> forward-compatibility to the `filepath` package:
>
>

```
module System.FilePath where

#if MIN_VERSION_base(4,9,0)
-- re-export {to,from}FilePath from System.IO
#else
-- | Total Unicode-friendly encoding
toFilePath :: String -> FilePath
toFilePath = id

fromFilePath :: FilePath -> String
fromFilePath = id
#endif
```

<table><tr><th><b>Phase 2</b></th>
<td>Have GHC warn when a <tt>String</tt>-value is used where the
<tt>FilePath</tt> synonym is expected
</td></tr></table>


>
>
> TODO needs investigation if it's feasible to implement
>
>

<table><tr><th><b>Phase 3</b></th>
<td>Make <tt>FilePath</tt> abstract in <tt>base</tt>, i.e. make
<tt>FilePath</tt> into a <tt>newtype</tt>/<tt>data</tt> but don&apos;t export its
constructor via non-internal modules. <tt>Prelude</tt>  and <tt>System.IO</tt> would continue to
export the type <tt>FilePath</tt>. <tt>System.IO</tt> will continue to export <tt>toFilePath</tt>/<tt>fromFilePath</tt>. 
</td></tr></table>


>
>
> Since the internal details are not exposed, they can be changed later,
> but we envisage:
>
>

```
-- | Internal module exposing internals needed for add-on packages to
-- provide efficient operations involving `FilePath`s
module GHC.FilePath(FilePath(..),...) where
...

-- Using unpinned bytearrays to avoid Heap fragmentation and
-- which are reasonably cheap to pass to FFI calls
-- wrapped with typeclass-friendly types allowing to avoid CPP
-- 
-- Note that, while unpinned bytearrays incur a memcpy on each
-- FFI call, this overhead is generally much preferable to
-- the memory fragmentation of pinned bytearrays
data WindowsFilePath = WFP ByteArray# -- UTF16 data
data PosixFilePath   = PFP ByteArray# -- char[] data as passed to syscalls

#ifdef WINDOWS
type PlatformFilePath = WindowsFilePath
#elif POSIX
type PlatformFilePath = PosixFilePath
#else
# error "no filepath representation available for this platform yet"
#endif

-- | Type representing filenames/pathnames
newtype FilePath = FilePath PlatformFilePath -- constructor not exported from Prelude

instance IsString FilePath where 
    fromString = toFilePath

-- | \"String-Concatenation\" for 'FilePaths'
--
-- This allows to write forward-compatible code for Haskell2010 'FilePath`s
--
-- E.g. code can be written (assuming `-XOverloadedStrings`) like
--
-- > tarfname = basedir </> "ghc-" <> ver <> "~" <> gitid <.> "tar.xz"
--
-- That has the same semantics with pre-AFPP and post-AFPP 'FilePath's
--
-- NB: 'mappend' is *not* the same as '(</>)', but rather matches the semantics for pre-AFPP 'FilePaths'
instance Monoid FilePath where 
    mempty      = <...>
    mappend a b = <...string-concat...>
```


Throughout the process, the `System.FilePath` module in the [hackage:filepath](http://hackage.haskell.org/package/filepath) package would continue to expose the same API as now, changing with the internal `FilePath` definition as necessary.

## Issues not addressed by this Proposal

- This proposal does not define the platform/implementation-specific representation of the new `FilePath` type.

## Decisions assumed by this Proposal

- This propsoal assumes there is no type-level distinction of path-subtypes, e.g. relative/absolute or directory/file.
