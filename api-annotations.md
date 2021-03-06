# This is a decription of the API Annotations introduced with GHC 7.10 RC2


The hsSyn AST does not directly capture the locations of certain keywords and
punctuation, such as 'let', 'in', 'do', etc.


These locations are required by any tools wanting to parse a haskell
file, transform the AST in some way, and then regenerate the original
layout for the unchaged parts.


Rather than pollute the AST with information irrelevant to the actual
compilation process, these locations are captured in the lexer /
parser and returned as a separate `ApiAnns` structure in the
`ParsedSource`.


This keeps both the keyword location and the original comments with their
locations.


```
type ApiAnns = ( Map.Map ApiAnnKey [SrcSpan]
               , Map.Map SrcSpan [Located AnnotationComment])

type ApiAnnKey = (SrcSpan,AnnKeywordId)
```


Each AST element with annotations has an entries in this Map. The key comprises the SrcSpan of the original element and the AnnKeywordId of the stored annotation. The value is a list of SrcSpans where that particular keyword appears. This is a list to cater for e.g. ";;;;". Top level elements are captured against 'nullSrcSpan'.



So for the source file "examples/Test.hs" having contents


```
-- |Comment not in a SrcSpan
foo x = -- Compute foo
  let a = 4 -- using a
  in a + x
```


The returned ApiAnns are


```
([((examples/Test.hs:(2,1)-(4,10), AnnEqual), [examples/Test.hs:2:7]),
  ((examples/Test.hs:(2,1)-(4,10), AnnFunId), [examples/Test.hs:2:1-3]),
  ((examples/Test.hs:(2,1)-(4,10), AnnSemi),  [examples/Test.hs:5:1]),
  ((examples/Test.hs:(3,3)-(4,10), AnnIn),    [examples/Test.hs:4:3-4]),
  ((examples/Test.hs:(3,3)-(4,10), AnnLet),   [examples/Test.hs:3:3-5]),
  ((examples/Test.hs:3:7-11,       AnnEqual), [examples/Test.hs:3:9]),
  ((examples/Test.hs:3:7-11,       AnnFunId), [examples/Test.hs:3:7]),
  ((examples/Test.hs:4:6-10,       AnnVal),   [examples/Test.hs:4:8]),
  ((<no location info>, AnnEofPos),           [examples/Test.hs:5:1])],

 [(examples/Test.hs:(2,1)-(4,10),
   [AnnLineComment "-- Compute foo"]),
  (examples/Test.hs:(3,3)-(4,10), [AnnLineComment "-- using a"]),
  (<no location info>,
   [AnnLineComment "-- |Comment not in a SrcSpan"])])
```


This allows the annotation to be retrieved by


```
-- | Retrieve a list of annotation 'SrcSpan's based on the 'SrcSpan'
-- of the annotated AST element, and the known type of the annotation.
getAnnotation :: ApiAnns -> SrcSpan -> AnnKeywordId -> [SrcSpan]
getAnnotation (anns,_) span ann
   = case Map.lookup (span,ann) anns of
       Nothing -> []
       Just ss -> ss
```

### Annotation structures



Each annotation is simply a `SrcSpan`.


```
-- | Note: in general the names of these are taken from the
-- corresponding token, unless otherwise noted
data Ann = AnnAs
         | AnnAt
         | AnnBang
         | AnnBy
         | AnnCase
         | AnnClass
         | AnnClose -- ^ } or ] or ) or #) etc
         | AnnColon
         | AnnColon2
         ..
```


Points to note:

1. `AnnOpen` / `AnnClose` capture all bracketed structures.

1. Where a value is being captured via e.g. `getINTEGER` the annotation index is called `AnnVal`.

### Capturing in the parser



The annotations are captured in the lexer / parser by extending `PState` to include a field


```
data PState = PState {
       ...
        annotations :: [(ApiAnnKey,SrcSpan)]
       }
```


The lexer exposes a helper function to add an annotation


```
addAnnotation :: SrcSpan -> Ann -> SrcSpan -> P ()
addAnnotation l a v = P $ \s -> POk s {
  annotations = ((AK l a), v) : annotations s
  } ()
```


The parser also has some helper functions of the form


```
type MaybeAnn = Maybe (SrcSpan -> P ())

gl = getLoc
gj x = Just (gl x)

aa :: Located a -> (Ann,Located c) -> P (Located a)
aa a@(L l _) (b,s) = addAnnotation l b (gl s) >> return a

ams :: Located a -> [MaybeAnn] -> P (Located a)
ams a@(L l _) bs = (mapM_ (\a -> a l) $ catMaybes bs) >> return a
```


This allows the annotations to be added in the parser productions as follows


```
ctypedoc :: { LHsType RdrName }
        : 'forall' tv_bndrs '.' ctypedoc {% hintExplicitForall (getLoc $1) >>
                                            ams (LL $ mkExplicitHsForAllTy $2 (noLoc []) $4)
                                                [mj AnnForall $1,mj AnnDot $3] }
        | context '=>' ctypedoc         {% ams (LL $ mkQualifiedHsForAllTy   $1 $3)
                                               [mj AnnDarrow $2] }
        | ipvar '::' type               {% ams (LL (HsIParamTy (unLoc $1) $3))
                                               [mj AnnDcolon $2] }
        | typedoc                       { $1 }
```

### Parse result


```
data HsParsedModule = HsParsedModule {
    hpm_module    :: Located (HsModule RdrName),
    hpm_src_files :: [FilePath],
       -- ^ extra source files (e.g. from #includes).  The lexer collects
       -- these from '# <file> <line>' pragmas, which the C preprocessor
       -- leaves behind.  These files and their timestamps are stored in
       -- the .hi file, so that we can force recompilation if any of
       -- them change (#3589)
    hpm_annotations :: ApiAnns
  }

-- | The result of successful parsing.
data ParsedModule =
  ParsedModule { pm_mod_summary   :: ModSummary
               , pm_parsed_source :: ParsedSource
               , pm_extra_src_files :: [FilePath]
               , pm_annotations :: ApiAnns }
```

## Notes / Shortcomings



Currently the annotations are only guaranteed to apply to the `ParsedSource`, the renaming process flattens some of the location hooks for


```
data HsType name
  ...
  | HsRecTy [Located [ConDeclField name]] -- Only in data type declarations
  ...

-- and

    HsDataDefn { 
    ...
                 dd_cons   :: [Located [LConDecl name]],
                     -- ^ Data constructors
   ...
```

# Comments


Comments are returned as annotations, if enabled by the existing `Opt_KeepRawTokenStream` flag.


There is an additional structure to the `PState` for comments indexed by `SrcSpan`, and also a queue of pending comments.


The basic mechanism is for `lexToken` in `Lexer.x` to accumulate the comments in the queue as parsing progressed, and when the helper function `addComments` is called with a `SrcSpan` argument it moves any comments in the queue that fit within the `SrcSpan` into the comment annotation structure indexed by the given `SrcSpan`.


These are then available for retrieval at the and, as with the existing annotations.

## Notes on comments

1. In practical terms, the usage of comments by tools requires them to be attached in some way to the logical units to which they apply. A case in point is comments preceding a function. In general, this is a hard problem, which is attacked via heuristics in `hindent` and `HaRe`.  This comment allocation would aim for a mechanistic allocation to the lowest enclosing `SrcSpan`, as a start.

1. It is possible to put the lexer into `Opt_Haddock` mode, a flag which is currently unset when `Opt_KeepRawTokenStream` is enabled. If these were made inclusive, it would be possible to explicitly tag the comments as being of type haddock, so that at a future date the annotations could possibly be used directly by haddock, rather than the very complicated parsing rules currently in place.
