# This is a proposal / discussion page for adding annotations to the AST.


Right now the locations of syntactic markers such as `do`/`let`/`where`/`in`/`of` in the source are discarded from the AST, although they are retained in the rich token stream.


The `haskell-src-exts` package deals with this by means of using the [ \`SrcSpanInfo\`](http://hackage.haskell.org/package/haskell-src-exts-1.15.0.1/docs/Language-Haskell-Exts-SrcLoc.html#t:SrcSpanInfo) data type which contains the SrcSpan as per the current GHC Located type but also has a list of `SrcSpan`s for the  syntactic markers, depending on the particular AST fragment being annotated.


In addition, the annotation type is provided as a parameter to the AST, so that it can be changed as required, see [ here](http://hackage.haskell.org/package/haskell-src-exts-1.15.0.1/docs/Language-Haskell-Exts-Annotated-Syntax.html#t:Annotated).


The motivation for this change is then

1. Simplify the roundtripping and modification of source by explicitly capturing the missing location information for the syntactic markers.

1. Allow the annotation to be a parameter so that it can be replaced with a different one in tools, for example HaRe would include the tokens for the AST fragment leaves.

1. Aim for some level compatibility with haskell-src-exts so that tools developed for it could be easily ported to GHC, for example [ exactPrint](http://hackage.haskell.org/package/haskell-src-exts-1.15.0.1/docs/Language-Haskell-Exts-Annotated-ExactPrint.html#v:exactPrint).

## Richard Eisenberg response


For what it's worth, my thought is not to use SrcSpanInfo (which, to me, is the wrong way to slice the abstraction) but instead to add SrcSpan fields to the relevant nodes. For example:

```
|HsDoSrcSpan-- of the word "do"BlockSrcSpans(HsStmtContextName)-- The parameterisation is unimportant-- because in this context we never use-- the PatGuard or ParStmt variant[ExprLStmt id]-- "do":one or more stmtsPostTcType-- Type of the whole expression...dataBlockSrcSpans=LayoutBlockInt-- the parameter is the indentation level...-- stuff to track the appearance of any semicolons|BracesBlock...-- stuff to track the braces and semicolons
```


The way I understand it, the SrcSpanInfo proposal means that we would have lots of empty SrcSpanInfos, no? Most interior nodes don't need one, I think.


Popping up a level, I do support the idea of including this info in the AST.

## SPJ response to concern re extra noise in AST

>
> I thiink the key question is whether it is acceptable to sprinkle this kind of information throughout the AST. For someone interested in source-to-source conversions (like me) this is great, others may find it intrusive.


It’s probably not too bad if you use record syntax; thus

```
|HsDo{ hsdo_do_loc ::SrcSpan-- of the word "do", hsdo_blocks ::BlockSrcSpans, hsdo_ctxt   ::HsStmtContextName, hsdo_stmts  ::[ExprLStmt id], hsdo_type    ::PostTcType}
```

## Other issues


The AST is initially created by the parser, and then changed through the renamer and type checker.


From a source to source conversion perspective the `ParsedSource` is closest to the original source code, as it respects the original linear ordering of the declarations, which are each wrapped in an appropriate constructor from `HsDecl`.


The `RenamedSource` gathers all the like declarations together, and strips out the `HsDecl`, as well as re-ordering binds to appear in dependency order.


The `TypeCheckedSource` further changes the `RenamedSource` to replace the original type information with the calculated types.


So manipulations need to happen at the `ParsedSource` level, but with the ability to query information from the `RenamedSource` or `TypecheckedSource` as required.


At the moment HaRe manages this by building up a token tree indexed by SrcSpan with tokens at the leaves, constructed from the `ParsedSource`, and then indexes into it for changes based on the `RenamedSource`.  The token tree is fiddly and brittle, so it would be better to keep this information directy in the AST.