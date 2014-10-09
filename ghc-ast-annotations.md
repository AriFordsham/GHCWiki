# This is a proposal / discussion page for adding annotations to the AST, for ticket [\#9628](https://gitlab.haskell.org//ghc/ghc/issues/9628)


Right now the locations of syntactic markers such as `do`/`let`/`where`/`in`/`of` in the source are discarded from the AST, although they are retained in the rich token stream.


The `haskell-src-exts` package deals with this by means of using the [ \`SrcSpanInfo\`](http://hackage.haskell.org/package/haskell-src-exts-1.15.0.1/docs/Language-Haskell-Exts-SrcLoc.html#t:SrcSpanInfo) data type which contains the SrcSpan as per the current GHC Located type but also has a list of `SrcSpan`s for the  syntactic markers, depending on the particular AST fragment being annotated.


In addition, the annotation type is provided as a parameter to the AST, so that it can be changed as required, see [ here](http://hackage.haskell.org/package/haskell-src-exts-1.15.0.1/docs/Language-Haskell-Exts-Annotated-Syntax.html#t:Annotated).


The motivation for this change is then

1. Simplify the roundtripping and modification of source by explicitly capturing the missing location information for the syntactic markers.

1. Allow the annotation to be a parameter so that it can be replaced with a different one in tools, for example HaRe would include the tokens for the AST fragment leaves.

1. Aim for some level compatibility with haskell-src-exts so that tools developed for it could be easily ported to GHC, for example [ exactPrint](http://hackage.haskell.org/package/haskell-src-exts-1.15.0.1/docs/Language-Haskell-Exts-Annotated-ExactPrint.html#v:exactPrint).

1. Allow simple round-tripping of code via a GHC variant of [ hindent](https://hackage.haskell.org/package/hindent)

1. There is a strong motivation for the annotation to be / support Functor [ here](http://www.davidchristiansen.dk/2014/09/06/pretty-printing-idris/) for the idris IDE support

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

## Abortive annotation parameter attempt

[ D246](https://phabricator.haskell.org/D246) captures an attempt to work through a type parameter. This exploded in complexity, and was abandoned.

## SPJ alternative suggestion

>
> Another way to tackle this would be to ensure that syntax tree nodes have a "node-key" (a bit like their source location) that clients could use in a finite map, to map node-key to values of their choice.


An initial investigation shows some complexity blow up too.  The same effect can be achieved with a virtual node key.

## AZ Virtual node key proposal


Instead of physically placing a "node-key" in each AST Node, a virtual
node key can be generated from any `GenLocated SrcSpan e` comprising a
combination of the `SrcSpan` value and a unique identifier from the
constructor for `e`, perhaps using its `TypeRep`, since the entire AST
derives Typeable.


To further reduce the intrusiveness, a base Annotation type can be
defined that captures the location of noise tokens for each AST
constructor. This can then be emitted from the parser, if the
appropriate flag is set to enable it.


So

```
dataApiAnnKey=AKSrcSpanTypeRep

    mkApiAnnKey ::(Located e)->ApiAnnKey
    mkApiAnnKey =...dataAnn=....|AnnHsLetSrcSpan-- of the word "let"SrcSpan-- of the word "in"|AnnHsDoSrcSpan-- of the word "do"
```


And then in the parser

```
| 'let' binds 'in' exp   { mkAnnHsLet $1$3(LL$HsLet(unLoc $2)$4)}
```


The helper is

```

    mkAnnHsLet ::Located a ->Located b ->LHsExprRdrName->P(LHsExprRdrName)
    mkAnnHsLet (L l_let _)(L l_in _) e =do
      addAnnotation (mkAnnKey e)(AnnHsLet l_let l_in)
      return e;
```


The Parse Monad would have to accumulate the annotations to be
returned at the end, if called with the appropriate flag.


There will be some boilerplate in getting the annotations and helper
functions defined, but it will not pollute the rest.


This technique can also potentially be backported to support older GHC
versions via a modification to ghc-parser\[1\].

[ https://hackage.haskell.org/package/ghc-parser](https://hackage.haskell.org/package/ghc-parser)

## Neil Mitchell Response


I was getting a bit lost between the idea and the implementation. Let
me try rephrasing the idea in my own words.


The goal: Capture inner source spans in AST syntax nodes. At the
moment if ... then ... else ... captures the spans \[if \[...\] then
\[...\] else \[...\]\]. We want to capture the spans for each keyword as
well, so: \[{if} \[...\] {then} \[...\] {else} \[...\]\].


The proposal: Rather than add anything to the AST, have a separate
mapping `(SrcSpan,AstCtor)` to `[SrcSpan]`. So you give in the `SrcSpan`
from the `IfThenElse` node, and some token for the `IfThenElse`
constructor, and get back a list of `IfThenElse` for the particular
keyword.


I like the proposal because it adds nothing inside the AST, and
requires no fresh invariants of the AST. I dislike it because the
contents of that separate mapping are highly tied up with the AST, and
easy to get out of sync. I think it's the right choice for three
reasons, 1) it is easier to try out and doesn't break the AST, so we
have more scope for changing our minds later; 2) the same technique is
able to represent things other than `SrcSpan` without introducing a
polymorphic src span; 3) the people who pay the complexity are the
people who use it, which is relatively few people.


That said, as a tweak to the API, rather than a single data type for
all annotations, you could have:

```
dataAnnIfThenElse=AnnIfThenElse{posIf, posThen, posElse ::SrcSpan}dataAnnDo=AnnDo{posDo ::SrcSpan}
```


Then you could just have an opaque `Map (SrcSpan, TypeRep) Dynamic`,
with the invariant that the `TypeRep` in the key matches the `Dynamic`.
Then you can have: 

```
getAnnotation::Typeable a =>Annotations->SrcSpan->Maybe a
```


I think it simplifies some of the TypeRep trickery
you are engaging in with `mkAnnKey`.


There was some further email between AZ and NDM (teaching AZ some basics) resulting in the following

## Current POC implementation [ D297](https://phabricator.haskell.org/D297)


Theory of operation.


The HsSyn AST does not capture the locations of certain keywords and
punctuation, such as 'let', 'in', 'do', etc.


These locations are required by any tools wanting to parse a haskell
file, transform the AST in some way, and then regenerate the original
layout for the unchaged parts.


Rather than pollute the AST with information irrelevant to the actual
compilation process, these locations are captured in the lexer /
parser and returned as a separate structure ApiAnns structure in the
ParsedSource.


Each AST element that needs an annotation has an entry in this Map,
which as a key comprising the SrcSpan of the original element and the
TyeRep of the stored annotation, if it were wrapped in a Just.


This allows code using the annotation to access this as follows

<table><tr><th>processHsLet</th>
<td>ApiAnns -\> LHsExpr -\> CustomReturnType
processHsLet anns (L l (HsExpr localBinds expr)) = r
where
</td></tr>
<tr><th>Just ann = getAnnotation anns l</th>
<td>Maybe AnnHsLet
...
</td></tr></table>

```
typeApiAnns=Map.MapApiAnnKeyValuedataApiAnnKey=AKSrcSpanTypeRepderiving(Eq,Ord,Show)mkApiAnnKey::(Typeable a)=>SrcSpan-> a ->ApiAnnKeymkApiAnnKey l a =AK l (typeOf (Just a))
```