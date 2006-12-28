CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/TypeFunctionsSyntax"
  queryString          = "?version=5"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 07:00:09 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","258"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/TypeFunctionsSyntax\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
= Type Functions: Syntax and Representation =

== Syntax of kind signatures and definitions of indexed types ==

A tolevel kind signature consists of a type declaration head using `family` as a special following the declaration keyword.  It is optionally followed by a `::` and a kind (which is by default `*` if not specified).  In the case of  a data declaration, we addititonally require that there is no `where` clause.   In associated kind signature, the `family` special is dropped, but the kind is currently compulsory.  Toplevel indexed type defintions, use the `instance` keyword after the main declarations keyword; associated declarations don't use `instance`.  We require for every definition of an indexed type (i.e., type equations or indexed data/newtype declaration) that a matching kind signature is in scope.  Vanilla type synonym definitions and data/newtype declarations fall out as special cases of type function equations and indexed type declarations that have variable-only patterns, for which we require no kind signatures.  The vanilla forms are also closed (further definitions would be useless, as they are bound to overlap).

== Representation of indexed types ==

=== Kind signatures ===

`HsDecls.TyClDecl` has a new variant `TyFunction` to represent signatures of type functions.  These consist of the name, type parameters, an iso flag, and optionally an explicit result kind.  The type parameters can have kind signatures as usual.  

Signatures for indexed data and newtypes are represented as a special case of `TyData`, namely when `TyData` has a kind signature, but no constructors. 

We recognise both forms of kind signatures by the predicate `HsDecls.isKindSigDecl`.

=== Instances of indexed types ===

We represent type functions and indexed data and newtypes by generalising type synonym declarations `TySynonym` and data type declarations `TyData` to allow patterns ofr type indexes instead of just type variables as parameters.  In both variants, we do so by way of the field `tcdPats` of type `Maybe [LHsType name]`, used as follows:
 * If it is `Nothing`, we have a ''vanilla'' data type declaration or type synonym declaration and `tcdVars` contains the type parameters of the type constructor.
 * If it is `Just pats`, we have the definition of an a indexed type (toplevel or nested in an instance declarations).  Then, 'pats' are type patterns for the type-indexes of the type constructor and `tcdVars` are the variables in those patterns.  Hence, the arity of the type constructor is `length tcdPats` and ''not'' `length tcdVars`.
In both cases (and as before we had type functions), `tcdVars` collects all variables we need to quantify over.

=== Parsing and AST construction ===

The LALR parser allows arbitrary types as left-hand sides in '''data''', '''newtype''', and '''type''' declarations.  The parsed type is, then, passed to {{{RdHsSyn.checkTyClHdr}}} for closer analysis (possibly via `RdHsSyn.checkSynHdr`).  It decomposes the type and, among other things, yields the type arguments in their original form plus all type variables they contain.  Subsequently, {{{RdrHsSyn.checkTyVars}}} is used to either enforce that all type arguments are variables (second argument is `False`) or to simply check whether the type arguments are variables (second argument `True`).  If in enforcing mode, `checkTyVars` will raise an error if it encounters a non-variable (e.g., required for class declarations).  If in checking mode, it yields the value placed in the `tcdPats` field described above; i.e., returns `Nothing` instead of the type arguments if these arguments are all only variables.

== Representation of associated types ==

We add type declarations to class declarations and instance declarations by a new field, of type `[LTyClDecl]`, to both `TyClDecl.ClassDecl` (known by the field name `tcdATs`) and `TyClDecl.InstDecl`.  For classes, this new field contains values constructed from `TyData`, `TyFunction`, and `TySynonym`, whereas for instances, we only have `TyData` and `TySynonym`.  This is due to (a) `TyData` representing both signatures and definitions of associated data types (whereas the two are split into `TyFunction` and `TySynonym` for associated synonyms) and (b) associated synonyms having default definitions, which associated data types do not possess.

== Representation of equational constraints ==

Equational constraints are parsed into a new variant of `HsPred`, called `HsEqualP`.  Renaming (by `RnTypes.rnPred`) and kind checking (by `TcHsType.kc_pred`) is straight forward.  Afterwards, `HsPred` is desugared into `TypeRep.PredType`, where the wellformedness of equational constraints in type contexts is further tested by `TcMType.check_pred_ty`; in particular, we require the type arguments to be rank 0.

== Type tags ==

To enable the listing of associated types in the sub-binder list of import/export items for classes, we extend the parser with a production that allows a constructor name (upper case or approppriate infix operator in parenthesis) to be prefixed by the keyword `type`.  

NB: There is a cavet at the moment, in error messages the type prefix is not printed, as the `ppr` instance on `HsImpExp.IE` is polymorphic in the name (and hence, we cannot get at the name space in which a name is).

```
