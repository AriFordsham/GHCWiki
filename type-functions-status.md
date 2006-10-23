CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/TypeFunctionsStatus"
  queryString          = "?version=103"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 06:58:15 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","257"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/TypeFunctionsStatus\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
= Type Functions: Implementation Status =

Back to TypeFunctions.

'''Current:''' 
 * Where should the family instance consistency check go for GHCi?  `tcGetModuleExports`?  (Where is that function called?)
 * There really is no reason anymore to disallow family instances where all arguments are type variables.  Such instances are rarely useful, but may be when creating stub files or so; so why disallow them.  Check that they are working.

== Parsing and Renaming ==

Todo (low-level):
  * Should family declarations be optional with ATs, too?  (See comment at patch making kinds optional at toplevel declarations.)

Todo (high-level):
 1. Parse and rename equality constraints in signatures.
 2. Defaults for associated type synonyms.  (Having both a kind signature and vanilla synonym is problematic as in `RnNames.getLocalDeclBinders` its hard to see that not both of them are defining declarations, which leads to a multiple declarations error.  Defaults are quite different from vanilla synonyms anyway, as they usually have tyvars on their rhs that do not occur on the lhs.)
 3. Import/export lists:
   * We need to be able to write something like `GMapKey(GMap,empty)`.
   * Export and import of data constructors declarated in `data instance`s.  We should be able to use the same syntax for the entity specs as for closed data types, but the meaning is somewhat different.

Done:
 * Parsing and renaming of kind signatures (toplevel and in classes).
 * Parsing and renaming of indexed type declarations (toplevel and in classes).
 * Using new syntax with `family` and `instance` on top level.
 * Added `-findexed-types` switch.
 * Allowing `type` tag in export lists to list associated types in the sub-binder list of an import/export item for a class.

== Type Checking ==

Todo (low-level):
 * RHS of a `type instance` must be a tau type.
 * Check that patterns of type indexes don't contain type functions.
 * Construct `TyCon` for type equation in `tcIdxTyInstDecl1`.  This needs to be a synonym tycon, which still needs to be extended to include family information.
 * Implement the equation of `conflict`, which is local to `FamInst.addLocalFamInst`, for synonyms; ie, check that the left hand sides coincide under the substitution.
 * If an associated synonym has a default definition, use that in the instances.  In contrast to methods, this cannot be overridden by a specialised definition.  (Confluence requires that any specialised version is extensionally the same as the default.)
 * It should be ok to allow newtype instances for data families.  (NB: the rhs of a newtype is guaranteed to be a lifted type.)  Is this desirable?

Todo (high-level):
 1. Type checking of type functions (and hence, associated type synonyms); forget about `iso` for the moment.
 2. Type checking in the presence of associated synonym defaults.  (Default AT synonyms are only allowed for ATs defined in the same class.)
 3. Type check functional dependencies as type functions.

Done: 
 * Kind and type checking of kind signatures.
 * Kind and type checking of instance declarations of indexed types.
 * Wrapper generation and type checking of pattern matching for indexed data and newtypes.
 * Consistency checking for family instances.

== Desugaring ==

Todo (low-level):
 * Derivings on an associated data type ''declaration'' need to be inherited by all definitions of that data type in instances.

Todo (high-level):
  1. Extend interface files to include equality axioms:
   * How do we exactly want to represent type equations in interface files?
     * SPJ pointed out that instances are maintained in `InstEnv.InstEnv` with different values for the home packages and others. Type instances may have to be maintained in a similar way, as they are also incrementally collected during compiling a program.  (We probably include them in the same structure, as they will also be of type `InstInfo`.)
     * `IfaceInst` contains the instance declaration information for interfaces.
    '''Answer:''' We don't put anything extra into interface files.  Instead, we derive the information corresponding to`IfaceInst` list in `ModIface` and the `Instance` list in `ModDetails` from the interface declarations and type environment, respectively.  I.e., it is the type instances that carry the whole payload.
    '''Update:''' We may actually want to put a rough match signature in the iface seperate from the full instance declaration, so we can delay type checking the full instance declaration until we get a rough match.  (This makes only sense for `type instance`s, not for `data instance`s, as the latter are loaded when their constructors are mentioned.  Well actually, it does make sense for `data instance`s as far as loading them for overlap checking is concerned.)
 3. Desugar type functions and equality constraints.

Done:
 * Representation of family kind signatures as `TyCon.TyCon`s.
 * Extension of `Class.Class` by associated `TyCon`s.
 * Extension of `TyCon.TyCon` with a reference to the parent `TyCon` for data instances.
 * Extension of `DataCon.DataCon` with instance types for constructors belonging to data instances.
 * Extension of `TyCon.TyCon` such that the parent of a data instance is paired with a coercion identifying family instance and representation type.
 * For indexed data types, the datacon wrapper uses data instance coercion and pattern matching casts the scrutinee via an `ExprCoFn` in a `CoPat`.
 * Import and exporting.
 * Generation and plumbing through of rough matches.

== Testsuite ==

Todo:
 * Compile libraries with !CoreLint.
```
