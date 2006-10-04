CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/ModuleDependencies"
  queryString          = "?version=18"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 06:57:07 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","256"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/ModuleDependencies\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac


= The Marvellous Module Structure of GHC =
(possibly outdated, please fix anything that is wrong)

GHC is built out of about 245 Haskell modules. It can be quite tricky to figure out what the module dependency graph looks like. It can be important, too, because loops in the module dependency graph need to be broken carefully using .hi-boot interface files.

This section of the commentary documents the subtlest part of the module dependency graph, namely the part near the bottom.

 * The list is given in compilation order: that is, module near the top are more primitive, and are compiled earlier.
 * Each module is listed together with its most critical dependencies in parentheses; that is, the dependencies that prevent it being compiled earlier.
 * Modules in the same bullet don't depend on each other.
 * Loops are documented by a dependency such as "loop Type.Type". This means tha the module imports Type.Type, but module Type has not yet been compiled, so the import comes from Type.hi-boot. 

== Compilation order is as follows: ==

 * First comes a layer of modules that have few interdependencies, and which implement very basic data types:
   * Util
   * !OccName
   * Pretty
   * Outputable
   * !StringBuffer
   * !ListSetOps
   * Maybes
   * etc 

 * Now comes the main subtle layer, involving types, classes, type constructors identifiers, expressions, rules, and their operations.
   * Name, !PrimRep
   * !PrelNames
   * Var (Name, loop !IdInfo.!IdInfo, loop Type.Type, loop Type.Kind)
   * !VarEnv, !VarSet, !ThinAir
   * Class (loop !TyCon.!TyCon, loop Type.Type)
   * !TyCon (loop Type.Type, loop !DataCon.!DataCon, loop Generics.!GenInfo)
   * !TypeRep (loop !DataCon.!DataCon, loop Subst.substTyWith)
   * Type (loop !PprType.pprType, loop Subst.substTyWith)
   * !FieldLabel (Type), !TysPrim (Type)
   * Literal (!TysPrim, !PprType), !DataCon (loop !PprType, loop Subst.substTyWith, !FieldLabel.!FieldLabel)
   * !TysWiredIn (loop !MkId.mkDataConIds)
   * !TcType ( lots of !TysWiredIn stuff)
   * !PprType ( lots of !TcType stuff )
   * !PrimOp (!PprType, !TysWiredIn)
   * !CoreSyn [does not import Id]
   * !IdInfo (!CoreSyn.Unfolding, !CoreSyn.!CoreRules)
   * Id (lots from !IdInfo)
   * CoreFVs, !PprCore
   * !CoreUtils (!PprCore.pprCoreExpr, CoreFVs.exprFreeVars, !CoreSyn.isEvaldUnfolding !CoreSyn.maybeUnfoldingTemplate)
   * !CoreLint ( !CoreUtils ), !OccurAnal (!CoreUtils.exprIsTrivial), !CoreTidy (!CoreUtils.exprArity )
   * !CoreUnfold (!OccurAnal.occurAnalyseGlobalExpr)
   * Subst (!CoreUnfold.Unfolding, CoreFVs), Generics (!CoreUnfold.mkTopUnfolding), Rules (!CoreUnfold.Unfolding, !PprCore.pprTidyIdRules)
   * !MkId (!CoreUnfold.mkUnfolding, Subst, Rules.addRule)
   * !PrelInfo (!MkId), !HscTypes ( Rules.!RuleBase ) 

 * That is the end of the infrastructure. Now we get the main layer of mdoules that perform useful work.
   * !CoreTidy (!HscTypes.!PersistentCompilerState) 

== !HsSyn stuff ==
 * !HsPat.hs-boot
 * !HsExpr.hs-boot (loop !HsPat.LPat)
 * !HsTypes (loop !HsExpr.!HsSplice)
 * !HsBinds (!HsTypes.LHsType, loop !HsPat.LPat, !HsExpr.pprFunBind and others) !HsLit (!HsTypes.!SyntaxName)
 * !HsPat (!HsBinds, !HsLit) !HsDecls (!HsBinds)
 * !HsExpr (!HsDecls, !HsPat) 

== Library stuff: base package ==
 * GHC.Base
 * Data.Tuple (GHC.Base), GHC.Ptr (GHC.Base)
 * GHC.Enum (Data.Tuple)
 * GHC.Show (GHC.Enum)
 * GHC.Num (GHC.Show)
 * GHC.ST (GHC.Num), GHC.Real (GHC.Num)
 * GHC.Arr (GHC.ST) GHC.STRef (GHC.ST)
 * GHC.!IOBase (GHC.Arr)
 * Data.Bits (GHC.Real)
 * Data.!HashTable (Data.Bits, Control.Monad)
 * Data.Typeable (GHC.IOBase, Data.!HashTable)
 * GHC.Weak (Data.Typeable, GHC.IOBase) 

```
