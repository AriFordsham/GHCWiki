CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/Commentary/Compiler/RdrNameType"
  queryString          = "?version=6"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 06:56:09 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","265"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/Commentary/Compiler/RdrNameType\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac


= {{{RdrNames}}}, {{{Modules}}}, and {{{OccNames}}} =

When the parser parses an identifier, it generates a {{{RdrName}}}.  A {{{RdrName}}} is pretty much just a string, or a pair of strings, for a qualified name, such as {{{M.x}}}.  Here's the data type declaration, from [[GhcFile(compiler/basicTypes/RdrName.lhs)]]:
{{{
data RdrName 
  = Unqual OccName
	-- Used for ordinary, unqualified occurrences 

  | Qual ModuleName OccName
	-- A qualified name written by the user in 
	--  *source* code.  The module isn't necessarily 
	-- the module where the thing is defined; 
	-- just the one from which it is imported

  | Orig Module OccName
	-- An original name; the module is the *defining* module.
	-- This is used when GHC generates code that will be fed
	-- into the renamer (e.g. from deriving clauses), but where
	-- we want to say "Use Prelude.map dammit".  
 
  | Exact Name
	-- We know exactly the Name. This is used 
	--  (a) when the parser parses built-in syntax like "[]" 
	--	and "(,)", but wants a RdrName from it
	--  (b) when converting names to the RdrNames in IfaceTypes
	--	Here an Exact RdrName always contains an External Name
	--	(Internal Names are converted to simple Unquals)
	--  (c) by Template Haskell, when TH has generated a unique name
}}}

== The `Module` and `ModuleName` types ==

In GHC, a ''module'' is uniquely defined by a pair of the module name and the package where the module is defined.  The details are in [[GhcFile(compiler/basicTypes/Module.lhs)]] and  [[GhcFile(compiler/main/PackageConfig.hs)]], but here are the key definitions:
{{{
newtype PackageId = PId FastString

newtype ModuleName = ModuleName FastString

data Module = Module {
   modulePackageId :: !PackageId,  -- pkg-1.0
   moduleName      :: !ModuleName  -- A.B.C  }
}}}
You'll notice that a `Qual` `RdrName` contains a `ModuleName`; which module is referred to depends on the import declarations in that module.  In contrast, a `Orig` `RdrName` refers to a unique `Module`.

== The {{{OccName}}} type ==

An {{{OccName}}} is more-or-less just a string, like "foo" or "Tree", giving the (unqualified) name of an entity. 
Well, not quite just a string, because in Haskell a name like "C" could mean a type constructor or data constructor, depending on context. So GHC defines a type OccName (defined in basicTypes/OccName.lhs) that is a pair of a {{{FastString}}} and a {{{NameSpace}}} indicating which name space the name is drawn from. The data type is defined (abstractly) in [[GhcFile(compiler/basicTypes/OccName.lhs)]]:
{{
data OccName = OccName 
    { occNameSpace  :: !NameSpace
    , occNameFS     :: !FastString
    }
}}}
{{{FastString}}} is a packed string implementation of strings. 

The name spaces are: 

{{{
data NameSpace = VarName	-- Variables, including "source" data constructors
	       | DataName	-- "Real" data constructors 
	       | TvName		-- Type variables
	       | TcClsName	-- Type constructors and classes; Haskell has them
				-- in the same name space for now.
}}}


```
