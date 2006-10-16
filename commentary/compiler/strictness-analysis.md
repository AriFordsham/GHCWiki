CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/Commentary/Compiler/StrictnessAnalysis"
  queryString          = "?version=1"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 06:57:46 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","269"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/Commentary/Compiler/StrictnessAnalysis\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
[ Up: [wiki:Commentary/Compiler] ]

= IMPORTANT NOTE =

This commentary describes code that is not checked in to the HEAD yet.

= The strictness analyzer =

Most of the strictness analyzer lives in two files:

 * [[GhcFile(compiler/basicTypes/NewDemand.lhs)]] (defines the datatypes used by the strictness analyzer, and some functions on them)
 * [[GhcFile(compiler/stranal/DmdAnal.lhs)]] (the strictness analyzer itself)

The strictness analyzer does demand analysis, absence analysis, and box-demand analysis in a single pass. (!ToDo: explain what these are.)

In [[GhcFile(compiler/stranal/DmdAnal.lhs)]], {{{dmdAnal}}} is the function that performs strictness analysis on an expression. It has the following type:
{{{
dmdAnal :: SigEnv -> Demands -> CoreExpr -> (DmdType, CoreExpr)
}}}
The first argument is an environment mapping variables onto demand signatures. (!ToDo: explain more.) The third argument is the expression being analyzed. {{{dmdAnal}}} returns a pair of a new expression (possibly with strictness information added to any [wiki:Commentary/Compiler/NameType Ids] in it), and a {{{DmdType}}}.

== Important datatypes ==
{{{
data Demand
  = D Usage Demands    
}}}
A demand consists of usage information, along with information about usage of the subcomponents of the expression it's associated with.

{{{
data Usage
  = U Str Abs Box        
}}}
Usage information consists of a triple of three properties: strictness (or evaluation demand), usage demand, and box demand.

{{{
data Str 
  = Bot                 
  | Strict       
  | Lazy           
}}}
Something that is {{{Lazy}}} may or may not be evaluated. Something that is {{{Strict}}} will definitely be evaluated at least to its outermost constructor. Something that is {{{Bot}}} will be fully evaluated (e.g., in {{{x `seq` (error "urk")}}}, {{{x}}} can be said to have strictness {{{Bot}}}, because it doesn't matter how much we evaluate {{{x}}} -- this expression will diverge anyway.)

{{{
data Abs
  = Zero     
  | OneOrZero     
  | Many       
}}}
In the context of function arguments, an argument that is {{{Zero}}} is never used by its caller (e.g., syntactically, it doesn't appear in the body of the function at all). An argument that is {{{OneOrZero}}} will be used zero or one times, but not more. Something that is {{{Many}}} may be used zero, one, or many times -- we don't know.

{{{
data Box
  = Box  
  | Unpack  
}}}
Again in the context of function arguments, an argument that is {{{Box}}} is a value constructed by a data constructor of a product type whose "box" is going to be needed. For example, we say that {{{f x = case x of { (a, b) -> x}}}} "uses the box", so in {{{f}}}, {{{x}}} has box-demand information {{{Box}}}. In {{{g x = case x of { (a, b) -> a}}}}, {{{g}}} doesn't "use the box" for its argument, so in {{{g}}}, {{{x}}} has box-demand information {{{Unpack}}}. When in doubt, we assume {{{Box}}}.

{{{
data Demands = Poly          
            |  Prod [Demand] (Maybe Coercion)
}}}
For a compound data value, the {{{Demands}}} type describes demands on its components. {{{Poly}}} means that we don't know anything about the expression's type. {{{Prod}}} says "this expression has a product type, and the demands on its components consist of the demands in the following list". If the {{{Coercion}}} is supplied, that means that this expression must be cast using the given coercion before it is evaluated. (!ToDo: explain this more.)

(!ToDo: explain why all the above information is important, and what {{{DmdTypes}}} are.)
```
