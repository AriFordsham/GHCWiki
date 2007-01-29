CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/ExplicitCallStack"
  queryString          = "?version=15"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 07:01:13 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","256"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/ExplicitCallStack\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
= Maintaining an explicit call stack =

There has been a vigorous thread on error attribution ("I get a `head []` error; but who called `head`?").  This page summarises some half baked ideas that Simon and I have been discussing. Do by all means edit this page to add comments and further ideas or pointers.  (As usual, ''discussion'' is best done by email; but this page could be a place to record ideas, design alternatives, list pros and cons, pointers to related work etc.)

See also
  * [http://www.haskell.org/pipermail/haskell-cafe/2006-November/019549.html The Haskell cafe thread]
  * [http://www.cse.unsw.edu.au/~dons/loch.html]
  * [http://haskell.org/hat HAT]


== The basic idea == 

1.  GHC's 'assert' magically injects the current file location.  One could imagine generalising this a bit so that you could say
{{{
	...(f $currentLocation)...
}}}
to pass a string describing the current location to f.

2.  But that doesn't help with 'head'.  We want to pass head's ''call site'' to head. That's what jhc does when you give 'head' the a magic [http://repetae.net/john/computer/jhc/jhc.html SRCLOC_ANNOTATE pragma]:
	* every call to `head` gets replaced with `head_check $currentLocation`
	* in jhc, you get to write `head_check` yourself, with type
{{{
		head_check :: String -> [a] -> a
}}}
It'd be nicer if you didn't have to write `head_check` yourself, but instead the compiler wrote it.

3.  But what about the caller of the function that calls head?  Obviously we'd like to pass that on too!
{{{
	foo :: [Int] -> Int
	{-# SRCLOC_ANNOTATE foo #-}
	foo xs = head (filter odd xs)
===>
	foo_check :: String -> [Int] -> Int
	foo_check s xs = head_check ("line 5 in Bar.hs" ++ s) xs
}}}
Now in effect, we build up a call stack.  Now we ''really'' want the compiler to write `foo_check`.

4.  In fact, it's very similar to the "cost-centre stack" that GHC builds for profiling, except that it's explicit rather than implicit.  (Which is good.   Of course the stack should be a proper data type, not a String.)

However, unlike GHC's profiling stuff, it is ''selective''.  You can choose to annotate just one function, or 10, or all.  If call an annotated function from an unannotated one, you get only the information that it was called from the unannotated one:
{{{
	foo :: [Int] -> Int   -- No SRCLOC_ANNOTATE
	foo xs = head (filter odd xs)
===>
	foo:: [Int] -> Int
	foo xs = head_check ("line 5 in Bar.hs") xs
}}}
This selectiveness makes it much less heavyweight than GHC's currrent "recompile everything" story.

5. The dynamic hpc tracer will allow reverse time-travel, from an exception to the call site, by keeping a small queue of recently ticked locations. This will make it easier to find out what called the error calling function (head, !, !!, etc.), but will require a hpc-trace compiled prelude if we want to find places in the prelude that called the error. (A standard prelude would find the prelude function that was called that called the error inducing function).

== Open questions ==

Lots of open questions

 * It would be great to use the exact same stack value for profiling.  Not so easy...for example, time profiling uses sampling based on timer interrupts that expect to find the current cost centre stack in a particular register.  But a big pay-off; instead of having magic rules in GHC to handle SCC annotations, we could throw the full might of the Simplifier at it.
  
 * CAFs are a nightmare.  Here's a nasty case:
{{{
  foo :: Int -> Int -> Int
  foo = \x. if fac x > 111 then \y. stuff else \y. other-stuff

  bad :: Int -> Int
  bad = foo 77
}}}
 How would you like to transform this?

== Transformation rules ==

The key issues are:

 * '''Higher-order calls'''. Where does a partially applied function receive its stack trace from? Possible options include:
   1. The lexical call site (corresponding to where the function is mentioned in the source code.
   2. The context in which the function receives a particular argument, for instance the one where it is saturated.  

 * '''CAFs'''. The problem with CAFs is that, at least for expensive ones, we want to preserve the sharing of their evaluation. Therefore we cannot simply extend them into functions which take a stack trace as an argument - this would cause the CAF to be recomputed at each place where it is called. The simplest solution is to make CAFs the roots of call stacks, but it seems like there will be situations where it would be useful to know more about the context in which a CAF was evaluated.

 * '''Recursion'''. Obviously the call stack will grow in size proportional to the depth of recursion. This could lead to prohibitive space usage, thus it is desirable that the size of the stack be kept within reasonable bounds. We will probably need some way to dynamically prune the stack.

 * '''Extent'''. It is desirable to have a scheme where only some functions in the program are transformed for stack tracing, whilst others remain in their original form. We want to avoid the all-or-nothing situation, where the whole program has to be recompiled before tracing can be done. For example, with the current state of profiling in GHC, the whole program has to be recompiled, and special profiling libraries must be linked against. This is a nuisance which reduces the usability of the system. Similar problems occur with other debugging tools, such as Hat and buddha, and this really hampers their acceptance by programmers.

=== An abstract syntax ===

For the purpose of exploring the rules we need an abstract syntax. Below is one for a simple core functional language:

{{{
   Decls(D) --> x :: T | x = E | data f a1 .. an = K1 .. Km

   Constructors(K) --> k T1 .. Tn

   Types(T) --> f | a | T1 T2
}}}


Attached in a pdf are the beginnings of some transformation rules to consider.
```
