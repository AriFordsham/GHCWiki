# Desugaring of array comprehensions


Wadler's desugaring for list comprehensions is not suitable for arrays, as we need to use collective operations to get good parallel code.  The build/foldr desugaring, although using collective operations, isn't a good match for how the array operations are implemented.  In fact, the *naive* desugaring from the H98 report is a much better fit:

```wiki
[: e | :] 	     = [:e:]
[: e | b, qs :]      = if b then [: e | qs :] else [::]
[: e | p <- a, qs :] = let ok p = [: e | qs :]
		           ok _ = [::]
		     in concatMap ok a
[: e | let ds, qs :] = let ds in [: e | qs :]
[: e | qs | qss   :] = 
  [: e | (XS, XSS) <- zip [: XS | qs :] [: XSS | qss :] :]
  where XS & XSS are the bound variables in qs & qss
```