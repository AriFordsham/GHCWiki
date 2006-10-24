# Linearity


Instead of:

```wiki
type DmdEnv = VarEnv Demand
```


we want:

```wiki
type DmdEnv = Env CoreExpr Demand
```


We keep track of demands on partial applications.


After calling dmd_anal on the body of a let, which results in demand type `dmd_ty` with demand env `dmd_env`, we do the following for each let-bound variable `f`:

1. Iterate through all the keys in `dmd_env`, finding all applications of `f` to *n* arguments.
1. For each *i* from 1 through *n* (where *n* is `f`'s arity), if each of the applications of `f` to *i* arguments has usage demand `OneOrZero`, then it's safe to mark the corresponding lambda-expression as a one-shot lambda. 


This might work, but is kind of kludgy.


This may be *way* too unnecessarily complicated. Can't we just get the same information from the demand on f in the free-var environment of the let body as it is, without changing the environment?


suppose the demand on f is

```wiki
S1K(S1K(LMX))
```


if f = 

```wiki
(\ x. (\ y. ...))
```


then we can mark the outer two lambdas as being one-shot. Right?


Not exactly. Suppose:

```wiki
let f = \ x. \ y. ... in
  ...(f 1 2)...(f 3 4)...
```


f will have demand on it:

```wiki
SMK(SMK(LMX))
```


because it's called more than once. (But maybe that's okay? If it instead were:

```wiki
let f = \ x. \ y. ... in
  ...(f 1)...(f 3 4)...
```


then the demand would be:

```wiki
SMK(LMX)
```


and we would know that there was a partial application? Getting confused here.)
