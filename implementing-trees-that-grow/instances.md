
This page discusses the interaction of Trees That Grow with derived for `Data`.  (But it might apply to manual instances for, say, `Outputable` too.)


Here's our example:

```
typefamilyXOverLit p
  dataHsOverLit p =OverLit(XOverLit p)(HsExpr p)dataGhcPass(c ::Pass)derivinginstanceTypeable c =>Data(GhcPass c)dataPass=Parsed|Renamed|TypecheckedtypeinstanceXOverLit(GhcPass'Parsed)=PlaceHoldertypeinstanceXOverLit(GhcPass'Renamed)=NametypeinstanceXOverLit(GhcPass'Typechecked)=Type
```


We want a `Data` instance for this type.

### PLAN A


I propose

```
derivinginstance(Data(XOverLit(GhcPass p)))=>Data(OverLit(GhcPass p))where…
```


But that gives rise to big constraint sets; for each data constructor
we get another `X` field, and another constraint in the `Data` instance.


This is what is currently implemented in GHC master. It works, but every time the constraint set is enlarged as the next step of Trees that Grow goes in, the time taken to compile GHC increases.

### PLAN B


Alan would like to see

```
derivinginstanceData(OverLit(GhcPass p))
```


which should have all the information available to the derivation process, since `p` can only have one of three values and each of them has a type family instance for `XOverLit`.


This "does not compile".


Alan has discovered that the three instances:

```
derivinginstanceData(OverLit(GhcPass'Parsed))derivinginstanceData(OverLit(GhcPass'Renamed))derivinginstanceData(OverLit(GhcPass'Typechecked))
```

**will** compile, but only with GHC 8.2.1, not with 8.0.2, due to a flaw in the standalone deriving process.


That is: instead of one `Data` instance for the `HsSyn` traversals,
make three.


\[ The spurious constraint problem was resolved by including `deriving instance Typeable c => Data (GhcPass c)`, as recommended by \@RyanGlScott \]

### PLAN C


It is still painful generating three virtually identical chunks of traversal code.
So suppose we went back to

```
derivinginstance(...)=>Data(OverLit(GhcPass p))where…
```


We'd get unresolved constraints like `Data (XOverLit (GhcPass p))`.  Perhaps we
could resolve them like this

```
instanceData(XOverLIt(GhcPass p))where
     gmapM f x = x
     ...etc...
```


That is: a no-op `Data` instances.  Traversals would not traverse extension fields.
That might not be so bad, for now!

### PLAN D


If there were cases when we really did want to look at those extension fields,
we still could, by doing a runtime test, like this:

```
instanceIsGhcPass p =>Data(XOverLIt(GhcPass p))where
     gmapM =case ghcPass @p ofIsParsed-> gmapM
                IsRenamed-> gmapM
                IsTypechecked-> gmapM
     ...etc...
```


Here I'm positing a new class and GADT:

```
classIsGhcPass p where
  ghcPass ::IsGhcPassT p

dataIsGhcPassT p whereIsParsed::IsGhcPassParsedIsRenamed::IsGhcPassRenamedIsTypechecked::IsGhcPassTypecheckedinstanceIsGhcPassParsedwhere
  ghcPass =IsParsed...etc...
```


Now, instead of passing lots of functions, we just pass a single GADT value
which we can dispatch on.


We can mix Plan C and D.


Alan:


Note:

-  We are solving a compilation time problem for GHC stage1/stage2. The produced compiler has the same performance regardless of which derivation mechanism.

- Ideally we would like to end up with `data` instances for an arbitrary `OverLit p`, which is an outcome for Plan A. i.e. Plan A will play nice with external index types,for GHC API users.

### PLAN E


Proceed as per Plan A, but move all the standalone deriving code to a new file, which will produce orphan instances.


Inside this, use CPP to detect a modern enough compiler (GHC 8.2.1) to generate via Plan B, otherwise fall back to Plan A.


Eventually improve the standalone deriving sufficiently that it is able to generate a single traversal, instead of the three.


So for day-to-day work ghc devs can use GHC 8.2.1, and we confirm is still works with GHC 8.0.2 less frequently.