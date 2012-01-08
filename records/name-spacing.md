
See [Records](records) for the bigger picture. This is a proposal to solve the records name-spacing issue with simple name-spacing and simple type resolution. 


This approach is an attempt to port the records solution in [ Frege](http://code.google.com/p/frege/), a haskell-like language on the JVM. Please read Sections 3.2 (primary expressions) and 4.2.1 (Algebraic Data type Declaration - Constructors with labeled fields) of the [ Frege user manual](http://code.google.com/p/frege/downloads/detail?name=Language-411.pdf)


Many thanks to the Frege author, Ingo Wechsung for explaining his implementation and exploring this implementation territory for us.

## Better name spacing


In Haskell, you can look at an occurrence of any identifier `f` or `M.f` and decide where it is bound without thinking about types at all.  Broadly speaking it works like this:

- For qualified names, `M.f`, find an import that binds `M.f`.
- For unqualified names, `f`, find the innermost binding of `f`; or, if that takes you to top level, look for top level binding of `f` or an import that binds `f`.


If there is ambiguity (eg two imports both import something called `f`) then an error is reported.  And that's what happens for the `Record` and `RecordClash` example above.


So the proposed solution for record field names is to specify more precisely which one you mean by using the type name. Note that a data declaration now creates a module-like namespace, so we aren't so much using the type name as using the data type namespace in the same way we use a module namespace.


So you could say `Record.a` or `RecordClash.a` rather than `a`, to specify which field selector you mean.  The difficulty here is that it's hard to know whether you are writing `<module-name>.f` or `<record-name>.f`.  That is, is `Record` the name of a type or of a module? (Currently it legally could be both.)

>
> The module/record ambiguity is dealt with in Frege by preferring modules and requiring a module prefix for the record if there is ambiguity. So if your record named Record was inside a module named Record you would need `Record.Record.a`. I think we could improve upon this case to prefer a record rather than the name of the existing module, which should not need to be referenced. So just `Record.a`.


However, we still have the case of conflicting imports between the names of modules and records. We have the choice of either requiring a module prefix or making this a compilation error. Generally, programmers will avoid this situation by doing what they do now: structuring their programs to avoid name collisions. We can try and give the greater assistance in this regard by providing simpler ways for them to alter the names of import types.


One way to avoid the Module.Record.x problem is to use type alias names, for example:

```wiki
data InconvenientName = X { f :: Int }
type IN = InconvenientName
-- IN.f is the same as InconvenientName.f 
```

### Alternative name-spacing techiniques

**Use the module name space mechanism**.
But putting each record definition in its own module is a bit heavyweight. So maybe we need local modules (just for name space control) and local import declarations.  Details are unclear. (This was proposed in 2008 in [ this discussion](http://www.haskell.org/pipermail/haskell-cafe/2008-August/046494.html) on the Haskell cafe mailing list and in [\#2551](https://gitlab.haskell.org//ghc/ghc/issues/2551). - Yitz).

>
> Rather than strictly re-use modules it may make more sense to have a name-spacing implementation construct that is shared between both records and modules - hopefully this would make implementation easier and unify behavior. In the Frege approach, each data declaration is its own namespace - if we were to go this far (instead of stopping purely at records) there may be much less need for local namespaces. Overall this seems to be more of an interesting implementation detail than a concrete design proposal relating to records. -- Greg Weber.

## Getting rid of the Verbosity


We have name-spaces, but the equivalent is already being accomplished by adding prefixes to record fields: `data Record = Record { recordA :: String }`


Verbosity is solved in Frege by using the TDNR syntax concept. In `data Record = Record {a::String};r = Record "A"; r.a` The final `r.a` resolves to `Record.a r`.
See below for how we resolve the type of this code.

## Simple type resolution


Frege has a detailed explanation of the semantics of its record implementation, and the language is \*very\* similar to Haskell. After reading the Frege manual sections, one is still left wondering: how does Frege implement type resolution for its TDNR syntax. The answer is fairly simple: overloaded record fields are not allowed. So you can't write code that works against multiple record types. Please see the comparison with Overloading in \[wiki Records\], which includes a discussion of the relative merits. Back to simple type resolution. From the Frege Author:

- Expressions of the form T.n are trivial, just look up n in the namespace T.
- Expressions of the form x.n: first infer the type of x. If this is just an unbound type variable (i.e. the type is unknown yet), then check if n is an overloaded name (i.e. a class operation). If this is not the case, then x.n is not typeable. OTOH, if the type of x can be inferred, find the type constructor and look up n in the associated name space.


Under no circumstances, however, will the notation x.n contribute in any way in inferring the type of x, except for the case when n is a class operation, where an appropriate class constraint is generated.


Note that this means it is possible to improve upon Frege in the number of cases where the type can be inferred - we could look to see if there is only one record namespace containing n, and if that is the case infer the type of x -- Greg Weber


So lets see examples behavior from the Frege Author:


For example, lets say we have:

```wiki
data R = R { f :: Int }

bar R{f=42} = true
bar R{} = false

foo r = bar r || r.f==43
baz r = r.f==47 || bar r

foobaz r = r.f
```


Function bar has no difficulties, after desugaring of the record patterns it's just plain old pattern matching.


Function foo is also ok, because through the application of `r` to bar the type checker knows already that r must be an R when it arrives at `r.f`


Function baz is ok as long as the type checker does not have a left to right bias (Frege currently does have this bias, but will hopefully be improved).


The last function foobaz gives a type error too, as there is no way to find out the type of `r`.


Hence, the records in Frege are a very conservative extension to plain old algebraic data types, actually all record constructs will be desugared and reduced to non-record form in the way I have described in the language reference. For example, the data R above will become:

```wiki
data R = R Int where
    f (R x) = x
    ...
```


To be sure, the where clause is the crucial point here. It puts f in the name space R. The global scope is not affected, there is nothing named f outside the R namespace.


The record namespace is searched only in 3 cases:

- when some name is explicitly qualifed with `R`:   `R.f`
- when the type checker sees `x.f` and knows that `x::R`
- In code that lives itself in the namespace `R`, here even an unqualified `f` will resolve to `R.f` (unless, of course, if there is a local binding for `f`)

### Increased need for type annotation


This is the only real downside of the proposal. The Frege author says:


I estimate that in 2/3 of all cases one does not need to write `T.e x` in sparsely type annotated code, despite the fact that the frege type checker has a left to right bias and does not yet attempt to find the type of `x` in the code that "follows" the `x.e` construct (after let unrolling etc.) I think one could do better and guarantee that, if the type of `x` is inferrable at all, then so will be `x.e` (Still, it must be more than just a type variable.)

## Syntax for updates (in the Frege manual)

- the function that updates field `x` of data type `T` is `T.{x=}`
- the function that sets field x in a `T` to `42` is `T.{x=42}`
- If `a::T` then `a.{x=}` and `a.{x=42}` are valid
- the function that changes field x of a T by applying some function to it is `T.{x <-}`

## Compatibility with existing records


Seems like it should be OK to use old records in the new system playing by the new rules, although those records likely already include some type of prefixing and would be verbose.
There is a chance for deeper though on this issue.

## Extending data name-spacing and TDNR syntax


This is mostly just something interesting to contemplate.
TDNR syntax does not have to be limited to records (although it probably should be for the initial implementation until this new record system is vetted). I think it is a bad idea to attempt to accomplish general function chaining through extending TDNR. However, we can extent the function name-spaced to a data type concept to any data type (as it is in Frege), and use TDNR syntax for that. This way the dot (without spaces) \*always\* means tapping into a namespace (and simple type resolution).


Placing functions within a data name-space can make for nicer data-structure oriented code where the intent is clearer. It can help to achieve the data-oriented goal of OO without the entanglement of state. Is it possible to create "virtual" record field setters and getters that can be accessed through TDNR syntax and to control exactly what parts of the data namespace are accessible?
