
See [Records](records) for the bigger picture. This is a proposal to solve the records name-spacing issue with simple name-spacing and simple type resolution.


This approach is an attempt to port the records solution in [ Frege](http://code.google.com/p/frege/), a haskell-like language on the JVM. Please read Sections 3.2 (primary expressions) and 4.2.1 (Algebraic Data type Declaration - Constructors with labeled fields) of the [ Frege user manual](http://code.google.com/p/frege/downloads/detail?name=Language-411.pdf)
Many thanks to the Frege author, Ingo Wechsung for explaining his implementation and exploring this implementation territory for us.


The DDC language (again, very much like Haskell, but focused on effect tracking and an overall different conceptual approach to purity) puts forth a similar solution. See the [ thesis](http://www.cse.unsw.edu.au/~benl/papers/thesis/lippmeier-impure-world.pdf) section 2.7 - 2.7.4 pages 115 - 119

## Better name spacing


In Haskell, you can look at an occurrence of any identifier `f` or `M.f` and decide where it is bound without thinking about types at all.  Broadly speaking it works like this:

- For qualified names, `M.f`, find an import that binds `M.f`.
- For unqualified names, `f`, find the innermost binding of `f`; or, if that takes you to top level, look for top level binding of `f` or an import that binds `f`.


If there is ambiguity (eg two imports both import something called `f`) then an error is reported.  And that's what happens for the `Record` and `RecordClash` example above.


So the proposed solution for record field names is to specify more precisely which one you mean by using the type name. Note that a data declaration now creates a module-like namespace, so we aren't so much using the type name as using the data type namespace in the same way we use a module namespace.


So you could say `Record.a` or `RecordClash.a` rather than `a`, to specify which field selector you mean.  This creates a potential ambiguity: did you mean `<module-name>.f` or `<record-name>.f`.  `Record` could be the name of a type or of a module.


There are 2 cases to consider:
1) inside module M naming a record M, and also importing both that module and record
2) importing 2 different modules, M1 and M2, where M2 defines a record named M1.


The module/record ambiguity is dealt with in Frege by preferring modules and requiring a module prefix for the record if there is ambiguity. So for 1) you need M.M.field. For 2) you need M2.M1.field.


We could instead prefer a record rather than a module - this would normally be the desired behavior, but then we must figure out how to still be able to access the module. This is particularly the case for modules not marked as qualified - generally all modules are under a verbose namespace and the Module.identifier syntax is only used if the module is first qualified.


Generally, programmers will avoid this situation by doing what they do now: structuring their programs to avoid name collisions. We can try and give the greater assistance in this regard by providing simpler ways for them to alter the names of import types.


One way to avoid the Module.Record.x problem is to use type alias names, for example:

```wiki
data InconvenientName = X { f :: Int }
type IN = InconvenientName
-- IN.f is the same as InconvenientName.f
```

## Getting rid of the Verbosity with the dot operator


We have name-spaces, but it is hard to see how this is better than the current practice of adding prefixes to record fields: `data Record = Record { recordA :: String }`


Verbosity is solved in Frege and DDC by using the dot syntax concept. In `data Record = Record {a::String};r = Record "A"; r.a` The final `r.a` resolves to `Record.a r`.


This is the TDNR syntax concept. See 'Simple Type Resolution' for how we resolve the type of this code.
Also see 'Details on the dot' for a lengthy discussion of the dot.

## Simple type resolution


Frege has a detailed explanation of the semantics of its record implementation, and the language is \*very\* similar to Haskell. After reading the Frege manual sections, one is still left wondering: how does Frege implement type resolution for its dot syntax. The answer is fairly simple: overloaded record fields are not allowed. So you can't write code that works against multiple record types. Please see the comparison with Overloading in \[wiki Records\], which includes a discussion of the relative merits. Note that the DDC thesis takes the same approach.


Back to simple type resolution. From the Frege Author:

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


This is the only real downside of \*this\* proposal (most downsides discussed here are inherent to any records proposal). The Frege author says:


I estimate that in 2/3 of all cases one does not need to write `T.e x` in sparsely type annotated code, despite the fact that the frege type checker has a left to right bias and does not yet attempt to find the type of `x` in the code that "follows" the `x.e` construct (after let unrolling etc.) I think one could do better and guarantee that, if the type of `x` is inferrable at all, then so will be `x.e` (Still, it must be more than just a type variable.)

## Syntax for updates (in the Frege manual)

- the function that updates field `x` of data type `T` is `T.{x=}`
- the function that sets field x in a `T` to `42` is `T.{x=42}`
- If `a::T` then `a.{x=}` and `a.{x=42}` are equivalent to `T.{x=} a` and `T.{x=42} a`
- the function that changes field x of a T by applying some function to it is `T.{x <-}`


The function update syntax is a new addition to Haskell that we do not need to immediately implement.

### Alternative update syntax: using tuple selectors

```wiki
let { r.x = x'; r.y = y'; r.z = z'; } in r
```


If we allow tuples of selectors:

```wiki
r.(x, y, z) = (r.x, r.y, r.z)
```


then one can simply write

```wiki
let r.(x, y, z) = (x', y', z') in r   
```

## Interaction with Typeclasses


In the Frege system, the record's namespace is closed where it is defined.
However, making a record an instance of a class puts the class functions in the record name-space.

```wiki
module RExtension where

import original.M(R)    -- access the R record defined in module original.M

class Rextension1 r where
      f :: .....
      g :: .....

instance Rextension1 R where
     -- implementation for new functions

And now, in another module one could

import RExtension()      -- equivalent to qualified import in Haskell
```


the new functions `f` and `g` are accessible (only) through R.
So we have a technique for lifting new functions into the Record namespace.
For the initial records implementaion we definitely want to maintain `f` and `g` at the top-level, but should consider also adding through the record name-space. See related discussion below on future directions.

## Compatibility with existing records


The new record system is enabled with `-XNAMESPACEDATA`.

- Should new modules be infectious? That is, if I turn the extension on for my module and export a record, does a user that wants to import the record also have to use the extension?

- Records from modules without this extension can be imported into a module using it.


Ideally the old record fields would now only be accessed through a namespace. Also, we would ideally be able to strip any now useless field prefixes.

```wiki
module OldModule ( Record(..) ) where data Prefix = Prefix { prefixA :: String }

module NewModule where
import OldModule ( Prefix(..) strip prefix )

aFunc = let r = Prefix "A" in r.a
```

## Details on the dot


This proposal requires the current Haskell function composition dot operator to have spaces on both sides. No spaces around the dot are reserved for name-spacing: this use and the current module namespace use. No space to the right would be partial application (see 
[ TDNR](http://hackage.haskell.org/trac/haskell-prime/wiki/TypeDirectedNameResolution). The dot operator should bind as tightly as possible.

### Partial application


see [ TDNR](http://hackage.haskell.org/trac/haskell-prime/wiki/TypeDirectedNameResolution) syntax discusion for an explanation.

```wiki
(.a) r == r.a
```


.x (no space after the dot), for any identifier x, is a postfix operator that binds more tightly than function application, so that parentheses are not usually required.

```wiki
.a r == r.a
```


When there are multiple operators, they chain left to right

```wiki
(r.a.b.c) == (.c $ .b $ .a r)
```


See below for how partial application can allow for different code styles.


Question: does this now hold?

```wiki
r.a == r.(Record.a) == r.Record.a
```

### Dealing with dot-heavy code

#### Identifying the difference between a name-space dot and function composition


Given the dot's expanded use here, plus its common use in custom operators, it is possible to end up with dot-heavy code.

```wiki
quux (y . (foo>.<  bar).baz (f . g)) moo
```


It's not that easy to distinguish from

```wiki
quux (y . (foo>.<  bar) . baz (f . g)) moo
```


What then is the future of the dot if this proposal is accepted? The community needs to consider ways to reduce the dot:


1) discourage the use of dot in custom operators: `>.<` could be discouraged, use a different character or none: `><`
In most cases the dot in custom operators has little to no inherent meaning. Instead it is just the character available for custom operators that takes up the least real-estate. This makes it the best choice for implementing a custom operator modeled after an existing Haskell operator: `.==` or `.<` is normably preferable to `@==` and `@<`.


2) discourage the use of dot for function composition - use a different operator for that task. Indeed, Frege users have the choice between `<~` or the proper unicode dot.


Discouraging the use of the dot in custom operators makes the example code only slightly better. With the second we now have:


{{
quux (y \<\~ (foo\>.\<  bar).baz (f \<\~ g)) moo
}}}


Very easy to distinguish from

```wiki
quux (y <~ (foo>.<  bar) <~ baz (f <~ g)) moo
```


If you are disgusted by `<~` than you can use the very pretty unicode dot.

#### Downside: mixing of 2 styles of code

```wiki
data Record = Record { a::String }
b :: Record -> String

let r = Record "a" in b r.a 
```


It bothers some that the code does not read strictly left to right as in: `b . a . r`. Chaining can make this even worse: `(e . d) r.a.b.c`

##### Solution: Partial Application


Partial application provides a potential solution: `b . .a $ r`


So if we have a function `f r = b r.a` then one can write it points-free: `b . .a`


Our longer example from above: `e . d . .c . .b . .a`


Let us consider real use with longer names:

```wiki
echo . delta . .charlie . .beta . .alpha
```


Note that a move to a different operator for function composition (see discussion above) would make things much nicer:

```wiki
echo <~ delta <~ .charlie <~ .beta <~ .alpha
```

##### Solution: Field selector to the left of the record


We could have an equivalent of the dot where the field is to the left of the record: `b a@r`
Could this also be used in a partial syntax?

```wiki
echo . delta . charlie@ . beta@ . alpha@
```


Can this be shortened to:

```wiki
echo . delta . charlie@beta@alpha@
```


Or would this syntax alway need to be applied?

```wiki
echo . delta $ charlie@beta@alpha@r
```

## Extending data name-spacing


This is mostly just something interesting to contemplate.


Dot syntax does not have to be limited to records (although it probably should be for the initial implementation until this new record system is vetted). I think it is a bad idea to attempt to attempt to extend the dot syntax to accomplish general function chaining through extending the dot syntax - we are simply asking too much of the dot right now. However, it is consistent to extend the function name-spaced to a record data type concept to any data type (as it is in Frege), and use dot syntax for that. The dot (without spaces) then \*always\* means tapping into a namespace (and simple type resolution).


Placing functions within a data name-space can make for nicer data-structure oriented code where the intent is clearer. It can help to achieve the data-oriented goal of OO (without the entanglement of state). With control over how the data namespace is exported (similar to controlling module namesapces), it is possible to create virtual record field setters and getters that can be accessed through dot syntax.


Both Frege and the DDC thesis take this approach.


In this brave new world (see above where typeclass functions are also placed under the namespace of the data), there are few functions that \*absolutlely must\* be at the top level of a module. Although a library author might take attempt the approach of no top-level functions, obviously it will still be most convenient for users to define functions at the top level of modules rather than have to lift them into data structure namespaces.
