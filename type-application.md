# Visible Type Application


Visible Type Application, language extension `TypeApplications`, allows you to give explicit type arguments to a polymorphic function; e.g. `map @Int @Bool isEven xs`.  Visible type application was introduced in GHC 8.0.


This the feature page that summarises status, open issues, etc.

## Status



Use Keyword = `TypeApplications` to ensure that a ticket ends up on these lists.



**Open Tickets:**

<table><tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11349">#11349</a></th>
<td>[TypeApplications] Create Proxy-free alternatives of functions in base</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11350">#11350</a></th>
<td>Allow visible type application in patterns</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11352">#11352</a></th>
<td>Allow applying type to label</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11387">#11387</a></th>
<td>Typecasting using type application syntax</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11398">#11398</a></th>
<td>Type application for operator sections</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11409">#11409</a></th>
<td>Cannot instantiate literals using TypeApplications</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12085">#12085</a></th>
<td>Premature defaulting and variable not in scope</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12363">#12363</a></th>
<td>Type application for infix</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12446">#12446</a></th>
<td>Doesn&apos;t suggest TypeApplications when `~` used prefix</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12569">#12569</a></th>
<td>TypeApplications allows instantiation of implicitly-quantified kind variables</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12794">#12794</a></th>
<td>Out of scope error not reported</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13042">#13042</a></th>
<td>Allow type annotations / visible type application in pattern synonyms</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13512">#13512</a></th>
<td>GHC incorrectly warns that a variable used in a type application is unused</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13834">#13834</a></th>
<td>Error cascade with type applications</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14722">#14722</a></th>
<td>Error message points to wrong location</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15577">#15577</a></th>
<td>TypeApplications-related infinite loop (GHC 8.6+ only)</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15596">#15596</a></th>
<td>When a type application cannot be applied to an identifier due to the absence of an explicit type signature, let the error just say so!</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15782">#15782</a></th>
<td>Visible type/kind applications in declaration of data/type constructors</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15942">#15942</a></th>
<td>Associated type family can&apos;t be used at the kind level within other parts of parent class</td></tr></table>




**Closed Tickets:**

<table><tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11329">#11329</a></th>
<td>Visible type applications: failing tests with WAY=hpc</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11333">#11333</a></th>
<td>GHCi does not discharge satisfied constraints</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11355">#11355</a></th>
<td>TypeApplications + RankNTypes permits &quot;impredicativity&quot;</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11376">#11376</a></th>
<td>Inconsistent specified type variables among functions and datatypes/classes when using -XTypeApplications</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11385">#11385</a></th>
<td>Unify named wildcards in different type applications</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11397">#11397</a></th>
<td>Type mismatch in local definitions in Haskell 98 code</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11428">#11428</a></th>
<td>ImpredicativeTypes causes GHC panic with 8.0.1-rc1</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11431">#11431</a></th>
<td>GHC instantiates levity-polymorphic type variables with foralls</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11456">#11456</a></th>
<td>Type application and :set +c command cause panic</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11458">#11458</a></th>
<td>Terrible failure of type inference in visible type application</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11512">#11512</a></th>
<td>An unwritten kind variable is &quot;specified&quot;, when it shouldn&apos;t be.</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11513">#11513</a></th>
<td>Work out when GADT parameters should be specified</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11616">#11616</a></th>
<td>Kinds aren&apos;t instantiated</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11638">#11638</a></th>
<td>referring to the existential type from a GADT pattern match with a type application</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11721">#11721</a></th>
<td>GADT-syntax data constructors don&apos;t work well with TypeApplications</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/11947">#11947</a></th>
<td>GHC mistakenly warns about type defaulting in the presence of -XTypeApplications</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12025">#12025</a></th>
<td>Order of constraints forced (in pattern synonyms, type classes in comments)</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12033">#12033</a></th>
<td>[TypeApplications] GHC internal error</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12045">#12045</a></th>
<td>Visible kind application</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12092">#12092</a></th>
<td>Out-of-scope variable leads to type error, not scope error</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12093">#12093</a></th>
<td>Wrong argument count in error message with TypeApplications</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12220">#12220</a></th>
<td>TypeApplications and DefaultSignatures - problems deducing type variables.</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12371">#12371</a></th>
<td>Error message, room for improvement</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12411">#12411</a></th>
<td>GHC panic on TypeApplications + TemplateHaskell</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12529">#12529</a></th>
<td>Error message: visible type application of constructor/variable that is not in scope</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12565">#12565</a></th>
<td>unhelpful error message about enabling TypeApplications</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/12601">#12601</a></th>
<td>explicit foralls do not distinguish applicable types</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13401">#13401</a></th>
<td>GHCi gives conflicting information about visible type application</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13466">#13466</a></th>
<td>Ghci panics with type applications to unknown functions</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13524">#13524</a></th>
<td>GHC does not preserve order of forall&apos;d vars with TypeApplications</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13680">#13680</a></th>
<td>Can&apos;t use TypeApplications with [] data constructor</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13738">#13738</a></th>
<td>TypeApplications-related GHC internal error</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13819">#13819</a></th>
<td>TypeApplications-related GHC panic</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13846">#13846</a></th>
<td>GHC Panic: Visible type application + function type @(_ -&gt; _)</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13848">#13848</a></th>
<td>Unexpected order of variable quantification with GADT constructor</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13853">#13853</a></th>
<td>TypeApplications and record syntax don&apos;t mix</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13877">#13877</a></th>
<td>GHC panic: No skolem info: k2</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13902">#13902</a></th>
<td>Misleading function arity mismatch error with TypeApplications</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13938">#13938</a></th>
<td>Iface type variable out of scope:  k1</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/13986">#13986</a></th>
<td>TypeApplications causes parse errors in @-patterns with certain Unicode characters</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14038">#14038</a></th>
<td>TypeApplications regression in GHC HEAD: ‘p0’ is untouchable inside the constraints: ()</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14348">#14348</a></th>
<td>Poly-kinded definitions silently introduce extra type arguments captured by TypeApplications</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/14796">#14796</a></th>
<td>Pretty Printing: GHC doesn&apos;t parenthesise (() :: Constraint)</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15527">#15527</a></th>
<td>TypeApplications error message doesn&apos;t parenthesize infix name</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15568">#15568</a></th>
<td>Kind variables in type family aren&apos;t quantified in toposorted order</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15591">#15591</a></th>
<td>Inconsistent kind variable binder visibility between associated and non-associated type families</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15592">#15592</a></th>
<td>Type families without CUSKs cannot be given visible kind variable binders</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15793">#15793</a></th>
<td>Type family doesn&apos;t reduce with visible kind application</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15797">#15797</a></th>
<td>GHC panic using visible kind application</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15801">#15801</a></th>
<td>&quot;ASSERT failed!&quot; with visible kind applications</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15807">#15807</a></th>
<td>GHC panic with visible kind applications</td></tr>
<tr><th><a href="https://gitlab.haskell.org//ghc/ghc/issues/15816">#15816</a></th>
<td>Visible kind applications + data family: `U :: Type&apos; said to be of kind `k0 -&gt; k1` in error message</td></tr></table>



---

**Everything below here is the old wiki page.  Much of it is relevant, but it needs curation. Richard will do that in due course.**


Another relevant out of date page is [ExplicitTypeApplication](explicit-type-application)

## Type Application


\[Explicit\] Type Application is a feature requested for Haskell that lets a programmer explicitly declare what types should be instantiated for the arguments to a function application, in which the function is polymorphic (containing type variables and possibly constraints) . Doing so essentially “short-circuits” much of the type variable unification process, which is what GHC normally attempts when dealing with polymorphic function application.


In general, Haskell’s unification algorithm is robust enough that it can do this without the programmer needing to specify explicit types. There are a few edge cases, however: see below for examples. Though some of these cases can be solved with type annotations, such annotations can be cumbersome, as the programmer needs to provide the entire signature, which can be cumbersome in complicated expressions. 


It is worth noting that GHC’s intermediate language, “Core”, is fully typed and has no ambiguity - every polymorphic function application is explicitly typed. Thus, one way to think of this addition is “exposing” this feature of Core to the programmer. Indeed, once the evidence (generated by instantiating the types) is propagated to Core, it is able to be handled completely by that part of the compiler - implementing this feature did not require any changes to Core, nor to the lower-level compiler pipeline (optimizations, assembly generation, etc).
See directly below for usage examples. More detailed design decisions follow the examples.

---

## **Usage:**



 



This extension can be enabled with “TypeApplications” within the LANGUAGE pragma, or use the flag “-XTypeApplications”. The usage of this flag does not turn on any other flag implicitly. Using this flag will make a whitespace-sensitive distinction preceding each ‘@’ character: No whitespace before ‘@’ will result in an as-pattern, while having whitespace before the ‘@’ will parse as a type-application. (When this flag is turned off, the behavior is the same as it is now - no whitespace sensitivity). See **Design** with more information on the syntax.


Here are some examples where type application is useful:


This test case shows how using explicit type application resolves ambiguity, by
using the infamous "show/read" problem as an example. (The show/read problem is
a problem in which the answer to using show followed by read - 'show (read "3")'
should be obvious, but fails in the typechecker because of an ambiguity in the
constraints). Using type application on either of the functions, or both, allows the compiler
to correctly typecheck the program.

```wiki
answer_read = show (read @Int "3") -- "3" :: String
answer_show = show @Integer (read "5") -- "5" :: String
answer_showread = show @Int (read @Int "7") -- "7" :: String
```


A more difficult example involves type families, which are generally difficult to infer, and this creates problems when attempting to unify types involving type families. In this specific example, the declaration "g False" would be ill-typed \[since it has no type signature\], but adding the explicit type “\@Char” will resolve this problem.

```wiki
type family F a
type instance F Char = Bool

g :: F a -> a
g _ = undefined

f :: Char
f = g True

h = g False -- will cause an error
h' = g @Char False
```


One does not need to have the “-XExplicitForAll” extension turned on to use “-XExplicitTypeApplication”. Without the forall flag, generally types will be inferred by simply stacking all of the foralls at the beginning. You can also leave off explicit types by simply withholding the annotation, or by providing “\@_” to let the typechecker instantiate the variable regularly. This is particularly useful when you want to provide a type to the first and third type variable, but not the second, for example:

```wiki
-- Will be treated as:
-- forall a b c d. a -> b -> c -> d -> (a, b, c, d)...
quad :: a -> b -> c -> d -> (a, b, c, d)
quad w x y z = (w, x, y, z)

foo = quad @Bool @_ @Int False 'c' 17 "Hello!" -- Char and [Char] will be inferred for type variables ‘b’ and ‘d’.
```


This also works with types of arbitrary kinds, including with kind variables. (There is no explicit kind application yet, but with the current implementation, it is very easy to extend it in the future). Of course, the types within the type applications will have to satisfy the kinds you are providing.

```wiki
data Two (a :: * -> k) = T

two :: Two a -> Int
two _ = 2

twoBase = two T
twoOk = two @(Either Int) T
twoBad = too @Maybe (T :: Two Either)
```

---

## **Design**


There are several small design questions that can be asked of Explicit Type Application. Below are the questions and the decisions that were made:

- *Is a type annotation and/or signature required for a function in order to use type applications when applying it?*

>
>
> No. Haskell generalizes certain functions, with a simple, straightforward signature; all the type variables are at the top, and it is a fairly simple signature to instantiate and work with. 
>
>

- *Should we require a forall in the signature?* 

>
>
> No, for similar reasons as above. Additionally, we did not want to create a dependency on the "ExplicitForAll" flag, and wanted type applications to be a small, surgical feature that has as few side effects as possible.
>
>

- *What order is used to instantiate the type variables?* 

>
>
> Left-to-right order of the type variables appearing in the foralls. This is the most logical order that occurs when the instantiation is done at the type-variable level. Nested foralls work slightly differently, but at a single forall location with multiple variables, left-to-right order takes place. (See below for nested foralls).
>
>

- *How will it work with partial function application? Will we allow: leaving out arguments in function application, but allow type application to de-generalize the expression?*

>
>
> Yes. This will allow the programmer to use the partially applied the function later, but only to arguments with specific types. This could be useful for a library designer, to use a generalized function internally, and only expose a specialized version of that function in the interface.  
>
>

- * Wildcard Application*
  Allows the programmer to not instantiate every type variable if they do not want to. Example

  ```wiki
  f xs = reverse @ (Maybe _) xs
  -- Instantiates reverse at a Maybe type
  -- but lets GHC infer which
  ```

  An explicit application `@ _` is just like the implicit form: instantiate with a fresh unification variable.  

- *Named wildcards*.  Can you write

  ```wiki
  f xs = reverse @ (_a -> _a) xs
  ```

  The intent here is that `_a` stands for a type, but not necessarily a type variable.

- *Should non-prenex-form functions be allowed to use type applications? If so, how should we allow it? *

>
>
> Yes. We allow this by requiring that type application appear where the forall is located in the type. See the following example:
>
>
> ```wiki
> many :: forall a b. a -> b -> forall c. [c] -> forall d . Num d => d -> (a, b, [c], d)
> many a b c d = (a, b, c, d)
> foo =  many @Int @Bool 5 True @Char "hello" @Float 17
> ```

- *Concrete Syntax*:


We choose to use the ‘@’ symbol, as this is the symbol that is used in Core, GHC’s intermediate language. Turning on this extension will make the ‘@’ symbol whitespace-sensitive in the front: whitespace before an ‘@’ will parse as a type-application, while no whitespace in front of the ‘@’ will parse as an as-pattern. This is similar to the way ‘.’ behaves differently with whitespace (function composition vs. module naming), but note that the only whitespace sensitivity occurs ‘’’before’’’ the ‘@’ and not after. Additionally, when the extension is off, there is no change in current behavior and no whitespace sensitivity.
