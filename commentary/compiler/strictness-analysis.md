# General overview


GHC's approach to strictness analysis is that of "demand analysis", a backwards analysis in which strictness analysis and absence analysis are done in a single pass. In the future, analysis to perform unboxing, as well as other analyses, may be implemented within this framework as well.

# IMPORTANT NOTE


The rest of this commentary describes code that is not checked in to the HEAD yet.


Update: as of 2014-02-12, newer documentation (apparently on the same topic and apparently more up-to-date) is available at [Commentary/Compiler/Demand](commentary/compiler/demand) (I am not an expert on the GHC internals though).
Also, [compiler/basicTypes/NewDemand.lhs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/basicTypes/NewDemand.lhs) is not any more in the sources, replaced by (or renamed to?) [compiler/GHC/Types/Demand.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Types/Demand.hs).

# The demand analyzer


Most of the demand analyzer lives in two files:

- [compiler/basicTypes/NewDemand.lhs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/basicTypes/NewDemand.lhs) (defines the datatypes used by the demand analyzer, and some functions on them)
- [compiler/GHC/Core/Opt/DmdAnal.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Core/Opt/DmdAnal.hs) (the demand analyzer itself)


The demand analyzer does strictness analysis, absence analysis, and box-demand analysis in a single pass. (ToDo: explain what these are.)


In [compiler/GHC/Core/Opt/DmdAnal.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Core/Opt/DmdAnal.hs), `dmdAnal` is the function that performs demand analysis on an expression. It has the following type:

```haskell
dmdAnal :: SigEnv -> Demand-> CoreExpr -> (DmdType, CoreExpr)
```


The first argument is an environment mapping variables onto demand signatures. (ToDo: explain more.) The second argument is the demand that's being placed on the expression being analyzed, which was determined from the context already. The third argument is the expression being analyzed. `dmdAnal` returns a pair of a new expression (possibly with demand information added to any [Ids](commentary/compiler/name-type) in it), and a `DmdType`.

## Important datatypes

```haskell
data Demand
  = D Usage Demands    
```


A demand consists of usage information, along with information about usage of the subcomponents of the expression it's associated with.

```haskell
data Usage
  = U Str Abs Box        
```


Usage information consists of a triple of three properties: strictness (or evaluation demand), usage demand, and box demand.

```haskell
data Str 
  = Bot                 
  | Strict       
  | Lazy           
```


Something that is `Lazy` may or may not be evaluated. Something that is `Strict` will definitely be evaluated at least to its outermost constructor. Something that is `Bot` will be fully evaluated (e.g., in `x `seq` (error "urk")`, `x` can be said to have strictness `Bot`, because it doesn't matter how much we evaluate `x` -- this expression will diverge anyway.)

```haskell
data Abs
  = Zero     
  | OneOrZero     
  | Many       
```


In the context of function arguments, an argument that is `Zero` is never used by its caller (e.g., syntactically, it doesn't appear in the body of the function at all). An argument that is `OneOrZero` will be used zero or one times, but not more. Something that is `Many` may be used zero, one, or many times -- we don't know.

```haskell
data Box
  = Box  
  | Unpack  
```


Again in the context of function arguments, an argument that is `Box` is a value constructed by a data constructor of a product type whose "box" is going to be needed. For example, we say that `f x = case x of { (a, b) -> x`} "uses the box", so in `f`, `x` has box-demand information `Box`. In `g x = case x of { (a, b) -> a`}, `g` doesn't "use the box" for its argument, so in `g`, `x` has box-demand information `Unpack`. When in doubt, we assume `Box`.

```haskell
data Demands = Poly          
            |  Prod [Demand] (Maybe Coercion)
```


For a compound data value, the `Demands` type describes demands on its components. `Poly` means that we don't know anything about the expression's type. `Prod` says "this expression has a product type, and the demands on its components consist of the demands in the following list". If the `Coercion` is supplied, that means that this expression must be cast using the given coercion before it is evaluated. (ToDo: explain this more.)


(ToDo: explain why all the above information is important)


Though any expression can have a `Demand` associated with it, another datatype, `DmdType`, is associated with a function body.

```haskell
data DmdType = DmdType 
		    DmdEnv	
		    [Demand]	
		    DmdResult
```


A `DmdType` consists of a `DmdEnv` (which provides demands for all explicitly mentioned free variables in a functions body), a list of `Demand`s on the function's arguments, and a `DmdResult`, which indicates whether this function returns an explicitly constructed product:

```haskell
data DmdResult = TopRes	-- Nothing known	
	       | RetCPR	-- Returns a constructed product
	       | BotRes	-- Diverges or errors
```


The `dmdTransform` function takes a strictness environment, an [Id](commentary/compiler/name-type) corresponding to a function, and a `Demand` representing demand on the function -- in a particular context -- and returns a `DmdType`, representing the function's demand type in this context.

```haskell
dmdTransform :: SigEnv		
	     -> Id		
	     -> Demand		
	     -> DmdType		
```


Demand analysis is implemented as a backwards analysis, so `dmdTransform` takes the demand on a function's result (which was inferred based on how the function's result is used) and uses that to compute the demand type of this particular occurrence of the function itself.

`dmdTransform` has four cases, depending on whether the function being analyzed is a [data constructor](commentary/compiler/entity-types) worker, an imported (global) function, a local `let`-bound function, or "anything else" (e.g., a local lambda-bound function).


The data constructor case checks whether this particular constructor call is saturated. If not, it returns `topDmdType`, indicating that we know nothing about the demand type. If so, it returns a `DmdType` with an empty environment (since there are no free variables), a list of arg-demands based on the `Demand` that was passed in to `dmdTransform` (that is, the demand on the result of the data constructor call), and a `DmdResult` taken from the constructor Id's strictness signature.


There are a couple of tricky things about the list of arg-demands:

- If the result demand (i.e., the passed-in demand) has its box demanded, then we want to make sure the box is demanded in each of the demands for the args. (ToDo: this may not be true)
- If the result demand is not strict, we want to use *n* copies of `topDmd` as the list of arg-demands, where *n* is this data constructor's arity.


(ToDo: explain the other cases of `dmdTransform`)



[even more sketchy notes](commentary/compiler/strictness-analysis/kirsten-notes)



[Commentary/Compiler/StrictnessAnalysis/Examples](commentary/compiler/strictness-analysis/examples)


