# Documentation for the new GHCi debugger


These notes detail the breakpoint debugger which is being incorportated into GHCi. Note that there was/is a previous prototype debugger, and we share some of its code (specifically the term printer) (see: [GhciDebugger](ghci-debugger)).

---

## User's Manual

### Command summary


Parameters to commands are indicated in angular brackets `<...>`, optional parameters are followed by a question mark. Ellipses `...` indicate where a sequence of parameters is allowed. Everything else is literal text, as you would type it at the prompt.


Setting breakpoints:

```wiki
   :break <module>? <line>
   :break <module>? <line> <column>
   :break <module>? <identifier>
```


Listing breakpoints:

```wiki
   :show breaks   
```


Deleting breakpoints:

```wiki
   :delete <break_number> ... <break_number>
   :delete *
```


Inspecting values:

```wiki
   :print <expression>
```


Single stepping:

```wiki
   :step
   :step <expression>
```


Continuting execution from a breakpoint:

```wiki
   :continue
```

### Starting the debugger


The debugger is integrated with GHCi, and it is on by default. The debugger slows program execution down by a factor of approximately XXX times. You can turn it off (to avoid the slowdown) using the `-fno-debug` command line argument, when you start GHCi.

### Setting breakpoints


The general rule of thumb for breakpoints is that you can set a breakpoint on any thing which is not a value (though there are some exceptions). For example, a literal character is a value, but a case expression is not. 


We call the places where you can set breakpoints as **breakable expressions** (even if some of them aren't strictly expressions).


You **can** set breakpoints on the following things: (XXX) Check this list carefully!

1. Function applications. We allow breakpoints on partial applications, even though they are technically values. Also, if there is an application with more than one argument, we only allow breaks on the whole expression, not on the sub-applications within: e.g. for the expression `map f list`, we allow a break on the whole expression, but not on the sub-application of `map f`.
1. Case expressions.
1. Function declarations (all the equations of a function).
1. Case alternatives.
1. Do statements.
1. Guards.
1. Bodies of functions, pattern bindings, lambdas, guarded equations.


Conversely, you **cannot** set breakpoints on the following things, except if they occur as the outermost expression in the body of a declaration:

1. Literals.
1. Variables.
1. Do blocks. XXX check this one
1. List comprehensions. XXX check this one


You can set a breakpoint in three ways:

1. By line number.
1. By line and column number.
1. By function name (not implemented yet).


In each case you can specify in which module you want to set the breakpoint, however, if that is omitted, the debugger will choose a suitable default module for you (XXX give a better explanation of what module is chosen by default).


The syntax for setting breakpoints by line number is:

```wiki
   :break <module>? <line>
```


This will activate the breakpoint which corresponds to the leftmost outermost breakable expression which *begins* and *ends* on the line indicated by the `<line>` parameter, if such an expression exists. XXX If no such expression exists then what happens? Currently the debugger will report an error message, but perhaps it is nicer for it to probe a few lines ahead until it finds a breakable expression, or give up after some threshold number of lines?


The syntax for setting breakpoints by line and column is:

```wiki
   :break <module>? <line> <column>
```


This will activate the breakpoint which corresponds to the *smallest* breakable expression which encloses the source location `(<line>, <column>)`, if such an expression exists. If no such expression exists the debugger will report an error message.


The syntax for setting breakpoints by function name is: (XXX not yet implemented)

```wiki
   :break <module>? <identifier>
```


This will activate the outermost breakpoint associated with the definition of `<identifier>`. If `<identifier>` is defined by multiple equations, the breakpoint will cover them all. This means the computation will stop whenever any of those equations is evaluated. XXX What about local functions? XXX What about functions defined in type classes (default methods) and instance declarations?

### Listing the active breakpoints


You can list the set of active breakpoints with the following command:

```wiki
   :show breaks
```


Each breakpoint is given a unique number, which can be used to identify the breakpoint should you wish to delete it (see the `:delete` command). Here is an example list of breakpoints:

```wiki
   0) Main (12,4)-(12,8)
   1) Foo (13,9)-(13,13)
   2) Bar (14,4)-(14,47)
```


Breakpoint 0 is set in the module `Main` on the breakable expression which spans between the source locations (12,4) to (12,8). Similarly for breakpoints 1 and 2.

### Deleting breakpoints


You can delete any active breakpoint with the `:delete` command. Breakpoints are refered to by their unique number which is displayed by the `:show breaks` command (see above). You can refer to more than one breakpoint at a time, for example:

```wiki
   :delete <break_number> ... <break_number>
```


This will delete all the breakpoints which are identified by the numbers `<break_number> ... <break_number>`. If you specify a breakpoint which does not exist, the debugger will simply ignore it.


You can also delete all the active breakpoints by giving the asterisk as an argument to `delete`, like so:

```wiki
   :delete *
```

### What happens when the debugger hits a breakpoint?


When an executing computation hits an active breakpoint, control is returned to the GHCi prompt. The debugger will print out a message indicating where the breakpoint occurred, and the names and types of the local variables which are in scope at that point. Here is an example:

```wiki
   Stopped at breakpoint in Main. Location: (6,6)-(6,20).
   Locals: x :: Bool, f :: Bool -> Bool, xs :: [Bool], fx :: Bool, j :: Bool
   *Main>
```


The string "`*Main>`" is GHCi's prompt marker. Note that it can change depending on what modules you have loaded. 


All the normal GHCi commands work at the prompt, including the evaluation of arbitrary expressions. In addition to the normal prompt behaviour, the local variables of the breakpoint are also made available. For instance, in the above example the variable `f` is a function from booleans to booleans, and we can apply it to an argument in the usual way:

```wiki
   *Main> f False
   True
```


The debugger also provides commands for inspecting the values of local variables without forcing their evaluation any further (see Inspecting values below). 


You can continue execution of the current computation with the `:continue` and `:step` commands, explained below.

### Inspecting values


It is important to note that, due to the non-strict semantics of Haskell (particularly lazy evaluation), the values of local variables at a breakpoint may only be partially evaluated. Therefore printing values may cause them to be further evaluated. This raises some interesting issues for the debugger because evaluating something could raise an exception, or it could cause another breakpoint to be fired, or it could cause non-termination. For these reasons we want to be able to print values in a way which preserves their current state of evaluation. The debugger provides the `:print` command for this purpose.


For example, suppose the local variable `xs` is bound to a list of booleans, but the list is completely unevaluated at a breakpoint. We can inspect its value without forcing any more evaluation like so:

```wiki
   *Main> :print xs
   xs = (_t1::[Bool])
```


The debugger uses fresh variable names (starting with underscores) to display unevaluated expressions (often called *thunks*). Here `_t1` is a thunk. A side effect of the `:print` command is that these fresh variables are made available to the command line, so we can refer to them future commands. 


Sometimes we want to evaluate thunks a little bit further. This is easy to do because they are bound to variable names. For example, we can evaluate the outermost data constructor of `_t1` using `seq` like so:

```wiki
   *Main> seq _t1 ()
   ()
```


This forces the evaluation of the thunk bound to `_t1` to Weak Head Normal Form (WHNF), and then returns `()`. The purpose of the expression is to force the evaluation of `_t1`, we don't actually care about the answer, so `()` makes a good dummy value.


If we print `xs` again we can see that it has been evaluated a little bit more:

```wiki
   *Main> :print xs
   xs = [True | (_t2::[Bool])]
```


Here we discover that the value of `xs` is a list with `True` as its head and a thunk as its tail. The thunk is bound to the fresh variable `_t2`, which can be manipulated at the command line as usual.


Another way to force further evaluation of a thunk is to use it inside another expression. For instance, we could examine the spine of the list `xs` by computing its length:

```wiki
   *Main> length xs
   3
   *Main> :print xs
   xs = [True,(_t3::Bool),(_t4::Bool)]
```

### Single stepping


When a computation has hit a breakpoint it is sometimes useful to continue execution up until the next breakable expression is evaluated, regardless of whether there is a breakpoint set at that location. This functionality is provided by the `:step` command:

```wiki
   :step
```


The `:step` command accepts an optional argument expression. The expression is evaluated as usual, but the computation will stop at the first breakable expression which is encountered:

```wiki
   :step <expression>
```

### Continuing execution after a breakpoint


A computation which has stopped at a breakpoint can be resumed with the `:continue` command:

```wiki
   :continue
```

---

## Known problems in the debugger

### Orphaned threads


Computations which fork concurrent threads can use breakpoints, but sometimes a thread gets blocked indefinitely. Consider this program:

```wiki
   main = do
      forkIO foo
      bar

   foo = do something

   bar = do something else
```


Suppose we have a breakpoint set somewhere inside the computation done by `foo`, but there are no breakpoints in the computation done by `bar`. When we run this program in GHCi the following things happen:

- `foo` gets forked and `foo` and `bar` begin their work 
- `bar` completes its job and we return to the GHCi prompt (uh oh!) 
- `foo` eventually hits a breakpoint and attempts to return to the command line, but it can't because we are already there (in the previous step).


Now the foo thread is blocked, so we can't witness the breakpoint.

### Wrong variable names in patterns


Consider this program:

```wiki
   foo (Just _  : xs) = [xs] 
   foo (Nothing : ys) = [ys]  {- set a breakpoint on this line -}

   main = print (foo [Nothing, Just ()])
```


If we hit a breakpoint in the second equation for `foo` we expect to see `ys` displayed as the local variables. Unfortunately, the debugger says that the locals are called `xs`:

```wiki
   *Main> :break 2
   Breakpoint activated in Main. Location: (2,22)-(2,25).
   *Main> main
   Stopped at breakpoint in Main. Location: (2,22)-(2,25).
   Locals: xs :: [Maybe ()]
```


The problem is the way that the compiler turns pattern matches into case expressions. XXX This issue deserves some extra thought.

---

## Wishlist of features (please feel free to add your ideas here)

### Backtracing


Perhaps the most useful missing feature is the ability to see where the program has been executing just prior to a breakpoint. A lexical call stack is probably out of the question (see: [ExplicitCallStack](explicit-call-stack), and [ExplicitCallStack/StackTraceExperience](explicit-call-stack/stack-trace-experience)), but we could keep track of the most recently visited breakable expressions. A key question is what kind of information should we keep about the evaluation history. The simplest thing is to record just source locations. A more adventurous idea is to keep the local variables too. One probem with keeping local variables is that we would have to make sure the GC keeps them alive, so the data structure for storing the evaluation history would have to be traversable by the GC. Note that by keeping things alive, there is the danger of introducing space leaks. Obviously we would not want to keep the whole evaluation history, but only the last N events, where N is a configurable parameter. Keeping track of the history is also likely to incur a performance penalty, so it might be advantageous to be able to turn it off, or perhaps have it off by default, and be able to turn it on.


It would be especially useful to be able to get backtraces when exceptions are raised.

### Listing source code at breakpoints


When a breakpoint is hit, we almost always want to look at the source code around the location of the breakpoint. It is a pain to have to do this manually in another window. It would be nice if the debugger provided a `:list` command, like gdb. 


Andy's interactive trace viewer is another way of watching the program exeuction, and it would be good to be able to connect the debugger to it. Nonetheless, I think a `:list` command would be useful on its own.

---

## Todo

### Pending

- Replace Loc with a proper source span type. (EASY)

- Investigate whether the compiler is eta contracting this def: "bar xs = print xs", this could be a problem if we want to print out "xs". (MODERATE)

- Fix the ghci help command. (EASY)

- Save/restore the link environment at breakpoints. At a breakpoint we modify both the hsc_env of the current Session, and also the persistent linker state. Both of these are held under IORefs, so we have to be careful about what we do here. The "obvious" option is to save both of these states on the resume stack when we enter a breakpoint and then restore them when we continue execution. I have to check with Simon if there are any difficult issues that need to be resolved here, like gracefully handling exceptions etc. (MODERATE)

- Remove dependency on -fhpc flag, put debugging on by default and have a flag to turn it off. (EASY)

- Allow breakpoints to be set by function name. Some questions: what about local functions? What about functions inside type class instances, and default methods of classes? (MODERATE)

- Support Unicode in data constructor names inside info tables. (MODERATE)

- Fix the slow search of the ticktree for larger modules, perhaps by keeping the ticktree in the module info, rather than re-generating it each time. (MODERATE)

- Use a primop for inspecting the STACK_AP, rather than a foreign C call. (MODERATE)

- Timing and correctness tests. (MODERATE)

- Wolfgang's patch for PIC seems to break the strings in Info tables, so we need to fix that. (MODERATE)

- Stabilise the API. (MODERATE)

- Fix the calculation of free variables at tick sites (currently done too late in the pipeline, gives some wrong results). Note a possible problem with letrecs, which means some locals vars are missing in where clause. (MODERATE/DIFFICULT)

- Extend the stack inspection primitive to allow unboxed things to be grabbed. (MODERATE)

### Partially done

- The delete command. It is fairly primitive, and probably not done in the best way. This will be fixed when the API is finalised.

- Look at slow behaviour of :print command on long list of chars. I've asked Pepe about this, he has an idea of what the problem is and will be working on a solution soon.

- User documentation. You're looking at it. The user manual will have to move into the main GHC docs at some point.

### Tentative

- Perhaps there are some redundant ticks we can delete, such as ones which begin at the same start position?

- Allow breakpoints to be enabled and disabled without deleting them, as in gdb.

- Extend breaks and step with counters, so that we stop after N hits, rather than immediately.

- Revert to adding tick information to the BCO directly, and remove the byte code instructions for breaks. I'm not sure that this is worth it. In some ways the implementation based on a byte code instruction is a little cleaner than adding breaks on BCOs directly. Though the bc instruction method may be a little slower than the other way.

---

## Implementation notes


How does the debugger work?

### Source code instrumentation


At the front end of the compiler we annotate the source code with **ticks**, based on the program coverage tool of Andy Gill and Colin Runciman. Ticks are uniquely numbered with respect to a particular module. Ticks are annotations on expressions, so each tick is associated with a source span, which identifies the start and end locations of the ticked expression.


Roughly, if `E` is an expression to be ticked, its annotated form becomes:

```wiki
   case tick<N> of _ -> E
```


where `<N>` is the module-unique number of the tick. 


The ticks are added in the de-sugaring phase of the front end, and the instrumentation is implemented in `deSugar/Coverage.lhs`. Note, we may eventually want to merge the source coverage tool and the debugger. If we do this, it would be useful to have a single piece of code for adding tick annotations. At the moment the debugger does not use all the ticks that the coverage tool uses. 


Slightly later in the de-sugarer we add arguments ticks which correspond to the free variables in scope at the ticked expression. This is done in `deSugar/DsUtils.lhs`, by the function `mkTickBox`. If `a,b,c` are the free variables of a ticked expression `E`, then the annotation from above is elaborated to:

```wiki
   case tick<N> a b c of _ -> E
```


To make the core lint happy, we must conjure up a type for `tick<N>`. If `a::T1, b::T2, c::T3` then the type is:

```wiki
   tick<N> :: T1 -> T2 -> T3 -> State# RealWorld
```


We are somewhat selective about where ticks go in the code, and it would be nice if this was documented properly. I will defer this until the code has stablised.


We assume, and indeed require, that each source span has at most one tick associated with it. This was not always upheld in the coverage tool (in the case of if-then-else expressions), so we had to modify the instrumentation a little bit. 


For each module we also allocate an array of breakpoint flags, with one entry for each tick in that module. This array is managed by the GHC storage manager, so it can be garbage collected if the module is re-loaded and re-ticked. We retain this array inside the `ModDetails` data structure, which is defined in `main/HscTypes.lhs`. In the current implementation the array is stored inside something called `ModBreaks`, which also stores an associtation list of source spans and ticks. However, the exact implementation of this depends on what we want in the API for the debugger, and it is likely that it will change soon. Also, `ModBreaks` is in desperate need of a new home. At the moment it is floating around somewhere in the `deSugar` directory, but that is almost certainly the wrong place for it.

### Byte code generation


In the coverage tool the ticks are turned into real code which performs a side effect when evaluated. In the debugger the ticks are purely annotations. They are used to pass information to the byte code generator, which generates special breakpoint instructions for ticked expressions. The ticks themselves are eventually deleted.


The byte code generator turns GHC Core into a bunch of Byte Code Objects (BCOs). BCOs are heap objects which correspond to top-level bindings, and `let` and `case` expressions. Each BCO contains a sequence of byte code instructions (BCIs), which are executed by the byte code interpreter (`rts/Interpreter.c`). Each BCO also contains some local data which is needed in the instructions. 


Given a ticked expression of the form:

```wiki
    case tick<N> a b c of _ -> E
```


we translate it into:

```wiki
   let freshVar = E in E
```


(Note: if the ticked expression was already let-bound we do not do this step, since it would be pointless.) The idea is that the let expression will be turned into a BCO. We annotate the BCO with information about the tick, such as its free variables, their offsets in the stack, and the tick number. We also store a pointer in the BCO to a breakpoint array for this particular module (which was introduced by the coverage transformation, see above), and an offset into that array. The offset corresponds to the tick number. The entries of the array are (currently) boolean flags which, at runtime, determine whether we should stop at the breakpoint or not. 


The BCIs for this BCO are generated as usual, and we prefix a new special breakpoint instruction on the front. Thus, when the BCO is evaluated, the first thing it will do is interpret the breakpoint instruction, and hence decide whether to break or not.


There is a downside to the introduction of lets: it causes more heap allocation in the debugged program. In particular we will allocate an expression on the heap, and then immediately evaluate it. We can tune the penalty to some extent by reducing the set of breakable expressions. More timing tests are needed to decide if the penalty is too high.


We experimented with alternative ways of implementing breakpoints, with the hope of avoiding this gratuitous heap allocation, but we ran into numerous obstacles which thwarted our attempts. The **big issue** is that when we hit a breakpoint we must leave the stack in a state which the GC can understand. The scheme described above works nicely because the first thing we do is interpret the break instruction for the BCO. At that point nothing has been done to the stack, so it is easy to leave it in a useful state. The same cannot be said for other types of expression (especially so because only lets and cases get turned into BCOs directly, everything else is just a sequence of BCIs). We initially thought that we could do something similar for the alternative branches of a case expression (since cases are the other kind of expression that gets turned into BCOs). The problem is that the scrutinee is unpacked before the branch is entered, and the unpacking pushes values onto the stack, leaving it in a state that the GC will not understand.  

### Stopping at a breakpoint at runtime in the byte code interpreter


Unfortunately this part of the story is somewhat complicated, *c'est la vie*.


To understand what happens it is necessary to know how GHCi evaluates an expression at the command line. When the user types in an expression (as a string) it is parsed, type checked, and compiled, and then run. In `main/GHC.hs` we have the function:

```wiki
   runStmt :: Session -> String -> IO RunResult
```


The `Session` argument contains the gobs of environmental information which is important to the compiler. The `String` is what the user typed in, and `RunResult`, is the answer that you get back if the execution terminates. `RunResult` is defined like so:

```wiki
   data RunResult
      = RunOk [Name]                -- names bound by this evaluation
      | RunFailed                   -- statement failed compilation
      | RunException Exception      -- statement raised an exception
      | forall a . RunBreak a ThreadId BreakInfo (IO RunResult)
```


The first three constructors are part of the original code, and the last one, `RunBreak` was added for the debugger. Hopefully the first three are self-explanatory; we will explain `RunBreak` in due course.


Normally what happens is that `runStmt` forks a new thread to handle the evaluation of the expression. It then blocks on an `MVar` and waits for the thread to finish. This MVar is (now) called `statusMVar`, because it carries the execution status of the computation which is being evaluated. We will discuss its type shortly. When the thread finishes it fills in `statusMVar`, which wakes up `runStmt`, and it returns a `RunResult`. Ultimately this gets passed back to the GHCi command line. Actually, GHCi is merely a *client* of the API, and other clients could also call `runStmt` if they wanted something evaluated. 


To make the discussion comprehensible let us distinguish two threads: 

1. The thread which runs the GHCi prompt.
1. The thread which is forked to run an expression.


We'll call the first one the *GHCi thread*, and the second the *expression thread*. 


In the debugger, the process of evaluating an expression is made more intricate. The reason is that if the expression thread hits a breakpoint it will want to return *early* to the GHCi thread, so that the user can access the GHCi prompt, issue commands *etcetera*. 


This raises a few questions:

- How do we arrange for the expression thread to stop and return early?
- What information needs to be passed from the expression thread to the GHCi thread, and how do we arrange that flow of information?
- How do we wake up the GHCi thread and return to the prompt?
- How do we continue execution of the expression thread after we have hit a breakpoint?
- What happens if we are running in the GHCi thread after a breakpoint, and we evaluate some other expression which also hits a breakpoint (i.e. what about nested breakpoints?)
- What happens if the expression thread forks more threads?


To arrange the early return of the expression thread when it hits a breakpoint we introduce a second MVar. At this point it is useful to give names to the two MVars:

1. `statusMVar`
1. `breakMVar`

### Inspecting values


This is done exactly as it was before in the prototype debugger. See: [GhciDebugger](ghci-debugger).
