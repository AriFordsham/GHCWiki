# The Haskell Execution Model


The [STG language](commentary/compiler/stg-syn-type) has a clear *operational* model, as well as having a declarative lambda-calculus reading.  The business of the [code generator](commentary/compiler/code-gen) is to translate the STG program into `C--`, and thence to machine code, but that is mere detail. From the STG program you should be able to understand:

- What functions are in the compiled program, and what their entry and return conventions are
- What heap objects are allocated, when, and what their layout is


GHC uses an eval/apply execution model, described in the paper [ How to make a fast curry: push/enter vs eval/apply](http://research.microsoft.com/%7Esimonpj/papers/eval-apply).  This paper is well worth reading if you are interested in this section.

## Registers


Source files: [includes/Regs.h](/trac/ghc/browser/ghc/includes/Regs.h), [includes/MachRegs.h](/trac/ghc/browser/ghc/includes/MachRegs.h)


During execution of Haskell code the following (virtual) registers are always valid:

- `Hp` points to the byte before the first free byte in the (contiguous) allocation space.

- `HpLim` points to the last avaiable byte in the current chunk of allocation space (see [Heap/Stack check failures](#Heap/Stackcheckfailures)).

- `Sp` points to the youngest allocated byte of stack.  The stack grows downwards.  Why?  Because that means that a return address is at a lower address than the stack frame it "knows about", and that in turn means that we can treat a stack frame very like a heap object, with an info pointer (return address) as its first word.

- `SpLim` points to the last available byte in the current stack.


There are bunch of other virtual registers, used for temporary argument passing, for words, floats and doubles: `R1` .. `R10`, `F1` .. `F4`, `D1` .. `D4`, `L1` .. `L2`.


In a register-rich machine, many of these virtual registers will be mapped to real registers.  In a register-poor machine, they are instead allocated in a static memory record, pointed to by a real register, `BaseReg`.


The code generator knows how many real registers there are, and tries to avoid using virtual registers that are not mapped to real registers.  So, for example, it does not use `R5` if the latter is memory-mapped; instead, it passes arguments on the stack.

## Function Calls


Source files: [rts/Apply.h](/trac/ghc/browser/ghc/rts/Apply.h), [rts/Apply.cmm](/trac/ghc/browser/ghc/rts/Apply.cmm)


Dealing with calls is by far the most complicated bit of the execution model, and hence of the code generator.  Before we can talk about that, we need some terminology:

- The **arity** of a function is the number of lambdas statically used in [the lambda-form of its definition](commentary/compiler/stg-syn-type).  Note that arity is not deducible from the type.  Example:

  ```wiki
  f :: Bool -> Bool -> Bool
  f = \x -> case x of 
                 True  -> not
                 False -> id
  ```

  Here, `f` has arity 1, even though its type suggests it takes two arguments.  The point is that the compiled code for `f` will expect to be passed just one argument, `x`.

- The **entry point** (sometimes called the **fast entry point**) of a function of arity N expects its first N  arguments to be passed in accordance with the standard **[Entry convention](commentary/rts/haskell-execution#)**.

- A **known call** is a call of a function whose binding site is statically visible:

  - The function is bound at top level in this module
  - The function is bound at top level in another module, and optimistion is on, so we can see the details (notably arity) of the function in the module's interface file.
  - The function is bound by an `let` binding that encloses the call.


When compiling a call, there are several cases to consider, which are treated separately.  

- **Known function, saturated call**.   The function is applied to exactly the right number of arguments to satisfy its arity.  In that case, we simply load the arguments according to the standard entry convention, and tail-call (jump to) the function's entry point.

- **Known function, too few arguments**.  In this case, we want to build a partial application (PAP), and return with a pointer to the PAP in the return register.  Since building a PAP is a complicated business, instead we just behave as for an unknown function call, which will end up calling into the [Generic apply](#Genericapply) code, which will build the PAP for us.

- **Known function, too many arguments**.  We want to save the extra arguments on the stack, push a return address, and then behave just like a saturated call.  When the result comes back, we should behave like "unknown call".  However, to avoid needing to generate code for a new continuation here, the return address that we push on the stack is that of an appropriate [Generic apply](#Genericapply) function, which will perform the application of the extra arguments to the (unknown) function returned by the saturated call.

- **Unknown function**;  a call in which we do not statically know what the function is.  In that case we must do a "generic apply".  This is so exciting that it deserves its [own section](commentary/rts/haskell-execution#generic-apply).

### Generic apply


Files: [utils/genapply](/trac/ghc/browser/ghc/utils/genapply)


When compiling a call that has an unknown function, we must generate code to

- Evaluate the function
- Scrutinise the function value returned to see its arity, and dispatch into the same three cases as in the case of known calls:

  - Exactly the right number of arguments: load them into the standard locations and tail-call the function's entry point
  - Too few arguments: build a PAP
  - Too many arguments: save the excess arguments, and tail call the function as for a saturated cal.


All of this takes quite a lot of code, so we pre-generate a whole bunch of generic-apply code sequencues, one for each combination of arguments.  This code is generated by the tool [utils/genapply](/trac/ghc/browser/ghc/utils/genapply), and the generated code appears in `rts/AutoApply.cmm`.


For example, if we find a call to an unknown function applied to two (boxed) `Int` arguments, load the function and its two arguments as for the standard entry convention and jump to `stg_ap_pp_fast`.  This latter code is in `rts/AutoApply.cmm`, generated by the `genapply` tool.  The "`pp`" part is the bit that says the code is specialised for two pointer arguments.


In addition to the family of `stg_ap_<pattern>_fast` functions for making calls to unknown functions with various argument patterns, there is a corresponding family of return addresses `stg_ap_<pattern>_info`.  The idea is that you can push a continuation that will make a call to the function that is returned to it.  For example, to push a continuation that will apply a single pointer argument, we would push the following words on the stack:

<table><tr><th> arg 
</th></tr>
<tr><th>`stg_ap_p_info`</th></tr></table>

## Entry conventions


Entry conventions are very conventional: the first N argumements in registers and the rest on the stack.

## Return Convention

### Direct Returns

### Vectored Returns

## Updates


Source files: [rts/Updates.h](/trac/ghc/browser/ghc/rts/Updates.h), [rts/Updates.cmm](/trac/ghc/browser/ghc/rts/Updates.cmm)

## Heap/Stack check failures


Source files: [rts/HeapStackCheck.cmm](/trac/ghc/browser/ghc/rts/HeapStackCheck.cmm)


When allocating a heap object, we bump `Hp` and compare to `HpLim`. If the test fails we branch to ???.  Usually this code tests an interrupt flag (to see if execution should be brought tidily to a halt); grabs the next block of alloaction space; makes `Hp` point to it and `HpLim` to its end; and returns.  If there are no more allocation-space blocks, garbage collection is triggered.
