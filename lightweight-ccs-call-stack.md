
We have been talking about this for a long time and never come around to actually implement it. During the Hackathon Freiburg'07 Alexey Rodriguez and me (Pepe Iborra) have been talking about how to do this. 

# The Basic Idea


The only known way to obtain a call trace in a lazy language which resembles in some way a strict call trace is by reusing the Cost Center Stack framework. In order to have backtraces one does not need the full CCS machinery. In particular there is no need for keeping track of costs and live objects in heap. 
But CCS are still too heavyweight because one needs to annotate thunks with the current CCS when they are allocated. Current implementation of CCS stores this in the info table. Our proposal is that, since all we need is a Word, we can move this to the header for AP and PAP closures. The interpreter will make use of this new field whereas compiled code can safely ignore it.

# The Details

## Storing the stack of CCS


For creating the CCS, we will follow the semantics found in \[Samson 97\] and extended in \[Jarvis 93\], ignoring all the cost assignment stuff. 
In principle, the current CCS configuration would be stored in a global variable in the C side, living in the interpreter.  However, For reasons that will become clear when we talk about case statements, we are going to need a stack of CCCS. Hence the current CCS will be a pointer to the head of this stack, and the stack will consist of pointers to rows in the table. A CCS configuration is a list of tuples of (tick \#, module). As expected, we also keep a table of possible CCS configurations; the current CCS conf. is a word pointing at one of these entries.


To make all of this clear, we have:

- A current CCS stack
- A pointer to the top of this stack
- A table of possible CCS configurations


We define the following primitive operations on these structures:

- PUSH_CENTER Pushes a new cost center in the current CCS at the top of the CCS stack. For this it needs to look at the table for the target configuration and possibly extend it if it does not already exist
- DUP_STACK Duplicates the current top of the stack. Used before entering the scrutinee in a case statement
- PUSH_STACK Pushes a CCS in the stack. Used when deallocating APs.
- POP_STACK  Discards the top of the stack out. Used after entering the scrutinee of a case statement


We will take advantage of ticks to guide the insertion of Cost Centers in the current CCS. For this, we will distinguish ticks that are placed at the beginning of top level function declarations (How? Perhaps with a distinguished BCI?) When the evaluator sees one such tick, it will call PUSH_CENTER with the current tick.

## Annotating thunks


There are two closure types that we must consider: APs and PAPs. APs are only created and seen by interpreted code (really?) whereas PAPs interact with compiled code too. These closures must be annotated with the CCS configuration at the moment of their allocation. We propose to simply extend their headers with an extra word to store this. 


We will have to change the following bits of code for this:

- Allocating AP and PAP closures. There are two scenarios we consider:

  - Allocating a PAP/AP from scratch: for this look at the current CCS (top of the CCS stack) and store it in the closure header.
  - Building a PAP/AP on top of a previous PAP: for this propagate the CCS stored in the base PAP, extend with an extra segment and store the result in the new closure header. The extra segment added is the identifier of the current function, which we can obtain by looking at the current CCS configuration and keeping the last element in the list of identifiers. 

    ```wiki
    Example: 
    We have the base PAP annotated with CCS '''[main,f,g,h]'''.
    The current CCS configuration at the moment of deallocating the base PAP is '''[main,f,j]'''.
    We deallocate the base PAP, allocate the new AP closure, and annotate it with CCS '''[main,f,g,h,j]'''.
    ```
- Deallocating AP closures: Following the semantics of \[Samson 97\], we must
  \# Extract the SCC annotated in the closure and push it in the CCS stack
  \# Deallocate the AP closure and enter it
  \# When we are done and before leaving, pop the CCS stack.

- Deallocating PAP closures: propagate the CCS annotation in the closure to the AP being allocated. (I am assuming that PAPs are never entered directly, even if the application is saturated)

## Handling case statements


Following the semantics, we must restore the CCS after the scrutinee of a case statement has been entered. I.e. we should duplicate the CCS stack top before entering the scrutinee, and pop out before continuing to the alternatives. We are not very sure on how to do this, but the current plan is the following. 


\# When we see a PUSH_ALT instruction, we duplicate the stack.
\# After that, we know that the scrutinee will be entered, with a continuation. We modify the code generated for this continuation so that it will pushes a POP_CCS in the GHC stack. 


POP_CCS will be some piece of code that will pop the CCS stack. 
(I am not very sure how this part works since Alexey is not here and my knowledge of all this is very vague).

# Further Details


The CCS table should be implemented efficiently following the data structure advices given in \[Samson 97\]. Hopefully there is already code for this in the profiling infrastructure.

# What we expect to obtain


We cannot observe compiled code for two reasons: it has no ticks that can guide cost center introduction, and the thunks it allocates lack CCS annotations. Therefore our backtraces will be restricted to interpreted code; this is not too bad in my opinion(pepe). 


In principle the overhead incurred should be acceptable, but we are worried that ticks will interact badly with dup'ing/pop'ing around case statements scrutinees, given the huge number of those showing up in the bytecode (as revealed by -ddump-bcos).

# References


\[Samson 97\] P. Samson and S.P.J. - Formally based profiling for Higher-Order languages
\[Jarvis 93\] R.G.Morgan and S.A.Jarvis - Profiling large scale lazy functional programs
