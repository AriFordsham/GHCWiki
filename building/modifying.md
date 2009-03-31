# Modifying the build system


This section covers making changes to the GHC build system.  We'll give some general advice on how to work with the build system, and then describe a few common scenarios, such as how to add a new source file.


Note that before making any non-trivial changes to the build system you should acquaint yourself with the overall [architecture](building/architecture).  Even if you're already familiar with GNU make, the GHC build system is probably quite different from most `Makefile`-based build systems you've seen before.  


Incedentally, it's a good idea to have a copy of the 
[ GNU make documentation](http://www.gnu.org/software/make/manual/make.html) to hand when working with the build system.

## Debugging


When the build system doesn't do what you want, the results can be
pretty cryptic.  Often the problem is that something is being built in
the wrong order, or some variable isn't being propagated to the places
you thought it was.  How do you go about debugging the build system?


Debugging techniques can also be a handy way to understand how the build system works.  Want to test your hypothesis about what the variable `rts_C_SRCS` contains?  Just add a `$(warning $(rts_C_SRCS))` somewhere.


Here are the techniques that we use.  Note, for many of these diagnosis techniques you may want to invoke
**make** on `ghc.mk` directly using `make -f ghc.mk`, to bypass the
[phase ordering](building/modifying#) machinery of the top-level
`Makefile`.

<table><tr><th>`make --debug=b --debug=m`</th>
<td>
causes **make** to show the sequence of dependencies that it is
following, which will often tell you *why* something is being
built.  This can help to track down missing or incorrect
dependencies.
</td></tr></table>

<table><tr><th>`make -p`</th>
<td>
prints out all the generated rules and variables.  The output can be
huge; so pipe it to a file, and search through it for the bits of
interest.
</td></tr></table>

<table><tr><th>`$(warning ... message ...)`</th>
<td>
equivalent to "printf-debugging\` in a C program: this causes
**make** to print a message when it reads the `$(warning ..)`
expression, and the message can include variable references.  Very
useful for finding out what **make** thinks the value of a
variable is at a particular point in the `Makefile`, or for finding
out the parameters for a particular macro call.
</td></tr></table>

<table><tr><th>`make show VALUE=VAR`</th>
<td>
prints the value of variable `VAR`.  Useful for quick diagnosis.
</td></tr></table>