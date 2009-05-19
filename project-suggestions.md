# Suggestions for projects related to GHC


Here are some suggestions for projects related to GHC that could be undertaken by an intern or undergraduate project student.  There are also lots of ideas in

- GHC's task list (Ticket query: status: !closed, type: task, order: priority)
- GHC's feature request list (Ticket query: status: !closed, type: feature+request, order: priority)

---

## Projects that should be within reach of a good undergraduate

- **Implement overlap and exhaustiveness checking for pattern matching**.  GHC's current overlap and exhaustiveness checker is old and inadequate.  Furthermore, it takes no account of GADTs and type families. See [\#595](https://gitlab.haskell.org//ghc/ghc/issues/595) and [\#2395](https://gitlab.haskell.org//ghc/ghc/issues/2395).  There's an excellent selection of background material:

  - [ Warnings for pattern matching](http://pauillac.inria.fr/~maranget/papers/warn/warn.pdf) by Luc Maranget (JFP 17(3), 2007)
  - [ Focusing on pattern matching](http://www.cs.cmu.edu/~neelk/pattern-popl09.pdf) by Neelakantan Krishnaswami (POPL 2009)
  - [ Compiling pattern matching to good decision trees](http://pauillac.inria.fr/~maranget/papers/ml05e-maranget.pdf) by Luc Maranget, ML Workshop 2008

- **Improve parallel profiling tools**.  Satnam Singh and Simon Marlow have made a start on some tools for visualising the behaviour of parallel programs, but there is much more to do here, and it'll be eagerly adopted by users.

- **Implement some low-level C-- optimisations**.  During 2009 we expect to have the new C-- code generation route in place, and that will open up new opportunities for doing classic compiler-course optimisations on the imperative C-- code.  There is more than routine stuff here, because we can use our [ generic dataflow framework](http://research.microsoft.com/~simonpj/papers/c--) to do the heavy lifting.  Here are some [particular ideas for optimisations](back-end-notes) we'd like to implement.

---

## More ambitious or less-well-defined projects (PhD students / Interns)

### Programming environment and tools

- Maintaining an explicit call stack [ExplicitCallStack](explicit-call-stack)

### Turning GHC into a platform


Projects aimed at making GHC into a user-extensible plug-in platform, and less of a monolithic compiler.

- **Allow much finer and more modular control over the way in which rewrite rules and inlining directives are ordered**.  See this [ email thread](http://www.haskell.org/pipermail/haskell-cafe/2008-January/038196.html)

- **Support dynamically-linked Core-to-Core plug-ins**, so that people can add passes simply by writing a Core-to-Core function, and dynamically linking it to GHC.  This would need to be supported by an extensible mechanism like ``attributes`` in mainstream OO languages, so that programmers can add declarative information to the source program that guides the transformation pass.  Likewise the pass might want to construct information that is accessible later.  This mechanism could obviously be used for optimisations, but also for program verifiers, and perhaps also for domain-specific code generation (the pass generates a GPU file, say, replacing the Core code with a foreign call to the GPU program). See [Plugins](plugins) for some early thoughts on this.

- **Improve the GHC API**, whereby you can import GHC as a library.  We make improvements now and then, but it would benefit from some sustained attention.  A particular project would be to port the Haskell refactorer [ HaRE](http://www.cs.kent.ac.uk/projects/refactor-fp/hare.html) to use the GHC API.

### Types

- **Allow unboxed tuples as function arguments**.   Currently unboxed tuples are second class; fixing this would be a nice simplification.

- **Extend kinds beyond \* and k1-\>k2**.  With GADTs etc we clearly want to have kinds like `Nat`, so that advanced hackery at the type level can be done in a typed language; currently it's all effectively untyped.  A neat approach would be to re-use any data type declaration as a kind declaration.

- **Extensible constraint domains**.  Andrew Kennedy shows how to incorporate [ dimensional analysis](http://research.microsoft.com/~akenn/units/index.html) into an ML-like type system.  Maybe we could do an extensible version of this, so that it wasn't restricted to dimensions.  Integer arithmetic is another obvious domain.  

- 

  <table><tr><th>[\#788](https://gitlab.haskell.org//ghc/ghc/issues/788)</th>
  <td>Implement class aliases and/or constraint synonyms</td></tr></table>

  [\#1872](https://gitlab.haskell.org//ghc/ghc/issues/1872)Extensible RecordsParallel stuffExperiment with multiprocessor Haskell and/or STM by building and measuring applications, investigate improvements
  Continue work on parallel GC: particularly independent minor-generation collections.
  Just Hacking
  Projects for people who want a decent-sized hacking project, with less research content.
  Compiler[\#602](https://gitlab.haskell.org//ghc/ghc/issues/602)Warning SuppressionWhole-program dead-code detection (with `--make`).
  Whole-program overloading elimination (with `--make`).
  Evolve a better ordering for the optimisation passes using [ Acovea](http://www.coyotegulch.com/products/acovea/).
  [\#1341](https://gitlab.haskell.org//ghc/ghc/issues/1341)allow loading partially correct modules[\#2362](https://gitlab.haskell.org//ghc/ghc/issues/2362)allow full import syntax in GHCi[\#2979](https://gitlab.haskell.org//ghc/ghc/issues/2979)better support for FFI C wrappers for macros in system headers[\#594](https://gitlab.haskell.org//ghc/ghc/issues/594)Support use of SSE2 in the x86 native code genreatorBuild system[\#989](https://gitlab.haskell.org//ghc/ghc/issues/989)Build GHC on Windows using Microsoft toolchain[\#1876](https://gitlab.haskell.org//ghc/ghc/issues/1876)Complete shared library supportRuntime system[\#599](https://gitlab.haskell.org//ghc/ghc/issues/599)The Front Panel[\#603](https://gitlab.haskell.org//ghc/ghc/issues/603)GC-spy connectionToolsUpdate/improve [ Visual Haskell](http://www.haskell.org/visualhaskell) to use the (free) [ Visual Studio Shell](http://msdn2.microsoft.com/en-us/vsx2008/products/bb933751.aspx).
  Download in other formats:[Plain Text](/trac/ghc/wiki/ProjectSuggestions?version=14&format=txt)[](http://trac.edgewall.org/)Powered by [Trac 1.2.2](/trac/ghc/about)

          By [Edgewall Software](http://www.edgewall.org/).Visit the Trac open source project at
  [http://trac.edgewall.org/](http://trac.edgewall.org/)