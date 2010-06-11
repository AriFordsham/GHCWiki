# The LLVM backend


David Terei wrote a new code generator for GHC which targets the LLVM compiler infrastructure. Most of the work was done as part of an honours thesis at the University of New South Wales under the supervision of Manuel Chakravarty. Its now at a stage where it is under consideration to be merged into GHC mainline.


Some documentation:

- The thesis paper which offers a detailed performance evaluation, as well as the motivation and design of the back-end can be found at: [ http://www.cse.unsw.edu.au/\~pls/thesis/davidt-thesis.pdf](http://www.cse.unsw.edu.au/~pls/thesis/davidt-thesis.pdf)
- Blog post [ http://blog.llvm.org/2010/05/glasgow-haskell-compiler-and-llvm.html](http://blog.llvm.org/2010/05/glasgow-haskell-compiler-and-llvm.html)


As the back-end isn't currently in GHC head, you need to follow the steps below to get it up and running.

- [Installing & Using](commentary/compiler/backends/llvm/installing)
- [Design & Implementation](commentary/compiler/backends/llvm/design)
- [Bugs & Other Problems](commentary/compiler/backends/llvm/development-notes)
- [Work Currently in Progress](commentary/compiler/backends/llvm/wip)