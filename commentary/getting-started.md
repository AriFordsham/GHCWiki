## Source Tree Layout


An overview of the source tree may be found [here](commentary/source-tree).

## Build System Basics


Detailed information about the build system may be found [here](building); what follows is a quick overview, highlighting the areas where GHC's build system diverges substantially from the way `make` is used in most other projects.


Most projects keep the parts of their build machinery in files called `Makefile` found in many/most subdirectories of the source tree.  GHC uses the filename `ghc.mk` instead; you'll find a file with this name in quite a number of subdirectories.


Other build system files are in `rules/` and `mk/`.

## Coding Style


The [Coding style guidelines](contributing#working-conventions) may be found on the wiki.
