# Building the documentation

## Haddock documentation


The GHC build includes Haddock, and the Haddock documentation for libraries is built and installed by default.  It is also possible to process the libraries sources using [ HsColour](http://hackage.haskell.org/cgi-bin/hackage-scripts/package/hscolour), and for the Haddock documentation to include links to the HsColoured source code; in order to do this, just install `HsColour` and run `./configure`.  The configure script will tell you whether it found `HsColour` at the end.

## DocBook documentation


The rest of the documentation, in particular the Users' Guide and Cabal documentation, are in [ DocBook](http://www.docbook.org/) XML format.  In order to process the documentation into HTML or printable formats, you need appropriate tools installed.  The `configure` script searches for the appropriate tools, and will tell you whether it found any.


To install the tools necessary for building the documentation, see [Building/Preparation](building/preparation).


At the moment, we are not able to build documentation in PDF format due to tool flakiness.  If you manage to find a way to process the documentation into readable PDF, please let us know!
