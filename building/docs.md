# Building the documentation

## Haddock documentation


The GHC build includes Haddock, and the Haddock documentation for libraries is built and installed by default.


You can disable Haddock documentation for your build by adding

```wiki
HADDOCK_DOCS = NO
```


to your `mk/build.mk`.


It is also possible to process the libraries sources using [ HsColour](http://hackage.haskell.org/cgi-bin/hackage-scripts/package/hscolour), and for the Haddock documentation to include links to the HsColoured source code; in order to do this, just install `HsColour` and re-run `./configure`.  The configure script will tell you whether it found `HsColour` at the end.


To build the Haddock docs for a given library, do this:

```wiki
cd libraries/base
make html
```

## DocBook documentation


The rest of the documentation, in particular the Users' Guide and Cabal documentation, are in [ DocBook](http://www.docbook.org/) XML format.  In order to process the documentation into HTML or printable formats, you need appropriate tools installed.  The `configure` script searches for the appropriate tools, and will tell you whether it found any.


To install the tools necessary for building the documentation, see [Building/Preparation](building/preparation).


The following make variables control the building of each type of documentation:

```wiki
BUILD_DOCBOOK_HTML = YES/NO
BUILD_DOCBOOK_PS   = YES/NO
BUILD_DOCBOOK_PDF  = YES/NO
```


They are set to `YES` or `NO` in `mk/config.mk` by configure, depending on whether the necessary tools were detected on your system.  You can override the automatic settings in your `mk/build.mk` file.

## Installing documentation


Documentation is installed by default by 'make install'.
