# Mac OS X Installer Packages

## Roll your own


In a GHC build tree, after having run `./configure` (if the tree came straight out of darcs, also `sh boot`), issue

```wiki
make framework-pkg
```


The result will be a file `GHC-<version>-<arch>.pkg`, where `<version>` is the full version string and `<arch>` is `i386` or `ppc`.  The build process uses `xcodebuild` and `packagemaker` and has only been tested with Xcode 3.0.


The command `make framework-pkg` runs `./configure` again to ensure that GHC is compiled for installation under `/Library/Frameworks`.  If you need to specify extra arguments to `./configure` (e.g., to use a version of the readline library installed in a non-standard location), set the environment variable `XCODE_EXTRA_CONFIGURE_ARGS`.


To create a package that links against readline statically, you can use the following trick.  Create a private library directory, say `/Users/chak/lib`, from which you symbolically link the static readline and ncurses libraries.  For example, if the latter were installed (e.g., via MacPorts) under `/opt/local/lib` execute

```wiki
cd /Users/chak
mkdir lib
cd lib
ln -s /opt/local/lib/readline.a .
ln -s /opt/local/lib/ncurses.a .
```


Now, build the package with

```wiki
cd <LOCATION_OF_MY_GHC_TREE>
env CFLAGS=-Wl,-search_paths_first\
    XCODE_EXTRA_CONFIGURE_ARGS="--with-readline-includes=/opt/local/include --with-readline-libraries=/Users/chak/lib"\
    make framework-pkg
```


(Don't forget to replace `/Users/chak/lib` by your private library directory.)  Note that simply removing (or renaming) the dynamic libraries of readline will not work, as this will lead the configure script of the readline package to assume that readline isn't installed at all.  As a result, you will get a GHC without readline support.

## What's inside?


GHC is packaged as a [ framework bundle](http://developer.apple.com/documentation/MacOSX/Conceptual/BPFrameworks/Frameworks.html), which uses GHC's integer version number, consisting of the major and minor version component only, to assign framework versions - i.e., packages of the 6.8 branch install framework version 608.  This is in line with Apple's recommendation to use version numbers that signify API changes for frameworks.


The package installs appropriate links in /usr/bin, /usr/man/man1, and /usr/share/doc to make the binaries, ghc manpage, and html documentation easily accessible.  Furthermore, it comes with a shell script, installed at `/Library/Frameworks/GHC.framework/Tools/Uninstaller`, that removes the GHC.framework and all symbolic links into the framework.


Currently, GHC only supports building systemwide frameworks installed at `/Library/Frameworks`.  Relocatable frameworks would be desirable, but are much more messy as GHC (once installed) is currently not easily relocatable.  (The GHC Xcode project under `distrib/MacOS/GHC.xcodeproj`, which builds GHC frameworks, includes a partially completed target to build a relocatable package if anybody is interested in getting their hands dirty.)  A hybrid installer that let's users choose between a systemwide and a non-admin install is AFAIK currently not feasible due to limitations of Apple's PackageMaker software unless we include two separate distributions in one package (leading to a very large package).

## PPC and Tiger


I have tested package building only on Intel Leopard with Xcode 3.0.  I expect that PPC Leopard works without any further adjustments.  However, the Xcode project building the framework may not be suitable for Xcode 2.5, which is the latest version on Tiger.  Nevertheless, it should be possible to build packages on Leopard that run on both Tiger and Leopard.  This will require some tweaks, but may not be difficult to achieve.

## Where is the code?


All code and documents needed to build Mac installers is below `distrib/MacOS`.  There is an Xcode project `GHC.xcodeproj` that builds GHC appropriately, creates the framework structure (at `/tmp/GHC.dest/Library/Frameworks`), and installs GHC into that framework structure.  Moreover the PackageMaker document `GHC-system.pmdoc` specifies the installer package layout and meta information.  The post-install script setting up the links and the uninstaller are in the `installer-scripts` directory.
