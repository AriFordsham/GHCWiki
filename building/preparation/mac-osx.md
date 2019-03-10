# Setting up a MacOS X system for building GHC


You will need to install [ Homebrew](http://mxcl.github.com/homebrew/) for getting some of the tools needed. Homebrew is recommended over other systems on OS X such as MacPorts and Fink.

## Xcode (GCC)

### For Lion (10.7), Xcode 7.3 or higher


Firstly, you need to install the Xcode Command Line tools from Apple. You can do that in two ways (the second is faster):

1. Install all of Xcode:

  - Install Xcode from the Mac App Store.
  - Launch Xcode.
  - In the Preference dialog of Xcode, select the "Downloads" pane and install "Command line tools".
1. Install the command line tools only:

  - At the [ downloads page of Apple Developer](http://developer.apple.com/downloads), download the latest "Command line tools".
  - Install them.


In both cases, you need to [ register](https://developer.apple.com/programs/register/) as an Apple developer first (free).

### Previous versions of OS X and Xcode


Get the most recent version of Apple's Xcode tools that you can. Your OS X CD has a version on it. You may be able to download a newer version from the [ Apple Developer Connection](http://developer.apple.com/tools/xcode) website. You may need to sign up for a free membership in the Apple Developer Connection, and downloading may still cost a little money.  In later versions of OS X (10.6 / 10.7), Apple added the "App Store". Xcode is available within the App Store for "Free".


Successful builds of older GHC sources have been reported using Xcode 3.0, 2.4 and 2.4.1 on Intel Macs. Xcode 2.2.1 is known *not* to work out of the box on Intel Macs, and Xcode 3.0 is known *not* to work out of the box on PowerPC Macs ([\#2887](https://gitlab.haskell.org//ghc/ghc/issues/2887)). Versions prior to 3.1 may build GHC successfully, but choke on certain libraries.

## GHC


Secondly, you need a installation of GHC for use as your bootstrap compiler.


You can install a binary distribution of GHC in three ways:

1. Install the [ Haskell Platform](http://www.haskell.org/platform/).
1. Install via Homebrew: `brew install ghc` OR `brew install haskell-platform`.
1. Install a [binary distribution from GHC](http://www.haskell.org/ghc/download).

**NB:** You need to use a binary distribution of GHC 7.4.1 (or later) as your bootstrap compiler.

## GNU tools


Thirdly, if you want to build the development version of GHC from the Git repositories, you also need to install the GNU auto tools. You can get them as follows:

- (Homebrew): `brew install autoconf automake`

## LLVM


Fourthly, if you like to use GHC's LLVM backend:

- (Homebrew): `brew install llvm`

## Installing DocBook


Finally, if you want to build the documentation you need to install DocBook. You can install it like so:

- (Homebrew): `brew install docbook`