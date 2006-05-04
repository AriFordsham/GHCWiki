# GHC port for arm-unknown-linux-gnu


My goal is to create a registerised port of GHC to the nokia 770.

## Setting up the build environment


I have been using the standard maemo cross-development environment. Instructions for setting
up this environment can be found here:

[ http://www.maemo.org/platform/docs/tutorials/Maemo_tutorial.html\#settingup](http://www.maemo.org/platform/docs/tutorials/Maemo_tutorial.html#settingup)

## Changes to standard procedure


The updated instructions on this page should now work:

[http://www.haskell.org/ghc/docs/latest/html/building/sec-porting-ghc.html\#unregisterised-porting](http://www.haskell.org/ghc/docs/latest/html/building/sec-porting-ghc.html#unregisterised-porting)


With two small changes:


(1) I had to add --srcdir=. 


Anyplace configure is called I get this error:

```wiki
This configuration does not support the `--srcdir' option..
```


Adding --srcdir=. makes the error go away.


(2) ghc/Makefile SUBDIRS ordering


This has been fixed in head, but if you download the 6.4.2 release you will need to
edit ghc/Makefile and change the ordering of the SUBDIRS so that lib comes before compiler.


This is the default ordering:

```wiki
ifeq "$(BootingFromHc)" "YES"
SUBDIRS = includes rts docs compiler lib utils driver
else
```


and you want

```wiki
ifeq "$(BootingFromHc)" "YES"
SUBDIRS = includes rts docs lib compiler utils driver
else
```


That should get to the point of having a ghc-inplace built. Next I will figure out how to
using the ghc-inplace to build ghc without the .hc files.
