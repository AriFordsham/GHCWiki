# Building on MacOS X

## Getting Readline to work


Thanks to Paul R Brown for the following [ instructions](http://mult.ifario.us/articles/2006/10/17/ghc-6-6-and-mac-os-x-readline-quick-fix).


Building GHC 6.6 out of the box on MacOS X will leave you with a GHCi binary that has no readline support.  This is because MacOS X comes with a cut-down readline library that doesn't support all the things that GHC requires, so the GHC configure script decides not to use it.


To get readline working, you first need to install GNU readline:

```wiki
cd ~/work
mkdir gnu-readline
cd !$
wget ftp://ftp.cwru.edu/pub/bash/readline-5.2.tar.gz
tar xzvf readline-5.2.tat.gz
cd readline-5.2
./configure
make && sudo make install
```


Now you have to tell the GHC build about readline:

```wiki
cd ~/work
mkdir ghc
cd !$
wget http://www.haskell.org/ghc/dist/6.6/ghc-6.6-src.tar.bz2
wget http://www.haskell.org/ghc/dist/6.6/ghc-6.6-src-extralibs.tar.bz2
tar xjvf ghc-6.6-src.tar.bz2
tar xjvf ghc-6.6-src-extralibs.tar.bz2
cd ghc-6.6
./configure --with-readline-includes=/usr/local \
            --with-readline-libraries=/usr/local
make -j && sudo make install
```


(`-j` tells make to spawn lots of processes building in parallel, it will probably save some time especially if you have a multi-core machine).
