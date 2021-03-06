# The GHC Builder


This infrastructure is currently unmaintained and can be assumed to be bit-rotted. Currently GHC is tested via external [wiki:ContinuousIntegration](continuous-integration) services.


The **GHC builder** is a client/server system that allows us to build and test GHC on lots of different computers scattered around the world (the *clients*, or *build slaves*), and aggregate the test results centrally (the *server*).


Each night we build GHC on each slave, in various different ways, run the test suite and performance benchmarks, and mail the results to the `ghc-builds@haskell.org` mailing list.  

## Seeing build results


The build results are uploaded to [http://haskell.inf.elte.hu/builders/](http://haskell.inf.elte.hu/builders/)  See the [documentation on the summary](builder-summary) for more information.

## Can you offer a build slave?


We're always keen to add more build slaves to the setup. If a platform is represented in the nightly builds, it's we can identify and fix problems specific to that platform much more quickly.  If you'd like to join the fun, please let Gábor (us) know at `pali.gabor(at)gmail.com`. 

# How to set up a build slave


The GHC Builder is written in Haskell as a pair of Cabal packages (one for the clients/slaves, and one for the server).

## Prerequisites


Before installing the Builder, be sure that *[all the tools required for building and testing GHC](building)* is up-to-date and present on the system.  It is also advised to have at least **6 GB** of free space per builds, otherwise the build may fail.  It could be also useful to have an sufficiently isolated (or virtualized) system, dedicated to this task only.

## Install OpenSSL

- On Windows, install OpenSSL from here (not the Light Version): [http://www.openssl.org/related/binaries.html](http://www.openssl.org/related/binaries.html)
- On Linux, get OpenSSL from your distro.  E.g. install `openssl-devel` on RedHat-derived distros (e.g. Fedora), or `libssl-dev` (or `openssl-dev`) on Debian-derived distros (e.g. Ubuntu)
- On FreeBSD, OpenSSL is included in the base system and it is also available as a [port](http://www.freshports.org/security/openssl).
- On Mac OS X, install openssl from [http://www.macports.org/](http://www.macports.org/): `sudo port install openssl`.
- On Solaris: depending on what Solaris version you run you either need to install pkg:/library/security/openssl package (Solaris 11 Express) or install SUNWopenssl-libraries, SUNWopenssl-include packages (Solaris 10)

## Install HsOpenSSL


On non-Windows:

```wiki
$ cabal install HsOpenSSL
```


On Windows: You might have to add explicit include and lib directories:

```wiki
$ cabal install --extra-include-dirs="c:/OpenSSL/include" --extra-lib-dirs="c:/OpenSSL"
```

## To create a new build slave


You can get the code for the builder with

```wiki
$ git clone https://github.com/haskell/ghc-builder
```


and then build the Cabal package in the `client/` subdirectory:

```wiki
$ cd ghc-builder
$ cabal install common/ client/
```


If you experience problems feel free to fix them up -- and do not forget to submit a pull request with your patches! :-)


Once you have built it, mail Gábor Páli (`pali.gabor(at)gmail.com`) along any extra information (e.g. "GNU make is gmake", or "builds need these lines added to mk/build.mk").  You will then get a username and a password.  The username is used so we know which machine the build log came from, and the password is used to verify that the client is who it claims it is.


Then initialise the client by creating a new directory, and running:

```wiki
$ builder-client init username password haskell.inf.elte.hu
```


in it, where `username` and `password` are your username and password. This will create various files and subdirectories that the client will use. Then put a copy of [http://haskell.inf.elte.hu/ghcBuilder/cert/root.pem](http://haskell.inf.elte.hu/ghcBuilder/cert/root.pem) in `certs/` so that the client can verify that it is connecting to the right server.


First, you may want to do test some runs with the client, just to verify if everything works well:

```wiki
$ builder-client --do-build
```


then watch for the output on the `ghc-builds` mailing list.  If everything seems to be okay, you can now run the client with:

```wiki
$ builder-client
```


or

```wiki
$ builder-client -v
```


We recommend running in screen for now, as the client does not daemonise itself yet. The client will connect to the server, and the server will tell the client how and when to do builds. The client therefore needs to be left running unless you want to stop builds from happening.
