# Preparing a build environment

In order to build GHC, you need a UNIX-like environment with:
* some GNU tools (autoconf, m4, gcc, etc.)
* a GHC compiler ([bootstrap compiler](https://en.wikipedia.org/wiki/Bootstrapping_(compilers)))

This can be difficult to set up on some platforms (e.g. Windows) so there are two documented ways to do it:
* [automatically using Stack](building-ghc-with-stack)
* [manually](configuring-a-build-environment)

## Building GHC with Stack

Stack can automatically setup an environment suitable to build GHC. Follow the instructions on [Stack website](https://haskellstack.org) to install Stack on your platform.

Stack is particularly helpful on non UNIX platforms (e.g. Windows) as it installs GNU tools automatically by downloading and installing an [MSYS2 distribution](https://www.msys2.org/). On UNIX-like systems you are expected to install GNU tools (such as GCC) yourself.

Then to build GHC, you only have to execute one of the following commands:

```
hadrian/build.stack.sh  -c -j --flavour=quick # on UNIX-like platforms
hadrian\build.stack.bat -c -j --flavour=quick # on Windows platform
```

Upon the first build, Stack downloads and installs all the required dependencies in a Stack private directory, so you don't have to worry about messing with your system and the next builds are much faster because it is only done once.

The built compiler can be found in "_build/stage1/bin".

Refer to Hadrian documentation ("hadrian/README.md" file) to learn about the other available build system flags.

The environment set up by Stack is configured by the "hadrian/stack.yaml" file in GHC source tree. This ensures that you use dependencies and programs (bootstrap GHC compiler, happy/alex programs, etc.) that are known to work together and that are known to be able to build the current source tree (otherwise it is a bug).

Note: in the commands above we have used the "-c" flag to automatically execute "boot" and "configure" scripts in the environment managed by Stack. If you want to pass additional flags to these scripts, use the following commands instead:
```
cd hadrian
stack exec --cwd=.. boot <boot-params>
stack exec --cwd=.. configure <configure-params>
cd ..
hadrian/build.stack.{sh,bat} -j --flavour=quick # don't use "-c" here

```


## Configuring a building environment

It is also possible to set up a build environment for GHC by installing the required dependencies and tools manually. The following pages explain how to do it depending on your [platform](platforms). Note that some pages still refer to the legacy Make-based build system and should be updated (any help is welcome!).

- [Preparation Linux](building/preparation/linux)
- [Preparation MacOS X](building/preparation/mac-osx)
- [Preparation Windows](building/preparation/windows) ([Windows support for GHC](windows-ghc))
- [Preparation FreeBSD](building/preparation/free-bsd) ([FreeBSD support for GHC](free-bsd-ghc))
- [Preparation Solaris](building/preparation/solaris) and [Building under Solaris](building/solaris)
- [Illumos](building/preparation/illumos) (SmartOS, OpenIndiana, etc.)
- [Raspberry Pi](building/preparation/raspberry-pi) (cross-compiling from Linux)
- [Preparing and Building OpenBSD](building/preparation/open-bsd)

Reference: [list of tools needed to build GHC](building/preparation/tools)