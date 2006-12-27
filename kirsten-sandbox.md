# Building GHC on Mac OS X 10.2, by Kirsten aged 26 1/52

1. grab the HEAD off darcs, per [Building/GettingTheSources](building/getting-the-sources) (no problems here)
1.  run autoreconf (this went ok, because I had just upgraded my autoconf in order to build darcs)
1.  run ./configure
1.  oops, I only have happy 1.13, it wants happy 1.15
1.  go to the happy download page. what, no Mac OS X binary?
1.  grab the happy sources
1.  ./configure, make, make install. so far so good
1.  ./configure GHC again
1.  LOL, I need alex 2.0
1.  why isn't this integrated into the GHC build process? also, googling for just "alex" is un-useful. so is googling for "alex lexer" and "alex lexer haskell"
1.  LOL, \*still\* no Mac OS X binary.
1.  ./configure; make in alex
1.  have disgusting IM conversation with friend while waiting
1.  alex: "you lose at life":

  ```wiki
  Creating a symbolic link from alex-2.0.1 to alex in /usr/local/bin failed: `/usr/local/bin/alex' already exists
  Perhaps remove `/usr/local/bin/alex' manually?
  make[2]: *** [install] Error 1
  make[1]: *** [install] Error 1
  make: *** [install] Error 1
  ```
1.  consider a career change
1.  rm /usr/local/bin/alex
1.  sudo rm /usr/local/bin/alex
1.  sudo make me a sandwich
1.  sudo make install
1.  okay, I have alex. yippee.
1.  ./configure in GHC again
1.  seems to have worked. With trembling fingers (or maybe that's just the freezing Southern California weather), type "make".
1. OH NOEZ:

  ```wiki
  gcc -O -DTABLES_NEXT_TO_CODE -I. -I../rts    -c mkDerivedConstants.c -o mkDerivedConstants.o
  InfoTables.h:314: illegal member declaration, missing name, found `}'
  OSThreads.h:135: #error "Threads not supported"
  OSThreads.h:141: undefined type, found `OSThreadId'
  OSThreads.h:145: illegal external declaration, missing `;' after `OSThreadProcAttr'
  OSThreads.h:145: illegal external declaration, missing `;' after `*'
  OSThreads.h:147: undefined type, found `OSThreadId'
  OSThreads.h:148: undefined type, found `OSThreadProc'
  OSThreads.h:153: undefined type, found `Condition'
  OSThreads.h:154: undefined type, found `Condition'
  OSThreads.h:155: undefined type, found `Condition'
  OSThreads.h:156: undefined type, found `Condition'
  OSThreads.h:157: undefined type, found `Condition'
  OSThreads.h:158: undefined type, found `Mutex'
  OSThreads.h:163: undefined type, found `Mutex'
  OSThreads.h:164: undefined type, found `Mutex'
  OSThreads.h:169: undefined type, found `ThreadLocalKey'
  OSThreads.h:170: undefined type, found `ThreadLocalKey'
  OSThreads.h:171: undefined type, found `ThreadLocalKey'
  Storage.h:211: undefined type, found `Mutex'
  Storage.h:212: undefined type, found `Mutex'
  ../rts/Task.h:88: undefined type, found `OSThreadId'
  ../rts/Task.h:115: undefined type, found `Condition'
  ../rts/Task.h:116: undefined type, found `Mutex'
  ../rts/Task.h:225: illegal function prototype, found `*'
  ../rts/Task.h:225: illegal function definition, found `)'
  ../rts/Task.h:235: undefined type, found `ThreadLocalKey'
  ../rts/Capability.h:74: undefined type, found `Mutex'
  ../rts/Capability.h:197: undefined type, found `Mutex'
  cpp-precomp: warning: errors during smart preprocessing, retrying in basic mode
  make[1]: *** [mkDerivedConstants.o] Error 1
  make: *** [stage1] Error 1
  ```
1.  Cry.
1.  Hmm, how old is my gcc, anyway?

  ```wiki
  % gcc --version
  gcc (GCC) 3.1 20020420 (prerelease)
  Copyright (C) 2002 Free Software Foundation, Inc.
  This is free software; see the source for copying conditions.  There is NO
  warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. [ yeah, no shit, Sherlock ]
  ```
1. Will upgrading my gcc break everything? Probably. Should I do it anyway? Probably.
1. Actually, upgrading gcc sounds about as appealing as a quadruple root canal. Maybe I can use gcc2 (2.95.2) instead.
1. Aaaaargh, of course I'm not that lucky:

  ```wiki
  checking for gcc... gcc2
  checking for C compiler default output file name... 
  configure: error: C compiler cannot create executables
  ```
1.  Start downloading tarball for newer gcc from: [ http://www.opensource.apple.com/darwinsource/August2003GCCUpdate/](http://www.opensource.apple.com/darwinsource/August2003GCCUpdate/). Also consider becoming religious so I can pray for it to be actually useful.
1. ```wiki
  Length: 31,674,171 [application/x-tar]

   2% [>                                    ] 895,686       14.36K/s    ETA 34:52
  ```
1.  Wish that parents-in-law would get the faster internets at their house.
1.  Eat breakfast.
