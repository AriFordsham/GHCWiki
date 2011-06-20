### Porting GHC using LLVM backend


This document is kind of short porting roadmap which serves as a high-level overview for porters of GHC who decided to use LLVM instead of implementing new NCG for their target platform. Please have [Design & Implementation](commentary/compiler/backends/llvm/design) at hand since this contains more in-depth information.
The list of steps needed for new GHC/LLVM port is:

**(1)** Make sure GHC unregisterised build is working on your target platform.

**(2)** Now try to compile some very simple programs such as 'hello world'
or simpler using the GHC you just built, both with the C backend and
the LLVM backend. If the llvm backend built programs are failing find
out why. This is done using a combination of things such as the error
message you get when the program fails, tracing the execution with GDB
and also just comparing the assembly code produced by the C backend to
what LLVM produces. This last method is often the easiest.

**(3)** When the programs you throw at the LLVM backend are running, try
running the GHC testsuite. First run it against the C backend to get a
baseline, then run it against the LLVM backend. Fix any failures that
are LLVM backend specific.

**(4)** If the testsuite is passing, now try to build GHC itself using the
LLVM backend. This is a very tough test. When working though its a
good proof that the LLVM backend is working well on your platform.

**(5)** Now you have LLVM working in unregistered mode, so the next thing
is to implement custom GHC calling convention and get the llvm backend
working in registered mode but with (TABLES_NEXT_TO_CODE = NO in your
build.mk). Majority of this step involves hacking inside the LLVM code. Usually lib/Target/\<your target platform name\> is the best way to start. Also you might study what David Terei did for x86 support: [ http://lists.cs.uiuc.edu/pipermail/llvmdev/2010-March/030031.html](http://lists.cs.uiuc.edu/pipermail/llvmdev/2010-March/030031.html) and his patch itself: [ http://lists.cs.uiuc.edu/pipermail/llvmdev/attachments/20100307/714e5c37/attachment-0001.obj](http://lists.cs.uiuc.edu/pipermail/llvmdev/attachments/20100307/714e5c37/attachment-0001.obj) to get an idea what's really needed.

**(6)** Once **(5)** is working you have it all running except
TABLES_NEXT_TO_CODE. So change that to Yes in your build.mk and get
that working.
