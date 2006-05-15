
Back to the [GarbageCollectorNotes](garbage-collector-notes)

# Problems Compiling GHC


I think it is useful to have a small section about compiling GHC. I ran into several magic problems getting GHC to build. I don’t fully understand the reasons for some of the fantastic sounding ones, however its worth mentioning them. Some of these are just general discipline guidelines, but are useful to keep in mind.

### Problem 1


Make sure that you do actually have the latest versions of everything involved. These include:

- Darcs (a version control system that GHC is shifting towards)
- GHC source (get it using Darcs)
- Alex (the Lexer generator)
- Happy (the Parser generator)


Since the compilation of Haskell is very Unix styled, on Windows (I work using a Win XP machine), one needs to add several unix tools to windows. I don’t know why GHC building doesn’t target SFU on windows yet – maybe that’s something that they just haven’t got down to doing yet. Here are the unix-ish components that you need:

- MinGW (The compiler set, please get the latest – they have a downloading installer available somewhere, I used that one. I had several magic problems with many packaged binaries that I found on the net).
- MSYS (These maybe downloaded as binaries from the Msys site).
- MSYS DTK. 


The GHC compiler notes explains all of this rather well. 

### Problem 2


Make sure your path is right. Make sure you get the right versions of things in your path. As an example, I have SFU binaries in my path before I had the MSYS ones they apparently don’t like each other too much.

### Problem 3


Watch out for what files you edit. There are some files in the build process are parsed by a simplistic C preprocessor and will not understand C++ style comments. Here are a list of these files:


\[get file list\]

### Problem 4


This is a classical magic problem. I got GHC to build on my desktop but it would not my laptop. As a matter of fact when I type in “autoreconf” the machine freezes up after about 30 seconds. Very puzzling. 


This behavior would often end with power cycling the laptop and was rather frustrating for a while. After studying the process with procexp, filemon and some standard monitoring tools it seemed like the “Logitech LVPrcSrv module” related to my Logitech webcam seems to hog the CPU. Since I wasn’t using the webcam, I killed the process and the build went through fine. At this point I can’t guess at what the relationship is or why there should be one. 


Roshan James (rpjames \[at\] cs \[dot\] indiana \[dot\] edu)
