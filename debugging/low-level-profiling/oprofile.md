# oprofile


OProfile does a system-wide profile using performance counters.  It can also generate annotated source/assembly just like VTune.  Unfortunately, because GHC can't export source-level debugging symbols, OProfile cannot do source-level annotation of GHC programs.


It can be installed from a package on Ubuntu, which is cool.  But beware: Ubuntu bug 172495: [ https://bugs.launchpad.net/ubuntu/+source/oprofile/+bug/172495/](https://bugs.launchpad.net/ubuntu/+source/oprofile/+bug/172495/) : change the `/bin/sh` to `/bin/bash` at the top of `/usr/bin/opcontrol`.


There seems to be a problem with the counters just stopping arbitrarily, requiring a restart of the daemon.


Recipe for profiling something:

```wiki
$ sudo opcontrol --init
$ sudo opcontrol --reset
$ sudo opcontrol --setup --event=<event>
$ sudo opcontrol --start
$ ... run program...
$ sudo opcontrol --stop
$ sudo opreport
```


check that the program you ran is at the top of the profile somewhere.  Sometimes for me it doesn't appear, and have to run it again.  To see a list of possible `<event>`s, use

```wiki
$ opcontrol --list-events
```


Then to annotate the source:

```wiki
$ sudo opannotate --source <program> >&! log}}}
lists the contents of source files in order of importance.  Alternatively use --output-dir to put the source files in a directory, but then 
you don't get to find out which are the important ones.
```