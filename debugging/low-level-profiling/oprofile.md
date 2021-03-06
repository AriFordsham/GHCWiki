# oprofile


OProfile does a system-wide profile using performance counters.  It can also generate annotated source/assembly just like VTune.  Unfortunately, because GHC can't export source-level debugging symbols, OProfile cannot do source-level annotation of GHC programs.


It can be installed from a package on Ubuntu, which is cool.


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


check that the program you ran is at the top of the profile somewhere.  Sometimes for me it doesn't appear, and have to run it again.  To see a list of possible `<event>`s, use `$ opcontrol --list-events`.


It may be necessary to use `opcontrol --dump; opcontrol --shutdown` prior to `sudo opreport` above.  We are not sure.


Then to annotate the source:

```wiki
$ sudo opannotate --source <program> >&! log
```


lists the contents of source files in order of importance.  Alternatively use --output-dir to put the source files in a directory, but then 
you don't get to find out which are the important ones.

## Things to be aware of

- You might think that you could use the `-fvia-C -optc-g` options to force GHC to export C-source-level debugging symbols.  The Evil Mangler script that post-processes the `.s` file output by `gcc` does not understand the debug annotations added by `gcc`'s `-g` flag, and so `-optc-g` will not work.  You can, however, get GHC to output assembly-level debugging symbols by using `-opta-g`.  You'll probably also want to use the `-keep-s-files` option so that `opannotate` can get at the relevant `.s` files.

- Suppose that you do 

  ```wiki
  $ sudo opcontrol --setup --event=<eventname>:<count>
  ```

  and receive an error message that "Count `<count>` for event `<eventname>` is below the minimum."  If you check the minimum value for
  `<eventname>` using the `--list-events` option and see that `<count>` is in fact not below the purported minimum count, then you may have passed
  in a non-zero value for `--callgraph` option.  When doing callgraph profiling, OProfile requires that the sample count be at least 15 times the
  minimum.
  `opcontrol --status` will show you if callgraph is currently set.  To reset it, make sure the oprofile daemon is stopped and reset the callgraph
  value:

  ```wiki
  $ sudo opcontrol --shutdown
  $ sudo opcontrol --callgraph=0
  ```

  Now you can re-run your profiling and the original `<count>` should work.
