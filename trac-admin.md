# [TracAdmin](trac-admin)


Trac is distributed with a powerful command-line configuration tool. This tool can be used  to configure and customize your Trac-installation to better fit your needs.

## Usage


You can get a comprehensive list of the available options, commands and sub-commands by invoking `trac-admin` with the `help` command:

```wiki
trac-admin help
```


Unless you're executing the `help`, `about` or `version` sub-commands, you'll need to specify the path to the [TracEnvironment](trac-environment) that you want to administer as the first argument, for example:

```wiki
trac-admin /path/to/projenv wiki list
```

## Interactive Mode


When passing the environment path as the only argument, `trac-admin` starts in interactive mode.
Commands can then be executed on the selected environment using the prompt, which offers tab-completion
(on non-Windows environments, and when the Python `readline` module is available) and automatic repetition of the last command issued.


Once you're in interactive mode, you can also get help on specific commands or subsets of commands:


For example, to get an explanation of the `resync` command, run:

```wiki
> help resync
```


To get help on a all the Wiki-related commands, run:

```wiki
> help wiki
```

---


See also: [TracGuide](trac-guide), [TracBackup](trac-backup), [TracPermissions](trac-permissions), [TracEnvironment](trac-environment), [TracIni](trac-ini)