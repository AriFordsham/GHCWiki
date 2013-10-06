# Trac Backup


Since Trac uses a database backend, some extra care is required to safely create a backup of a [project environment](trac-environment). Luckily, [trac-admin](trac-admin) has a command to make backups easier: `hotcopy`.

> *Note: Trac uses the `hotcopy` nomenclature to match that of [ Subversion](http://subversion.tigris.org/), to make it easier to remember when managing both Trac and Subversion servers.*

## Creating a Backup


To create a backup of a live [TracEnvironment](trac-environment), simply run:

```wiki

  $ trac-admin /path/to/projenv hotcopy /path/to/backupdir

```

[trac-admin](trac-admin) will lock the database while copying.**


The resulting backup directory is safe to handle using standard file-based backup tools like `tar` or `dump`/`restore`.


Please, note, that hotcopy command does not overwrite target directory and when such exists, hotcopy ends with error: `Command failed: [Errno 17] File exists:` This is discussed in [ \#3198](http://trac.edgewall.org/intertrac/ticket%3A3198).

### Restoring a Backup


Backups are simply a copied snapshot of the entire [project environment](trac-environment) directory, including the SQLite database. 


To restore an environment from a backup, stop the process running Trac (i.e. the Web server or [tracd](trac-standalone)), restore the contents of your backup (path/to/backupdir) to your [project environment](trac-environment) directory and restart the service.

---


See also: [TracAdmin](trac-admin), [TracEnvironment](trac-environment), [TracGuide](trac-guide), [ TracMigrate](http://trac.edgewall.org/intertrac/TracMigrate)