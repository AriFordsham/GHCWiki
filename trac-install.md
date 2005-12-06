# Trac Installation Guide


Trac is a lightweight project management tool that is implemented as a web-based application. Trac is written in the Python programming language and can use [ SQLite](http://sqlite.org/) or [ PostgreSQL](http://www.postgresql.org/) as  database. For HTML rendering, Trac uses the [ Clearsilver](http://www.clearsilver.net/) templating system.


What follows are generic instructions for installing and setting up Trac and its requirements. While you can find instructions for installing Trac on specific systems at [ TracInstallPlatforms](http://projects.edgewall.com/trac/wiki/TracInstallPlatforms) on the main Trac site, please be sure to first read through these general instructions to get a good understanding of the tasks involved.

## Requirements


To install Trac, the following software packages must be installed:

- [ Python](http://www.python.org/), version \>= 2.3.

  - Python 2.4 is not supported on Windows since there are no Subversion bindings available for it.
  - For RPM-based systems you might also need the `python-devel` and `python-xml` packages.
- [ Subversion](http://subversion.tigris.org/), version \>= 1.0. (\>= 1.1 recommended) and corresponding [ Python bindings](http://svnbook.red-bean.com/svnbook-1.1/ch08s02.html#svn-ch-8-sect-2.3)

  - Trac uses the [ SWIG](http://www.swig.org/) bindings included in the Subversion distribution, **not**[ PySVN](http://pysvn.tigris.org/) (which is sometimes confused with the standard SWIG bindings).
  - If Subversion was already installed without the SWIG bindings, you'll need to re-`configure` Subversion and `make swig-py`, `make install-swig-py`.
- [ ClearSilver](http://www.clearsilver.net/), version \>= 0.9.3

  - With python-bindings (`./configure --with-python=/usr/bin/python`)

### For SQLite

- [ SQLite](http://www.sqlite.org/), version 2.8.x or 3.x
- [ PySQLite](http://pysqlite.org/)

  - version 1.0.x (for SQLite 2.8.x)
  - version 1.1.x or 2.x (for SQLite 3.x)

### For PostgreSQL

- [ PostgreSQL](http://www.postgresql.org/)
- [ psycopg1](http://initd.org/projects/psycopg1), [ psycopg2](http://initd.org/projects/psycopg2), or [ pyPgSQL](http://pypgsql.sourceforge.net/)

### Optional Requirements

- A CGI-capable web server (see [TracCgi](trac-cgi)), or
- a [ FastCGI](http://www.fastcgi.com/)-capable web server (see [TracFastCgi](trac-fast-cgi)), or
- [ Apache](http://httpd.apache.org/) with [ mod_python 3.1.3+](http://www.modpython.org/) (see [TracModPython](trac-mod-python))
- [ setuptools](http://peak.telecommunity.com/DevCenter/setuptools), version \>= 0.5a13 for using plugins (see [TracPlugins](trac-plugins))
- [ docutils](http://docutils.sourceforge.net/), version \>= 0.3.3 for [WikiRestructuredText](wiki-restructured-text).
- [ SilverCity](http://silvercity.sourceforge.net/) and/or [ Enscript](http://www.gnu.org/software/enscript/enscript.html) for [syntax highlighting](trac-syntax-coloring).

**Attention**: The various available versions of these dependencies are not necessarily interchangable, so please pay attention to the version numbers above. If you are having trouble getting Trac to work please double-check all the dependencies before asking for help on the [ MailingList](http://projects.edgewall.com/trac/wiki/MailingList) or [ IrcChannel](http://projects.edgewall.com/trac/wiki/IrcChannel).


Please refer to the documentation of these packages to find out how they are best installed. In addition, most of the [ platform-specific instructions](http://projects.edgewall.com/trac/wiki/TracInstallPlatforms) also describe the installation of the dependencies.

## Installing Trac


Like most Python programs, the Trac Python package is installed by running the following command at the top of the source directory:

```wiki
$ python ./setup.py install
```

*Note: you'll need root permissions or equivalent for this step.*


This will byte-compile the python source code and install it in the `site-packages` directory
of your Python installation. The directories `cgi-bin`, `templates`, `htdocs`, `wiki-default` and `wiki-macros` are all copied to `$prefix/share/trac/.`


The script will also install the [trac-admin](trac-admin) command-line tool, used to create and maintain [project environments](trac-environment), as well as the [tracd](trac-standalone) standalone server.

### Advanced Users


To install Trac to a custom location, or find out about other advanced installation options, run:

```wiki
$ python ./setup.py --help
```


Specifically, you might be interested in:

```wiki
$ python ./setup.py install --prefix=/path/you/want
```

## Creating a Project Environment


A [Trac environment](trac-environment) is the backend storage where Trac stores information like wiki pages, tickets, reports, settings, etc. An environment is basically a directory that contains a human-readable configuration file and various other files and directories.


A new environment is created using [trac-admin](trac-admin):

```wiki
$ trac-admin /path/to/trac_project_env initenv
```

[trac-admin](trac-admin) will prompt you for the information it needs to create the environment, such as the name of the project, the path to an existing subversion repository, the [database connection string](trac-environment#), and so on. If you're not sure what to specify for one of these options, just leave it blank to use the default value. The database connection string in particular will always work as long as you have SQLite installed. The only option where the default value is likely to not work is the path to the Subversion repository, so make sure that one's correct.


Also note that the values you specify here can be changed later by directly editing the [TracIni](trac-ini) configuration file.

*Note: The user account under which the web server runs will require write permissions to the environment
directory and all the files inside.*

## Running the Standalone Server


After having created a Trac environment, you can easily try the web interface by running the standalone server [tracd](trac-standalone):

```wiki
$ tracd --port 8000 /path/to/projectenv
```


Then, fire up a browser and visit `http://localhost:8000/`. You should get a simple listing of all environments that tracd knows about. Follow the link to the environment you just created, and you should see Trac in action.

## Running Trac on a Web Server


Trac provides three options for connecting to a “real” web server: [CGI](trac-cgi), [FastCGI](trac-fast-cgi) and [mod_python](trac-mod-python). For decent performance, it is recommended that you use either FastCGI or mod_python.

## Configuring Authentication


The process of adding, removing, and configuring user accounts for authentication depends on the specific way you run Trac.  To learn about how to accomplish these tasks, please visit one of the following pages:

- [TracStandalone](trac-standalone) if you use the standalone server, `tracd`.
- [TracCgi](trac-cgi) if you use the CGI or FastCGI methods.
- [TracModPython](trac-mod-python) if you use the mod_python method.

## Using Trac


Once you have your Trac site up and running, you should be able to browse your subversion repository, create tickets, view the timeline, etc.


Keep in mind that anonymous (not logged in) users can by default access most but not all of the features. You will need to configure authentication and grant additional [permissions](trac-permissions) to authenticated users to see the full set of features.

*Enjoy''
*

[ The Trac Team](http://projects.edgewall.com/trac/wiki/TracTeam)

---


See also:  [TracGuide](trac-guide), [TracCgi](trac-cgi), [TracFastCgi](trac-fast-cgi), [TracModPython](trac-mod-python), [TracUpgrade](trac-upgrade), [TracPermissions](trac-permissions)