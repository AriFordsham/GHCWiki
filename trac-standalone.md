# Tracd


Tracd is a lightweight standalone Trac web server. In most cases it's easier to setup and runs faster than the [CGI script](trac-cgi).

## Pros

- Fewer dependencies: You don't need to install apache or any other web-server.
- Fast: Should be almost as fast as the [mod_python](trac-mod-python) version (and much faster than the [CGI](trac-cgi)).
- Automatic reloading: For development, Tracd can be used in *auto_reload* mode, which will automatically restart the server whenever you make a change to the code (in Trac itself or in a plugin).

  - Options for tracd: `-r, --auto-reload`

## Cons

- Fewer features: Tracd implements a very simple web-server and is not as configurable or as scalable as Apache HTTPD.
- No native HTTPS support: [ sslwrap](http://www.rickk.com/sslwrap/) can be used instead,
  or [ stunnel -- a tutorial on how to use stunnel with tracd](http://trac.edgewall.org/wiki/STunnelTracd) or Apache with mod_proxy.

## Usage examples


A single project on port 8080. ([ http://localhost:8080/](http://localhost:8080/))

```wiki
 $ tracd -p 8080 /path/to/project
```


Stricly speaking this will make your Trac accessible to everybody from your network rather than *localhost only*. To truly limit it use *--hostname* option.

```wiki
 $ tracd --hostname=localhost -p 8080 /path/to/project
```


With more than one project. ([ http://localhost:8080/project1/](http://localhost:8080/project1/) and [ http://localhost:8080/project2/](http://localhost:8080/project2/))

```wiki
 $ tracd -p 8080 /path/to/project1 /path/to/project2
```


You can't have the last portion of the path identical between the projects since Trac uses that name to keep the URLs of the
different projects unique. So if you use `/project1/path/to` and `/project2/path/to`, you will only see the second project.


An alternative way to serve multiple projects is to specify a parent directory in which each subdirectory is a Trac project, using the `-e` option. The example above could be rewritten:

```wiki
 $ tracd -p 8080 -e /path/to
```


To exit the server on Windows, be sure to use `CTRL-BREAK` -- using `CTRL-C` will leave a Python process running in the background.

## Installing as a Windows Service


To install as a Windows service, get the [ SRVANY](http://www.google.com/search?q=srvany.exe) utility and run:

```wiki
 C:\path\to\instsrv.exe tracd C:\path\to\srvany.exe
 reg add HKLM\SYSTEM\CurrentControlSet\Services\tracd\Parameters /v Application /d "\"C:\path\to\python.exe\" \"C:\path\to\python\scripts\tracd-script.py\" <your tracd parameters>"
 net start tracd
```

**DO NOT** use `tracd.exe`.  Instead register `python.exe` directly with `tracd-script.py` as a parameter.  If you use `tracd.exe`, it will spawn the python process without SRVANY's knowledge.  This python process will survive a `net stop tracd`.


If you want tracd to start automatically when you boot Windows, do:

```wiki
 sc config tracd start= auto
```


The spacing here is important.

## Using Authentication


Using tracd with Apache .htpasswd files:


To create a .htpasswd file using htpasswd:

```wiki
 $ sudo htpasswd -c /path/to/env/.htpasswd username
```


then for additional users:

```wiki
 $ sudo htpasswd /path/to/env/.htpasswd username2
```


then for starting the tracd (on windows skip the "=" after --basic-auth):

```wiki
 $ tracd -p 8080 --basic-auth=environmentname,/fullpath/environmentname/.htpasswd,/fullpath/environmentname /fullpath/environmentname
```


Tracd provides support for both Basic and Digest authentication. The default is to use Digest; to use Basic authentication, replace `--auth` with `--basic-auth` in the examples below. (You must still specify a dialogic "realm", which can be an empty string by trailing the BASICAUTH with a comma.)

> *Support for Basic authentication was added in version 0.9.*


The general format for using authentication is (on windows skip the "=" after --auth):

```wiki
 $ tracd -p port --auth=base_project_dir,password_file_path,realm project_path
```


where:

- **base_project_dir** is the base directory of the project; note: this doesn't refer to the project name, and it is case-sensitive even for windows environments
- **password_file_path** path of the password file
- **realm** realm
- **project_path** path of the project


Example (on windows skip the "=" after --auth):

```wiki
 $ tracd -p 8080 \
   --auth=project1,/path/to/users.htdigest,mycompany.com /path/to/project1
```


Of course, the digest file can be be shared so that it is used for more than one project:

```wiki
 $ tracd -p 8080 \
   --auth=project1,/path/to/users.htdigest,mycompany.com \
   --auth=project2,/path/to/users.htdigest,mycompany.com \
   /path/to/project1 /path/to/project2
```


Another way to share the digest file is to specify "\*"
for the project name:

```wiki
 $ tracd -p 8080 \
   --auth="*",/path/to/users.htdigest,mycompany.com \
   /path/to/project1 /path/to/project2
```


If using the `-s` parameter for serving a Trac environment from the root of a domain, one must use `*` for the project name

## How to set up an htdigest password file


If you have Apache available, you can use the htdigest command to generate the password file. Type 'htdigest' to get some usage instructions, or read [ this page](http://httpd.apache.org/docs/2.0/programs/htdigest.html) from the Apache manual to get precise instructions.  You'll be prompted for a password to enter for each user that you create.  For the name of the password file, you can use whatever you like, but if you use something like `users.htdigest` it will remind you what the file contains. As a suggestion, put it in your \<projectname\>/conf folder along with the [trac.ini](trac-ini) file.


Note that you can start tracd without the --auth argument, but if you click on the *Login* link you will get an error.

## Generating Passwords Without Apache


If you don't have Apache available, you can use this simple Python script to generate your passwords:

```
fromoptparseimport OptionParser
# The md5 module is deprecated in Python 2.5try:fromhashlibimport md5
exceptImportError:frommd5import md5
realm ='trac'# build the options
usage ="usage: %prog [options]"
parser = OptionParser(usage=usage)
parser.add_option("-u","--username",action="store", dest="username",type="string",
                  help="the username for whom to generate a password")
parser.add_option("-p","--password",action="store", dest="password",type="string",
                  help="the password to use")
parser.add_option("-r","--realm",action="store", dest="realm",type="string",
                  help="the realm in which to create the digest")(options, args)= parser.parse_args()# check optionsif(options.username isNone)or(options.password isNone):
   parser.error("You must supply both the username and password")if(options.realm isnotNone):
   realm = options.realm
   
# Generate the string to enter into the htdigest file
kd =lambda x: md5(':'.join(x)).hexdigest()print':'.join((options.username, realm, kd([options.username, realm, options.password])))
```


Note: If you use the above script you must use the --auth option to tracd, not --basic-auth, and you must set the realm in the --auth value to 'trac' (without the quotes). Example usage (assuming you saved the script as trac-digest.py):

```wiki
 $ python trac-digest.py -u username -p password >> c:\digest.txt
 $ tracd --port 8000 --auth=proj_name,c:\digest.txt,trac c:\path\to\proj_name
```


Note: If you would like to use --basic-auth you need to use htpasswd tool from apache server to generate .htpasswd file. The remaining part is similar but make sure to use empty realm (i.e. coma after path). When using on Windows make sure to use -m option for it (did not tested it on \*nix, so not sure if that is the case there).  If you do not have Apache, [ htpasswd.py](http://trac.edgewall.org/intertrac/source%3A/tags/trac-0.11b2/contrib/htpasswd.py) may help.  (Note that it requires a `crypt` or `fcrypt` module; see the source comments for details.)


It is possible to use md5sum utility to generate digest-password file using such method:

```wiki
 $ printf "${user}:trac:${password}" | md5sum - >>user.htdigest
```


and manually delete " -" from the end and add "${user}:trac:" to the start of line from 'to-file'.

## Tips

### Serving static content


If `tracd` is the only webserver used for the project, 
it can also be used to distribute static content 
(tarballs, Doxygen documentation, etc.)


This static content should be put in the `$TRAC_ENV/htdocs` folder,
and is accessed by URLs like `<project_URL>/chrome/site/...`.


Example: given a `$TRAC_ENV/htdocs/software-0.1.tar.gz` file,
the corresponding relative URL would be `/<project_name>/chrome/site/software-0.1.tar.gz`, 
which in turn can be written using the relative link syntax
in the Wiki: `[/<project_name>/chrome/site/software-0.1.tar.gz]`


Since 0.10, Trac supports a new `htdocs:`[TracLinks](trac-links) 
syntax for the above. With this, the example link above can be written simply 
`htdocs:software-0.1.tar.gz`. 

### Using apache rewrite rules


In some situations when you choose to use tracd behind apache, you might experience issues with redirects, like being redirected to URLs with the wrong host or protocol. In this case (and only in this case), setting the `[trac] use_base_url_for_redirect` to `true` can help, as this will force Trac to use the value of `[trac] base_url` for doing the redirects.

### Serving a different base path than /


Tracd supports serving projects with different base urls than /\<project\>. The parameter name to change this is

```wiki
 $ tracd --base-path=/some/path
```

---


See also: [TracInstall](trac-install), [TracCgi](trac-cgi), [TracModPython](trac-mod-python), [TracGuide](trac-guide), [ Running tracd.exe as a Windows service](http://trac.edgewall.org/intertrac/TracOnWindowsStandalone%3Fversion%3D13%23RunningTracdasservice)