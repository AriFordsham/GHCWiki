# Trac with FastCGI


Since version 0.9, Trac supports being run through the [ FastCGI](http://www.fastcgi.com/) interface. Like [mod_python](trac-mod-python), this allows Trac to remain resident, and is faster than external CGI interfaces which must start a new process for each request. However, unlike mod_python, it is able to support [ SuEXEC](http://httpd.apache.org/docs/suexec.html). Additionally, it is supported by much wider variety of web servers.

## Simple Apache configuration

```wiki
# Enable fastcgi for .fcgi files
# (If you're using a distro package for mod_fcgi, something like
# this is probably already present)
<IfModule mod_fastcgi.c>
   AddHandler fastcgi-script .fcgi
   FastCgiIpcDir /var/lib/apache2/fastcgi 
</IfModule>
LoadModule fastcgi_module /usr/lib/apache2/modules/mod_fastcgi.so
```


You can either setup the `TRAC_ENV` as an overall default:

```wiki
FastCgiConfig -initial-env TRAC_ENV=/path/to/env/trac
```


Or you can serve multiple Trac projects in a directory like:

```wiki
FastCgiConfig -initial-env TRAC_ENV_PARENT_DIR=/parent/dir/of/projects
```


Configure `ScriptAlias` or similar options as described in [TracCgi](trac-cgi), but calling `trac.fcgi` instead of `trac.cgi`.

## Simple Lighttpd Configuration


The FastCGI front-end was developed primarily for use with alternative webservers, such as [ lighttpd](http://www.lighttpd.net/).


lighttpd is a secure, fast, compliant and very flexible web-server that has been optimized for high-performance
environments.  It has a very low memory footprint compared to other web servers and takes care of CPU load.


For using `trac.fcgi` with lighttpd add the following to your lighttpd.conf:

```wiki
fastcgi.server = ("/trac" =>
                   ("trac" =>
                     ("socket" => "/tmp/trac-fastcgi.sock",
                      "bin-path" => "/path/to/cgi-bin/trac.fcgi",
                      "check-local" => "disable",
                      "bin-environment" =>
                        ("TRAC_ENV" => "/path/to/projenv")
                     )
                   )
                 )
```


Note that you will need to add a new entry to `fastcgi.server` for each separate Trac instance that you wish to run. Alternatively, you may use the `TRAC_ENV_PARENT_DIR` variable instead of `TRAC_ENV` as described  above.


Other important information like [ this updated TracInstall page](http://trac.lighttpd.net/trac/wiki/TracInstall), [and this](trac-cgi#) are useful for non-fastcgi specific installation aspects.


Relaunch lighttpd, and browse to `http://yourhost.example.org/trac` to access Trac.

---


See also [TracCgi](trac-cgi), [TracModPython](trac-mod-python), [TracInstall](trac-install), [TracGuide](trac-guide)