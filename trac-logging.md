# Trac Logging


Trac supports logging of system messages using the standard [logging module](http://docs.python.org/library/logging.html) that comes with Python.


Logging is configured in the `[logging]` section in [trac.ini](trac-ini#).

## Supported Logging Methods



The log method is set using the `log_type` option in [trac.ini](trac-ini#), which takes any of the following values:


<table><tr><th><b>none<i></i></b></th>
<td>Suppress all log messages.
</td></tr>
<tr><th><b>file</b></th>
<td>Log messages to a file, specified with the <tt>log_file</tt> option in <a href="trac-ini#">trac.ini</a>. Relative paths in <tt>log_file</tt> are resolved relative to the <tt>log</tt> directory of the environment.
</td></tr>
<tr><th><b>stderr</b></th>
<td>Output all log entries to console (<a href="trac-standalone">tracd</a> only).
</td></tr>
<tr><th><b>syslog</b></th>
<td>(UNIX) Send all log messages to the local syslogd via named pipe <tt>/dev/log</tt>. By default, syslog will write them to the file /var/log/messages.
</td></tr>
<tr><th><b>eventlog</b></th>
<td>(Windows) Use the system&apos;s NT Event Log for Trac logging.
</td></tr></table>


## Log Levels



The verbosity level of logged messages can be set using the `log_level` option in [trac.ini](trac-ini#). The log level defines the minimum level of urgency required for a message to be logged, and those levels are:


<table><tr><th><b>CRITICAL</b></th>
<td>Log only the most critical (typically fatal) errors.
</td></tr>
<tr><th><b>ERROR</b></th>
<td>Log failures, bugs and errors. 
</td></tr>
<tr><th><b>WARN</b></th>
<td>Log warnings, non-interrupting events.
</td></tr>
<tr><th><b>INFO</b></th>
<td>Diagnostic information, log information about all processing.
</td></tr>
<tr><th><b>DEBUG</b></th>
<td>Trace messages, profiling, etc.
</td></tr></table>


Additionally, you can  enable logging of SQL statements at debug level. This is turned off by default, as it's very verbose. Set `[trac] debug_sql = yes` in [TracIni](trac-ini) to activate.

## Log Format



The output format for log entries can be specified through the `log_format` option in [trac.ini](trac-ini#). The format is a string which can contain any of the [Python logging Formatter variables](http://docs.python.org/library/logging.html#logrecord-attributes). Additonally, the following Trac-specific variables can be used:


<table><tr><th><b>$(basename)s</b></th>
<td>The last path component of the current environment.
</td></tr>
<tr><th><b>$(path)s</b></th>
<td>The absolute path for the current environment.
</td></tr>
<tr><th><b>$(project)s</b></th>
<td>The originating project&apos;s name.
</td></tr></table>


Note that variables are identified using a dollar sign (`$(...)s`) instead of percent sign (`%(...)s`).



The default format is:


```
log_format = Trac[$(module)s] $(levelname)s: $(message)s
```


In a multi-project environment where all logs are sent to the same place (e.g. `syslog`), it makes sense to add the project name. In this example we use `basename` since that can generally be used to identify a project:


```
log_format = Trac[$(basename)s:$(module)s] $(levelname)s: $(message)s
```

---



See also: [TracIni](trac-ini), [TracGuide](trac-guide), [TracEnvironment](trac-environment)


