# Trac Logging


Trac supports logging of system messages using the standard [ logging module](http://docs.python.org/lib/module-logging.html) that comes with Python.


Logging is configured in the `[logging]` section in [trac.ini](trac-ini).

## Supported Logging Methods


The log method is set using the `log_type` configuration option, which takes any of the following values:

<table><tr><th>**none****</th>
<td>Suppress all log messages.
</td></tr>
<tr><th>**file**</th>
<td>Log messages to a file, specified with the `log_file` option in [trac.ini](trac-ini). 
</td></tr>
<tr><th>**stderr**</th>
<td>Output all log entries to console ([tracd](trac-standalone) only).
</td></tr>
<tr><th>**syslog**</th>
<td>(UNIX) Send messages to local syslogd via named pipe `/dev/log`.
</td></tr>
<tr><th>**eventlog**</th>
<td>(Windows) Use the system's NT eventlog for Trac logging.
</td></tr></table>

## Log Levels


The verbosity level of logged messages can be set using the *log_level* directive in [trac.ini](trac-ini). The log level defines the minimum level of urgency required for a message to be logged.


The levels are:

<table><tr><th>**CRITICAL**</th>
<td>Log only the most critical (typically fatal) errors.
</td></tr>
<tr><th>**ERROR**</th>
<td>Log failures, bugs and errors. 
</td></tr>
<tr><th>**WARN**</th>
<td>Log warnings, non-interrupting events.
</td></tr>
<tr><th>**INFO**</th>
<td>Diagnostic information, log information about all processing.
</td></tr>
<tr><th>**DEBUG**</th>
<td>Trace messages, profiling, etc.
</td></tr></table>

---


See also: [TracIni](trac-ini), [TracGuide](trac-guide), [TracEnvironment](trac-environment)