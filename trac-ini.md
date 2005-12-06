# The Trac Configuration File


Trac configuration is done by editing the **`trac.ini`** config file, located in `<projectenv>/conf/trac.ini`.

## Global Configuration


Since version 0.9, Trac can also read the configuration from a global `trac.ini` file. These global options will then be merged with the environment-specific options, where local options override global options.


The global configuration is by default localted in `$prefix/share/trac/conf/trac.ini`. It can be moved to a different location (for example, `/etc/trac.ini`), but that requires changing the file `trac/siteconfig.py` which gets created when Trac is installed. 

## Reference


This is a brief reference of available configuration options.

## \[trac\]

<table><tr><th>`database`</th>
<th>[Database connection string](trac-environment#) for this project 
</th></tr>
<tr><th>`default_charset`</th>
<th> Charset used in text files in the subversion repository (default is `iso-8859-15`) 
</th></tr>
<tr><th>`default_handler`</th>
<th> Name of the component that handles requests to the base URL (default is `WikiHandler`) (*since 0.9*) 
</th></tr>
<tr><th>`repository_dir`</th>
<th> Path to local Subversion repository 
</th></tr>
<tr><th>`authz_file`</th>
<th> Path to Subversion [ authorization (authz) file](http://svnbook.red-bean.com/en/1.1/ch06s04.html#svn-ch-6-sect-4.4.2). 
</th></tr>
<tr><th>`authz_module_name`</th>
<th> The module prefix used in the `authz_file` (See FineGrainedPermissions)
</th></tr>
<tr><th>`check_auth_ip`</th>
<th> Whether the IP address of the user should be checked for authentication (true, false) (*since 0.9*) 
</th></tr>
<tr><th>`ignore_auth_case`</th>
<th> Whether case should be ignored for login names (true, false) (*since 0.9*) 
</th></tr>
<tr><th>`templates_dir`</th>
<th> Path to the ClearSilver templates 
</th></tr></table>

## \[project\]

<table><tr><th>`name`</th>
<th> Project name 
</th></tr>
<tr><th>`descr`</th>
<th> Short project description 
</th></tr>
<tr><th>`url`</th>
<th> URL to the main project website 
</th></tr>
<tr><th>`icon`</th>
<th> URL to icon file to use as shortcut icon (favicon) 
</th></tr>
<tr><th>`footer`</th>
<th> Page footer text (right-aligned) 
</th></tr></table>

## \[header_logo\]

<table><tr><th>`src`</th>
<th> URL to image to use as header logo 
</th></tr>
<tr><th>`link`</th>
<th> Destination URL to link to from header logo 
</th></tr>
<tr><th>`alt`</th>
<th>*alt* text for header logo 
</th></tr>
<tr><th>`width`</th>
<th> Header logo width in pixels 
</th></tr>
<tr><th>`height`</th>
<th> Header logo height in pixels 
</th></tr></table>


See also: [TracInterfaceCustomization](trac-interface-customization).

## \[logging\]

<table><tr><th>`log_type`</th>
<th> Logging facility to use. (none, file, stderr, syslog, winlog) 
</th></tr>
<tr><th>`log_file`</th>
<th> If *log_type* is *file*, this should be a path to the log-file 
</th></tr>
<tr><th>`log_level`</th>
<th> Level of verbosity in log (CRITICAL, ERROR, WARN, INFO, DEBUG) 
</th></tr></table>


See also: [TracLogging](trac-logging)

## \[attachment\]

<table><tr><th>`max_size`</th>
<th> Maximum allowed file size for ticket and wiki attachments 
</th></tr></table>

## \[notification\]

<table><tr><th>`smtp_enabled`</th>
<th> Enable SMTP (email) notification (true, false) 
</th></tr>
<tr><th>`smtp_server`</th>
<th> SMTP server to use for email notifications 
</th></tr>
<tr><th>`smtp_user`</th>
<th> Username for SMTP server (*since 0.9*) 
</th></tr>
<tr><th>`smtp_password`</th>
<th> Password for SMTP server (*since 0.9*) 
</th></tr>
<tr><th>`smtp_from`</th>
<th> Sender address to use in notification emails 
</th></tr>
<tr><th>`smtp_replyto`</th>
<th> Reply-To address to use in notification emails 
</th></tr>
<tr><th>`smtp_always_cc`</th>
<th> Email address(es) to always send notifications to 
</th></tr>
<tr><th>`always_notify_reporter`</th>
<th> Always send notifications to any address in the *reporter* field 
</th></tr>
<tr><th>`always_notify_owner`</th>
<th> Always send notifications to the ticket owner (*since 0.9*) 
</th></tr></table>


See also: [TracNotification](trac-notification)

## \[mimeviewer\]

<table><tr><th>`enscript_path`</th>
<th> Path to the Enscript program 
</th></tr>
<tr><th>`php_path`</th>
<th> Path to the PHP program 
</th></tr>
<tr><th>`max_preview_size`</th>
<th> Maximum file size for HTML preview (*since 0.9*) 
</th></tr>
<tr><th>`tab_width`</th>
<th> Displayed tab width in file preview (*since 0.9*) 
</th></tr></table>

## \[ticket\]

<table><tr><th>`default_version`</th>
<th> Default version for newly created tickets 
</th></tr>
<tr><th>`default_severity`</th>
<th> Default severity for newly created tickets 
</th></tr>
<tr><th>`default_priority`</th>
<th> Default priority for newly created tickets 
</th></tr>
<tr><th>`default_milestone`</th>
<th> Default milestone for newly created tickets 
</th></tr>
<tr><th>`default_component`</th>
<th> Default component for newly created tickets 
</th></tr>
<tr><th>`restrict_owner`</th>
<th> Make the owner field of tickets use a drop-down menu (*since 0.9*) 
</th></tr></table>

## \[ticket-custom\]


Creates [user-defined ticket fields](trac-tickets-custom-fields).

## \[timeline\]

<table><tr><th>`default_daysback`</th>
<th> Default "depth" of the Timeline, in days (*since 0.9*) 
</th></tr>
<tr><th>`changeset_show_files`</th>
<th> Number of files to show (-1 for unlimited, 0 to disable) 
</th></tr>
<tr><th>`ticket_show_details`</th>
<th> Enable the display of all ticket changes in the timeline 
</th></tr></table>

## \[browser\]

<table><tr><th>`hide_properties`</th>
<th> List of subversion properties to hide from the repository browser (*since 0.9*) 
</th></tr></table>

## \[wiki\]

<table><tr><th>`ignore_missing_pages`</th>
<th> enable/disable highlighting [CamelCase](camel-case) links to missing pages (*since 0.9*) 
</th></tr></table>

## \[darcs\]

<table><tr><th>`cachedir`</th>
<th> By default the darcs backend keeps a cache of the visited files at various revision inside the repository itself, in `_darcs/trac_cache`, that may be overridden by this option, setting it to the desired directory that needs to be writeable by the trac process. 
</th></tr>
<tr><th>`command`</th>
<th> Name of the external darcs executable, default to `darcs`. This can be used to set up the environment as well, like in "`DARCS_DONT_ESCAPE_ANYTHING=1 /usr/local/bin/darcs`" 
</th></tr>
<tr><th>`dont_escape_8bit`</th>
<th> This is a shortcut for "`command=DARCS_DONT_ESCAPE_8BIT=1 darcs`" (true, false). Default to false. 
</th></tr></table>

## \[components\]


(*since 0.9*)


This section is used to enable or disable components provided by plugins, as well as by Trac itself. The component to enable/disable is specified via the name of the option. Whether its enabled is determined by the option value; setting the value to `enabled` or `on` will enable the component, any other value (typically `disabled` or `off`) will disable the component.


The option name is either the fully qualified name of the components or the module/package prefix of the component. The former enables/disables a specific component, while the latter enables/disables any component in the specified package/module.


Consider the following configuration snippet:

```wiki
[components]
trac.ticket.report.ReportModule = disabled
webadmin.* = enabled
```


The first option tells Trac to disable the [report module](trac-reports). The second option instructs Trac to enable all components in the `webadmin` package. Note that the trailing wildcard is required for module/package matching.


See the *Plugins* page on *About Trac* to get the list of active components (requires `CONFIG_VIEW`[permissions](trac-permissions).)


See also: [TracPlugins](trac-plugins)

> *Note that prior to Trac [r2335](/trac/ghc/changeset/2335/ghc) (that applies to 0.9b1 and 0.9b2), you would use a `[disabled_components]` section instead. See a [ previous version](http://projects.edgewall.com/trac/wiki/TracIni?version=42) of this page for the details.*

---


See also: [TracGuide](trac-guide), [TracAdmin](trac-admin), [TracEnvironment](trac-environment)