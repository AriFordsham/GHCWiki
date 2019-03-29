# The Trac Configuration File


Trac is configured through the **`trac.ini`** file, located in the `<projectenv>/conf` directory. The `trac.ini` configuration file and its parent directory should be writable by the web server.


Trac monitors the timestamp of the file to trigger a complete environment reload and flush its caches when the timestamp changes. Most changes to the configuration will be reflected immediately, though changes to the `[components]` or `[logging]` sections will require restarting the web server. You may also need to restart the web server after creating a [global configuration](trac-ini#global-configuration) file when none was previously present.

## Global Configuration



Configuration can be shared among environments using one or more global configuration files. Options in the global configuration will be merged with the environment-specific options, with local options overriding global options. The global configuration file is specified as follows:


```
[inherit]
file = /path/to/global/trac.ini
```


Multiple files can be specified using a comma-separated list.


Note that you can also specify a global option file when creating a new project, by adding the option `--inherit=/path/to/global/trac.ini` to [trac-admin](trac-admin#)'s `initenv` command. If you do not do this but nevertheless intend to use a global option file with your new environment, you will have to go through the newly generated `conf/trac.ini` file and delete the entries that will otherwise override those set in the global file.


There are two more entries in the [ \[inherit\] ](trac-ini#) section, `templates_dir` for sharing global templates and `plugins_dir`, for sharing plugins. Those entries can themselves be specified in the shared configuration file, and in fact, configuration files can even be chained if you specify another `[inherit] file` there.


Note that the templates found in the `templates/` directory of the [TracEnvironment](trac-environment) have precedence over those found in `[inherit] templates_dir`. In turn, the latter have precedence over the installed templates, so be careful about what you put there. Notably, if you override a default template, refresh your modifications when you upgrade to a new version of Trac. The preferred way to perform [TracInterfaceCustomization](trac-interface-customization) is still to write a custom plugin doing an appropriate `ITemplateStreamFilter` transformation.

## Reference for settings


This is a brief reference of available configuration options, and their default settings.



Documentation improvements should be discussed on the [ trac-dev mailing list](http://trac.edgewall.org/intertrac/MailingList%23Trac-dev) or described in a [ ticket](http://trac.edgewall.org/intertrac/NewTicket). Even better, [ submit a patch](http://trac.edgewall.org/intertrac/TracDev/SubmittingPatches) against the docstrings in the code.



### `[account-manager]`

<table><tr><th><a href="#account-manager-account_changes_notify_addresses-option">account_changes_notify_addresses</a></th>
<th>
List of email addresses that get notified of user changes, ie,
new user, password change and delete user.


</th>
<th>(no default)</th></tr>
<tr><th><a href="#account-manager-allow_delete_account-option">allow_delete_account</a></th>
<th>
Allow users to delete their own account.


</th>
<th><tt>enabled</tt></th></tr>
<tr><th><a href="#account-manager-auth_init-option">auth_init</a></th>
<th>
Launch an initial Trac authentication setup.


</th>
<th><tt>enabled</tt></th></tr>
<tr><th><a href="#account-manager-cookie_refresh_pct-option">cookie_refresh_pct</a></th>
<th>
Persistent sessions randomly get a new session cookie ID with
likelihood in percent per work hour given here (zero equals to never)
to decrease vulnerability of long-lasting sessions.


</th>
<th><tt>10</tt></th></tr>
<tr><th><a href="#account-manager-db_htdigest_realm-option">db_htdigest_realm</a></th>
<th>
Realm to select relevant htdigest db entries


</th>
<th>(no default)</th></tr>
<tr><th><a href="#account-manager-db_htpasswd_hash_type-option">db_htpasswd_hash_type</a></th>
<th>
Default hash type of new/updated passwords


</th>
<th><tt>crypt</tt></th></tr>
<tr><th><a href="#account-manager-email_regexp-option">email_regexp</a></th>
<th>
A validation regular expression describing new account emails.
Define constraints for a valid email address. A custom pattern can
narrow or widen scope i.e. to accept UTF-8 characters.


</th>
<th><tt>(?i)^[A-Z0-9._%+-]+@(?:[A-Z0-9-]+\.)+[A-Z0-9-]{2,63}$</tt></th></tr>
<tr><th><a href="#account-manager-environ_auth_overwrite-option">environ_auth_overwrite</a></th>
<th>
Whether environment variable REMOTE_USER should get overwritten
after processing login form input. Otherwise it will only be set,
if unset at the time of authentication.


</th>
<th><tt>enabled</tt></th></tr>
<tr><th><a href="#account-manager-force_passwd_change-option">force_passwd_change</a></th>
<th>
Force the user to change password when it&apos;s reset.


</th>
<th><tt>enabled</tt></th></tr>
<tr><th><a href="#account-manager-generated_password_length-option">generated_password_length</a></th>
<th>
Length of the randomly-generated passwords created when resetting
the password for an account.


</th>
<th><tt>8</tt></th></tr>
<tr><th><a href="#account-manager-hash_method-option">hash_method</a></th>
<th>
IPasswordHashMethod used to create new/updated passwords


</th>
<th><tt>HtDigestHashMethod</tt></th></tr>
<tr><th><a href="#account-manager-login_attempt_max_count-option">login_attempt_max_count</a></th>
<th>
Lock user account after specified number of login attempts.
Value zero means no limit.


</th>
<th><tt>0</tt></th></tr>
<tr><th><a href="#account-manager-login_opt_list-option">login_opt_list</a></th>
<th>
Set to True, to switch login page style showing alternative actions
in a single listing together.


</th>
<th><tt>disabled</tt></th></tr>
<tr><th><a href="#account-manager-notify_actions-option">notify_actions</a></th>
<th>
Comma separated list of actions to notify of.
Available actions &apos;new&apos;, &apos;change&apos;, &apos;delete&apos;.


</th>
<th>(no default)</th></tr>
<tr><th><a href="#account-manager-password_store-option">password_store</a></th>
<th>
Ordered list of password stores, queried in turn.


</th>
<th>(no default)</th></tr>
<tr><th><a href="#account-manager-persistent_sessions-option">persistent_sessions</a></th>
<th>
Allow the user to be remembered across sessions without
needing to re-authenticate. This is, user checks a
&quot;Remember Me&quot; checkbox and, next time he visits the site,
he&apos;ll be remembered.


</th>
<th><tt>disabled</tt></th></tr>
<tr><th><a href="#account-manager-refresh_passwd-option">refresh_passwd</a></th>
<th>
Re-set passwords on successful authentication.
This is most useful to move users to a new password store or
enforce new store configuration (i.e. changed hash type),
but should be disabled/unset otherwise.


</th>
<th><tt>disabled</tt></th></tr>
<tr><th><a href="#account-manager-register_basic_question-option">register_basic_question</a></th>
<th>
A question to ask instead of the standard prompt, to which the value of register_basic_token is the answer. Setting to empty string (default value) keeps the standard prompt.


</th>
<th>(no default)</th></tr>
<tr><th><a href="#account-manager-register_basic_token-option">register_basic_token</a></th>
<th>
A string required as input to pass verification.


</th>
<th>(no default)</th></tr>
<tr><th><a href="#account-manager-register_check-option">register_check</a></th>
<th>
Ordered list of IAccountRegistrationInspector&apos;s to use for
registration checks.


</th>
<th><tt>BasicCheck,EmailCheck,BotTrapCheck,RegExpCheck,UsernamePermCheck</tt></th></tr>
<tr><th><a href="#account-manager-require_approval-option">require_approval</a></th>
<th>
Whether account registration requires administrative approval to
enable the account or not.


</th>
<th><tt>disabled</tt></th></tr>
<tr><th><a href="#account-manager-reset_password-option">reset_password</a></th>
<th>
Set to False, if there is no email system setup.


</th>
<th><tt>enabled</tt></th></tr>
<tr><th><a href="#account-manager-user_lock_max_time-option">user_lock_max_time</a></th>
<th>
Limit user account lock time to specified time (seconds).
This is relevant only with user_lock_time_progression &gt; 1.


</th>
<th><tt>86400</tt></th></tr>
<tr><th><a href="#account-manager-user_lock_time-option">user_lock_time</a></th>
<th>
Drop user account lock after specified time (seconds).
Value zero means unlimited lock time.


</th>
<th><tt>0</tt></th></tr>
<tr><th><a href="#account-manager-user_lock_time_progression-option">user_lock_time_progression</a></th>
<th>
Extend user account lock time incrementally. This is
based on logarithmic calculation and decimal numbers accepted:
Value &apos;1&apos; means constant lock time per failed login attempt.
Value &apos;2&apos; means double locktime after 2nd lock activation,
four times the initial locktime after 3rd, and so on.


</th>
<th><tt>1</tt></th></tr>
<tr><th><a href="#account-manager-username_char_blacklist-option">username_char_blacklist</a></th>
<th>
Always exclude some special characters from usernames.
This is enforced upon new user registration.


</th>
<th><tt>:[]</tt></th></tr>
<tr><th><a href="#account-manager-username_regexp-option">username_regexp</a></th>
<th>
A validation regular expression describing new usernames. Define
constraints for allowed user names corresponding to local naming
policy.


</th>
<th><tt>(?i)^[A-Z0-9.\-_]{5,}$</tt></th></tr>
<tr><th><a href="#account-manager-verify_email-option">verify_email</a></th>
<th>
Verify the email address of Trac users.


</th>
<th><tt>enabled</tt></th></tr></table>

### `[attachment]`

<table><tr><th><a href="#attachment-max_size-option">max_size</a></th>
<th>
Maximum allowed file size (in bytes) for attachments.


</th>
<th><tt>262144</tt></th></tr>
<tr><th><a href="#attachment-max_zip_size-option">max_zip_size</a></th>
<th>
Maximum allowed total size (in bytes) for an attachment list to be
downloadable as a <tt>.zip</tt>. Set this to -1 to disable download as <tt>.zip</tt>.
(<i>since 1.0</i>)


</th>
<th><tt>2097152</tt></th></tr>
<tr><th><a href="#attachment-render_unsafe_content-option">render_unsafe_content</a></th>
<th>
Whether attachments should be rendered in the browser, or
only made downloadable.


Pretty much any file may be interpreted as HTML by the browser,
which allows a malicious user to attach a file containing cross-site
scripting attacks.


For public sites where anonymous users can create attachments it is
recommended to leave this option disabled.


</th>
<th><tt>disabled</tt></th></tr></table>

### `[browser]`

<table><tr><th><a href="#browser-color_scale-option">color_scale</a></th>
<th>
Enable colorization of the <i>age</i> column.



This uses the same color scale as the source code annotation:
blue is older, red is newer.


</th>
<th><tt>enabled</tt></th></tr>
<tr><th><a href="#browser-downloadable_paths-option">downloadable_paths</a></th>
<th>
List of repository paths that can be downloaded.



Leave this option empty if you want to disable all downloads, otherwise
set it to a comma-separated list of authorized paths (those paths are
glob patterns, i.e. &quot;*&quot; can be used as a wild card). In a
multi-repository environment, the path must be qualified with the
repository name if the path does not point to the default repository
(e.g. /reponame/trunk). Note that a simple prefix matching is
performed on the paths, so aliases won&apos;t get automatically resolved.


</th>
<th><tt>/trunk,/branches/*,/tags/*</tt></th></tr>
<tr><th><a href="#browser-hide_properties-option">hide_properties</a></th>
<th>
Comma-separated list of version control properties to hide from
the repository browser.


</th>
<th><tt>svk:merge</tt></th></tr>
<tr><th><a href="#browser-intermediate_color-option">intermediate_color</a></th>
<th>
(r,g,b) color triple to use for the color corresponding
to the intermediate color, if two linear interpolations are used
for the color scale (see <tt>intermediate_point</tt>).
If not set, the intermediate color between <tt>oldest_color</tt> and
<tt>newest_color</tt> will be used.


</th>
<th>(no default)</th></tr>
<tr><th><a href="#browser-intermediate_point-option">intermediate_point</a></th>
<th>
If set to a value between 0 and 1 (exclusive), this will be the
point chosen to set the <tt>intermediate_color</tt> for interpolating
the color value.


</th>
<th>(no default)</th></tr>
<tr><th><a href="#browser-newest_color-option">newest_color</a></th>
<th>
(r,g,b) color triple to use for the color corresponding
to the newest color, for the color scale used in <i>blame</i> or
the browser <i>age</i> column if <tt>color_scale</tt> is enabled.


</th>
<th><tt>(255, 136, 136)</tt></th></tr>
<tr><th><a href="#browser-oldest_color-option">oldest_color</a></th>
<th>
(r,g,b) color triple to use for the color corresponding
to the oldest color, for the color scale used in <i>blame</i> or
the browser <i>age</i> column if <tt>color_scale</tt> is enabled.


</th>
<th><tt>(136, 136, 255)</tt></th></tr>
<tr><th><a href="#browser-oneliner_properties-option">oneliner_properties</a></th>
<th>
Comma-separated list of version control properties to render
as oneliner wiki content in the repository browser.


</th>
<th><tt>trac:summary</tt></th></tr>
<tr><th><a href="#browser-render_unsafe_content-option">render_unsafe_content</a></th>
<th>
Whether raw files should be rendered in the browser, or only made
downloadable.


Pretty much any file may be interpreted as HTML by the browser,
which allows a malicious user to create a file containing cross-site
scripting attacks.


For open repositories where anyone can check-in a file, it is
recommended to leave this option disabled.


</th>
<th><tt>disabled</tt></th></tr>
<tr><th><a href="#browser-wiki_properties-option">wiki_properties</a></th>
<th>
Comma-separated list of version control properties to render
as wiki content in the repository browser.


</th>
<th><tt>trac:description</tt></th></tr></table>

### `[changeset]`

<table><tr><th><a href="#changeset-max_diff_bytes-option">max_diff_bytes</a></th>
<th>
Maximum total size in bytes of the modified files (their old size
plus their new size) for which the changeset view will attempt to show
the diffs inlined.


</th>
<th><tt>10000000</tt></th></tr>
<tr><th><a href="#changeset-max_diff_files-option">max_diff_files</a></th>
<th>
Maximum number of modified files for which the changeset view will
attempt to show the diffs inlined.


</th>
<th><tt>0</tt></th></tr>
<tr><th><a href="#changeset-wiki_format_messages-option">wiki_format_messages</a></th>
<th>
Whether wiki formatting should be applied to changeset messages.


If this option is disabled, changeset messages will be rendered as
pre-formatted text.


</th>
<th><tt>enabled</tt></th></tr></table>

### `[components]`


This section is used to enable or disable components
provided by plugins, as well as by Trac itself. The component
to enable/disable is specified via the name of the
option. Whether its enabled is determined by the option value;
setting the value to `enabled` or `on` will enable the
component, any other value (typically `disabled` or `off`)
will disable the component.


The option name is either the fully qualified name of the
components or the module/package prefix of the component. The
former enables/disables a specific component, while the latter
enables/disables any component in the specified
package/module.


Consider the following configuration snippet:

```wiki
[components]
trac.ticket.report.ReportModule = disabled
acct_mgr.* = enabled
```


The first option tells Trac to disable the
[report module](trac-reports).
The second option instructs Trac to enable all components in
the `acct_mgr` package. Note that the trailing wildcard is
required for module/package matching.



To view the list of active components, go to the *Plugins*
page on *About Trac* (requires `CONFIG_VIEW`
[permissions](trac-permissions)).



See also: [TracPlugins](trac-plugins)


### `[fullblog]`

<table><tr><th><a href="#fullblog-all_rss_icons-option">all_rss_icons</a></th>
<th>
Controls whether or not to display rss icons more than once


</th>
<th><tt>disabled</tt></th></tr>
<tr><th><a href="#fullblog-archive_rss_icon-option">archive_rss_icon</a></th>
<th>
Controls whether or not to display the rss icon


</th>
<th><tt>enabled</tt></th></tr>
<tr><th><a href="#fullblog-default_postname-option">default_postname</a></th>
<th>
Option for a default naming scheme for new posts. The string
can include substitution markers for time (UTC) and user: %Y=year,
%m=month, %d=day, %H=hour, %M=minute, %S=second, $USER.
Example template string: <tt>%Y/%m/%d/my_topic</tt>


</th>
<th>(no default)</th></tr>
<tr><th><a href="#fullblog-month_names-option">month_names</a></th>
<th>
Ability to specify a list of month names for display in groupings.
If empty it will make a list from default locale setting.
Enter list of 12 months like:
<tt>month_names = January, February, ..., December</tt>


</th>
<th>(no default)</th></tr>
<tr><th><a href="#fullblog-num_items_front-option">num_items_front</a></th>
<th>
Option to specify how many recent posts to display on the
front page of the Blog (and RSS feeds).


</th>
<th><tt>20</tt></th></tr>
<tr><th><a href="#fullblog-personal_blog-option">personal_blog</a></th>
<th>
When using the Blog as a personal blog (only one author), setting to &apos;True&apos;
will disable the display of &apos;Browse by author:&apos; in sidebar, and also removes
various author links and references.


</th>
<th><tt>disabled</tt></th></tr></table>

### `[git]`

<table><tr><th><a href="#git-cached_repository-option">cached_repository</a></th>
<th>
Wrap <tt>GitRepository</tt> in <tt>CachedRepository</tt>.


</th>
<th><tt>disabled</tt></th></tr>
<tr><th><a href="#git-git_bin-option">git_bin</a></th>
<th>
Path to the git executable.


</th>
<th><tt>git</tt></th></tr>
<tr><th><a href="#git-git_fs_encoding-option">git_fs_encoding</a></th>
<th>
Define charset encoding of paths within git repositories.


</th>
<th><tt>utf-8</tt></th></tr>
<tr><th><a href="#git-persistent_cache-option">persistent_cache</a></th>
<th>
Enable persistent caching of commit tree.


</th>
<th><tt>disabled</tt></th></tr>
<tr><th><a href="#git-shortrev_len-option">shortrev_len</a></th>
<th>
The length at which a sha1 should be abbreviated to (must
be &gt;= 4 and &lt;= 40).


</th>
<th><tt>7</tt></th></tr>
<tr><th><a href="#git-trac_user_rlookup-option">trac_user_rlookup</a></th>
<th>
Enable reverse mapping of git email addresses to trac user ids.
Performance will be reduced if there are many users and the
<tt>cached_repository</tt> option is <tt>disabled</tt>.



A repository resync is required after changing the value of this
option.


</th>
<th><tt>disabled</tt></th></tr>
<tr><th><a href="#git-use_committer_id-option">use_committer_id</a></th>
<th>
Use git-committer id instead of git-author id for the
changeset <i>Author</i> field.


</th>
<th><tt>enabled</tt></th></tr>
<tr><th><a href="#git-use_committer_time-option">use_committer_time</a></th>
<th>
Use git-committer timestamp instead of git-author timestamp
for the changeset <i>Timestamp</i> field.


</th>
<th><tt>enabled</tt></th></tr>
<tr><th><a href="#git-wikishortrev_len-option">wikishortrev_len</a></th>
<th>
The minimum length of an hex-string for which
auto-detection as sha1 is performed (must be &gt;= 4 and &lt;= 40).


</th>
<th><tt>40</tt></th></tr></table>

### `[gitweb-repositories]`

<table><tr><th><a href="#gitweb-repositories-projects_base-option">projects_base</a></th>
<th>
Path to the base of your git projects


</th>
<th>(no default)</th></tr>
<tr><th><a href="#gitweb-repositories-projects_list-option">projects_list</a></th>
<th>
Path to a gitweb-formatted projects.list


</th>
<th>(no default)</th></tr>
<tr><th><a href="#gitweb-repositories-projects_url-option">projects_url</a></th>
<th>
Template for project URLs. <tt>%s</tt> will be replaced with the repo
name


</th>
<th>(no default)</th></tr>
<tr><th><a href="#gitweb-repositories-sync_per_request-option">sync_per_request</a></th>
<th>
Repositories to sync on every request
(not recommended).

</th>
<th>(no default)</th></tr></table>

### `[header_logo]`

<table><tr><th><a href="#header_logo-alt-option">alt</a></th>
<th>
Alternative text for the header logo.


</th>
<th><tt>(please configure the [header_logo] section in trac.ini)</tt></th></tr>
<tr><th><a href="#header_logo-height-option">height</a></th>
<th>
Height of the header logo image in pixels.


</th>
<th><tt>-1</tt></th></tr>
<tr><th><a href="#header_logo-link-option">link</a></th>
<th>
URL to link to, from the header logo.


</th>
<th>(no default)</th></tr>
<tr><th><a href="#header_logo-src-option">src</a></th>
<th>
URL of the image to use as header logo.
It can be absolute, server relative or relative.



If relative, it is relative to one of the <tt>/chrome</tt> locations:
<tt>site/your-logo.png</tt> if <tt>your-logo.png</tt> is located in the <tt>htdocs</tt>
folder within your <a href="trac-environment">TracEnvironment</a>;
<tt>common/your-logo.png</tt> if <tt>your-logo.png</tt> is located in the
folder mapped to the <a href="trac-ini#">htdocs_location</a> URL.
Only specifying <tt>your-logo.png</tt> is equivalent to the latter.


</th>
<th><tt>site/your_project_logo.png</tt></th></tr>
<tr><th><a href="#header_logo-width-option">width</a></th>
<th>
Width of the header logo image in pixels.


</th>
<th><tt>-1</tt></th></tr></table>

### `[httpauth]`

<table><tr><th><a href="#httpauth-formats-option">formats</a></th>
<th>
Request formats to force HTTP authentication on


</th>
<th>(no default)</th></tr>
<tr><th><a href="#httpauth-paths-option">paths</a></th>
<th>
Paths to force HTTP authentication on.


</th>
<th><tt>/login/xmlrpc</tt></th></tr></table>

### `[inherit]`

<table><tr><th><a href="#inherit-htdocs_dir-option">htdocs_dir</a></th>
<th>
Path to the <i>shared htdocs directory</i>.



Static resources in that directory are mapped to /chrome/shared
under the environment URL, in addition to common and site locations.


This can be useful in site.html for common interface customization
of multiple Trac environments.



(<i>since 1.0</i>)


</th>
<th>(no default)</th></tr>
<tr><th><a href="#inherit-plugins_dir-option">plugins_dir</a></th>
<th>
Path to the <i>shared plugins directory</i>.



Plugins in that directory are loaded in addition to those in
the directory of the environment <tt>plugins</tt>, with this one
taking precedence.


</th>
<th>(no default)</th></tr>
<tr><th><a href="#inherit-templates_dir-option">templates_dir</a></th>
<th>
Path to the <i>shared templates directory</i>.



Templates in that directory are loaded in addition to those in the
environments <tt>templates</tt> directory, but the latter take precedence.


</th>
<th>(no default)</th></tr></table>

### `[intertrac]`


This section configures [InterTrac](inter-trac) prefixes. Options in this section
whose name contain a `.` define aspects of the [InterTrac](inter-trac) prefix
corresponding to the option name up to the `.`. Options whose name
don't contain a `.` define an alias.


The `.url` is mandatory and is used for locating the other Trac.
This can be a relative URL in case that Trac environment is located
on the same server.


The `.title` information is used for providing a useful tooltip when
moving the cursor over an [InterTrac](inter-trac) link.


Example configuration:

```wiki
[intertrac]
# -- Example of setting up an alias:
t = trac

# -- Link to an external Trac:
trac.title = Edgewall's Trac for Trac
trac.url = http://trac.edgewall.org
```

### `[interwiki]`


Every option in the `[interwiki]` section defines one [InterWiki](inter-wiki)
prefix. The option name defines the prefix. The option value defines
the URL, optionally followed by a description separated from the URL
by whitespace. Parametric URLs are supported as well.

**Example:**

```wiki
[interwiki]
MeatBall = http://www.usemod.com/cgi-bin/mb.pl?
PEP = http://www.python.org/peps/pep-$1.html Python Enhancement Proposal $1
tsvn = tsvn: Interact with TortoiseSvn
```

### `[logging]`

<table><tr><th><a href="#logging-log_file-option">log_file</a></th>
<th>
If <tt>log_type</tt> is <tt>file</tt>, this should be a path to the
log-file.  Relative paths are resolved relative to the <tt>log</tt>
directory of the environment.


</th>
<th><tt>trac.log</tt></th></tr>
<tr><th><a href="#logging-log_format-option">log_format</a></th>
<th>
Custom logging format.



If nothing is set, the following will be used:



<tt>Trac[$(module)s] $(levelname)s: $(message)s</tt>



In addition to regular key names supported by the
<a href="http://docs.python.org/library/logging.html"> Python logger library</a>
one could use:


- <tt>$(path)s</tt>     the path for the current environment
- <tt>$(basename)s</tt> the last path component of the current environment
- <tt>$(project)s</tt>  the project name


Note the usage of <tt>$(...)s</tt> instead of <tt>%(...)s</tt> as the latter form
would be interpreted by the ConfigParser itself.



Example:
<tt>($(thread)d) Trac[$(basename)s:$(module)s] $(levelname)s: $(message)s</tt>


</th>
<th>(no default)</th></tr>
<tr><th><a href="#logging-log_level-option">log_level</a></th>
<th>
Level of verbosity in log.



Should be one of (<tt>CRITICAL</tt>, <tt>ERROR</tt>, <tt>WARNING</tt>, <tt>INFO</tt>, <tt>DEBUG</tt>).


</th>
<th><tt>DEBUG</tt></th></tr>
<tr><th><a href="#logging-log_type-option">log_type</a></th>
<th>
Logging facility to use.



Should be one of (<tt>none</tt>, <tt>file</tt>, <tt>stderr</tt>, <tt>syslog</tt>, <tt>winlog</tt>).


</th>
<th><tt>none</tt></th></tr></table>

### `[mainnav]`


Configures the main navigation bar,
which by default contains *Wiki*, *Timeline*, *Roadmap*,
*Browse Source*, *View Tickets*, *New Ticket*, *Search*  and
*Admin*.


The `label`, `href`, and `order`  attributes can be specified. Entries
can be disabled by setting the value of the navigation item to
`disabled`.


The following example renames the link to [WikiStart](wiki-start) to *Home*,
links the *View Tickets* entry to a specific report and disables
the *Search* entry.


```
[mainnav]
wiki.label = Home
tickets.href = /report/24
search = disabled
```


See [TracNavigation](trac-navigation) for more details.


### `[mastertickets]`

<table><tr><th><a href="#mastertickets-acceptable_formats-option">acceptable_formats</a></th>
<th>
The formats that may be chosen; execute dot -T? for a
list of options.


</th>
<th><tt>png,cmapx</tt></th></tr>
<tr><th><a href="#mastertickets-closed_color-option">closed_color</a></th>
<th>
Color of closed tickets


</th>
<th><tt>green</tt></th></tr>
<tr><th><a href="#mastertickets-closed_text-option">closed_text</a></th>
<th>
Text for key showing closed tickets


</th>
<th><tt>Done</tt></th></tr>
<tr><th><a href="#mastertickets-dot_path-option">dot_path</a></th>
<th>
Path to the dot executable.


</th>
<th><tt>dot</tt></th></tr>
<tr><th><a href="#mastertickets-full_graph-option">full_graph</a></th>
<th>
Show full dep. graph, not just direct blocking links


</th>
<th><tt>disabled</tt></th></tr>
<tr><th><a href="#mastertickets-graph_direction-option">graph_direction</a></th>
<th>
Direction of the dependency graph (TD = Top Down,
DT = Down Top, LR = Left Right, RL = Right Left).


</th>
<th><tt>TD</tt></th></tr>
<tr><th><a href="#mastertickets-gs_path-option">gs_path</a></th>
<th>
Path to the ghostscript executable.


</th>
<th><tt>gs</tt></th></tr>
<tr><th><a href="#mastertickets-highlight_target-option">highlight_target</a></th>
<th>
Highlight target tickets in graph


</th>
<th><tt>disabled</tt></th></tr>
<tr><th><a href="#mastertickets-opened_color-option">opened_color</a></th>
<th>
Color of opened tickets


</th>
<th><tt>red</tt></th></tr>
<tr><th><a href="#mastertickets-opened_text-option">opened_text</a></th>
<th>
Text for key showing opened tickets


</th>
<th><tt>ToDo</tt></th></tr>
<tr><th><a href="#mastertickets-show_key-option">show_key</a></th>
<th>
Show a key for open/closed nodes


</th>
<th><tt>disabled</tt></th></tr>
<tr><th><a href="#mastertickets-use_gs-option">use_gs</a></th>
<th>
If enabled, use ghostscript to produce nicer output.


</th>
<th><tt>disabled</tt></th></tr></table>

### `[metanav]`


Configures the meta navigation
entries, which by default are *Login*, *Logout*, *Preferences*,
*Help/Guide* and *About Trac*. The allowed attributes are the
same as for `[mainnav]`. Additionally, a special entry is supported -
`logout.redirect` is the page the user sees after hitting the logout
button. For example:


```
[metanav]
logout.redirect = wiki/Logout
```


See [TracNavigation](trac-navigation) for more details.


### `[milestone]`

<table><tr><th><a href="#milestone-default_group_by-option">default_group_by</a></th>
<th>
Default field to use for grouping tickets in the grouped
progress bar. (<i>since 1.2</i>)


</th>
<th><tt>component</tt></th></tr>
<tr><th><a href="#milestone-default_retarget_to-option">default_retarget_to</a></th>
<th>
Default milestone to which tickets are retargeted when
closing or deleting a milestone. (<i>since 1.1.2</i>)


</th>
<th>(no default)</th></tr>
<tr><th><a href="#milestone-stats_provider-option">stats_provider</a></th>
<th>
Name of the component implementing <tt>ITicketGroupStatsProvider</tt>,
which is used to collect statistics on groups of tickets for display
in the milestone views.


</th>
<th><tt>DefaultTicketGroupStatsProvider</tt></th></tr></table>

### `[milestone-groups]`


As the workflow for tickets is now configurable, there can
be many ticket states, and simply displaying closed tickets
vs. all the others is maybe not appropriate in all cases. This
section enables one to easily create *groups* of states that
will be shown in different colors in the milestone progress
bar.


Note that the groups can only be based on the ticket
*status*, nothing else. In particular, it's not possible to
distinguish between different closed tickets based on the
*resolution*.


Example configuration with three groups, *closed*, *new*
and *active* (the default only has closed and active):

```wiki
# the 'closed' group correspond to the 'closed' tickets
closed = closed

# .order: sequence number in the progress bar
closed.order = 0

# .query_args: optional parameters for the corresponding
#              query.  In this example, the changes from the
#              default are two additional columns ('created' and
#              'modified'), and sorting is done on 'created'.
closed.query_args = group=resolution,order=time,col=id,col=summary,col=owner,col=type,col=priority,col=component,col=severity,col=time,col=changetime

# .overall_completion: indicates groups that count for overall
#                      completion percentage
closed.overall_completion = true

new = new
new.order = 1
new.css_class = new
new.label = new

# Note: one catch-all group for other statuses is allowed
active = *
active.order = 2

# .css_class: CSS class for this interval
active.css_class = open

# .label: displayed label for this group
active.label = in progress
```


The definition consists in a comma-separated list of accepted
status.  Also, '\*' means any status and could be used to
associate all remaining states to one catch-all group.


The CSS class can be one of: new (yellow), open (no color) or
closed (green). Other styles can easily be added using custom
CSS rule: `table.progress td.<class> { background: <color> }`
to a [site/style.css](trac-interface-customization#site-appearance) file
for example.


### `[mimeviewer]`

<table><tr><th><a href="#mimeviewer-max_preview_size-option">max_preview_size</a></th>
<th>
Maximum file size for HTML preview.


</th>
<th><tt>262144</tt></th></tr>
<tr><th><a href="#mimeviewer-mime_map-option">mime_map</a></th>
<th>
List of additional MIME types and keyword mappings.
Mappings are comma-separated, and for each MIME type,
there&apos;s a colon (&quot;:&quot;) separated list of associated keywords
or file extensions.


</th>
<th><tt>text/x-dylan:dylan,text/x-idl:ice,text/x-ada:ads:adb</tt></th></tr>
<tr><th><a href="#mimeviewer-mime_map_patterns-option">mime_map_patterns</a></th>
<th>
List of additional MIME types associated to filename patterns.
Mappings are comma-separated, and each mapping consists of a MIME type
and a Python regexp used for matching filenames, separated by a colon
(&quot;:&quot;). (<i>since 1.0</i>)


</th>
<th><tt>text/plain:README(?!\.rst)|INSTALL(?!\.rst)|COPYING.*</tt></th></tr>
<tr><th><a href="#mimeviewer-pygments_default_style-option">pygments_default_style</a></th>
<th>
The default style to use for Pygments syntax highlighting.


</th>
<th><tt>trac</tt></th></tr>
<tr><th><a href="#mimeviewer-pygments_modes-option">pygments_modes</a></th>
<th>
List of additional MIME types known by Pygments.



For each, a tuple <tt>mimetype:mode:quality</tt> has to be
specified, where <tt>mimetype</tt> is the MIME type,
<tt>mode</tt> is the corresponding Pygments mode to be used
for the conversion and <tt>quality</tt> is the quality ratio
associated to this conversion. That can also be used
to override the default quality ratio used by the
Pygments render.


</th>
<th>(no default)</th></tr>
<tr><th><a href="#mimeviewer-tab_width-option">tab_width</a></th>
<th>
Displayed tab width in file preview.


</th>
<th><tt>8</tt></th></tr>
<tr><th><a href="#mimeviewer-treat_as_binary-option">treat_as_binary</a></th>
<th>
Comma-separated list of MIME types that should be treated as
binary data.


</th>
<th><tt>application/octet-stream,application/pdf,application/postscript,application/msword,application/rtf</tt></th></tr></table>

### `[notification]`

<table><tr><th><a href="#notification-admit_domains-option">admit_domains</a></th>
<th>
Comma-separated list of domains that should be considered as
valid for email addresses (such as localdomain).


</th>
<th>(no default)</th></tr>
<tr><th><a href="#notification-ambiguous_char_width-option">ambiguous_char_width</a></th>
<th>
Width of ambiguous characters that should be used in the table
of the notification mail.



If <tt>single</tt>, the same width as characters in US-ASCII. This is
expected by most users. If <tt>double</tt>, twice the width of
US-ASCII characters.  This is expected by CJK users. (<i>since
0.12.2</i>)


</th>
<th><tt>single</tt></th></tr>
<tr><th><a href="#notification-batch_subject_template-option">batch_subject_template</a></th>
<th>
Like <tt>ticket_subject_template</tt> but for batch modifications.
(<i>since 1.0</i>)


</th>
<th><tt>${prefix} Batch modify: ${tickets_descr}</tt></th></tr>
<tr><th><a href="#notification-default_format.email-option">default_format.email</a></th>
<th>
Default format to distribute email notifications.


</th>
<th><tt>text/plain</tt></th></tr>
<tr><th><a href="#notification-email_address_resolvers-option">email_address_resolvers</a></th>
<th>
Comma separated list of email resolver components in the order
they will be called.  If an email address is resolved, the remaining
resolvers will not be called.


</th>
<th><tt>SessionEmailResolver</tt></th></tr>
<tr><th><a href="#notification-email_sender-option">email_sender</a></th>
<th>
Name of the component implementing <tt>IEmailSender</tt>.



This component is used by the notification system to send emails.
Trac currently provides <tt>SmtpEmailSender</tt> for connecting to an SMTP
server, and <tt>SendmailEmailSender</tt> for running a <tt>sendmail</tt>-compatible
executable. (<i>since 0.12</i>)


</th>
<th><tt>SmtpEmailSender</tt></th></tr>
<tr><th><a href="#notification-ignore_domains-option">ignore_domains</a></th>
<th>
Comma-separated list of domains that should not be considered
part of email addresses (for usernames with Kerberos domains).


</th>
<th>(no default)</th></tr>
<tr><th><a href="#notification-message_id_hash-option">message_id_hash</a></th>
<th>
Hash algorithm to create unique Message-ID header.
<i>(since 1.0.13)</i>


</th>
<th><tt>md5</tt></th></tr>
<tr><th><a href="#notification-mime_encoding-option">mime_encoding</a></th>
<th>
Specifies the MIME encoding scheme for emails.



Supported values are: <tt>none</tt>, the default value which uses 7-bit
encoding if the text is plain ASCII or 8-bit otherwise. <tt>base64</tt>,
which works with any kind of content but may cause some issues with
touchy anti-spam/anti-virus engine. <tt>qp</tt> or <tt>quoted-printable</tt>,
which works best for european languages (more compact than base64) if
8-bit encoding cannot be used.


</th>
<th><tt>none</tt></th></tr>
<tr><th><a href="#notification-sendmail_path-option">sendmail_path</a></th>
<th>
Path to the sendmail executable.



The sendmail program must accept the <tt>-i</tt> and <tt>-f</tt> options.


>
>
> (<i>since 0.12</i>)
>
>

</th>
<th><tt>sendmail</tt></th></tr>
<tr><th><a href="#notification-smtp_always_bcc-option">smtp_always_bcc</a></th>
<th>
Comma-separated list of email addresses to always send
notifications to. Addresses are not public (Bcc:).


</th>
<th>(no default)</th></tr>
<tr><th><a href="#notification-smtp_always_cc-option">smtp_always_cc</a></th>
<th>
Comma-separated list of email addresses to always send
notifications to. Addresses can be seen by all recipients
(Cc:).


</th>
<th>(no default)</th></tr>
<tr><th><a href="#notification-smtp_default_domain-option">smtp_default_domain</a></th>
<th>
Default host/domain to append to addresses that do not specify
one. Fully qualified addresses are not modified. The default
domain is appended to all username/login for which an email
address cannot be found in the user settings.


</th>
<th>(no default)</th></tr>
<tr><th><a href="#notification-smtp_enabled-option">smtp_enabled</a></th>
<th>
Enable email notification.


</th>
<th><tt>disabled</tt></th></tr>
<tr><th><a href="#notification-smtp_from-option">smtp_from</a></th>
<th>
Sender address to use in notification emails.



At least one of <tt>smtp_from</tt> and <tt>smtp_replyto</tt> must be set, otherwise
Trac refuses to send notification mails.


</th>
<th><tt>trac@localhost</tt></th></tr>
<tr><th><a href="#notification-smtp_from_author-option">smtp_from_author</a></th>
<th>
Use the author of the change as the sender in notification emails
(e.g. reporter of a new ticket, author of a comment). If the
author hasn&apos;t set an email address, <tt>smtp_from</tt> and
<tt>smtp_from_name</tt> are used instead.
(<i>since 1.0</i>)


</th>
<th><tt>disabled</tt></th></tr>
<tr><th><a href="#notification-smtp_from_name-option">smtp_from_name</a></th>
<th>
Sender name to use in notification emails.


</th>
<th>(no default)</th></tr>
<tr><th><a href="#notification-smtp_password-option">smtp_password</a></th>
<th>
Password for authenticating with SMTP server.


</th>
<th>(no default)</th></tr>
<tr><th><a href="#notification-smtp_port-option">smtp_port</a></th>
<th>
SMTP server port to use for email notification.


</th>
<th><tt>25</tt></th></tr>
<tr><th><a href="#notification-smtp_replyto-option">smtp_replyto</a></th>
<th>
Reply-To address to use in notification emails.



At least one of <tt>smtp_from</tt> and <tt>smtp_replyto</tt> must be set, otherwise
Trac refuses to send notification mails.


</th>
<th><tt>trac@localhost</tt></th></tr>
<tr><th><a href="#notification-smtp_server-option">smtp_server</a></th>
<th>
SMTP server hostname to use for email notifications.


</th>
<th><tt>localhost</tt></th></tr>
<tr><th><a href="#notification-smtp_subject_prefix-option">smtp_subject_prefix</a></th>
<th>
Text to prepend to subject line of notification emails.



If the setting is not defined, then <tt>[$project_name]</tt> is used as the
prefix. If no prefix is desired, then specifying an empty option
will disable it.


</th>
<th><tt>__default__</tt></th></tr>
<tr><th><a href="#notification-smtp_user-option">smtp_user</a></th>
<th>
Username for authenticating with SMTP server.


</th>
<th>(no default)</th></tr>
<tr><th><a href="#notification-ticket_subject_template-option">ticket_subject_template</a></th>
<th>
A Genshi text template snippet used to get the notification
subject.



The template variables are documented on the
<a href="trac-notification#">TracNotification</a> page.


</th>
<th><tt>${prefix} #${ticket.id}: ${summary}</tt></th></tr>
<tr><th><a href="#notification-use_public_cc-option">use_public_cc</a></th>
<th>
Addresses in the To and Cc fields are visible to all recipients.



If this option is disabled, recipients are put in the Bcc list.


</th>
<th><tt>disabled</tt></th></tr>
<tr><th><a href="#notification-use_short_addr-option">use_short_addr</a></th>
<th>
Permit email address without a host/domain (i.e. username only).



The SMTP server should accept those addresses, and either append
a FQDN or use local delivery. See also <tt>smtp_default_domain</tt>. Do not
use this option with a public SMTP server.


</th>
<th><tt>disabled</tt></th></tr>
<tr><th><a href="#notification-use_tls-option">use_tls</a></th>
<th>
Use SSL/TLS to send notifications over SMTP.


</th>
<th><tt>disabled</tt></th></tr></table>

### `[notification-subscriber]`


The notifications subscriptions are controlled by plugins. All
`INotificationSubscriber` components are in charge. These components
may allow to be configured via this section in the `trac.ini` file.


See [TracNotification](trac-notification) for more details.


Available subscribers:

<table><tr><th>Subscriber</th>
<th>Description</th></tr>
<tr><th><tt>AlwaysEmailSubscriber</tt></th>
<th></th></tr>
<tr><th><tt>CarbonCopySubscriber</tt></th>
<th>Ticket that I&apos;m listed in the CC field is modified</th></tr>
<tr><th><tt>TicketAlwaysEmailSubscriber</tt></th>
<th></th></tr>
<tr><th><tt>TicketOwnerSubscriber</tt></th>
<th>Ticket that I own is created or modified</th></tr>
<tr><th><tt>TicketPreviousUpdatersSubscriber</tt></th>
<th>Ticket that I previously updated is modified</th></tr>
<tr><th><tt>TicketReporterSubscriber</tt></th>
<th>Ticket that I reported is modified</th></tr>
<tr><th><tt>TicketUpdaterSubscriber</tt></th>
<th>I update a ticket</th></tr></table>



### `[project]`

<table><tr><th><a href="#project-admin-option">admin</a></th>
<th>
E-Mail address of the project&apos;s administrator.


</th>
<th>(no default)</th></tr>
<tr><th><a href="#project-admin_trac_url-option">admin_trac_url</a></th>
<th>
Base URL of a Trac instance where errors in this Trac
should be reported.



This can be an absolute or relative URL, or &apos;.&apos; to reference
this Trac instance. An empty value will disable the reporting
buttons.


</th>
<th><tt>.</tt></th></tr>
<tr><th><a href="#project-descr-option">descr</a></th>
<th>
Short description of the project.


</th>
<th><tt>My example project</tt></th></tr>
<tr><th><a href="#project-footer-option">footer</a></th>
<th>
Page footer text (right-aligned).


</th>
<th><tt>Visit the Trac open source project at<br /><a href="http://trac.edgewall.org/">http://trac.edgewall.org/</a></tt></th></tr>
<tr><th><a href="#project-icon-option">icon</a></th>
<th>
URL of the icon of the project.


</th>
<th><tt>common/trac.ico</tt></th></tr>
<tr><th><a href="#project-name-option">name</a></th>
<th>
Name of the project.


</th>
<th><tt>My Project</tt></th></tr>
<tr><th><a href="#project-url-option">url</a></th>
<th>
URL of the main project web site, usually the website in
which the <tt>base_url</tt> resides. This is used in notification
e-mails.


</th>
<th>(no default)</th></tr></table>

### `[pygments-lexer]`


Configure Pygments [ lexer](http://pygments.org/docs/lexers/) options.


For example, to set the
[ PhpLexer](http://pygments.org/docs/lexers/#lexers-for-php-and-related-languages) options
`startinline` and `funcnamehighlighting`:


```
[pygments-lexer]
php.startinline = True
php.funcnamehighlighting = True
```


The lexer name is derived from the class name, with `Lexer` stripped
from the end. The lexer *short names* can also be used in place
of the lexer name.


### `[query]`

<table><tr><th><a href="#query-default_anonymous_query-option">default_anonymous_query</a></th>
<th>
The default query for anonymous users. The query is either
in <a href="trac-query#query-language">query language</a> syntax, or a URL query
string starting with <tt>?</tt> as used in <tt>query:</tt>
<a href="trac-query#using-traclinks">Trac links</a>.


</th>
<th><tt>status!=closed&cc~=$USER</tt></th></tr>
<tr><th><a href="#query-default_query-option">default_query</a></th>
<th>
The default query for authenticated users. The query is either
in <a href="trac-query#query-language">query language</a> syntax, or a URL query
string starting with <tt>?</tt> as used in <tt>query:</tt>
<a href="trac-query#using-traclinks">Trac links</a>.


</th>
<th><tt>status!=closed&owner=$USER</tt></th></tr>
<tr><th><a href="#query-items_per_page-option">items_per_page</a></th>
<th>
Number of tickets displayed per page in ticket queries,
by default.


</th>
<th><tt>100</tt></th></tr>
<tr><th><a href="#query-ticketlink_query-option">ticketlink_query</a></th>
<th>
The base query to be used when linkifying values of ticket
fields. The query is a URL query
string starting with <tt>?</tt> as used in <tt>query:</tt>
<a href="trac-query#using-traclinks">Trac links</a>.
(<i>since 0.12</i>)


</th>
<th><tt>?status=!closed</tt></th></tr></table>

### `[report]`

<table><tr><th><a href="#report-items_per_page-option">items_per_page</a></th>
<th>
Number of tickets displayed per page in ticket reports,
by default.


</th>
<th><tt>100</tt></th></tr>
<tr><th><a href="#report-items_per_page_rss-option">items_per_page_rss</a></th>
<th>
Number of tickets displayed in the rss feeds for reports.


</th>
<th><tt>0</tt></th></tr></table>

### `[repositories]`


One of the alternatives for registering new repositories is to
populate the `[repositories]` section of the `trac.ini`.


This is especially suited for setting up convenience aliases,
short-lived repositories, or during the initial phases of an
installation.


See [TracRepositoryAdmin](trac-repository-admin#) for details
about the format adopted for this section and the rest of that page for
the other alternatives.



(*since 0.12*)


### `[revisionlog]`

<table><tr><th><a href="#revisionlog-default_log_limit-option">default_log_limit</a></th>
<th>
Default value for the limit argument in the <a href="trac-revision-log">TracRevisionLog</a>.


</th>
<th><tt>100</tt></th></tr>
<tr><th><a href="#revisionlog-graph_colors-option">graph_colors</a></th>
<th>
Comma-separated list of colors to use for the <a href="trac-revision-log">TracRevisionLog</a>
graph display. (<i>since 1.0</i>)


</th>
<th><tt>#cc0,#0c0,#0cc,#00c,#c0c,#c00</tt></th></tr></table>

### `[roadmap]`

<table><tr><th><a href="#roadmap-stats_provider-option">stats_provider</a></th>
<th>
Name of the component implementing <tt>ITicketGroupStatsProvider</tt>,
which is used to collect statistics on groups of tickets for display
in the roadmap views.


</th>
<th><tt>DefaultTicketGroupStatsProvider</tt></th></tr></table>

### `[search]`

<table><tr><th><a href="#search-default_disabled_filters-option">default_disabled_filters</a></th>
<th>
Specifies which search filters should be disabled by
default on the search page. This will also restrict the
filters for the quick search function. The filter names
defined by default components are: <tt>wiki</tt>, <tt>ticket</tt>,
<tt>milestone</tt> and <tt>changeset</tt>.  For plugins, look for
their implementation of the ISearchSource interface, in
the <tt>get_search_filters()</tt> method, the first member of
returned tuple. Once disabled, search filters can still
be manually enabled by the user on the search page.
(<i>since 0.12</i>)


</th>
<th>(no default)</th></tr>
<tr><th><a href="#search-min_query_length-option">min_query_length</a></th>
<th>
Minimum length of query string allowed when performing a search.


</th>
<th><tt>3</tt></th></tr></table>

### `[spam-filter]`


This section is used to handle all configurations used by
spam filter plugin.


<table><tr><th><a href="#spam-filter-account_karma-option">account_karma</a></th>
<th>
By how many points a failed registration check impacts
the overall score.


</th>
<th><tt>0</tt></th></tr>
<tr><th><a href="#spam-filter-account_replace_checks-option">account_replace_checks</a></th>
<th>
Replace checks in account manager totally.


</th>
<th><tt>disabled</tt></th></tr>
<tr><th><a href="#spam-filter-akismet_api_key-option">akismet_api_key</a></th>
<th>
Wordpress key required to use the Akismet API.


</th>
<th>(no default)</th></tr>
<tr><th><a href="#spam-filter-akismet_api_url-option">akismet_api_url</a></th>
<th>
URL of the Akismet service.


</th>
<th><tt>rest.akismet.com/1.1/</tt></th></tr>
<tr><th><a href="#spam-filter-akismet_karma-option">akismet_karma</a></th>
<th>
By how many points an Akismet reject impacts the overall karma of
a submission.


</th>
<th><tt>10</tt></th></tr>
<tr><th><a href="#spam-filter-attachment_karma-option">attachment_karma</a></th>
<th>
The karma given to attachments.


</th>
<th><tt>0</tt></th></tr>
<tr><th><a href="#spam-filter-attachment_sample_size-option">attachment_sample_size</a></th>
<th>
The maximum number of bytes from an attachment to pass through
the spam filters.


</th>
<th><tt>16384</tt></th></tr>
<tr><th><a href="#spam-filter-authenticated_karma-option">authenticated_karma</a></th>
<th>
The karma given to authenticated users, in case
<tt>trust_authenticated</tt> is false.


</th>
<th><tt>20</tt></th></tr>
<tr><th><a href="#spam-filter-badcontent_file-option">badcontent_file</a></th>
<th>
Local file to be loaded to get <a href="bad-content">BadContent</a>. Can be used in
addition to <a href="bad-content">BadContent</a> wiki page.


</th>
<th>(no default)</th></tr>
<tr><th><a href="#spam-filter-bayes_karma-option">bayes_karma</a></th>
<th>
By what factor Bayesian spam probability score affects the overall
karma of a submission.


</th>
<th><tt>15</tt></th></tr>
<tr><th><a href="#spam-filter-bayes_min_training-option">bayes_min_training</a></th>
<th>
The minimum number of submissions in the training database required
for the filter to start impacting the karma of submissions.


</th>
<th><tt>25</tt></th></tr>
<tr><th><a href="#spam-filter-blogspam_json_api_url-option">blogspam_json_api_url</a></th>
<th>
URL of the BlogSpam service.


</th>
<th><tt>test.blogspam.net:9999</tt></th></tr>
<tr><th><a href="#spam-filter-blogspam_json_skip_tests-option">blogspam_json_skip_tests</a></th>
<th>
Comma separated list of tests to skip.


</th>
<th><tt>45-wordcount.js,60-drone.js,80-sfs.js</tt></th></tr>
<tr><th><a href="#spam-filter-blogspam_karma-option">blogspam_karma</a></th>
<th>
By how many points an BlogSpam reject impacts the overall karma of
a submission.


</th>
<th><tt>5</tt></th></tr>
<tr><th><a href="#spam-filter-botscout_api_key-option">botscout_api_key</a></th>
<th>
API key required to use BotScout.


</th>
<th>(no default)</th></tr>
<tr><th><a href="#spam-filter-botscout_karma-option">botscout_karma</a></th>
<th>
By how many points a BotScout reject impacts the overall karma of
a submission.


</th>
<th><tt>3</tt></th></tr>
<tr><th><a href="#spam-filter-captcha-option">captcha</a></th>
<th>
CAPTCHA method to use for verifying humans.


</th>
<th><tt>ExpressionCaptcha</tt></th></tr>
<tr><th><a href="#spam-filter-captcha_areyouahuman_host-option">captcha_areyouahuman_host</a></th>
<th>
Host name for AreYouAHuman usage.


</th>
<th><tt>ws.areyouahuman.com</tt></th></tr>
<tr><th><a href="#spam-filter-captcha_areyouahuman_publisher_key-option">captcha_areyouahuman_publisher_key</a></th>
<th>
Publisher key for AreYouAHuman usage.


</th>
<th>(no default)</th></tr>
<tr><th><a href="#spam-filter-captcha_areyouahuman_scoring_key-option">captcha_areyouahuman_scoring_key</a></th>
<th>
Scoring key for AreYouAHuman usage.


</th>
<th>(no default)</th></tr>
<tr><th><a href="#spam-filter-captcha_expression_ceiling-option">captcha_expression_ceiling</a></th>
<th>
Maximum value of individual terms in numeric CAPTCHA
expression.


</th>
<th><tt>10</tt></th></tr>
<tr><th><a href="#spam-filter-captcha_expression_terms-option">captcha_expression_terms</a></th>
<th>
Number of terms in numeric CAPTCHA expression.


</th>
<th><tt>3</tt></th></tr>
<tr><th><a href="#spam-filter-captcha_failed_karma-option">captcha_failed_karma</a></th>
<th>
By how many points a failed CAPTCHA impacts the overall score.


</th>
<th><tt>1</tt></th></tr>
<tr><th><a href="#spam-filter-captcha_image_alphabet-option">captcha_image_alphabet</a></th>
<th>
Alphabet to choose image CAPTCHA challenge from.


</th>
<th><tt>abcdefghkmnopqrstuvwxyz</tt></th></tr>
<tr><th><a href="#spam-filter-captcha_image_font_size-option">captcha_image_font_size</a></th>
<th>
Font size to use in image CAPTCHA.


</th>
<th><tt>25</tt></th></tr>
<tr><th><a href="#spam-filter-captcha_image_fonts-option">captcha_image_fonts</a></th>
<th>
Set of fonts to choose from when generating image CAPTCHA.


</th>
<th><tt>vera.ttf</tt></th></tr>
<tr><th><a href="#spam-filter-captcha_image_letters-option">captcha_image_letters</a></th>
<th>
Number of letters to use in image CAPTCHA challenge.


</th>
<th><tt>6</tt></th></tr>
<tr><th><a href="#spam-filter-captcha_karma-option">captcha_karma</a></th>
<th>
By how many points a successful CAPTCHA response increases the
overall score.


</th>
<th><tt>20</tt></th></tr>
<tr><th><a href="#spam-filter-captcha_karma_lifetime-option">captcha_karma_lifetime</a></th>
<th>
Time in seconds that a successful CAPTCHA response increases
karma.


</th>
<th><tt>86400</tt></th></tr>
<tr><th><a href="#spam-filter-captcha_keycaptcha_private_key-option">captcha_keycaptcha_private_key</a></th>
<th>
Private key for KeyCaptcha usage.


</th>
<th>(no default)</th></tr>
<tr><th><a href="#spam-filter-captcha_keycaptcha_user_id-option">captcha_keycaptcha_user_id</a></th>
<th>
User id for KeyCaptcha usage.


</th>
<th>(no default)</th></tr>
<tr><th><a href="#spam-filter-captcha_lifetime-option">captcha_lifetime</a></th>
<th>
Time in seconds before database cleanup is called.


</th>
<th><tt>3600</tt></th></tr>
<tr><th><a href="#spam-filter-captcha_recaptcha_private_key-option">captcha_recaptcha_private_key</a></th>
<th>
Private key for reCaptcha usage.


</th>
<th>(no default)</th></tr>
<tr><th><a href="#spam-filter-captcha_recaptcha_public_key-option">captcha_recaptcha_public_key</a></th>
<th>
Public key for reCaptcha usage.


</th>
<th>(no default)</th></tr>
<tr><th><a href="#spam-filter-extlinks_allowed_domains-option">extlinks_allowed_domains</a></th>
<th>
List of domains that should be allowed in external links


</th>
<th><tt>example.com,example.org</tt></th></tr>
<tr><th><a href="#spam-filter-extlinks_karma-option">extlinks_karma</a></th>
<th>
By how many points too many external links in a submission impact
the overall score.


</th>
<th><tt>2</tt></th></tr>
<tr><th><a href="#spam-filter-fspamlist_api_key-option">fspamlist_api_key</a></th>
<th>
API key required to use FSpamList.


</th>
<th>(no default)</th></tr>
<tr><th><a href="#spam-filter-fspamlist_karma-option">fspamlist_karma</a></th>
<th>
By how many points a FSpamList reject impacts the overall karma of
a submission.


</th>
<th><tt>3</tt></th></tr>
<tr><th><a href="#spam-filter-httpbl_api_key-option">httpbl_api_key</a></th>
<th>
Http:BL API key required for use.


</th>
<th>(no default)</th></tr>
<tr><th><a href="#spam-filter-httpbl_spammer_karma-option">httpbl_spammer_karma</a></th>
<th>
By how many points listing as &quot;comment spammer&quot; impacts the
overall karma of a submission.


</th>
<th><tt>6</tt></th></tr>
<tr><th><a href="#spam-filter-ip6_blacklist_servers-option">ip6_blacklist_servers</a></th>
<th>
Servers used for IPv6 blacklisting.


</th>
<th><tt>all.s5h.net,dnsbl.dronebl.org,bl.ipv6.spameatingmonkey.net</tt></th></tr>
<tr><th><a href="#spam-filter-ip_blacklist_karma-option">ip_blacklist_karma</a></th>
<th>
By how many points blacklisting by a single server impacts the
overall karma of a submission.


</th>
<th><tt>5</tt></th></tr>
<tr><th><a href="#spam-filter-ip_blacklist_servers-option">ip_blacklist_servers</a></th>
<th>
Servers used for IPv4 blacklisting.


</th>
<th><tt>list.blogspambl.com,all.s5h.net,dnsbl.tornevall.org,dnsbl.dronebl.org</tt></th></tr>
<tr><th><a href="#spam-filter-ip_throttle_karma-option">ip_throttle_karma</a></th>
<th>
By how many points exceeding the configured maximum number of posts
per hour impacts the overall score.


</th>
<th><tt>3</tt></th></tr>
<tr><th><a href="#spam-filter-ipbadcontent_file-option">ipbadcontent_file</a></th>
<th>
Local file to be loaded to get BadIP. Can be used in
addition to BadIP wiki page.


</th>
<th>(no default)</th></tr>
<tr><th><a href="#spam-filter-ipregex_karma-option">ipregex_karma</a></th>
<th>
By how many points a match with a pattern on the BadIP page
impacts the overall karma of a submission.


</th>
<th><tt>20</tt></th></tr>
<tr><th><a href="#spam-filter-is_forwarded-option">is_forwarded</a></th>
<th>
Interpret X-Forwarded-For header for IP checks.


</th>
<th><tt>disabled</tt></th></tr>
<tr><th><a href="#spam-filter-logging_enabled-option">logging_enabled</a></th>
<th>
Whether all content submissions and spam filtering activity should
be logged to the database.


</th>
<th><tt>enabled</tt></th></tr>
<tr><th><a href="#spam-filter-max_external_links-option">max_external_links</a></th>
<th>
The maximum number of external links allowed in a submission until
that submission gets negative karma.


</th>
<th><tt>4</tt></th></tr>
<tr><th><a href="#spam-filter-max_posts_by_ip-option">max_posts_by_ip</a></th>
<th>
The maximum allowed number of submissions per hour from a single IP
address. If this limit is exceeded, subsequent submissions get negative
karma.


</th>
<th><tt>10</tt></th></tr>
<tr><th><a href="#spam-filter-min_karma-option">min_karma</a></th>
<th>
The minimum score required for a submission to be allowed.


</th>
<th><tt>0</tt></th></tr>
<tr><th><a href="#spam-filter-purge_age-option">purge_age</a></th>
<th>
The number of days after which log entries should be purged.


</th>
<th><tt>7</tt></th></tr>
<tr><th><a href="#spam-filter-regex_karma-option">regex_karma</a></th>
<th>
By how many points a match with a pattern on the <a href="bad-content">BadContent</a> page
impacts the overall karma of a submission.


</th>
<th><tt>5</tt></th></tr>
<tr><th><a href="#spam-filter-register_karma-option">register_karma</a></th>
<th>
The karma given to registrations.


</th>
<th><tt>0</tt></th></tr>
<tr><th><a href="#spam-filter-reject_handler-option">reject_handler</a></th>
<th>
The handler used to reject content.


</th>
<th><tt>FilterSystem</tt></th></tr>
<tr><th><a href="#spam-filter-report_pages-option">report_pages</a></th>
<th>
List of page types to add spam report link


</th>
<th><tt>wiki,attachment,ticket</tt></th></tr>
<tr><th><a href="#spam-filter-session_karma-option">session_karma</a></th>
<th>
By how many points an existing and configured session improves the
overall karma of the submission. A third of the points is granted for
having an existing session at all, the other two thirds are granted
when the user has his name and/or email address set in the session,
respectively.


</th>
<th><tt>9</tt></th></tr>
<tr><th><a href="#spam-filter-show_blacklisted-option">show_blacklisted</a></th>
<th>
Show the matched bad content patterns in rejection message.


</th>
<th><tt>enabled</tt></th></tr>
<tr><th><a href="#spam-filter-show_blacklisted_ip-option">show_blacklisted_ip</a></th>
<th>
Show the matched bad IP patterns in rejection message.


</th>
<th><tt>enabled</tt></th></tr>
<tr><th><a href="#spam-filter-show_train_only-option">show_train_only</a></th>
<th>
Show the buttons for training without deleting entry.


</th>
<th><tt>disabled</tt></th></tr>
<tr><th><a href="#spam-filter-skip_external-option">skip_external</a></th>
<th>
Skip external calls when this negative karma is already reached
by internal tests.


</th>
<th><tt>20</tt></th></tr>
<tr><th><a href="#spam-filter-skip_externalham-option">skip_externalham</a></th>
<th>
Skip external calls when this positive karma is already reached
by internal tests.


</th>
<th><tt>30</tt></th></tr>
<tr><th><a href="#spam-filter-spam_monitor_entries-option">spam_monitor_entries</a></th>
<th>
How many monitor entries are displayed by default (between 5 and 10000).


</th>
<th><tt>100</tt></th></tr>
<tr><th><a href="#spam-filter-spam_report_entries-option">spam_report_entries</a></th>
<th>
How many report entries are displayed by default (between 5 and 10000).


</th>
<th><tt>100</tt></th></tr>
<tr><th><a href="#spam-filter-spam_user_defaultmode-option">spam_user_defaultmode</a></th>
<th>
Default mode for spam user admin panel.


</th>
<th><tt>overview</tt></th></tr>
<tr><th><a href="#spam-filter-spam_user_maxage-option">spam_user_maxage</a></th>
<th>
How many days no login are considered for dead accounts.


</th>
<th><tt>200</tt></th></tr>
<tr><th><a href="#spam-filter-spam_user_minwiki-option">spam_user_minwiki</a></th>
<th>
How many wiki edits are still an unused account.


</th>
<th><tt>0</tt></th></tr>
<tr><th><a href="#spam-filter-stop_external-option">stop_external</a></th>
<th>
Stop external calls when this negative karma is reached.


</th>
<th><tt>50</tt></th></tr>
<tr><th><a href="#spam-filter-stop_externalham-option">stop_externalham</a></th>
<th>
Stop external calls when this positive karma is reached.


</th>
<th><tt>50</tt></th></tr>
<tr><th><a href="#spam-filter-stopforumspam_api_key-option">stopforumspam_api_key</a></th>
<th>
API key used to report SPAM.


</th>
<th>(no default)</th></tr>
<tr><th><a href="#spam-filter-stopforumspam_karma-option">stopforumspam_karma</a></th>
<th>
By how many points a StopForumSpam reject impacts the overall karma of
a submission.


</th>
<th><tt>4</tt></th></tr>
<tr><th><a href="#spam-filter-train_external-option">train_external</a></th>
<th>
Allow training of external services.


</th>
<th><tt>enabled</tt></th></tr>
<tr><th><a href="#spam-filter-trap_karma-option">trap_karma</a></th>
<th>
By how many points a trap reject impacts the overall karma of
a submission.


</th>
<th><tt>10</tt></th></tr>
<tr><th><a href="#spam-filter-trap_name-option">trap_name</a></th>
<th>
Name of the invisible trap field, should contain some reference
to e-mail for better results.


</th>
<th><tt>sfp_email</tt></th></tr>
<tr><th><a href="#spam-filter-trap_name_hidden-option">trap_name_hidden</a></th>
<th>
Name of the hidden trap field, should contain some reference
to e-mail for better results.


</th>
<th><tt>sfph_mail</tt></th></tr>
<tr><th><a href="#spam-filter-trap_name_register-option">trap_name_register</a></th>
<th>
Name of the register trap field, should contain some reference
to web/homepage for better results.


</th>
<th><tt>spf_homepage</tt></th></tr>
<tr><th><a href="#spam-filter-trust_authenticated-option">trust_authenticated</a></th>
<th>
Whether content submissions by authenticated users should be trusted
without checking for potential spam or other abuse.


</th>
<th><tt>disabled</tt></th></tr>
<tr><th><a href="#spam-filter-url_blacklist_karma-option">url_blacklist_karma</a></th>
<th>
By how many points blacklisting by a single bad URL impacts the
overall karma of a submission.


</th>
<th><tt>3</tt></th></tr>
<tr><th><a href="#spam-filter-url_blacklist_servers-option">url_blacklist_servers</a></th>
<th>
Servers used for URL blacklisting.


</th>
<th><tt>urired.spameatingmonkey.net,multi.surbl.org,dbl.spamhaus.org</tt></th></tr>
<tr><th><a href="#spam-filter-use_external-option">use_external</a></th>
<th>
Allow usage of external services.


</th>
<th><tt>enabled</tt></th></tr></table>

### `[sqlite]`

<table><tr><th><a href="#sqlite-extensions-option">extensions</a></th>
<th>
Paths to <a href="https://sqlite.org/loadext.html"> sqlite extensions</a>.
The paths may be absolute or relative to the Trac environment.
(<i>since 0.12</i>)


</th>
<th>(no default)</th></tr></table>

### `[svn]`

<table><tr><th><a href="#svn-authz_file-option">authz_file</a></th>
<th>
The path to the Subversion
<a href="http://svnbook.red-bean.com/en/1.7/svn.serverconfig.pathbasedauthz.html"> authorization (authz) file</a>.
To enable authz permission checking, the <tt>AuthzSourcePolicy</tt> permission
policy must be added to <tt>[trac] permission_policies</tt>. Non-absolute
paths are relative to the Environment <tt>conf</tt> directory.


</th>
<th>(no default)</th></tr>
<tr><th><a href="#svn-authz_module_name-option">authz_module_name</a></th>
<th>
The module prefix used in the <tt>authz_file</tt> for the default
repository. If left empty, the global section is used.


</th>
<th>(no default)</th></tr></table>

### `[tags]`

<table><tr><th><a href="#tags-listtagged_items_per_page-option">listtagged_items_per_page</a></th>
<th>
Number of tagged resources displayed per page of tag query results requested by <tt>ListTagged</tt> macros and from <tt>/tags</tt>.


</th>
<th><tt>100</tt></th></tr></table>

### `[ticket]`

<table><tr><th><a href="#ticket-allowed_empty_fields-option">allowed_empty_fields</a></th>
<th>
Comma-separated list of <tt>select</tt> fields that can have
an empty value. (<i>since 1.1.2</i>)


</th>
<th><tt>milestone,version</tt></th></tr>
<tr><th><a href="#ticket-commit_ticket_update_check_perms-option">commit_ticket_update_check_perms</a></th>
<th>
Check that the committer has permission to perform the requested
operations on the referenced tickets.


This requires that the user names be the same for Trac and repository
operations.


</th>
<th><tt>enabled</tt></th></tr>
<tr><th><a href="#ticket-commit_ticket_update_commands.close-option">commit_ticket_update_commands.close</a></th>
<th>
Commands that close tickets, as a space-separated list.


</th>
<th><tt>close closed closes fix fixed fixes</tt></th></tr>
<tr><th><a href="#ticket-commit_ticket_update_commands.refs-option">commit_ticket_update_commands.refs</a></th>
<th>
Commands that add a reference, as a space-separated list.



If set to the special value <tt><ALL></tt>, all tickets referenced by the
message will get a reference to the changeset.


</th>
<th><tt>addresses re references refs see</tt></th></tr>
<tr><th><a href="#ticket-commit_ticket_update_envelope-option">commit_ticket_update_envelope</a></th>
<th>
Require commands to be enclosed in an envelope.



Must be empty or contain two characters. For example, if set to <tt>[]</tt>,
then commands must be in the form of <tt>[closes #4]</tt>.


</th>
<th>(no default)</th></tr>
<tr><th><a href="#ticket-commit_ticket_update_notify-option">commit_ticket_update_notify</a></th>
<th>
Send ticket change notification when updating a ticket.


</th>
<th><tt>enabled</tt></th></tr>
<tr><th><a href="#ticket-default_cc-option">default_cc</a></th>
<th>
Default cc: list for newly created tickets.


</th>
<th>(no default)</th></tr>
<tr><th><a href="#ticket-default_component-option">default_component</a></th>
<th>
Default component for newly created tickets.


</th>
<th>(no default)</th></tr>
<tr><th><a href="#ticket-default_description-option">default_description</a></th>
<th>
Default description for newly created tickets.


</th>
<th>(no default)</th></tr>
<tr><th><a href="#ticket-default_keywords-option">default_keywords</a></th>
<th>
Default keywords for newly created tickets.


</th>
<th>(no default)</th></tr>
<tr><th><a href="#ticket-default_milestone-option">default_milestone</a></th>
<th>
Default milestone for newly created tickets.


</th>
<th>(no default)</th></tr>
<tr><th><a href="#ticket-default_owner-option">default_owner</a></th>
<th>
Default owner for newly created tickets.


</th>
<th><tt>< default ></tt></th></tr>
<tr><th><a href="#ticket-default_priority-option">default_priority</a></th>
<th>
Default priority for newly created tickets.


</th>
<th><tt>major</tt></th></tr>
<tr><th><a href="#ticket-default_resolution-option">default_resolution</a></th>
<th>
Default resolution for resolving (closing) tickets.


</th>
<th><tt>fixed</tt></th></tr>
<tr><th><a href="#ticket-default_severity-option">default_severity</a></th>
<th>
Default severity for newly created tickets.


</th>
<th>(no default)</th></tr>
<tr><th><a href="#ticket-default_summary-option">default_summary</a></th>
<th>
Default summary (title) for newly created tickets.


</th>
<th>(no default)</th></tr>
<tr><th><a href="#ticket-default_type-option">default_type</a></th>
<th>
Default type for newly created tickets.


</th>
<th><tt>defect</tt></th></tr>
<tr><th><a href="#ticket-default_version-option">default_version</a></th>
<th>
Default version for newly created tickets.


</th>
<th>(no default)</th></tr>
<tr><th><a href="#ticket-max_comment_size-option">max_comment_size</a></th>
<th>
Maximum allowed comment size in characters.


</th>
<th><tt>262144</tt></th></tr>
<tr><th><a href="#ticket-max_description_size-option">max_description_size</a></th>
<th>
Maximum allowed description size in characters.


</th>
<th><tt>262144</tt></th></tr>
<tr><th><a href="#ticket-max_summary_size-option">max_summary_size</a></th>
<th>
Maximum allowed summary size in characters. (<i>since 1.0.2</i>)


</th>
<th><tt>262144</tt></th></tr>
<tr><th><a href="#ticket-preserve_newlines-option">preserve_newlines</a></th>
<th>
Whether Wiki formatter should respect the new lines present
in the Wiki text.
If set to &apos;default&apos;, this is equivalent to &apos;yes&apos; for new environments
but keeps the old behavior for upgraded environments (i.e. &apos;no&apos;).


</th>
<th><tt>default</tt></th></tr>
<tr><th><a href="#ticket-restrict_owner-option">restrict_owner</a></th>
<th>
Make the owner field of tickets use a drop-down menu.
Be sure to understand the performance implications before activating
this option. See
<a href="trac-tickets#">Assign-to as Drop-Down List</a>.



Please note that e-mail addresses are <b>not</b> obfuscated in the
resulting drop-down menu, so this option should not be used if
e-mail addresses must remain protected.


</th>
<th><tt>disabled</tt></th></tr>
<tr><th><a href="#ticket-workflow-option">workflow</a></th>
<th>
Ordered list of workflow controllers to use for ticket actions.


</th>
<th><tt>ConfigurableTicketWorkflow</tt></th></tr></table>

### `[ticket-custom]`


In this section, you can define additional fields for tickets. See
[TracTicketsCustomFields](trac-tickets-custom-fields) for more details.

### `[ticket-workflow]`


The workflow for tickets is controlled by plugins. By default,
there's only a `ConfigurableTicketWorkflow` component in charge.
That component allows the workflow to be configured via this section
in the `trac.ini` file. See [TracWorkflow](trac-workflow) for more details.


### `[timeline]`

<table><tr><th><a href="#timeline-abbreviated_messages-option">abbreviated_messages</a></th>
<th>
Whether wiki-formatted event messages should be truncated or not.


This only affects the default rendering, and can be overriden by
specific event providers, see their own documentation.


</th>
<th><tt>enabled</tt></th></tr>
<tr><th><a href="#timeline-changeset_collapse_events-option">changeset_collapse_events</a></th>
<th>
Whether consecutive changesets from the same author having
exactly the same message should be presented as one event.
That event will link to the range of changesets in the log view.


</th>
<th><tt>disabled</tt></th></tr>
<tr><th><a href="#timeline-changeset_long_messages-option">changeset_long_messages</a></th>
<th>
Whether wiki-formatted changeset messages should be multiline or
not.



If this option is not specified or is false and <tt>wiki_format_messages</tt>
is set to true, changeset messages will be single line only, losing
some formatting (bullet points, etc).


</th>
<th><tt>disabled</tt></th></tr>
<tr><th><a href="#timeline-changeset_show_files-option">changeset_show_files</a></th>
<th>
Number of files to show (<tt>-1</tt> for unlimited, <tt>0</tt> to disable).



This can also be <tt>location</tt>, for showing the common prefix for the
changed files.


</th>
<th><tt>0</tt></th></tr>
<tr><th><a href="#timeline-default_daysback-option">default_daysback</a></th>
<th>
Default number of days displayed in the Timeline, in days.


</th>
<th><tt>30</tt></th></tr>
<tr><th><a href="#timeline-max_daysback-option">max_daysback</a></th>
<th>
Maximum number of days (-1 for unlimited) displayable in the
Timeline.


</th>
<th><tt>90</tt></th></tr>
<tr><th><a href="#timeline-newticket_formatter-option">newticket_formatter</a></th>
<th>
Which formatter flavor (e.g. &apos;html&apos; or &apos;oneliner&apos;) should be
used when presenting the description for new tickets.
If &apos;oneliner&apos;, the [timeline] abbreviated_messages option applies.


</th>
<th><tt>oneliner</tt></th></tr>
<tr><th><a href="#timeline-ticket_show_component-option">ticket_show_component</a></th>
<th>
Enable the display of component of tickets in the timeline.
(<i>since 1.1.1</i>)


</th>
<th><tt>disabled</tt></th></tr>
<tr><th><a href="#timeline-ticket_show_details-option">ticket_show_details</a></th>
<th>
Enable the display of all ticket changes in the timeline, not only
open / close operations.


</th>
<th><tt>disabled</tt></th></tr></table>

### `[trac]`

<table><tr><th><a href="#trac-auth_cookie_lifetime-option">auth_cookie_lifetime</a></th>
<th>
Lifetime of the authentication cookie, in seconds.


This value determines how long the browser will cache
authentication information, and therefore, after how much
inactivity a user will have to log in again. The default value
of 0 makes the cookie expire at the end of the browsing
session. (<i>since 0.12</i>)


</th>
<th><tt>0</tt></th></tr>
<tr><th><a href="#trac-auth_cookie_path-option">auth_cookie_path</a></th>
<th>
Path for the authentication cookie. Set this to the common
base path of several Trac instances if you want them to share
the cookie.  (<i>since 0.12</i>)


</th>
<th>(no default)</th></tr>
<tr><th><a href="#trac-auto_preview_timeout-option">auto_preview_timeout</a></th>
<th>
Inactivity timeout in seconds after which the automatic wiki preview
triggers an update. This option can contain floating-point values. The
lower the setting, the more requests will be made to the server. Set
this to 0 to disable automatic preview. (<i>since 0.12</i>)


</th>
<th><tt>2.0</tt></th></tr>
<tr><th><a href="#trac-auto_reload-option">auto_reload</a></th>
<th>
Automatically reload template files after modification.


</th>
<th><tt>disabled</tt></th></tr>
<tr><th><a href="#trac-backup_dir-option">backup_dir</a></th>
<th>
Database backup location


</th>
<th><tt>db</tt></th></tr>
<tr><th><a href="#trac-base_url-option">base_url</a></th>
<th>
Reference URL for the Trac deployment.


This is the base URL that will be used when producing
documents that will be used outside of the web browsing
context, like for example when inserting URLs pointing to Trac
resources in notification e-mails.


</th>
<th>(no default)</th></tr>
<tr><th><a href="#trac-check_auth_ip-option">check_auth_ip</a></th>
<th>
Whether the IP address of the user should be checked for
authentication (<i>since 0.9</i>).


</th>
<th><tt>disabled</tt></th></tr>
<tr><th><a href="#trac-database-option">database</a></th>
<th>
Database connection
<a href="trac-environment#database-connection-strings">string</a> for this
project


</th>
<th><tt>sqlite:db/trac.db</tt></th></tr>
<tr><th><a href="#trac-debug_sql-option">debug_sql</a></th>
<th>
Show the SQL queries in the Trac log, at DEBUG level.


</th>
<th><tt>disabled</tt></th></tr>
<tr><th><a href="#trac-default_charset-option">default_charset</a></th>
<th>
Charset to be used when in doubt.


</th>
<th><tt>utf-8</tt></th></tr>
<tr><th><a href="#trac-default_date_format-option">default_date_format</a></th>
<th>
The date format. Valid options are &apos;iso8601&apos; for selecting
ISO 8601 format, or leave it empty which means the default
date format will be inferred from the browser&apos;s default
language. (<i>since 1.0</i>)


</th>
<th>(no default)</th></tr>
<tr><th><a href="#trac-default_dateinfo_format-option">default_dateinfo_format</a></th>
<th>
The date information format. Valid options are &apos;relative&apos; for
displaying relative format and &apos;absolute&apos; for displaying absolute
format. (<i>since 1.0</i>)


</th>
<th><tt>relative</tt></th></tr>
<tr><th><a href="#trac-default_handler-option">default_handler</a></th>
<th>
Name of the component that handles requests to the base
URL.



Options include <tt>TimelineModule</tt>, <tt>RoadmapModule</tt>,
<tt>BrowserModule</tt>, <tt>QueryModule</tt>, <tt>ReportModule</tt>, <tt>TicketModule</tt>
and <tt>WikiModule</tt>.


</th>
<th><tt>WikiModule</tt></th></tr>
<tr><th><a href="#trac-default_language-option">default_language</a></th>
<th>
The preferred language to use if no user preference has
been set. (<i>since 0.12.1</i>)


</th>
<th>(no default)</th></tr>
<tr><th><a href="#trac-default_timezone-option">default_timezone</a></th>
<th>
The default timezone to use


</th>
<th>(no default)</th></tr>
<tr><th><a href="#trac-genshi_cache_size-option">genshi_cache_size</a></th>
<th>
The maximum number of templates that the template loader will cache
in memory. You may want to choose a higher value if your site uses a
larger number of templates, and you have enough memory to spare, or
you can reduce it if you are short on memory.


</th>
<th><tt>128</tt></th></tr>
<tr><th><a href="#trac-htdocs_location-option">htdocs_location</a></th>
<th>
Base URL for serving the core static resources below
<tt>/chrome/common/</tt>.



It can be left empty, and Trac will simply serve those resources
itself.



Advanced users can use this together with
<a href="trac-admin">trac-admin ... deploy &lt;deploydir&gt;</a> to allow serving the
static resources for Trac directly from the web server.
Note however that this only applies to the <tt><deploydir>/htdocs/common</tt>
directory, the other deployed resources (i.e. those from plugins)
will not be made available this way and additional rewrite
rules will be needed in the web server.


</th>
<th>(no default)</th></tr>
<tr><th><a href="#trac-ignore_auth_case-option">ignore_auth_case</a></th>
<th>
Whether login names should be converted to lower case
(<i>since 0.9</i>).


</th>
<th><tt>disabled</tt></th></tr>
<tr><th><a href="#trac-jquery_location-option">jquery_location</a></th>
<th>
Location of the jQuery JavaScript library (version 1.11.3).


An empty value loads jQuery from the copy bundled with Trac.



Alternatively, jQuery could be loaded from a CDN, for example:
<a href="http://code.jquery.com/jquery-1.11.3.min.js"> http://code.jquery.com/jquery-1.11.3.min.js</a>,
<a href="http://ajax.aspnetcdn.com/ajax/jQuery/jquery-1.11.3.min.js"> http://ajax.aspnetcdn.com/ajax/jQuery/jquery-1.11.3.min.js</a> or
<a href="https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"> https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js</a>.



(<i>since 1.0</i>)


</th>
<th>(no default)</th></tr>
<tr><th><a href="#trac-jquery_ui_location-option">jquery_ui_location</a></th>
<th>
Location of the jQuery UI JavaScript library (version 1.11.4).


An empty value loads jQuery UI from the copy bundled with Trac.



Alternatively, jQuery UI could be loaded from a CDN, for example:
<a href="https://ajax.googleapis.com/ajax/libs/jqueryui/1.11.4/jquery-ui.min.js"> https://ajax.googleapis.com/ajax/libs/jqueryui/1.11.4/jquery-ui.min.js</a>
or
<a href="http://ajax.aspnetcdn.com/ajax/jquery.ui/1.11.4/jquery-ui.min.js"> http://ajax.aspnetcdn.com/ajax/jquery.ui/1.11.4/jquery-ui.min.js</a>.



(<i>since 1.0</i>)


</th>
<th>(no default)</th></tr>
<tr><th><a href="#trac-jquery_ui_theme_location-option">jquery_ui_theme_location</a></th>
<th>
Location of the theme to be used with the jQuery UI JavaScript
library (version 1.11.4).


An empty value loads the custom Trac jQuery UI theme from the copy
bundled with Trac.



Alternatively, a jQuery UI theme could be loaded from a CDN, for
example:
<a href="https://ajax.googleapis.com/ajax/libs/jqueryui/1.11.4/themes/start/jquery-ui.css"> https://ajax.googleapis.com/ajax/libs/jqueryui/1.11.4/themes/start/jquery-ui.css</a>
or
<a href="http://ajax.aspnetcdn.com/ajax/jquery.ui/1.11.4/themes/start/jquery-ui.css"> http://ajax.aspnetcdn.com/ajax/jquery.ui/1.11.4/themes/start/jquery-ui.css</a>.



(<i>since 1.0</i>)


</th>
<th>(no default)</th></tr>
<tr><th><a href="#trac-never_obfuscate_mailto-option">never_obfuscate_mailto</a></th>
<th>
Never obfuscate <tt>mailto:</tt> links explicitly written in the wiki,
even if <tt>show_email_addresses</tt> is false or the user doesn&apos;t have
EMAIL_VIEW permission.


</th>
<th><tt>disabled</tt></th></tr>
<tr><th><a href="#trac-permission_policies-option">permission_policies</a></th>
<th>
List of components implementing <tt>IPermissionPolicy</tt>, in the order
in which they will be applied. These components manage fine-grained
access control to Trac resources.


</th>
<th><tt>ReadonlyWikiPolicy,DefaultPermissionPolicy,LegacyAttachmentPolicy</tt></th></tr>
<tr><th><a href="#trac-permission_store-option">permission_store</a></th>
<th>
Name of the component implementing <tt>IPermissionStore</tt>, which is used
for managing user and group permissions.


</th>
<th><tt>DefaultPermissionStore</tt></th></tr>
<tr><th><a href="#trac-pg_dump_path-option">pg_dump_path</a></th>
<th>
Location of pg_dump for Postgres database backups


</th>
<th><tt>pg_dump</tt></th></tr>
<tr><th><a href="#trac-request_filters-option">request_filters</a></th>
<th>
Ordered list of filters to apply to all requests.


</th>
<th>(no default)</th></tr>
<tr><th><a href="#trac-resizable_textareas-option">resizable_textareas</a></th>
<th>
Make <tt><textarea></tt> fields resizable. Requires JavaScript.
(<i>since 0.12</i>)


</th>
<th><tt>enabled</tt></th></tr>
<tr><th><a href="#trac-secure_cookies-option">secure_cookies</a></th>
<th>
Restrict cookies to HTTPS connections.



When true, set the <tt>secure</tt> flag on all cookies so that they
are only sent to the server on HTTPS connections. Use this if
your Trac instance is only accessible through HTTPS.


</th>
<th><tt>disabled</tt></th></tr>
<tr><th><a href="#trac-show_email_addresses-option">show_email_addresses</a></th>
<th>
Show email addresses instead of usernames. If false, email
addresses are obfuscated for users that don&apos;t have EMAIL_VIEW
permission.


</th>
<th><tt>disabled</tt></th></tr>
<tr><th><a href="#trac-show_full_names-option">show_full_names</a></th>
<th>
Show full names instead of usernames. (<i>since 1.2</i>)


</th>
<th><tt>enabled</tt></th></tr>
<tr><th><a href="#trac-show_ip_addresses-option">show_ip_addresses</a></th>
<th>
Show IP addresses for resource edits (e.g. wiki). Since 1.0.5 this
option is deprecated and will be removed in 1.3.1.


</th>
<th><tt>disabled</tt></th></tr>
<tr><th><a href="#trac-timeout-option">timeout</a></th>
<th>
Timeout value for database connection, in seconds.
Use &apos;0&apos; to specify <i>no timeout</i>.


</th>
<th><tt>20</tt></th></tr>
<tr><th><a href="#trac-use_base_url_for_redirect-option">use_base_url_for_redirect</a></th>
<th>
Optionally use <tt>[trac] base_url</tt> for redirects.



In some configurations, usually involving running Trac behind
a HTTP proxy, Trac can&apos;t automatically reconstruct the URL
that is used to access it. You may need to use this option to
force Trac to use the <tt>base_url</tt> setting also for
redirects. This introduces the obvious limitation that this
environment will only be usable when accessible from that URL,
as redirects are frequently used.


</th>
<th><tt>disabled</tt></th></tr>
<tr><th><a href="#trac-use_chunked_encoding-option">use_chunked_encoding</a></th>
<th>
If enabled, send contents as chunked encoding in HTTP/1.1.
Otherwise, send contents with <tt>Content-Length</tt> header after entire of
the contents are rendered. (<i>since 1.0.6</i>)


</th>
<th><tt>disabled</tt></th></tr>
<tr><th><a href="#trac-use_xsendfile-option">use_xsendfile</a></th>
<th>
When true, send a <tt>X-Sendfile</tt> header and no content when sending
files from the filesystem, so that the web server handles the content.
This requires a web server that knows how to handle such a header,
like Apache with <tt>mod_xsendfile</tt> or lighttpd. (<i>since 1.0</i>)


</th>
<th><tt>disabled</tt></th></tr>
<tr><th><a href="#trac-wiki_toolbars-option">wiki_toolbars</a></th>
<th>
Add a simple toolbar on top of Wiki &lt;textarea&gt;s.
(<i>since 1.0.2</i>)


</th>
<th><tt>enabled</tt></th></tr>
<tr><th><a href="#trac-xsendfile_header-option">xsendfile_header</a></th>
<th>
The header to use if <tt>use_xsendfile</tt> is enabled. If Nginx is used,
set <tt>X-Accel-Redirect</tt>. (<i>since 1.0.6</i>)


</th>
<th><tt>X-Sendfile</tt></th></tr></table>

### `[versioncontrol]`

<table><tr><th><a href="#versioncontrol-allowed_repository_dir_prefixes-option">allowed_repository_dir_prefixes</a></th>
<th>
Comma-separated list of allowed prefixes for repository
directories when adding and editing repositories in the repository
admin panel. If the list is empty, all repository directories are
allowed. (<i>since 0.12.1</i>)


</th>
<th>(no default)</th></tr>
<tr><th><a href="#versioncontrol-default_repository_type-option">default_repository_type</a></th>
<th>
Default repository connector type.



This is used as the default repository type for repositories defined
in the <a href="trac-ini#">TracIni#repositories-section repositories</a> section or using
the &quot;Repositories&quot; admin panel. (<i>since 0.12</i>)


</th>
<th><tt>svn</tt></th></tr></table>

### `[wiki]`

<table><tr><th><a href="#wiki-default_edit_area_height-option">default_edit_area_height</a></th>
<th>
Default height of the textarea on the wiki edit page.
(<i>Since 1.1.5</i>)


</th>
<th><tt>20</tt></th></tr>
<tr><th><a href="#wiki-ignore_missing_pages-option">ignore_missing_pages</a></th>
<th>
Enable/disable highlighting <a href="camel-case">CamelCase</a> links to missing pages.


</th>
<th><tt>disabled</tt></th></tr>
<tr><th><a href="#wiki-max_size-option">max_size</a></th>
<th>
Maximum allowed wiki page size in characters.


</th>
<th><tt>262144</tt></th></tr>
<tr><th><a href="#wiki-render_unsafe_content-option">render_unsafe_content</a></th>
<th>
Enable/disable the use of unsafe HTML tags such as <tt><script></tt> or
<tt><embed></tt> with the HTML <a href="wiki-processors">WikiProcessor</a>.



For public sites where anonymous users can edit the wiki it is
recommended to leave this option disabled.


</th>
<th><tt>disabled</tt></th></tr>
<tr><th><a href="#wiki-safe_origins-option">safe_origins</a></th>
<th>
List of URIs considered &quot;safe cross-origin&quot;, that will be
rendered as <tt>img</tt> element without <tt>crossorigin="anonymous"</tt> attribute
or used in <tt>url()</tt> of inline style attribute even if
<tt>[wiki] render_unsafe_content</tt> is <tt>false</tt> (<i>since 1.0.15</i>).



To make any origins safe, specify &quot;*&quot; in the list.


</th>
<th><tt>data:</tt></th></tr>
<tr><th><a href="#wiki-safe_schemes-option">safe_schemes</a></th>
<th>
List of URI schemes considered &quot;safe&quot;, that will be rendered as
external links even if <tt>[wiki] render_unsafe_content</tt> is <tt>false</tt>.


</th>
<th><tt>cvs,file,ftp,git,irc,http,https,news,sftp,smb,ssh,svn,svn+ssh</tt></th></tr>
<tr><th><a href="#wiki-split_page_names-option">split_page_names</a></th>
<th>
Enable/disable splitting the <a href="wiki-page-names">WikiPageNames</a> with space characters.


</th>
<th><tt>disabled</tt></th></tr></table>

### `[wikiextras]`

<table><tr><th><a href="#wikiextras-done_phrases-option">done_phrases</a></th>
<th>
Analogous to <tt>FIXME</tt>-phrases, but presentation is less eye-catching.


</th>
<th><tt>DONE,DEBUGGED,FIXED,REVIEWED</tt></th></tr>
<tr><th><a href="#wikiextras-fixme_phrases-option">fixme_phrases</a></th>
<th>
A list of attentional phrases or single words, separated by comma
(<tt>,</tt>) that will be highlighted to catch attention. Any delimiter
<tt>():<></tt> adjacent to a phrase will not be presented. (i.e. do not
include any of these delimiters in this list). This makes it possible
to naturally write, for example, <tt>FIXME:</tt> in a wiki text, but view the
phrase highlighted without the colon (<tt>:</tt>) which would not look
natural. Use the <tt>ShowPhrases</tt> macro to show a list of currently
defined phrases.


</th>
<th><tt>BUG,FIXME</tt></th></tr>
<tr><th><a href="#wikiextras-icon_limit-option">icon_limit</a></th>
<th>
To prevent exhaustive network traffic, limit the maximum number of
icons generated by the macro <tt>Icon</tt>. Set to 0 for unlimited number of
icons (this will produce exhaustive network traffic--you have been
warned!)


</th>
<th><tt>32</tt></th></tr>
<tr><th><a href="#wikiextras-rbox_width-option">rbox_width</a></th>
<th>
Width of right aligned boxes.


</th>
<th><tt>300</tt></th></tr>
<tr><th><a href="#wikiextras-shadowless_boxes-option">shadowless_boxes</a></th>
<th>
Use shadowless boxes.


</th>
<th><tt>disabled</tt></th></tr>
<tr><th><a href="#wikiextras-shadowless_icons-option">shadowless_icons</a></th>
<th>
Use shadowless icons.


</th>
<th><tt>disabled</tt></th></tr>
<tr><th><a href="#wikiextras-showicons_limit-option">showicons_limit</a></th>
<th>
To prevent exhaustive network traffic, limit the maximum number of
icons generated by the macro <tt>ShowIcons</tt>. Set to 0 for
unlimited number of icons (this will produce exhaustive network
traffic--you have been warned!)


</th>
<th><tt>96</tt></th></tr>
<tr><th><a href="#wikiextras-todo_phrases-option">todo_phrases</a></th>
<th>
Analogous to <tt>FIXME</tt>-phrases, but presentation is less eye-catching.


</th>
<th><tt>REVIEW,TODO</tt></th></tr>
<tr><th><a href="#wikiextras-wide_toc-option">wide_toc</a></th>
<th>
Right aligned boxes with table of contents,
produced by the <tt>PageOutline</tt> macro, are either
as wide as ordinary right aligned boxes (<tt>true</tt>) or
narrow (<tt>false</tt>).


</th>
<th><tt>disabled</tt></th></tr></table>



---



See also: [TracGuide](trac-guide), [TracAdmin](trac-admin), [TracEnvironment](trac-environment)


