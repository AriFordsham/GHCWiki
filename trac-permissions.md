# Trac Permissions


Trac uses a simple, case sensitive, permission system to control what users can and can't access.


Permission privileges are managed using the [trac-admin](trac-admin) tool or the *General / Permissions* panel in the *Admin* tab of the web interface.


In addition to the default permission policy described in this page, it is possible to activate additional permission policies by enabling plugins and listing them in the `[trac] permission_policies` configuration entry in the [TracIni](trac-ini). See [TracFineGrainedPermissions](trac-fine-grained-permissions) for more details.


Non-authenticated users accessing the system are assigned the name "anonymous". Assign permissions to the "anonymous" user to set privileges for anonymous/guest users. The parts of Trac that a user does not have the privileges for will not be displayed in the navigation.
In addition to these privileges, users can be granted additional individual rights in effect when authenticated and logged into the system. All logged in users belong to the virtual group "authenticated", which inherits permissions from "anonymous".

## Graphical Admin Tab


To access this tab, a user must have one of the following permissions: `TRAC_ADMIN`, `PERMISSION_ADMIN`, `PERMISSION_GRANT`, `PERMISSION_REVOKE`. The permissions can be granted using the `trac-admin` command (more on `trac-admin` below):

```wiki
  $ trac-admin /path/to/projenv permission add bob TRAC_ADMIN
```


Then, the user `bob` will be able to see the Admin tab, and can then access the permissions menu. This menu will allow you to perform all the following actions, but from the browser without requiring root access to the server (just the correct permissions for your user account). **Use at least one lowercase character in user names, as all-uppercase names are reserved for permissions.**

1. [](/trac/ghc/chrome/site/../common/guide/admin.png)
1. [](/trac/ghc/chrome/site/../common/guide/admin-permissions.png)
1. [](/trac/ghc/chrome/site/../common/guide/admin-permissions-TICKET_ADMIN.png)


An easy way to quickly secure a new Trac install is to run the above command on the anonymous user, install the [AccountManagerPlugin](http://trac-hacks.org/wiki/AccountManagerPlugin), create a new admin account graphically and then remove the TRAC_ADMIN permission from the anonymous user.


From the graphical admin tab, users with `PERMISSION_GRANT` will only be allowed to grant permissions that they possess, and users with `PERMISSION_REVOKE` will only be allowed to revoke permissions that they possess. For example, a user cannot grant `MILESTONE_ADMIN` unless they have `PERMISSION_GRANT` and `MILESTONE_ADMIN`, and they cannot revoke `MILESTONE_ADMIN` unless they have `PERMISSION_REVOKE` and `MILESTONE_ADMIN`. `PERMISSION_ADMIN` just grants the user both `PERMISSION_GRANT` and `PERMISSION_REVOKE`, and users with `TRAC_ADMIN` can grant or revoke any permission.

## Available Privileges


To enable all privileges for a user, use the `TRAC_ADMIN` permission. Having `TRAC_ADMIN` is like being `root` on a \*NIX system: it will allow you to perform any operation.



Otherwise, individual privileges can be assigned to users for the various different functional areas of Trac (**note that the privilege names are case-sensitive**):


### Repository Browser


<table><tr><th> <tt>BROWSER_VIEW</tt> </th>
<th> View directory listings in the <a href="trac-browser">repository browser</a> 
</th></tr>
<tr><th> <tt>LOG_VIEW</tt> </th>
<th> View revision logs of files and directories in the <a href="trac-browser">repository browser</a> 
</th></tr>
<tr><th> <tt>FILE_VIEW</tt> </th>
<th> View files in the <a href="trac-browser">repository browser</a> 
</th></tr>
<tr><th> <tt>CHANGESET_VIEW</tt> </th>
<th> View <a href="trac-changeset">repository check-ins</a> 
</th></tr></table>


### Ticket System


<table><tr><th> <tt>TICKET_VIEW</tt> </th>
<th> View existing <a href="trac-tickets">tickets</a> and perform <a href="trac-query">ticket queries</a> 
</th></tr>
<tr><th> <tt>TICKET_CREATE</tt> </th>
<th> Create new <a href="trac-tickets">tickets</a> 
</th></tr>
<tr><th> <tt>TICKET_APPEND</tt> </th>
<th> Add comments or attachments to <a href="trac-tickets">tickets</a> 
</th></tr>
<tr><th> <tt>TICKET_CHGPROP</tt> </th>
<th> Modify <a href="trac-tickets">ticket</a> properties (priority, assignment, keywords, etc.) with the following exceptions: edit description field, add/remove other users from cc field when logged in, and set email to pref 
</th></tr>
<tr><th> <tt>TICKET_MODIFY</tt> </th>
<th> Includes both <tt>TICKET_APPEND</tt> and <tt>TICKET_CHGPROP</tt>, and in addition allows resolving <a href="trac-tickets">tickets</a>. Tickets can be assigned to users through a <a href="trac-tickets#">drop-down list</a> when the list of possible owners has been restricted. 
</th></tr>
<tr><th> <tt>TICKET_EDIT_CC</tt> </th>
<th> Full modify cc field 
</th></tr>
<tr><th> <tt>TICKET_EDIT_DESCRIPTION</tt> </th>
<th> Modify description field 
</th></tr>
<tr><th> <tt>TICKET_EDIT_COMMENT</tt> </th>
<th> Modify another user&apos;s comments. Any user can modify their own comments by default. 
</th></tr>
<tr><th> <tt>TICKET_BATCH_MODIFY</tt> </th>
<th> <a href="trac-batch-modify">Batch modify</a> tickets 
</th></tr>
<tr><th> <tt>TICKET_ADMIN</tt> </th>
<th> All <tt>TICKET_*</tt> permissions, deletion of ticket attachments and modification of the reporter field, which grants ability to create a ticket on behalf of another user (it will appear that another user created the ticket). It also allows managing ticket properties through the web administration module. 
</th></tr></table>



Attention: the "view tickets" button appears with the `REPORT_VIEW` permission.


### Roadmap


<table><tr><th> <tt>MILESTONE_VIEW</tt> </th>
<th> View milestones and assign tickets to milestones. 
</th></tr>
<tr><th> <tt>MILESTONE_CREATE</tt> </th>
<th> Create a new milestone 
</th></tr>
<tr><th> <tt>MILESTONE_MODIFY</tt> </th>
<th> Modify existing milestones 
</th></tr>
<tr><th> <tt>MILESTONE_DELETE</tt> </th>
<th> Delete milestones 
</th></tr>
<tr><th> <tt>MILESTONE_ADMIN</tt> </th>
<th> All <tt>MILESTONE_*</tt> permissions 
</th></tr>
<tr><th> <tt>ROADMAP_VIEW</tt> </th>
<th> View the <a href="trac-roadmap">roadmap</a> page, is not (yet) the same as MILESTONE_VIEW, see <a href="http://trac.edgewall.org/intertrac/%234292"> #4292</a> 
</th></tr>
<tr><th> <tt>ROADMAP_ADMIN</tt> </th>
<th> to be removed with <a href="http://trac.edgewall.org/intertrac/%233022"> #3022</a>, replaced by MILESTONE_ADMIN 
</th></tr></table>


### Reports


<table><tr><th> <tt>REPORT_VIEW</tt> </th>
<th> View <a href="trac-reports">reports</a>, i.e. the &quot;view tickets&quot; link. 
</th></tr>
<tr><th> <tt>REPORT_SQL_VIEW</tt> </th>
<th> View the underlying SQL query of a <a href="trac-reports">report</a> 
</th></tr>
<tr><th> <tt>REPORT_CREATE</tt> </th>
<th> Create new <a href="trac-reports">reports</a> 
</th></tr>
<tr><th> <tt>REPORT_MODIFY</tt> </th>
<th> Modify existing <a href="trac-reports">reports</a> 
</th></tr>
<tr><th> <tt>REPORT_DELETE</tt> </th>
<th> Delete <a href="trac-reports">reports</a> 
</th></tr>
<tr><th> <tt>REPORT_ADMIN</tt> </th>
<th> All <tt>REPORT_*</tt> permissions 
</th></tr></table>


### Wiki System


<table><tr><th> <tt>WIKI_VIEW</tt> </th>
<th> View existing <a href="trac-wiki">wiki</a> pages 
</th></tr>
<tr><th> <tt>WIKI_CREATE</tt> </th>
<th> Create new <a href="trac-wiki">wiki</a> pages 
</th></tr>
<tr><th> <tt>WIKI_MODIFY</tt> </th>
<th> Change <a href="trac-wiki">wiki</a> pages 
</th></tr>
<tr><th> <tt>WIKI_RENAME</tt> </th>
<th> Rename <a href="trac-wiki">wiki</a> pages 
</th></tr>
<tr><th> <tt>WIKI_DELETE</tt> </th>
<th> Delete <a href="trac-wiki">wiki</a> pages and attachments 
</th></tr>
<tr><th> <tt>WIKI_ADMIN</tt> </th>
<th> All <tt>WIKI_*</tt> permissions, plus the management of <i>readonly</i> pages. 
</th></tr></table>


### Permissions


<table><tr><th> <tt>PERMISSION_GRANT</tt> </th>
<th> add/grant a permission 
</th></tr>
<tr><th> <tt>PERMISSION_REVOKE</tt> </th>
<th> remove/revoke a permission 
</th></tr>
<tr><th> <tt>PERMISSION_ADMIN</tt> </th>
<th> All <tt>PERMISSION_*</tt> permissions 
</th></tr></table>


### Others


<table><tr><th> <tt>TIMELINE_VIEW</tt> </th>
<th> View the <a href="trac-timeline">timeline</a> page 
</th></tr>
<tr><th> <tt>SEARCH_VIEW</tt> </th>
<th> View and execute <a href="trac-search">search</a> queries 
</th></tr>
<tr><th> <tt>CONFIG_VIEW</tt> </th>
<th> Enables additional pages on <i>About Trac</i> that show the current configuration or the list of installed plugins 
</th></tr>
<tr><th> <tt>EMAIL_VIEW</tt> </th>
<th> Shows email addresses even if <a href="trac-ini#">trac show_email_addresses</a> configuration option is false 
</th></tr></table>


## Creating New Privileges


To create custom permissions, for example to be used in a custom workflow, enable the optional [tracopt.perm.config_perm_provider.ExtraPermissionsProvider](http://trac.edgewall.org/intertrac/ExtraPermissionsProvider) component in the "Plugins" admin panel, and add the desired permissions to the `[extra-permissions]` section in your [trac.ini](trac-ini#). For more information, please refer to the documentation  on the [TracIni](trac-ini#) page after enabling the component.

## Granting Privileges


You grant privileges to users using [trac-admin](trac-admin). The current set of privileges can be listed with the following command:

```wiki
  $ trac-admin /path/to/projenv permission list
```


This command will allow the user *bob* to delete reports:

```wiki
  $ trac-admin /path/to/projenv permission add bob REPORT_DELETE
```


The `permission add` command also accepts multiple privilege names:

```wiki
  $ trac-admin /path/to/projenv permission add bob REPORT_DELETE WIKI_CREATE
```


Or add all privileges:

```wiki
  $ trac-admin /path/to/projenv permission add bob TRAC_ADMIN
```

## Permission Groups


There are two built-in groups, "authenticated" and "anonymous".
Any user who has not logged in is automatically in the "anonymous" group.
Any user who has logged in is also in the "authenticated" group.
The "authenticated" group inherits permissions from the "anonymous" group.
For example, if the "anonymous" group has permission WIKI_MODIFY, 
it is not necessary to add the WIKI_MODIFY permission to the "authenticated" group as well.


Custom groups may be defined that inherit permissions from the two built-in groups.


Permissions can be grouped together to form roles such as *developer*, *admin*, etc.

```wiki
  $ trac-admin /path/to/projenv permission add developer WIKI_ADMIN
  $ trac-admin /path/to/projenv permission add developer REPORT_ADMIN
  $ trac-admin /path/to/projenv permission add developer TICKET_MODIFY
  $ trac-admin /path/to/projenv permission add bob developer
  $ trac-admin /path/to/projenv permission add john developer
```


Group membership can be checked by doing a `permission list` with no further arguments; the resulting output will include group memberships. **Use at least one lowercase character in group names, as all-uppercase names are reserved for permissions**.

## Adding a New Group and Permissions


Permission groups can be created by assigning a user to a group you wish to create, then assign permissions to that group.


The following will add *bob* to the new group called *beta_testers* and then will assign WIKI_ADMIN permissions to that group. (Thus, *bob* will inherit the WIKI_ADMIN permission)

```wiki
   $ trac-admin /path/to/projenv permission add bob beta_testers
   $ trac-admin /path/to/projenv permission add beta_testers WIKI_ADMIN

```

## Removing Permissions


Permissions can be removed using the 'remove' command. For example:


This command will prevent the user *bob* from deleting reports:

```wiki
  $ trac-admin /path/to/projenv permission remove bob REPORT_DELETE
```


Just like `permission add`, this command accepts multiple privilege names.


You can also remove all privileges for a specific user:

```wiki
  $ trac-admin /path/to/projenv permission remove bob '*'
```


Or one privilege for all users:

```wiki
  $ trac-admin /path/to/projenv permission remove '*' REPORT_ADMIN
```

## Default Permissions


By default on a new Trac installation, the `anonymous` user will have *view* access to everything in Trac, but will not be able to create or modify anything.
On the other hand, the `authenticated` users will have the permissions to *create and modify tickets and wiki pages*.

**anonymous**

```wiki
 BROWSER_VIEW 
 CHANGESET_VIEW 
 FILE_VIEW 
 LOG_VIEW 
 MILESTONE_VIEW 
 REPORT_SQL_VIEW 
 REPORT_VIEW 
 ROADMAP_VIEW 
 SEARCH_VIEW 
 TICKET_VIEW 
 TIMELINE_VIEW
 WIKI_VIEW
```

**authenticated**

```wiki
 TICKET_CREATE 
 TICKET_MODIFY 
 WIKI_CREATE 
 WIKI_MODIFY  
```

---



See also: [TracAdmin](trac-admin), [TracGuide](trac-guide) and [TracFineGrainedPermissions](trac-fine-grained-permissions)


