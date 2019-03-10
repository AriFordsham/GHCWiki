# Trac Permissions


Trac uses a simple, case sensitive, permission system to control what users can and can't access.


Permission privileges are managed using the [trac-admin](trac-admin) tool or (new in version 0.11) the *General / Permissions* panel in the *Admin* tab of the web interface.


In addition to the default permission policy described in this page, it is possible to activate additional permission policies by enabling plugins and listing them in the `[trac] permission_policies` configuration entry in the [TracIni](trac-ini). See [TracFineGrainedPermissions](trac-fine-grained-permissions) for more details.


Non-authenticated users accessing the system are assigned the name "anonymous". Assign permissions to the "anonymous" user to set privileges for anonymous/guest users. The parts of Trac that a user does not have the privileges for will not be displayed in the navigation.
In addition to these privileges, users can be granted additional individual rights in effect when authenticated and logged into the system. All logged in users belong to the virtual group "authenticated", which inherits permissions from "anonymous".

## Graphical Admin Tab

*This feature is new in version 0.11.*


To access this tab, a user must have `TRAC_ADMIN privileges`. This can be performed as follows (more on the trac-admin script below):

```wiki
  $ trac-admin /path/to/projenv permission add bob TRAC_ADMIN
```


Then, the user `bob` will be able to see the Admin tab, and can then access the permissions menu. This menu will allow you to perform all the following actions, but from the browser without requiring root access to the server (just the correct permissions for your user account).


An easy way to quickly secure a new Trac install is to run the above command on the anonymous user, install the [ AccountManagerPlugin](http://trac-hacks.org/wiki/AccountManagerPlugin), create a new admin account graphically and then remove the TRAC_ADMIN permission from the anonymous user.

## Available Privileges


To enable all privileges for a user, use the `TRAC_ADMIN` permission. Having `TRAC_ADMIN` is like being `root` on a \*NIX system: it will allow you to perform any operation.


Otherwise, individual privileges can be assigned to users for the various different functional areas of Trac (**note that the privilege names are case-sensitive**):

### Repository Browser

<table><tr><th>`BROWSER_VIEW`</th>
<th> View directory listings in the [repository browser](trac-browser)</th></tr>
<tr><th>`LOG_VIEW`</th>
<th> View revision logs of files and directories in the [repository browser](trac-browser)</th></tr>
<tr><th>`FILE_VIEW`</th>
<th> View files in the [repository browser](trac-browser)</th></tr>
<tr><th>`CHANGESET_VIEW`</th>
<th> View [repository check-ins](trac-changeset)</th></tr></table>

### Ticket System

<table><tr><th>`TICKET_VIEW`</th>
<th> View existing [tickets](trac-tickets) and perform [ticket queries](trac-query)</th></tr>
<tr><th>`TICKET_CREATE`</th>
<th> Create new [tickets](trac-tickets)</th></tr>
<tr><th>`TICKET_APPEND`</th>
<th> Add comments or attachments to [tickets](trac-tickets)</th></tr>
<tr><th>`TICKET_CHGPROP`</th>
<th> Modify [ticket](trac-tickets) properties (priority, assignment, keywords, etc.) with the following exceptions: edit description field, add/remove other users from cc field when logged in, and set email to pref 
</th></tr>
<tr><th>`TICKET_MODIFY`</th>
<th> Includes both `TICKET_APPEND` and `TICKET_CHGPROP`, and in addition allows resolving [tickets](trac-tickets). Tickets can be assigned to users through a [drop-down list](trac-tickets#) when the list of possible owners has been restricted. 
</th></tr>
<tr><th>`TICKET_EDIT_CC`</th>
<th> Full modify cc field 
</th></tr>
<tr><th>`TICKET_EDIT_DESCRIPTION`</th>
<th> Modify description field 
</th></tr>
<tr><th>`TICKET_ADMIN`</th>
<th> All `TICKET_*` permissions, plus the deletion of ticket attachments and modification of the reporter and description fields. It also allows managing ticket properties in the WebAdmin panel. 
</th></tr></table>


Attention: the "view tickets" button appears with the `REPORT_VIEW` permission.

### Roadmap

<table><tr><th>`MILESTONE_VIEW`</th>
<th> View milestones and assign tickets to milestones. 
</th></tr>
<tr><th>`MILESTONE_CREATE`</th>
<th> Create a new milestone 
</th></tr>
<tr><th>`MILESTONE_MODIFY`</th>
<th> Modify existing milestones 
</th></tr>
<tr><th>`MILESTONE_DELETE`</th>
<th> Delete milestones 
</th></tr>
<tr><th>`MILESTONE_ADMIN`</th>
<th> All `MILESTONE_*` permissions 
</th></tr>
<tr><th>`ROADMAP_VIEW`</th>
<th> View the [roadmap](trac-roadmap) page, is not (yet) the same as MILESTONE_VIEW, see [ \#4292](http://trac.edgewall.org/intertrac/%234292)</th></tr>
<tr><th>`ROADMAP_ADMIN`</th>
<th> to be removed with [ \#3022](http://trac.edgewall.org/intertrac/%233022), replaced by MILESTONE_ADMIN 
</th></tr></table>

### Reports

<table><tr><th>`REPORT_VIEW`</th>
<th> View [reports](trac-reports), i.e. the "view tickets" link. 
</th></tr>
<tr><th>`REPORT_SQL_VIEW`</th>
<th> View the underlying SQL query of a [report](trac-reports)</th></tr>
<tr><th>`REPORT_CREATE`</th>
<th> Create new [reports](trac-reports)</th></tr>
<tr><th>`REPORT_MODIFY`</th>
<th> Modify existing [reports](trac-reports)</th></tr>
<tr><th>`REPORT_DELETE`</th>
<th> Delete [reports](trac-reports)</th></tr>
<tr><th>`REPORT_ADMIN`</th>
<th> All `REPORT_*` permissions 
</th></tr></table>

### Wiki System

<table><tr><th>`WIKI_VIEW`</th>
<th> View existing [wiki](trac-wiki) pages 
</th></tr>
<tr><th>`WIKI_CREATE`</th>
<th> Create new [wiki](trac-wiki) pages 
</th></tr>
<tr><th>`WIKI_MODIFY`</th>
<th> Change [wiki](trac-wiki) pages 
</th></tr>
<tr><th>`WIKI_DELETE`</th>
<th> Delete [wiki](trac-wiki) pages and attachments 
</th></tr>
<tr><th>`WIKI_ADMIN`</th>
<th> All `WIKI_*` permissions, plus the management of *readonly* pages. 
</th></tr></table>

### Permissions

<table><tr><th>`PERMISSION_GRANT`</th>
<th> add/grant a permission 
</th></tr>
<tr><th>`PERMISSION_REVOKE`</th>
<th> remove/revoke a permission 
</th></tr>
<tr><th>`PERMISSION_ADMIN`</th>
<th> All `PERMISSION_*` permissions 
</th></tr></table>

### Others

<table><tr><th>`TIMELINE_VIEW`</th>
<th> View the [timeline](trac-timeline) page 
</th></tr>
<tr><th>`SEARCH_VIEW`</th>
<th> View and execute [search](trac-search) queries 
</th></tr>
<tr><th>`CONFIG_VIEW`</th>
<th> Enables additional pages on *About Trac* that show the current configuration or the list of installed plugins 
</th></tr>
<tr><th>`EMAIL_VIEW`</th>
<th> Shows email addresses even if [ trac show_email_addresses configuration option is false](http://trac.edgewall.org/intertrac/wiki%3A0.11/TracIni)</th></tr></table>

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

eg. if the "anonymous" group has permission WIKI_MODIFY, it's not necessary to add the WIKI_MODIFY permisison to the "authenticated" group as well.


Custom groups may be defined that inherit permissions from the two built-in groups.


Permissions can be grouped together to form roles such as *developer*, *admin*, etc.

```wiki
  $ trac-admin /path/to/projenv permission add developer WIKI_ADMIN
  $ trac-admin /path/to/projenv permission add developer REPORT_ADMIN
  $ trac-admin /path/to/projenv permission add developer TICKET_MODIFY
  $ trac-admin /path/to/projenv permission add bob developer
  $ trac-admin /path/to/projenv permission add john developer
```


Group membership can be checked by doing a `permission list` with no further arguments; the resulting output will include group memberships. **Use lowercase for group names, as uppercase is reserved for permissions**.

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