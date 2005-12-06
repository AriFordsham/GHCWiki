# Trac Permissions


Trac uses a simple but flexible permission system to control what users can and can't access.


Permission privileges are managed using the [trac-admin](trac-admin) tool.


Regular visitors, non-authenticated users, accessing the system are assigned the default 
role (*user*) named `anonymous`. 
Assign permissions to the `anonymous` user to set privileges for non-authenticated/guest users.


In addition to these privileges users can be granted additional individual 
rights in effect when authenticated and logged into the system.

## Available Privileges


To enable all privileges for a user, use the `TRAC_ADMIN` permission. Having `TRAC_ADMIN` is like being `root` on a \*NIX system, it will let you do anything you want.


Otherwise, individual privileges can be assigned to users for the various different functional areas of Trac:

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
<th> Modify [ticket](trac-tickets) properties 
</th></tr>
<tr><th>`TICKET_MODIFY`</th>
<th> Includes both `TICKET_APPEND` and `TICKET_CHGPROP`, and in addition allows resolving [tickets](trac-tickets)</th></tr>
<tr><th>`TICKET_ADMIN`</th>
<th> All `TICKET_*` permissions, plus the deletion of ticket attachments. 
</th></tr></table>

### Roadmap

<table><tr><th>`MILESTONE_VIEW`</th>
<th> View a milestone 
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
<th> View the [roadmap](trac-roadmap) page 
</th></tr>
<tr><th>`ROADMAP_ADMIN`</th>
<th> Alias for `MILESTONE_ADMIN` (deprecated) 
</th></tr></table>

### Reports

<table><tr><th>`REPORT_VIEW`</th>
<th> View [reports](trac-reports)</th></tr>
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

### Others

<table><tr><th>`TIMELINE_VIEW`</th>
<th> View the [timeline](trac-timeline) page 
</th></tr>
<tr><th>`SEARCH_VIEW`</th>
<th> View and execute [search](trac-search) queries 
</th></tr>
<tr><th>`CONFIG_VIEW`</th>
<th> Enables additional pages on *About Trac* that show the current configuration or the list of installed plugins 
</th></tr></table>

## Granting Privileges


Currently the only way to grant privileges to users is by using the `trac-admin` script. The current set of privileges can be listed with the following command:

```wiki
  $ trac-admin /path/to/projenv permission list
```


This command will allow the user *bob* to delete reports:

```wiki
  $ trac-admin /path/to/projenv permission add bob REPORT_DELETE
```

## Permission Groups


Permissions can be grouped together to form roles such as *developer*, *admin*, etc.

```wiki
  $ trac-admin /path/to/projenv permission add developer WIKI_ADMIN
  $ trac-admin /path/to/projenv permission add developer REPORT_ADMIN
  $ trac-admin /path/to/projenv permission add developer TICKET_MODIFY
  $ trac-admin /path/to/projenv permission add bob developer
  $ trac-admin /path/to/projenv permission add john developer
```

## Default Permissions


Granting privileges to the special user *anonymous* can be used to control what an anonymous user can do before they have logged in.


In the same way, privileges granted to the special user *authenticated* will apply to any authenticated (logged in) user.

---


See also: [TracAdmin](trac-admin), [TracGuide](trac-guide)