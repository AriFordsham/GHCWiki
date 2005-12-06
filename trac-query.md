# Trac Ticket Queries


In addition to [reports](trac-reports), Trac provides support for *custom ticket queries*, used to display lists of tickets meeting a specified set of criteria. 


To configure and execute a custom query, switch to the *View Tickets* module from the navigation bar, and select the *Custom Query* link.

## Filters


When you first go to the query page the default filters will display all open tickets, or if you're logged in it will display open tickets assigned to you.  Current filters can be removed by clicking the button to the right with the minus sign on the label.  New filters are added from the pulldown list in the bottom-right corner of the filters box.  Filters with either a text box or a pulldown menu of options can be added multiple times to perform an *or* of the criteria.


You can use the fields just below the filters box to group the results based on a field, or display the full description for each ticket.


Once you've edited your filters click the *Update* button to refresh your results.

## Navigating Tickets


Clicking on one of the query results will take you to that ticket.  You can navigate through the results by clicking the *Next Ticket* or *Previous Ticket* links just below the main menu bar, or click the *Back to Query* link to return to the query page.  


You can safely edit any of the tickets and continue to navigate through the results using the *Next/Previous/Back to Query* links after saving your results.  When you return to the query any tickets you edited will be displayed with italicized text.  If one of the tickets was edited such that it no longer matches the query criteria the text will also be greyed.  The query results can be refreshed and cleared of these status indicators by clicking the *Update* button again.

## Saving Queries


While Trac does not yet allow saving a named query and somehow making it available in a navigable list, you can save references to queries in Wiki content, as described below.

### Using [TracLinks](trac-links)


You may want to save some queries so that you can come back to them later.  You can do this by making a link to the query from any Wiki page.

```wiki
[query:status!=closed&version=0.8 Active tickets against 0.8]
```


Which is displayed as:

> Active tickets against 0.8 (Ticket query: status: !closed, version: 0.8, order: priority)


This uses a very simple query language to specify the criteria (see [Query Language](trac-query#query-language)).


Alternatively, you can copy the query string of a query and paste that into the Wiki link, including the leading `?` character:

```wiki
[query:?status=assigned&group=owner Assigned tickets by owner]
```


Whis is displayed as:

> Assigned tickets by owner (Ticket query: status: assigned, group: owner)


The advantage of this approach is that you can also specify the grouping and ordering, which is not possible using the first syntax.

### Using the `[[TicketQuery]]` Macro


The `[[TicketQuery]]` macro lets you display lists of tickets matching certain criteria anywhere you can use [WikiFormatting](wiki-formatting).


Example:

```wiki
[[TicketQuery(version=0.9b1|0.9b2&resolution=duplicate)]]
```


This is displayed as:

> No results


Just like the [query: wiki links](trac-query#using-traclinks), the parameter of this macro expects a query string formatted according to the rules of the simple [ticket query language](trac-query#query-language).


A more compact representation without the ticket summaries is also available:

```wiki
[[TicketQuery(version=0.9b1|0.9b2&resolution=duplicate, compact)]]
```


This is displayed as:

> No results

### Query Language

`query:`[TracLinks](trac-links) and the `[[TicketQuery]]` macro both use a mini “query language” for specifying query filters. Basically, the filters are separate by ampersands (`&`). Each filter then consists of the ticket field name, an operator, and one or more values. More than one value are separated by a pipe (`|`), meaning that the filter matches any of the values.


The available operators are:

<table><tr><th>**=**</th>
<th> the field content exactly matches the one of the values 
</th></tr>
<tr><th>**\~=**</th>
<th> the field content contains one or more of the values 
</th></tr>
<tr><th>**\^=**</th>
<th> the field content starts with one of the values 
</th></tr>
<tr><th>**$=**</th>
<th> the field content ends with one of the values 
</th></tr></table>


All of these operators can also be negated:

<table><tr><th>**!=**</th>
<th> the field content matches none of the values 
</th></tr>
<tr><th>**!\~=**</th>
<th> the field content does not contain any of the values 
</th></tr>
<tr><th>**!\^=**</th>
<th> the field content does not start with any of the values 
</th></tr>
<tr><th>**!$=**</th>
<th> the field content does not end with any of the values 
</th></tr></table>

---


See also: [TracTickets](trac-tickets), [TracReports](trac-reports), [TracGuide](trac-guide)