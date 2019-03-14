# TicketQuery Wiki Macro


The TicketQuery macro lets you display information on tickets within wiki pages.
The query language used by the `[[TicketQuery]]` macro is described in [TracQuery](trac-query#using-the-[[ticketquery]]-macro) page.

## Usage

### `[[TicketQuery]]`


Wiki macro listing tickets that match certain criteria.


This macro accepts a comma-separated list of keyed parameters,
in the form "key=value".


If the key is the name of a field, the value must use the syntax
of a filter specifier as defined in [TracQuery\#QueryLanguage](trac-query#query-language).
Note that this is *not* the same as the simplified URL syntax
used for `query:` links starting with a `?` character. Commas (`,`)
can be included in field values by escaping them with a backslash (`\`).


Groups of field constraints to be OR-ed together can be separated by a
literal `or` argument.


In addition to filters, several other named parameters can be used
to control how the results are presented. All of them are optional.


The `format` parameter determines how the list of tickets is
presented:

- **list** -- the default presentation is to list the ticket ID next
  to the summary, with each ticket on a separate line.
- **compact** -- the tickets are presented as a comma-separated
  list of ticket IDs.
- **count** -- only the count of matching tickets is displayed
- **rawcount** -- only the count of matching tickets is displayed,
  not even with a link to the corresponding query (*since 1.1.1*)
- **table**  -- a view similar to the custom query view (but without
  the controls)
- **progress** -- a view similar to the milestone progress bars


The `max` parameter can be used to limit the number of tickets shown
(defaults to **0**, i.e. no maximum).


The `order` parameter sets the field used for ordering tickets
(defaults to **id**).


The `desc` parameter indicates whether the order of the tickets
should be reversed (defaults to **false**).


The `group` parameter sets the field used for grouping tickets
(defaults to not being set).


The `groupdesc` parameter indicates whether the natural display
order of the groups should be reversed (defaults to **false**).


The `verbose` parameter can be set to a true value in order to
get the description for the listed tickets. For **table** format only.
*deprecated in favor of the `rows` parameter*


The `rows` parameter can be used to specify which field(s) should
be viewed as a row, e.g. `rows=description|summary`


The `col` parameter can be used to specify which fields should
be viewed as columns. For **table** format only.


For compatibility with Trac 0.10, if there's a last positional parameter
given to the macro, it will be used to specify the `format`.
Also, using "&" as a field separator still works (except for `order`)
but is deprecated.

## Example

<table><tr><th>**Example**</th>
<th>**Result**</th>
<th>**Macro**</th></tr>
<tr><th>Number of Triage tickets (Ticket query: status: new, milestone: , order: priority): 
</th>
<th>**2276 (Ticket query: status: new, milestone: , max: 0, order: id)**</th>
<th>`[[TicketQuery(status=new&milestone=,count)]]`</th></tr>
<tr><th>Number of new tickets: 
</th>
<th>**3007 (Ticket query: status: new, max: 0, order: id)**</th>
<th>`[[TicketQuery(status=new,count)]]`</th></tr>
<tr><th>Number of reopened tickets: 
</th>
<th>**0 (Ticket query: status: reopened, max: 0, order: id)**</th>
<th>`[[TicketQuery(status=reopened,count)]]`</th></tr>
<tr><th>Number of assigned tickets: 
</th>
<th>**0 (Ticket query: status: assigned, max: 0, order: id)**</th>
<th>`[[TicketQuery(status=assigned,count)]]`</th></tr>
<tr><th>Number of invalid tickets: 
</th>
<th>**1118 (Ticket query: status: closed, resolution: invalid, max: 0, order: id)**</th>
<th>`[[TicketQuery(status=closed,resolution=invalid,count)]]`</th></tr>
<tr><th>Number of worksforme tickets: 
</th>
<th>**360 (Ticket query: status: closed, resolution: worksforme, max: 0, order: id)**</th>
<th>`[[TicketQuery(status=closed,resolution=worksforme,count)]]`</th></tr>
<tr><th>Number of duplicate tickets: 
</th>
<th>**1340 (Ticket query: status: closed, resolution: duplicate, max: 0, order: id)**</th>
<th>`[[TicketQuery(status=closed,resolution=duplicate,count)]]`</th></tr>
<tr><th>Number of wontfix tickets: 
</th>
<th>**676 (Ticket query: status: closed, resolution: wontfix, max: 0, order: id)**</th>
<th>`[[TicketQuery(status=closed,resolution=wontfix,count)]]`</th></tr>
<tr><th>Number of fixed tickets: 
</th>
<th>**7839 (Ticket query: status: closed, resolution: fixed, max: 0, order: id)**</th>
<th>`[[TicketQuery(status=closed,resolution=fixed,count)]]`</th></tr>
<tr><th>Total number of tickets: 
</th>
<th>**15046 (Ticket query: max: 0, order: id)**</th>
<th>`[[TicketQuery(count)]]`</th></tr>
<tr><th>Number of tickets reported **or** owned by current user: 
</th>
<th>**7 (Ticket query: reporter: %24USER, or: , owner: %24USER, max: 0, order: id)**</th>
<th>`[[TicketQuery(reporter=$USER,or,owner=$USER,count)]]`</th></tr>
<tr><th>Number of tickets created this month: 
</th>
<th>**40 (Ticket query: time: thismonth.., max: 0, order: id)**</th>
<th>`[[TicketQuery(created=thismonth..,count)]]`</th></tr>
<tr><th>Last 3 modified tickets: 
</th>
<th>**[\#18](https://gitlab.haskell.org//ghc/ghc/issues/18), [\#19](https://gitlab.haskell.org//ghc/ghc/issues/19), [\#20](https://gitlab.haskell.org//ghc/ghc/issues/20)**</th>
<th>`[[TicketQuery(max=3,order=modified,desc=1,compact)]]`</th></tr>
<tr><th>
Details of ticket [\#1](https://gitlab.haskell.org//ghc/ghc/issues/1):

</th>
<th></th>
<th>`[[TicketQuery(id=1,col=id|owner|reporter,rows=summary,table)]]`

</th></tr>
<tr><th>

<table><tr><th>Ticket (Ticket query: id: 1, max: 0, col: id, col: owner, col: reporter, desc: 1, order: id, row: summary)</th>
<th>Owner (Ticket query: id: 1, max: 0, col: id, col: owner, col: reporter, order: owner, row: summary)</th>
<th>Reporter (Ticket query: id: 1, max: 0, col: id, col: owner, col: reporter, order: reporter, row: summary)</th></tr>
<tr><th>[\#1](https://gitlab.haskell.org//ghc/ghc/issues/1)</th>
<th>nobody</th>
<th>nobody</th></tr>
<tr><th>Summary</th>
<th>
                      
                        Implicit parameters cause strange behavi
                      
                    </th>
<th></th></tr></table>

</th>
<th></th>
<th></th></tr>
<tr><td></td>
<td></td>
<td></td></tr></table>

## Using the `[[TicketQuery]]` Macro


The [TicketQuery](http://trac.edgewall.org/intertrac/TicketQuery) macro lets you display lists of tickets matching certain criteria anywhere you can use [WikiFormatting](wiki-formatting).


Example:

```wiki
[[TicketQuery(version=0.6|0.7&resolution=duplicate)]]
```


This is displayed as:

> No results


Just like the [query: wiki links](trac-query#using-traclinks), the parameter of this macro expects a query string formatted according to the rules of the simple [ticket query language](trac-query#query-language). This also displays the link and description of a single ticket:

```wiki
[[TicketQuery(id=123)]]
```


This is displayed as:

> <table><tr><th>[\#123](https://gitlab.haskell.org//ghc/ghc/issues/123)</th>
> <td>Unix manual pages not in release bundles</td></tr></table>


A more compact representation without the ticket summaries is:

```wiki
[[TicketQuery(version=0.6|0.7&resolution=duplicate, compact)]]
```


This is displayed as:

> No results


If you wish to receive only the number of defects that match the query, use the `count` parameter:

```wiki
[[TicketQuery(version=0.6|0.7&resolution=duplicate, count)]]
```


This is displayed as:

> 0 (Ticket query: version: 0.6, version: 0.7, resolution: duplicate, max: 0, order: id)


A graphical use of the macro is with the `format=progress` attribute:

```wiki
[[TicketQuery(milestone=0.12.8&group=type,format=progress)]]
```


For example for one of the upcoming milestones, bars are shown by ticket type:

<table></table>

---


See also: [TracQuery](trac-query), [TracTickets](trac-tickets), [TracReports](trac-reports), [TracGuide](trac-guide)