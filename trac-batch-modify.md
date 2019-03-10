# Trac Ticket Batch Modification


Trac supports modifying a batch of tickets in one request from [custom query](trac-query) results.


To perform a batch modification, select the tickets you wish to modify and set the new field values using the section underneath the query results. 

## List fields


The `Keywords` and `Cc` fields are treated as lists, where list items can be added and/or removed in addition of replacing the entire list value. All list field controls accept multiple items, such as multiple keywords or cc addresses.

## Excluded fields


Multi-line text fields are not supported, because no valid use-case has been presented for syncing them across several tickets. That restriction applies to the `Description` field as well as to any [custom field](trac-tickets-custom-fields#available-field-types-and-options) of type 'textarea'. However, future versions of Trac could support in conjunction with more suitable actions like 'prepend', 'append' or 'search & replace' ([ th:\#2415](http://trac-hacks.org/ticket/2415)).
