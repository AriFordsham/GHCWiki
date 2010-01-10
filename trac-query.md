# Trac Ticket Queries


In addition to [reports](trac-reports), Trac provides support for *custom ticket queries*, used to display lists of tickets meeting a specified set of criteria. 


To configure and execute a custom query, switch to the *View Tickets* module from the navigation bar, and select the *Custom Query* link.

## Filters


When you first go to the query page the default filters will display all open tickets, or if you're logged in it will display open tickets assigned to you.  Current filters can be removed by clicking the button to the right with the minus sign on the label.  New filters are added from the pulldown list in the bottom-right corner of the filters box.  Filters with either a text box or a pulldown menu of options can be added multiple times to perform an *or* of the criteria.


You can use the fields just below the filters box to group the results based on a field, or display the full description for each ticket.


Once you've edited your filters click the *Update* button to refresh your results.

## Navigating Tickets


Clicking on one of the query results will take you to that ticket.  You can navigate through the results by clicking the *Next Ticket* or *Previous Ticket* links just below the main menu bar, or click the *Back to Query* link to return to the query page.  


You can safely edit any of the tickets and continue to navigate through the results using the *Next/Previous/Back to Query* links after saving your results.  When you return to the query *any tickets which were edited* will be displayed with italicized text.  If one of the tickets was edited such that it no longer matches the query criteria  the text will also be greyed. Lastly, if **a new ticket matching the query criteria has been created**, it will be shown in bold. 


The query results can be refreshed and cleared of these status indicators by clicking the *Update* button again.

## Saving Queries


While Trac does not yet allow saving a named query and somehow making it available in a navigable list, you can save references to queries in Wiki content, as described below.

### Using [TracLinks](trac-links)


You may want to save some queries so that you can come back to them later.  You can do this by making a link to the query from any Wiki page.

```wiki
[query:status=new|assigned|reopened&version=1.0 Active tickets against 1.0]
```


Which is displayed as:

> Active tickets against 1.0 (Ticket query: status: new, status: assigned, status: reopened, version: 1.0, order: priority)


This uses a very simple query language to specify the criteria (see [Query Language](trac-query#query-language)).


Alternatively, you can copy the query string of a query and paste that into the Wiki link, including the leading `?` character:

```wiki
[query:?status=new&status=assigned&status=reopened&group=owner Assigned tickets by owner]
```


Which is displayed as:

> Assigned tickets by owner (Ticket query: status: new, status: assigned, status: reopened, group: owner)

### Using the `[[TicketQuery]]` Macro


The [ TicketQuery](http://trac.edgewall.org/intertrac/TicketQuery) macro lets you display lists of tickets matching certain criteria anywhere you can use [WikiFormatting](wiki-formatting).


Example:

```wiki
[[TicketQuery(version=0.6|0.7&resolution=duplicate)]]
```


This is displayed as:

> No results


Just like the [query: wiki links](trac-query#using-traclinks), the parameter of this macro expects a query string formatted according to the rules of the simple [ticket query language](trac-query#query-language).


A more compact representation without the ticket summaries is also available:

```wiki
[[TicketQuery(version=0.6|0.7&resolution=duplicate, compact)]]
```


This is displayed as:

> No results


Finally if you wish to receive only the number of defects that match the query using the ``count`` parameter.

```wiki
[[TicketQuery(version=0.6|0.7&resolution=duplicate, count)]]
```


This is displayed as:

> 0 (Ticket query: version: 0.6, version: 0.7, resolution: duplicate, max: 0, order: id)

### Customizing the *table* format


You can also customize the columns displayed in the table format (*format=table*) by using *col=\<field\>* - you can specify multiple fields and what order they are displayed by placing pipes (`|`) between the columns like below:

```wiki
[[TicketQuery(max=3,status=closed,order=id,desc=1,format=table,col=resolution|summary|owner|reporter)]]
```


This is displayed as:

##
    Results (1 - 3 of 11842)

12 (Ticket query: status: closed, max: 3, page: 2, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id)3 (Ticket query: status: closed, max: 3, page: 3, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id)4 (Ticket query: status: closed, max: 3, page: 4, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id)5 (Ticket query: status: closed, max: 3, page: 5, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id)6 (Ticket query: status: closed, max: 3, page: 6, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id)7 (Ticket query: status: closed, max: 3, page: 7, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id)8 (Ticket query: status: closed, max: 3, page: 8, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id)9 (Ticket query: status: closed, max: 3, page: 9, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id)10 (Ticket query: status: closed, max: 3, page: 10, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id)11 (Ticket query: status: closed, max: 3, page: 11, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id)→ (Ticket query: status: closed, max: 3, page: 2, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id)

<table><tr><th>Ticket (Ticket query: status: closed, max: 3, col: id, col: resolution, col: summary, col: owner, col: reporter, order: id)</th>
<th>Resolution (Ticket query: status: closed, max: 3, col: id, col: resolution, col: summary, col: owner, col: reporter, order: resolution)</th>
<th>Summary (Ticket query: status: closed, max: 3, col: id, col: resolution, col: summary, col: owner, col: reporter, order: summary)</th>
<th>Owner (Ticket query: status: closed, max: 3, col: id, col: resolution, col: summary, col: owner, col: reporter, order: owner)</th>
<th>Reporter (Ticket query: status: closed, max: 3, col: id, col: resolution, col: summary, col: owner, col: reporter, order: reporter)</th></tr>
<tr><th>[\#16410](https://gitlab.haskell.org//ghc/ghc/issues/16410)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      duplicate
                    </th>
<th>[Order of declarations matters](https://gitlab.haskell.org//ghc/ghc/issues/16410)</th>
<th></th>
<th>Iceland_jack</th></tr>
<tr><th>[\#16401](https://gitlab.haskell.org//ghc/ghc/issues/16401)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      duplicate
                    </th>
<th>[Bad error message if we use TypeApplications with an identifier that doesn't exist](https://gitlab.haskell.org//ghc/ghc/issues/16401)</th>
<th></th>
<th>Fuuzetsu</th></tr>
<tr><th>[\#16394](https://gitlab.haskell.org//ghc/ghc/issues/16394)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      fixed
                    </th>
<th>[GHC internal error while typechecking of instance definition](https://gitlab.haskell.org//ghc/ghc/issues/16394)</th>
<th></th>
<th>Day1721</th></tr></table>

12 (Ticket query: status: closed, max: 3, page: 2, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id)3 (Ticket query: status: closed, max: 3, page: 3, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id)4 (Ticket query: status: closed, max: 3, page: 4, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id)5 (Ticket query: status: closed, max: 3, page: 5, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id)6 (Ticket query: status: closed, max: 3, page: 6, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id)7 (Ticket query: status: closed, max: 3, page: 7, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id)8 (Ticket query: status: closed, max: 3, page: 8, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id)9 (Ticket query: status: closed, max: 3, page: 9, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id)10 (Ticket query: status: closed, max: 3, page: 10, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id)11 (Ticket query: status: closed, max: 3, page: 11, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id)→ (Ticket query: status: closed, max: 3, page: 2, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id)

#### Full rows


In *table* format you can also have full rows by using *rows=\<field\>* like below:

```wiki
[[TicketQuery(max=3,status=closed,order=id,desc=1,format=table,col=resolution|summary|owner|reporter,rows=description)]]
```


This is displayed as:

##
    Results (1 - 3 of 11842)

12 (Ticket query: status: closed, max: 3, page: 2, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id, row: description)3 (Ticket query: status: closed, max: 3, page: 3, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id, row: description)4 (Ticket query: status: closed, max: 3, page: 4, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id, row: description)5 (Ticket query: status: closed, max: 3, page: 5, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id, row: description)6 (Ticket query: status: closed, max: 3, page: 6, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id, row: description)7 (Ticket query: status: closed, max: 3, page: 7, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id, row: description)8 (Ticket query: status: closed, max: 3, page: 8, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id, row: description)9 (Ticket query: status: closed, max: 3, page: 9, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id, row: description)10 (Ticket query: status: closed, max: 3, page: 10, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id, row: description)11 (Ticket query: status: closed, max: 3, page: 11, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id, row: description)→ (Ticket query: status: closed, max: 3, page: 2, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id)

<table><tr><th>Ticket (Ticket query: status: closed, max: 3, col: id, col: resolution, col: summary, col: owner, col: reporter, order: id, row: description)</th>
<th>Resolution (Ticket query: status: closed, max: 3, col: id, col: resolution, col: summary, col: owner, col: reporter, order: resolution, row: description)</th>
<th>Summary (Ticket query: status: closed, max: 3, col: id, col: resolution, col: summary, col: owner, col: reporter, order: summary, row: description)</th>
<th>Owner (Ticket query: status: closed, max: 3, col: id, col: resolution, col: summary, col: owner, col: reporter, order: owner, row: description)</th>
<th>Reporter (Ticket query: status: closed, max: 3, col: id, col: resolution, col: summary, col: owner, col: reporter, order: reporter, row: description)</th></tr>
<tr><th>[\#16410](https://gitlab.haskell.org//ghc/ghc/issues/16410)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      duplicate
                    </th>
<th>[Order of declarations matters](https://gitlab.haskell.org//ghc/ghc/issues/16410)</th>
<th></th>
<th>Iceland_jack</th></tr>
<tr><th>Description</th>
<th>
This piece of code works.

```
{-# Language DataKinds    #-}{-# Language GADTs        #-}{-# Language InstanceSigs #-}{-# Language PolyKinds    #-}{-# Language TypeFamilies #-}importData.KindclassCategory(tag::Type)wheretypeStrip tag ::TypeclassCategory tag =>Stripped tag wheretypeHom tag::Strip tag ->Strip tag ->TypeinstanceCategory()wheretypeStrip()=()instanceStripped()wheretypeHom()=Unit1dataUnit1::()->()->TypewhereU1::Unit1'()'()dataTagdataUnit2::()->()->TypewhereU2::Unit2'()'()instanceCategoryTagwheretypeStripTag=()instanceStrippedTagwheretypeHomTag=Unit2
```


Note that `Unit1` and `Unit2` are declared identically, separated by `data Tag`. The order is important.


If we change the last line to `Hom Tag = Unit1` (same as `Hom ()`) we get

```wiki
$ ghc -ignore-dot-ghci 1152_bug.hs
GHCi, version 8.7.20190115: https://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( 1152_bug.hs, interpreted )

1152_bug.hs:28:18: error:
    • Expected kind ‘Strip Tag -> Strip Tag -> *’,
        but ‘Unit1’ has kind ‘() -> () -> *’
    • In the type ‘Unit1’
      In the type instance declaration for ‘Hom’
      In the instance declaration for ‘Stripped Tag’
   |
28 |  type Hom Tag = Unit1
   |                  ^^^^^
Failed, no modules loaded.
Prelude>
```


even though `Strip Tag` is defined to equal `()`. Here comes the weird part.


If I move `Unit1` decl beneath `data Tag`

```
dataTagdataUnit1::()->()->TypewhereU1::Unit1'()'()dataUnit2::()->()->TypewhereU2::Unit2'()'()
```


then `type Hom Tag = Unit1` and `type Hom Tag = Unit2` both work well.


If I move `Unit2` decl above `data Tag` then both `Hom Tag = Unit1` and `Hom Tag = Unit2` fail!

```
dataUnit1::()->()->TypewhereU1::Unit1'()'()dataUnit2::()->()->TypewhereU2::Unit2'()'()dataTag
```


If I replace all occurrences of `Tag` with `Bool` it succeeds.

</th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>[\#16401](https://gitlab.haskell.org//ghc/ghc/issues/16401)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      duplicate
                    </th>
<th>[Bad error message if we use TypeApplications with an identifier that doesn't exist](https://gitlab.haskell.org//ghc/ghc/issues/16401)</th>
<th></th>
<th>Fuuzetsu</th></tr>
<tr><th>Description</th>
<th>```
{-# LANGUAGE TypeApplications #-}{-# LANGUAGE DataKinds #-}moduleBadErrorwherefoo::Intfoo= doesNotExist @123{-
[1 of 1] Compiling BadError         ( BadError.hs, BadError.o )

BadError.hs:6:7: error:
    • Cannot apply expression of type ‘t1’
      to a visible type argument ‘123’
    • In the expression: doesNotExist @123
      In an equation for ‘foo’: foo = doesNotExist @123
  |
6 | foo = doesNotExist @123
-}
```


What I would expect is for it to say that `doesNotExist` is not bound as one might reasonably expect. This is very, very confusing and easy to hit in real life. If you are exposing a function `f`, using it somewhere with `f @123` but then later decide to remove or rename `f`, the error message doesn't suggest at all that `f` has disappeared and leaves the user scratching their head.

</th>
<th></th>
<th></th>
<th></th></tr>
<tr><th>[\#16394](https://gitlab.haskell.org//ghc/ghc/issues/16394)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      fixed
                    </th>
<th>[GHC internal error while typechecking of instance definition](https://gitlab.haskell.org//ghc/ghc/issues/16394)</th>
<th></th>
<th>Day1721</th></tr>
<tr><th>Description</th>
<th>
Hello. 

This code won't typecheck because of GHC internal error.

```
{-# LANGUAGE PolyKinds, TypeFamilies, DataKinds #-}classC a wheretypeT(n :: a)--       v--DIFF--vinstanceC a =>C b =>C(a, b)wheretypeT'(n, m)=(T n,T m)
```


with error message: 

```wiki
Bug.hs:7:10: error:
    • GHC internal error: ‘T’ is not in scope during type checking, but it passed the renamer
      tcl_env of environment: [a1LS :-> Type variable ‘a’ = a :: *,
                               a1LT :-> Type variable ‘b’ = b :: *]
    • In the type instance declaration for ‘T’
      In the instance declaration for ‘C b => C (a, b)’
  |
7 |     type T (n, m) = (T n, T m)
  |          ^
Failed, no modules loaded.

```


but this works fine:

```
{-# LANGUAGE PolyKinds, TypeFamilies, DataKinds #-}classC a wheretypeT(n :: a)--       v--DIFF--vinstance(C a,C b)=>C(a, b)wheretypeT'(n, m)=(T n,T m)
```


Not sure is a bug, but either way it would be better to make more understandable error message

</th>
<th></th>
<th></th>
<th></th></tr></table>

12 (Ticket query: status: closed, max: 3, page: 2, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id, row: description)3 (Ticket query: status: closed, max: 3, page: 3, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id, row: description)4 (Ticket query: status: closed, max: 3, page: 4, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id, row: description)5 (Ticket query: status: closed, max: 3, page: 5, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id, row: description)6 (Ticket query: status: closed, max: 3, page: 6, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id, row: description)7 (Ticket query: status: closed, max: 3, page: 7, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id, row: description)8 (Ticket query: status: closed, max: 3, page: 8, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id, row: description)9 (Ticket query: status: closed, max: 3, page: 9, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id, row: description)10 (Ticket query: status: closed, max: 3, page: 10, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id, row: description)11 (Ticket query: status: closed, max: 3, page: 11, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id, row: description)→ (Ticket query: status: closed, max: 3, page: 2, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id)

### Query Language

`query:`[TracLinks](trac-links) and the `[[TicketQuery]]` macro both use a mini “query language” for specifying query filters. Basically, the filters are separated by ampersands (`&`). Each filter then consists of the ticket field name, an operator, and one or more values. More than one value are separated by a pipe (`|`), meaning that the filter matches any of the values.


The available operators are:

<table><tr><th>**`=`**</th>
<th> the field content exactly matches the one of the values 
</th></tr>
<tr><th>**`~=`**</th>
<th> the field content contains one or more of the values 
</th></tr>
<tr><th>**`^=`**</th>
<th> the field content starts with one of the values 
</th></tr>
<tr><th>**`$=`**</th>
<th> the field content ends with one of the values 
</th></tr></table>


All of these operators can also be negated:

<table><tr><th>**`!=`**</th>
<th> the field content matches none of the values 
</th></tr>
<tr><th>**`!~=`**</th>
<th> the field content does not contain any of the values 
</th></tr>
<tr><th>**`!^=`**</th>
<th> the field content does not start with any of the values 
</th></tr>
<tr><th>**`!$=`**</th>
<th> the field content does not end with any of the values 
</th></tr></table>

---


See also: [TracTickets](trac-tickets), [TracReports](trac-reports), [TracGuide](trac-guide)