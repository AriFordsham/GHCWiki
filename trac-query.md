# Trac Ticket Queries


In addition to [reports](trac-reports), Trac provides support for *custom ticket queries*, which can be used to display tickets that meet specified criteria. 


To configure and execute a custom query, switch to the *View Tickets* module from the navigation bar, and select the *Custom Query* link.

## Filters


When you first go to the query page, the default filter will display tickets relevant to you:

- If logged in then all open tickets, it will display open tickets assigned to you.
- If not logged in but you have specified a name or email address in the preferences, then it will display all open tickets where your email (or name if email not defined) is in the CC list.
- If not logged in and no name/email is defined in the preferences, then all open issues are displayed.


Current filters can be removed by clicking the button to the left with the minus sign on the label. New filters are added from the pulldown lists at the bottom corners of the filters box; 'And' conditions on the left, 'Or' conditions on the right.  Filters with either a text box or a pulldown menu of options can be added multiple times to perform an *Or* on the criteria.


You can use the fields just below the filters box to group the results based on a field, or display the full description for each ticket.


After you have edited your filters, click the *Update* button to refresh your results.


Some shortcuts can be used to manipulate *checkbox* filters.

- Clicking on a filter row label toggles all checkboxes.
- Pressing the modifier key while clicking on a filter row label inverts the state of all checkboxes.
- Pressing the modifier key while clicking on a checkbox selects the checkbox and deselects all other checkboxes in the filter.


The modifier key is platform and browser dependent. On Mac the modified key is Option/Alt or Command. On Linux the modifier key is Ctrl + Alt. Opera on Windows seems to use Ctrl + Alt, while Alt is effective for other Windows browsers.

## Navigating Tickets


Clicking on one of the query results will take you to that ticket. You can navigate through the results by clicking the *Next Ticket* or *Previous Ticket* links just below the main menu bar, or click the *Back to Query* link to return to the query page.  


You can safely edit any of the tickets and continue to navigate through the results using the *Next/Previous/Back to Query* links after saving your results. When you return to the query *any tickets which were edited* will be displayed with italicized text. If one of the tickets was edited such that it no longer matches the query criteria , the text will also be greyed. Lastly, if **a new ticket matching the query criteria has been created**, it will be shown in bold. 


The query results can be refreshed and cleared of these status indicators by clicking the *Update* button again.

## Saving Queries


Trac allows you to save the query as a named query accessible from the reports module. To save a query ensure that you have *Updated* the view and then click the *Save query* button displayed beneath the results.
You can also save references to queries in Wiki content, as described below.

**Note:** one way to easily build queries like the ones below, you can build and test the queries in the Custom report module and when ready - click *Save query*. This will build the query string for you. All you need to do is remove the extra line breaks.

**Note:** you must have the **REPORT_CREATE** permission in order to save queries to the list of default reports. The *Save query* button will only appear if you are logged in as a user that has been granted this permission. If your account does not have permission to create reports, you can still use the methods below to save a query.

### Using [TracLinks](trac-links)


You may want to save some queries so that you can come back to them later. You can do this by making a link to the query from any Wiki page.

```wiki
[query:status=new|assigned|reopened&version=1.0 Active tickets against 1.0]
```


Which is displayed as:


>
>
> Active tickets against 1.0 (Ticket query: status: new, status: assigned, status: reopened, version: 1.0, order: priority)
>
>


This uses a very simple query language to specify the criteria, see [Query Language](trac-query#query-language).


Alternatively, you can copy the query string of a query and paste that into the Wiki link, including the leading `?` character:

```wiki
[query:?status=new&status=assigned&status=reopened&group=owner Assigned tickets by owner]
```


Which is displayed as:


>
>
> Assigned tickets by owner (Ticket query: status: new, status: assigned, status: reopened, group: owner)
>
>

### Customizing the *table* format


You can also customize the columns displayed in the table format (*format=table*) by using *col=\<field\>*. You can specify multiple fields and what order they are displayed in by placing pipes (`|`) between the columns:

```wiki
[[TicketQuery(max=3,status=closed,order=id,desc=1,format=table,col=resolution|summary|owner|reporter)]]
```


This is displayed as:


  

##
    Results (1 - 3 of 11843)
  


  



    
    
      
        1
      
      
    
      
      
        2 (Ticket query: status: closed, max: 3, page: 2, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id)
      
    
      
      
        3 (Ticket query: status: closed, max: 3, page: 3, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id)
      
    
      
      
        4 (Ticket query: status: closed, max: 3, page: 4, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id)
      
    
      
      
        5 (Ticket query: status: closed, max: 3, page: 5, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id)
      
    
      
      
        6 (Ticket query: status: closed, max: 3, page: 6, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id)
      
    
      
      
        7 (Ticket query: status: closed, max: 3, page: 7, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id)
      
    
      
      
        8 (Ticket query: status: closed, max: 3, page: 8, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id)
      
    
      
      
        9 (Ticket query: status: closed, max: 3, page: 9, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id)
      
    
      
      
        10 (Ticket query: status: closed, max: 3, page: 10, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id)
      
    
      
      
        11 (Ticket query: status: closed, max: 3, page: 11, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id)
      
    
    
      → (Ticket query: status: closed, max: 3, page: 2, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id)
    


  
  
  
    
  
  

<table><tr><td>
      </td>
<th>
        
        Ticket (Ticket query: status: closed, max: 3, col: id, col: resolution, col: summary, col: owner, col: reporter, order: id)
      </th>
<th>
        
        Resolution (Ticket query: status: closed, max: 3, col: id, col: resolution, col: summary, col: owner, col: reporter, order: resolution)
      </th>
<th>
        
        Summary (Ticket query: status: closed, max: 3, col: id, col: resolution, col: summary, col: owner, col: reporter, order: summary)
      </th>
<th>
        
        Owner (Ticket query: status: closed, max: 3, col: id, col: resolution, col: summary, col: owner, col: reporter, order: owner)
      </th>
<th>
        
        Reporter (Ticket query: status: closed, max: 3, col: id, col: resolution, col: summary, col: owner, col: reporter, order: reporter)
      </th>
<td>
    </td>
<td></td>
<td></td>
<td></td>
<td></td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org/ghc/ghc/issues/16410">#16410</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      duplicate
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org/ghc/ghc/issues/16410">Order of declarations matters</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      Iceland_jack
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org/ghc/ghc/issues/16401">#16401</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      duplicate
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org/ghc/ghc/issues/16401">Bad error message if we use TypeApplications with an identifier that doesn&apos;t exist</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      Fuuzetsu
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org/ghc/ghc/issues/16394">#16394</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      fixed
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org/ghc/ghc/issues/16394">GHC internal error while typechecking of instance definition</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      Day1721
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr></table>


  



    
    
      
        1
      
      
    
      
      
        2 (Ticket query: status: closed, max: 3, page: 2, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id)
      
    
      
      
        3 (Ticket query: status: closed, max: 3, page: 3, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id)
      
    
      
      
        4 (Ticket query: status: closed, max: 3, page: 4, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id)
      
    
      
      
        5 (Ticket query: status: closed, max: 3, page: 5, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id)
      
    
      
      
        6 (Ticket query: status: closed, max: 3, page: 6, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id)
      
    
      
      
        7 (Ticket query: status: closed, max: 3, page: 7, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id)
      
    
      
      
        8 (Ticket query: status: closed, max: 3, page: 8, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id)
      
    
      
      
        9 (Ticket query: status: closed, max: 3, page: 9, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id)
      
    
      
      
        10 (Ticket query: status: closed, max: 3, page: 10, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id)
      
    
      
      
        11 (Ticket query: status: closed, max: 3, page: 11, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id)
      
    
    
      → (Ticket query: status: closed, max: 3, page: 2, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id)
    




#### Full rows


In *table* format you can also have full rows by using *rows=\<field\>*:

```wiki
[[TicketQuery(max=3,status=closed,order=id,desc=1,format=table,col=resolution|summary|owner|reporter,rows=description)]]
```


This is displayed as:


  

##
    Results (1 - 3 of 11843)
  


  



    
    
      
        1
      
      
    
      
      
        2 (Ticket query: status: closed, max: 3, page: 2, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id, row: description)
      
    
      
      
        3 (Ticket query: status: closed, max: 3, page: 3, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id, row: description)
      
    
      
      
        4 (Ticket query: status: closed, max: 3, page: 4, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id, row: description)
      
    
      
      
        5 (Ticket query: status: closed, max: 3, page: 5, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id, row: description)
      
    
      
      
        6 (Ticket query: status: closed, max: 3, page: 6, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id, row: description)
      
    
      
      
        7 (Ticket query: status: closed, max: 3, page: 7, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id, row: description)
      
    
      
      
        8 (Ticket query: status: closed, max: 3, page: 8, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id, row: description)
      
    
      
      
        9 (Ticket query: status: closed, max: 3, page: 9, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id, row: description)
      
    
      
      
        10 (Ticket query: status: closed, max: 3, page: 10, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id, row: description)
      
    
      
      
        11 (Ticket query: status: closed, max: 3, page: 11, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id, row: description)
      
    
    
      → (Ticket query: status: closed, max: 3, page: 2, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id)
    


  
  
  
    
  
  

<table><tr><td>
      </td>
<th>
        
        Ticket (Ticket query: status: closed, max: 3, col: id, col: resolution, col: summary, col: owner, col: reporter, order: id, row: description)
      </th>
<th>
        
        Resolution (Ticket query: status: closed, max: 3, col: id, col: resolution, col: summary, col: owner, col: reporter, order: resolution, row: description)
      </th>
<th>
        
        Summary (Ticket query: status: closed, max: 3, col: id, col: resolution, col: summary, col: owner, col: reporter, order: summary, row: description)
      </th>
<th>
        
        Owner (Ticket query: status: closed, max: 3, col: id, col: resolution, col: summary, col: owner, col: reporter, order: owner, row: description)
      </th>
<th>
        
        Reporter (Ticket query: status: closed, max: 3, col: id, col: resolution, col: summary, col: owner, col: reporter, order: reporter, row: description)
      </th>
<td>
    </td>
<td></td>
<td></td>
<td></td>
<td></td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org/ghc/ghc/issues/16410">#16410</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      duplicate
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org/ghc/ghc/issues/16410">Order of declarations matters</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      Iceland_jack
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                    </td>
<th>Description</th>
<td>
                    
                    </td>
<th>
                      


This piece of code works.


```
{-# Language DataKinds    #-}
{-# Language GADTs        #-}
{-# Language InstanceSigs #-}
{-# Language PolyKinds    #-}
{-# Language TypeFamilies #-}

import Data.Kind

class Category (tag::Type) where
 type Strip tag :: Type

class Category tag => Stripped tag where
 type Hom tag::Strip tag -> Strip tag -> Type

instance Category () where
 type Strip () = ()
instance Stripped () where
 type Hom () = Unit1

data Unit1 :: () -> () -> Type where U1 :: Unit1 '() '()
data Tag
data Unit2 :: () -> () -> Type where U2 :: Unit2 '() '()

instance Category Tag where
 type Strip Tag = ()
instance Stripped Tag where
 type Hom Tag = Unit2
```


Note that <tt>Unit1</tt> and <tt>Unit2</tt> are declared identically, separated by <tt>data Tag</tt>. The order is important.



If we change the last line to <tt>Hom Tag = Unit1</tt> (same as <tt>Hom ()</tt>) we get


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


even though <tt>Strip Tag</tt> is defined to equal <tt>()</tt>. Here comes the weird part.



If I move <tt>Unit1</tt> decl beneath <tt>data Tag</tt>


```
data Tag
data Unit1 :: () -> () -> Type where U1 :: Unit1 '() '()
data Unit2 :: () -> () -> Type where U2 :: Unit2 '() '()
```


then <tt>type Hom Tag = Unit1</tt> and <tt>type Hom Tag = Unit2</tt> both work well.



If I move <tt>Unit2</tt> decl above <tt>data Tag</tt> then both <tt>Hom Tag = Unit1</tt> and <tt>Hom Tag = Unit2</tt> fail!


```
data Unit1 :: () -> () -> Type where U1 :: Unit1 '() '()
data Unit2 :: () -> () -> Type where U2 :: Unit2 '() '()
data Tag
```


If I replace all occurrences of <tt>Tag</tt> with <tt>Bool</tt> it succeeds.



                    </th>
<td>
                    
                  </td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org/ghc/ghc/issues/16401">#16401</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      duplicate
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org/ghc/ghc/issues/16401">Bad error message if we use TypeApplications with an identifier that doesn&apos;t exist</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      Fuuzetsu
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                    </td>
<th>Description</th>
<td>
                    
                    </td>
<th>
                      

```
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module BadError where

foo :: Int
foo = doesNotExist @123

{-
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


What I would expect is for it to say that <tt>doesNotExist</tt> is not bound as one might reasonably expect. This is very, very confusing and easy to hit in real life. If you are exposing a function <tt>f</tt>, using it somewhere with <tt>f @123</tt> but then later decide to remove or rename <tt>f</tt>, the error message doesn&apos;t suggest at all that <tt>f</tt> has disappeared and leaves the user scratching their head.



                    </th>
<td>
                    
                  </td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td></tr>
<tr><td>
                
                  
                    </td>
<th><a href="https://gitlab.haskell.org/ghc/ghc/issues/16394">#16394</a></th>
<td>
                    
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      fixed
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      <a href="https://gitlab.haskell.org/ghc/ghc/issues/16394">GHC internal error while typechecking of instance definition</a>
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
                  
                    
                    </td>
<th>
                      
                      
                      Day1721
                      
                      
                      
                      
                      
                      
                    </th>
<td>
                  
                
              </td></tr>
<tr><td>
                    </td>
<th>Description</th>
<td>
                    
                    </td>
<th>
                      


Hello. <br>
This code won&apos;t typecheck because of GHC internal error.


```
{-# LANGUAGE PolyKinds, TypeFamilies, DataKinds #-}

class C a where
    type T (n :: a)

--       v--DIFF--v
instance C a => C b => C (a, b) where
    type T '(n, m) = (T n, T m)
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
{-# LANGUAGE PolyKinds, TypeFamilies, DataKinds #-}

class C a where
    type T (n :: a)

--       v--DIFF--v
instance (C a, C b) => C (a, b) where
    type T '(n, m) = (T n, T m)
```


Not sure is a bug, but either way it would be better to make more understandable error message



                    </th>
<td>
                    
                  </td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td>
<td></td></tr></table>


  



    
    
      
        1
      
      
    
      
      
        2 (Ticket query: status: closed, max: 3, page: 2, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id, row: description)
      
    
      
      
        3 (Ticket query: status: closed, max: 3, page: 3, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id, row: description)
      
    
      
      
        4 (Ticket query: status: closed, max: 3, page: 4, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id, row: description)
      
    
      
      
        5 (Ticket query: status: closed, max: 3, page: 5, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id, row: description)
      
    
      
      
        6 (Ticket query: status: closed, max: 3, page: 6, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id, row: description)
      
    
      
      
        7 (Ticket query: status: closed, max: 3, page: 7, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id, row: description)
      
    
      
      
        8 (Ticket query: status: closed, max: 3, page: 8, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id, row: description)
      
    
      
      
        9 (Ticket query: status: closed, max: 3, page: 9, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id, row: description)
      
    
      
      
        10 (Ticket query: status: closed, max: 3, page: 10, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id, row: description)
      
    
      
      
        11 (Ticket query: status: closed, max: 3, page: 11, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id, row: description)
      
    
    
      → (Ticket query: status: closed, max: 3, page: 2, col: id, col: resolution, col: summary, col: owner, col: reporter, desc: 1, order: id)
    




## Query Language



`query:` [TracLinks](trac-links) and the `[[TicketQuery]]` macro both use a mini “query language” for specifying query filters. Filters are separated by ampersands (`&`). Each filter consists of the ticket field name, an operator and one or more values. More than one value are separated by a pipe (`|`), meaning that the filter matches any of the values. To include a literal `&` or `|` in a value, escape the character with a backslash (`\`).



The available operators are:


<table><tr><th> <b><tt>=</tt></b> </th>
<th> the field content exactly matches one of the values 
</th></tr>
<tr><th> <b><tt>~=</tt></b> </th>
<th> the field content contains one or more of the values 
</th></tr>
<tr><th> <b><tt>^=</tt></b> </th>
<th> the field content starts with one of the values 
</th></tr>
<tr><th> <b><tt>$=</tt></b> </th>
<th> the field content ends with one of the values 
</th></tr></table>



All of these operators can also be negated:


<table><tr><th> <b><tt>!=</tt></b> </th>
<th> the field content matches none of the values 
</th></tr>
<tr><th> <b><tt>!~=</tt></b> </th>
<th> the field content does not contain any of the values 
</th></tr>
<tr><th> <b><tt>!^=</tt></b> </th>
<th> the field content does not start with any of the values 
</th></tr>
<tr><th> <b><tt>!$=</tt></b> </th>
<th> the field content does not end with any of the values 
</th></tr></table>



The date fields `created` and `modified` can be constrained by using the `=` operator and specifying a value containing two dates separated by two dots (`..`). Either end of the date range can be left empty, meaning that the corresponding end of the range is open. The date parser understands a few natural date specifications like "3 weeks ago", "last month" and "now", as well as Bugzilla-style date specifications like "1d", "2w", "3m" or "4y" for 1 day, 2 weeks, 3 months and 4 years, respectively. Spaces in date specifications can be omitted to avoid having to quote the query string. 


<table><tr><th> <b><tt>created=2007-01-01..2008-01-01</tt></b> </th>
<th> query tickets created in 2007 
</th></tr>
<tr><th> <b><tt>created=lastmonth..thismonth</tt></b> </th>
<th> query tickets created during the previous month 
</th></tr>
<tr><th> <b><tt>modified=1weekago..</tt></b> </th>
<th> query tickets that have been modified in the last week 
</th></tr>
<tr><th> <b><tt>modified=..30daysago</tt></b> </th>
<th> query tickets that have been inactive for the last 30 days 
</th></tr></table>

---



See also: [TracTickets](trac-tickets), [TracReports](trac-reports), [TracGuide](trac-guide), [TicketQuery](ticket-query)


