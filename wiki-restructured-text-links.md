# [TracLinks](trac-links) in reStructuredText


This document is for testing the ``..trac::`` directive. The page is written like

```wiki
{{{
#!rst 

Examples
...
...

}}}
```


This is a list of example uses of the *trac* directive, providing use of [TracLinks](trac-links) in [WikiRestructuredText](wiki-restructured-text).

# Examples

# trac role

Syntax is \`link\`:trac: or :trac:\`link\`, and could be put anywhere in the text. 'link' has the same format as explain for the `.. trac::` directive below.

<table><tr><th>`In the middle of my text `WikiFormatting`:trac:see!!!!`</th>
<td>In the middle of my text [WikiFormatting](/trac/ghc/wiki/WikiFormatting) see!!!!</td></tr></table>

or

<table><tr><th>`In the middle of my text :trac:`WikiFormatting`see!!!!`</th>
<td>In the middle of my text [WikiFormatting](/trac/ghc/wiki/WikiFormatting) see!!!!</td></tr></table>

# wiki

<table><tr><th>`.. trac:: WikiFormatting`</th>
<td>[WikiFormatting](/trac/ghc/wiki/WikiFormatting)</td></tr>
<tr><th>`.. trac:: wiki:WikiFormatting`</th>
<td>[wiki:WikiFormatting](/trac/ghc/wiki/WikiFormatting)</td></tr>
<tr><th>`.. trac:: wiki:WikiFormatting WikiFormatting`</th>
<td>[WikiFormatting](/trac/ghc/wiki/WikiFormatting)</td></tr>
<tr><th>`.. trac:: wiki:WikiFormatting LinkText`</th>
<td>[LinkText](/trac/ghc/wiki/WikiFormatting)</td></tr></table>

# tickets

<table><tr><th>`.. trac:: #1`</th>
<td>[\#1](https://gitlab.haskell.org//ghc/ghc/issues/1)</td></tr>
<tr><th>`.. trac:: #1 ticket one`</th>
<td>[ticket one](https://gitlab.haskell.org//ghc/ghc/issues/1)</td></tr>
<tr><th>`.. trac:: ticket:1`</th>
<td>[ticket:1](https://gitlab.haskell.org//ghc/ghc/issues/1)</td></tr>
<tr><th>`.. trac:: ticket:1 ticket one`</th>
<td>[ticket one](https://gitlab.haskell.org//ghc/ghc/issues/1)</td></tr></table>

# reports

<table><tr><th>`.. trac:: {1}`</th>
<td>[{1}](/trac/ghc/report/1)</td></tr>
<tr><th>`.. trac:: {1} report one`</th>
<td>[report one](/trac/ghc/report/1)</td></tr>
<tr><th>`.. trac:: report:1`</th>
<td>[report:1](/trac/ghc/report/1)</td></tr>
<tr><th>`.. trac:: report:1 report one`</th>
<td>[report one](/trac/ghc/report/1)</td></tr></table>

# changesets

<table><tr><th>`.. trac:: [42]`</th>
<td>\[42\]</td></tr>
<tr><th>`.. trac:: [42] changeset 42`</th>
<td>changeset 42</td></tr>
<tr><th>`.. trac:: changeset:42`</th>
<td>changeset:42</td></tr>
<tr><th>`.. trac:: changeset:42 changeset 42`</th>
<td>changeset 42</td></tr>
<tr><th>`.. trac:: foo`</th>
<td>foo</td></tr></table>

# files

<table><tr><th>`.. trac:: browser:/trunk/trac`</th>
<td>browser:/trunk/trac</td></tr></table>

The leading `/` can be omitted...

<table><tr><th>`.. trac:: repos:trunk/trac trunk/trac`</th>
<td>trunk/trac</td></tr>
<tr><th>`.. trac:: source:trunk/trac Trac source code`</th>
<td>Trac source code</td></tr>
<tr><th>`.. trac:: browser:trunk/README`</th>
<td>browser:trunk/README</td></tr>
<tr><th>`.. trac:: repos:trunk/README trunk/README`</th>
<td>trunk/README</td></tr>
<tr><th>`.. trac:: source:trunk/README README in trunk`</th>
<td>README in trunk</td></tr></table>

Note that if `hoo` is a file, the link targets its revision log. In order to see the file's content, you need to specify the revision explicitely, like here:

<table><tr><th>`.. trac:: browser:/trunk/README#latest latest of trunk/README`</th>
<td>latest of trunk/README</td></tr>
<tr><th>`.. trac:: repos:trunk/README#42 trunk/README in rev 42`</th>
<td>trunk/README in rev 42</td></tr></table>

---


See also: [WikiRestructuredTextLinks](wiki-restructured-text-links), [TracLinks](trac-links)