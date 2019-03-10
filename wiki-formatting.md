# [WikiFormatting](wiki-formatting)


Wiki markup is a core feature in Trac, tightly integrating all the other parts of Trac into a flexible and powerful whole.


Trac has a built-in small and powerful wiki rendering engine. This wiki engine implements a growing subset of the commands from other popular Wikis, especially [ MoinMoin](http://moinmo.in/) and [ WikiCreole](http://trac.edgewall.org/intertrac/WikiCreole).


This page will give you an in-depth explanation of the wiki markup available anywhere [WikiFormatting](wiki-formatting) is allowed.


The sections below provide an overview for the most common syntax, each link in the *Category* column will lead you to the more detailed explanation later in this page.


A few other wiki pages present the advanced features of the Trac wiki markup in more depth: 

- [TracLinks](trac-links) covers all the possible ways to refer precisely to any Trac resource or parts thereof.
- [WikiPageNames](wiki-page-names) covers the various names a wiki page can take, whether in [CamelCase](camel-case) or not.
- [WikiMacros](wiki-macros) lists the macros available for generating dynamic content.
- [WikiProcessors](wiki-processors) and [WikiHtml](wiki-html) details how parts of the wiki text can be processed in special ways.
- [ AdvancedWikiOperations](http://trac.edgewall.org/intertrac/wiki%3ATracDev/Proposals/AdvancedWikiOperations) provides some operations in uncommon or administrative scenarios.

## Common wiki markup

<table><tr><th>**Category**</th>
<th>**Wiki Markup**</th>
<th>**Display**</th></tr>
<tr><th>[Font Styles](wiki-formatting#font-styles)

</th>
<th>`'''bold'''`, `''italic''`, `'''''Wikipedia style'''''`</th>
<th>**bold**, *italic*, ***Wikipedia style***</th></tr>
<tr><th>``monospaced (''other markup ignored'')``</th>
<th>`monospaced (''other markup ignored'')`</th>
<th></th></tr>
<tr><th>`**bold**`, `//italic//`, `**//!WikiCreole style//**`</th>
<th>**bold**, *italic*, ***WikiCreole style***</th>
<th></th></tr>
<tr><th>[Headings](wiki-formatting#headings)== Level 2
=== Level 3 \^(\[\#hn note\])\^
Level 2Level 3 <sup>([note](wiki-formatting#))</sup></th>
<th></th>
<th></th></tr>
<tr><th>[Paragraphs](wiki-formatting#paragraphs)First paragraph
on multiple lines.

Second paragraph.

First paragraph
on multiple lines.

Second paragraph.
</th>
<th></th>
<th></th></tr>
<tr><th>[Lists](wiki-formatting#lists)\* bullet list
  on multiple lines
  1. nested list
    a. different numbering 
       styles
bullet list
on multiple lines
nested list
different numbering
styles
</th>
<th></th>
<th></th></tr>
<tr><th>[Definition Lists](wiki-formatting#definition-lists)

</th>
<th>```wiki
 term:: definition on
        multiple lines
```

</th>
<th><table><tr><th>term</th>
<td>definition on
multiple lines
</td></tr></table>

</th></tr>
<tr><th>[Preformatted Text](wiki-formatting#preformatted-text){{{
multiple lines, ''no wiki''
      white space respected
}}}
multiple lines, ''no wiki''
      white space respected
</th>
<th></th>
<th></th></tr>
<tr><th>[Blockquotes](wiki-formatting#blockquotes)  if there's some leading
  space the text is quoted

if there's some leading
space the text is quoted
</th>
<th></th>
<th></th></tr>
<tr><th>[Discussion Citations](wiki-formatting#discussion-citations)\>\> ... (I said)
\> (he replied)

... (I said)

(he replied)
</th>
<th></th>
<th></th></tr>
<tr><th>[Tables](wiki-formatting#tables)\|\|= Table Header =\|\| Cell \|\|
\|\|\|\|  (details below)  \|\|
 Table Header  Cell 
  (details below)  
</th>
<th></th>
<th></th></tr>
<tr><th>[Links](wiki-formatting#links)

</th>
<th>`http://trac.edgewall.org`</th>
<th>[ http://trac.edgewall.org](http://trac.edgewall.org)</th></tr>
<tr><th>`WikiFormatting (CamelCase)`</th>
<th>[WikiFormatting](wiki-formatting) ([CamelCase](camel-case)) 
</th>
<th></th></tr>
<tr><th>[TracLinks](wiki-formatting#trac-links)

</th>
<th>`wiki:WikiFormatting`, `wiki:"WikiFormatting"`</th>
<th>[wiki:WikiFormatting](wiki-formatting), [wiki:"WikiFormatting"](wiki-formatting)</th></tr>
<tr><th>`#1 (ticket)`, `[1] (changeset)`, `{1} (report)`</th>
<th>[\#1](https://gitlab.haskell.org//ghc/ghc/issues/1) (ticket), \[1\] (changeset), [{1}](/trac/ghc/report/1) (report) 
</th>
<th></th></tr>
<tr><th>`ticket:1, ticket:1#comment:1`</th>
<th>[ticket:1](https://gitlab.haskell.org//ghc/ghc/issues/1), [ticket:1\#comment:1](https://gitlab.haskell.org//ghc/ghc/issues/1)</th>
<th></th></tr>
<tr><th>`Ticket [ticket:1]`, `[ticket:1 ticket one]`</th>
<th> Ticket [1](https://gitlab.haskell.org//ghc/ghc/issues/1), [ticket one](https://gitlab.haskell.org//ghc/ghc/issues/1)</th>
<th></th></tr>
<tr><th>`Ticket [[ticket:1]]`, `[[ticket:1|ticket one]]`</th>
<th> Ticket [1](https://gitlab.haskell.org//ghc/ghc/issues/1), [ticket one](https://gitlab.haskell.org//ghc/ghc/issues/1)</th>
<th></th></tr>
<tr><th>[Setting Anchors](wiki-formatting#setting-anchors)

</th>
<th>`[=#point1 (1)] First...`</th>
<th>(1) First... 
</th></tr>
<tr><th>`see [#point1 (1)]`</th>
<th> see [(1)](wiki-formatting#)</th>
<th></th></tr>
<tr><th>[Escaping Markup](wiki-formatting#)

</th>
<th>`!'' doubled quotes`</th>
<th> '' doubled quotes 
</th></tr>
<tr><th>`!wiki:WikiFormatting`, `!WikiFormatting`</th>
<th> wiki:WikiFormatting, WikiFormatting 
</th>
<th></th></tr>
<tr><th>``{{{-}}}` triple curly brackets`</th>
<th>`{{{-}}}` triple curly brackets 
</th>
<th></th></tr>
<tr><th>[Images](wiki-formatting#images)</th>
<th>`[[Image(`*link*`)]]`</th>
<th>[](/trac/ghc/chrome/site/../common/trac_logo_mini.png)</th></tr>
<tr><th>[Macros](wiki-formatting#macros)

</th>
<th>`[[MacroList(*)]]`</th>
<th>*(short list of all available macros)*</th></tr>
<tr><th>`[[Image?]]`</th>
<th>*(help for the Image macro)*</th>
<th></th></tr>
<tr><th>[Processors](wiki-formatting#processors){{{
\#!div style="font-size: 80%"
Code highlighting:
  {{{\#!python
  hello = lambda: "world"
  }}}
}}}

Code highlighting:
hello =lambda:"world"</th>
<th></th>
<th></th></tr>
<tr><th>[Comments](wiki-formatting#comments){{{\#!comment
Note to Editors: ...
}}}
</th>
<th></th>
<th></th></tr>
<tr><th>[Miscellaneous](wiki-formatting#miscellaneous)Line \[\[br\]\] break 
Line \\\\ break
----

Line 
 break
Line 
 break
</th>
<th></th>
<th></th></tr></table>

## Font Styles


The Trac wiki supports the following font styles:

<table><tr><th> Wiki Markup </th>
<th> Display 
</th></tr>
<tr><th>```wiki
 * '''bold''', 
   ''' triple quotes !''' 
   can be bold too if prefixed by ! ''', 
 * ''italic''
 * '''''bold italic''''' or ''italic and
   ''' italic bold ''' ''
 * __underline__
 * {{{monospace}}} or `monospace`
   (hence `{{{` or {{{`}}} quoting)
 * ~~strike-through~~
 * ^superscript^ 
 * ,,subscript,,
 * **also bold**, //italic as well//, 
   and **'' bold italic **'' //(since 0.12)//
 * [[span(style=color: #FF0000, a red text )]]
```

</th>
<th>- **bold**, 
  ** triple quotes ''' 
  can be bold too if prefixed by ! **, 
- *italic*
- ***bold italic*** or *italic and
  ** italic bold ***
- underline
- `monospace` or `monospace`
  (hence `{{{` or ``` quoting)
- ~~strike-through~~
- <sup>superscript</sup>
- <sub>subscript</sub>
- **also bold**, *italic as well*, 
  and *** bold italic ******(since 0.12)*
- a red text

</th></tr></table>


Notes:

- `{{{...}}}` and ``...`` commands not only select a monospace font, but also treat their content as verbatim text, meaning that no further wiki processing is done on this text.
- ` ! ` tells wiki parser to not take the following characters as wiki format, so pay attention to put a space after !, e.g. when ending bold.
- all the font styles marks have to be used in opening/closing pairs, 
  and they must nest properly; in particular, an `''` italic can't be paired 
  with a `//` one, and `'''` can't be paired with `**`.

## Headings


You can create heading by starting a line with one up to six *equal* characters ("=") followed by a single space and the headline text. 


The headline text can be followed by the same number of "=" characters, but this is not mandatory. That is, `=== Section3 ===` is identical to `=== Section3`.


Finally, the heading might optionally be followed by an explicit id. If not, an implicit but nevertheless readable id will be generated.

<table><tr><th> Wiki Markup </th>
<th> Display 
</th></tr>
<tr><th>```wiki
= Heading =
== Subheading
=== About ''this'' ===
=== Explicit id === #using-explicit-id-in-heading
== Subheading #sub2
```

</th>
<th># Heading

## Subheading

### About *this*

### Explicit id

## Subheading

</th></tr></table>

## Paragraphs


A new text paragraph is created whenever two blocks of text are separated by one or more empty lines.


A forced line break can also be inserted, using:

<table><tr><th> Wiki Markup </th>
<th> Display 
</th></tr>
<tr><th>```wiki
Line 1[[BR]]Line 2
```

```wiki
Paragraph
one

Paragraph 
two
```

</th>
<th>
> Line 1
> Line 2

>
> Paragraph 
> one

>
> Paragraph 
> two

</th></tr></table>

## Lists


The wiki supports both ordered/numbered and unordered lists.


Example:

<table><tr><th> Wiki Markup </th>
<th> Display 
</th></tr>
<tr><th>```wiki
 * Item 1
   * Item 1.1
      * Item 1.1.1   
      * Item 1.1.2
      * Item 1.1.3
   * Item 1.2
 * Item 2
- items can start at the beginning of a line
  and they can span multiple lines
  - be careful though to continue the line 
  with the appropriate indentation, otherwise
that will start a new paragraph...

 1. Item 1
   a. Item 1.a
   a. Item 1.b
      i. Item 1.b.i
      i. Item 1.b.ii
 1. Item 2
And numbered lists can also be restarted
with an explicit number:
 3. Item 3
```

</th>
<th>- Item 1

  - Item 1.1

    - Item 1.1.1   
    - Item 1.1.2
    - Item 1.1.3
  - Item 1.2
- Item 2

- items can start at the beginning of a line
  and they can span multiple lines

  - be careful though to continue the line 
    with the appropriate indentation, otherwise


that will start a new paragraph...

1. Item 1

  1. Item 1.a
  1. Item 1.b

    1. Item 1.b.i
    1. Item 1.b.ii
1. Item 2


And numbered lists can also be restarted with an explicit number:

1. Item 3

</th></tr></table>

## Definition Lists


The wiki also supports definition lists.

<table><tr><th> Wiki Markup </th>
<th> Display 
</th></tr>
<tr><th>```wiki
 llama::
   some kind of mammal, with hair
 ppython::
   some kind of reptile, without hair
   (can you spot the typo?)
```

</th>
<th><table><tr><th>llama</th>
<td>
some kind of mammal, with hair
</td></tr>
<tr><th>ppython</th>
<td>
some kind of reptile, without hair
(can you spot the typo?)
</td></tr></table>

</th></tr></table>


Note that you need a space in front of the defined term.

## Preformatted Text


Block containing preformatted text are suitable for source code snippets, notes and examples. Use three *curly braces* wrapped around the text to define a block quote. The curly braces need to be on a separate line.
  

<table><tr><th> Wiki Markup </th>
<th> Display 
</th></tr>
<tr><th>```wiki
{{{
def HelloWorld():
    print '''Hello World'''
}}}
```

</th>
<th>```wiki
def HelloWorld():
    print '''Hello World'''
```

</th></tr></table>


Note that this kind of block is also used for selecting lines that should be processed through [WikiProcessors](wiki-processors).

## Blockquotes


In order to mark a paragraph as blockquote, indent that paragraph with two spaces.

<table><tr><th> Wiki Markup </th>
<th> Display 
</th></tr>
<tr><th>```wiki
Paragraph
  This text is a quote from someone else.
```

</th>
<th>
Paragraph

>
> This text is a quote from someone else.

</th></tr></table>

## Discussion Citations


To delineate a citation in an ongoing discussion thread, such as the ticket comment area, email-like citation marks ("\>", "\>\>", etc.) may be used.  

<table><tr><th> Wiki Markup </th>
<th> Display 
</th></tr>
<tr><th>```wiki
>> Someone's original text
> Someone else's reply text
>  - which can be any kind of Wiki markup
My reply text
```

</th>
<th>
> > Someone's original text
>
>
> Someone else's reply text
>
> - which can be any kind of Wiki markup


My reply text

</th></tr></table>

## Tables

### Simple Tables


Simple tables can be created like this:

<table><tr><th> Wiki Markup </th>
<th> Display 
</th></tr>
<tr><th>```wiki
||Cell 1||Cell 2||Cell 3||
||Cell 4||Cell 5||Cell 6||
```

</th>
<th><table><tr><th>Cell 1</th>
<th>Cell 2</th>
<th>Cell 3
</th></tr>
<tr><th>Cell 4</th>
<th>Cell 5</th>
<th>Cell 6
</th></tr></table>

</th></tr></table>


Cell headings can be specified by wrapping the content in a pair of '=' characters.
Note that the '=' characters have to stick to the cell separators, like this:

<table><tr><th> Wiki Markup </th>
<th> Display 
</th></tr>
<tr><th>```wiki
||        ||= stable =||= latest =||
||= 0.10 =||  0.10.5  || 0.10.6dev||
||= 0.11 =||  0.11.6  || 0.11.7dev||
```

</th>
<th><table><tr><th></th>
<th> stable </th>
<th> latest 
</th></tr>
<tr><th> 0.10 </th>
<th>  0.10.5  </th>
<th> 0.10.6dev
</th></tr>
<tr><th> 0.11 </th>
<th>  0.11.6  </th>
<th> 0.11.7dev
</th></tr></table>

</th></tr></table>


Finally, specifying an empty cell means that the next non empty cell will span the empty cells. For example:

<table><tr><th> Wiki Markup </th>
<th> Display 
</th></tr>
<tr><th>```wiki
|| 1 || 2 || 3 ||
|||| 1-2 || 3 ||
|| 1 |||| 2-3 ||
|||||| 1-2-3 ||
```

</th>
<th><table><tr><th> 1 </th>
<th> 2 </th>
<th> 3 
</th></tr>
<tr><th> 1-2 </th>
<th> 3 
</th>
<th></th></tr>
<tr><th> 1 </th>
<th> 2-3 
</th>
<th></th></tr>
<tr><th> 1-2-3 
</th>
<th></th>
<th></th></tr></table>

</th></tr></table>


Note that if the content of a cell "sticks" to one side of the cell and only one, then the text will be aligned on that side. Example:

<table><tr><th> Wiki Markup </th>
<th> Display 
</th></tr>
<tr><th>```wiki
||=Text =||= Numbers =||
||left align    ||        1.0||
||  center      ||        4.5||
||      right align||     4.5||
|| default alignment ||   2.5||
||default||         2.5||
||  default ||      2.5||
|| default ||       2.5||
```

</th>
<th><table><tr><th>Text </th>
<th> Numbers 
</th></tr>
<tr><th>left align    </th>
<th>        1.0
</th></tr>
<tr><th>  center      </th>
<th>        4.5
</th></tr>
<tr><th>      right align</th>
<th>     4.5
</th></tr>
<tr><th> default alignment </th>
<th>   2.5
</th></tr>
<tr><th>default</th>
<th>         2.5
</th></tr>
<tr><th>  default </th>
<th>      2.5
</th></tr>
<tr><th> default </th>
<th>       2.5
</th></tr></table>

</th></tr></table>


If contrary to the example above, the cells in your table contain more text, it might be convenient to spread a table row over multiple lines of markup. The `\` character placed at the end of a line after a cell separator tells Trac to not start a new row for the cells on the next line.

<table><tr><th> Wiki Markup 
</th></tr>
<tr><th>```wiki
|| this is column 1 [http://trac.edgewall.org/newticket new ticket] || \
|| this is column 2 [http://trac.edgewall.org/roadmap the road ahead] || \
|| that's column 3 and last one ||
```

</th></tr>
<tr><th> Display 
</th></tr>
<tr><th><table><tr><th> this is column 1 [ new ticket](http://trac.edgewall.org/newticket)</th>
<th> this is column 2 [ the road ahead](http://trac.edgewall.org/roadmap)</th>
<th> that's column 3 and last one 
</th></tr></table>

</th></tr></table>

### Complex Tables

<table><tr><td>If the possibilities offered by the simple pipe-based markup ('</td>
<th>') for tables described above are not enough for your needs, you can create more elaborate tables by using [WikiProcessor based tables](wiki-formatting#).
</th></tr></table>

## Links


Hyperlinks are automatically created for [WikiPageNames](wiki-page-names) and URLs. WikiPageLinks can be disabled by prepending an exclamation mark ('!'), such as `!WikiPageLink`.

<table><tr><th> Wiki Markup </th>
<th> Display 
</th></tr>
<tr><th>```wiki
TitleIndex, http://www.edgewall.com/, !NotAlink
```

</th>
<th>[TitleIndex](title-index), [ http://www.edgewall.com/](http://www.edgewall.com/), NotAlink

</th></tr></table>


Links can be given a more descriptive title by writing the link followed by a space and a title and all this inside square brackets. 
If the descriptive title is omitted, then the explicit prefix is discarded, unless the link is an external link. This can be useful for wiki pages not adhering to the [WikiPageNames](wiki-page-names) convention.

<table><tr><th> Wiki Markup </th>
<th> Display 
</th></tr>
<tr><th>```wiki
 * [http://www.edgewall.com Edgewall Software]
 * [wiki:TitleIndex Title Index] 
 * [wiki:TitleIndex] 
 * [wiki:ISO9000]
```

</th>
<th>- [ Edgewall Software](http://www.edgewall.com)
- [Title Index](title-index)
- [TitleIndex](title-index)
- ISO9000?

</th></tr></table>


Following the [ WikiCreole](http://trac.edgewall.org/intertrac/WikiCreole) trend, the descriptive title can also be specified by writing the link followed by a pipe ('\|') and a title and all this inside *double* square brackets. 

<table><tr><th>```wiki
 * [[http://www.edgewall.com|Edgewall Software]]
 * [[wiki:TitleIndex|Title Index]]
   or even [[TitleIndex|Title Index]]
 * [[wiki:TitleIndex]]
   ''' but not ![[TitleIndex]]! '''
 * [[ISO9000]]
```

</th>
<th>- [ Edgewall Software](http://www.edgewall.com)
- [Title Index](title-index)
  or even [Title Index](title-index)
- [TitleIndex](title-index)** but not \[\[TitleIndex\]\]! **
- ISO9000?

</th></tr></table>

**Note**: the [ WikiCreole](http://trac.edgewall.org/intertrac/WikiCreole) style for links is quick to type and certainly looks familiar as it is the one used on Wikipedia and in many other wikis. Unfortunately it conflicts with the syntax for [macros](wiki-formatting#macros).
So in the rare case when you need to refer to a page which is named after a macro (typical examples being [TitleIndex](title-index), [InterTrac](inter-trac) and [InterWiki](inter-wiki)), by writing `[[TitleIndex]]` you will actually call the macro instead of linking to the page.

## Trac Links


Wiki pages can link directly to other parts of the Trac system. Pages can refer to tickets, reports, changesets, milestones, source files and other Wiki pages using the following notations:

<table><tr><th> Wiki Markup </th>
<th> Display 
</th></tr>
<tr><th>```wiki
 * Tickets: #1 or ticket:1
 * Reports: {1} or report:1
 * Changesets: r1, [1] or changeset:1
 * ...
 * targeting other Trac instances, 
   so called InterTrac links:
   - Tickets: #Trac1 or Trac:ticket:1
   - Changesets: [Trac1] or Trac:changeset:1
```

</th>
<th>- Tickets: [\#1](https://gitlab.haskell.org//ghc/ghc/issues/1) or [ticket:1](https://gitlab.haskell.org//ghc/ghc/issues/1)
- Reports: [{1}](/trac/ghc/report/1) or [report:1](/trac/ghc/report/1)
- Changesets: r1, \[1\] or changeset:1
- ... 
- targeting other Trac instances, 
  so called [InterTrac](inter-trac) links:

  - Tickets: \#Trac1 or Trac:ticket:1
  - Changesets: \[Trac1\] or Trac:changeset:1

</th></tr></table>


There are many more flavors of Trac links, see [TracLinks](trac-links) for more in-depth information and a reference for all the default link resolvers.

## Setting Anchors


An anchor, or more correctly speaking, an [ anchor name](http://www.w3.org/TR/REC-html40/struct/links.html#h-12.2.1) can be added explicitly at any place in the Wiki page, in order to uniquely identify a position in the document:

```wiki
[=#point1]
```


This syntax was chosen to match the format for explicitly naming the header id [documented above](wiki-formatting#headings). For example:

```wiki
== Long title == #title
```


It is also very close to the syntax for the corresponding link to that anchor:

```wiki
[#point1]
```


Optionally, a label can be given to the anchor:

```wiki
[[=#point1 '''Point 1''']]
```

<table><tr><th> Wiki Markup </th>
<th> Display 
</th></tr>
<tr><th>```wiki
[#point2 jump to the second point]

...

Point2:  [=#point2] Jump here
```

</th>
<th>[jump to the second point](wiki-formatting#)

>
> ...

>
> Point2:   Jump here

</th></tr></table>


For more complex anchors (eg when a custom title is wanted), you can use the Span macro: `[[span(id=point2, class=wikianchor, title=Point 2, ^(2)^)]]`.

## Escaping Links, [WikiPageNames](wiki-page-names) and other Markup


You may avoid making hyperlinks out of [TracLinks](trac-links) by preceding an expression with a single exclamation mark ('!').

<table><tr><th> Wiki Markup </th>
<th> Display 
</th></tr>
<tr><th>```wiki
 !NoHyperLink
 !#42 is not a link
```

```wiki
Various forms of escaping for list markup:
 ^^- escaped minus sign \\
 ^^1. escaped number  \\
 ^^* escaped asterisk sign
```

</th>
<th>
> NoHyperLink
> \#42 is not a link


Various forms of escaping for list markup:

> <sup></sup>- escaped minus sign 
> <sup></sup>1. escaped number  
> <sup></sup>\* escaped asterisk sign

</th></tr></table>

## Images


Urls ending with `.png`, `.gif` or `.jpg` are no longer automatically interpreted as image links, and converted to `<img>` tags.


You now have to use the \[\[Image\]\] macro. The simplest way to include an image is to upload it as attachment to the current page, and put the filename in a macro call like `[[Image(picture.gif)]]`.


In addition to the current page, it is possible to refer to other resources:

- `[[Image(wiki:WikiFormatting:picture.gif)]]` (referring to attachment on another page)
- `[[Image(ticket:1:picture.gif)]]` (file attached to a ticket)
- `[[Image(htdocs:picture.gif)]]` (referring to a file inside the [environment](trac-environment)`htdocs` directory)
- `[[Image(source:/trunk/trac/htdocs/trac_logo_mini.png)]]` (a file in repository)

<table><tr><th> Wiki Markup </th>
<th> Display 
</th></tr>
<tr><th>```wiki
[[Image(htdocs:../common/trac_logo_mini.png)]]
```

</th>
<th>[](/trac/ghc/chrome/site/../common/trac_logo_mini.png)

</th></tr></table>


See [WikiMacros](wiki-macros) for further documentation on the `[[Image()]]` macro, which has several useful options (`title=`, `link=`, etc.)

## Macros


Macros are *custom functions* to insert dynamic content in a page.

<table><tr><th> Wiki Markup </th>
<th> Display 
</th></tr>
<tr><th>```wiki
[[RecentChanges(Trac,3)]]
```

</th>
<th>

### Jan 4, 2019

- [TracWikiMisc](/trac/ghc/wiki/TracWikiMisc)<small> ([diff](/trac/ghc/wiki/TracWikiMisc?action=diff&version=14))</small>

### Feb 12, 2017

- [TracSearch](/trac/ghc/wiki/TracSearch)<small> ([diff](/trac/ghc/wiki/TracSearch?action=diff&version=4))</small>
- [TracSyntaxColoring](/trac/ghc/wiki/TracSyntaxColoring)<small> ([diff](/trac/ghc/wiki/TracSyntaxColoring?action=diff&version=5))</small>

</th></tr></table>


See [WikiMacros](wiki-macros) for more information, and a list of installed macros.


The detailed help for a specific macro can also be obtained more directly by appending a "?" to the macro name.

<table><tr><th> Wiki Markup </th>
<th> Display 
</th></tr>
<tr><th>```wiki
[[MacroList?]]
```

</th>
<th>

### `[[MacroList]]`


Display a list of all installed Wiki macros, including documentation if
available.


Optionally, the name of a specific macro can be provided as an argument. In
that case, only the documentation for that macro will be rendered.


Note that this macro will not be able to display the documentation of
macros if the `PythonOptimize` option is enabled for mod_python!

</th></tr></table>

## Processors


Trac supports alternative markup formats using [WikiProcessors](wiki-processors). For example, processors are used to write pages in 
[reStructuredText](wiki-restructured-text) or [HTML](wiki-html). 

<table><tr><th> Wiki Markup </th>
<th> Display 
</th></tr>
<tr><th>Example 1: HTML

</th>
<th></th></tr>
<tr><th>```wiki
{{{
#!html
<h1 style="text-align: right; color: blue">
 HTML Test
</h1>
}}}
```

</th>
<th># HTML Test

</th></tr>
<tr><th>Example 2: Code Highlighting

</th>
<th></th></tr>
<tr><th>```wiki
{{{
#!python
class Test:

    def __init__(self):
        print "Hello World"
if __name__ == '__main__':
   Test()
}}}
```

</th>
<th>```
classTest:def__init__(self):print"Hello World"if__name__=='__main__':
   Test()
```

</th></tr>
<tr><th>Example 3: Complex Tables

</th>
<th></th></tr>
<tr><th>```wiki
{{{#!th rowspan=4 align=justify
With the `#td` and `#th` processors,
table cells can contain any content:
}}}
|----------------
{{{#!td
  - lists
  - embedded tables
  - simple multiline content
}}}
|----------------
{{{#!td
As processors can be easily nested, 
so can be tables:
  {{{#!th
  Example:
  }}}
  {{{#!td style="background: #eef"
  || must be at the third level now... ||
  }}}
}}}
|----------------
{{{#!td
Even when you don't have complex markup,
this form of table cells can be convenient
to write content on multiple lines.
}}}
```

</th>
<th><table><tr><th>
With the `#td` and `#th` processors,
table cells can contain any content:

</th></tr>
<tr><th>- lists
- embedded tables
- simple multiline content

</th></tr>
<tr><th>
As processors can be easily nested, 
so can be tables:

<table><tr><th>
Example:

</th>
<th><table><tr><th> must be at the third level now... 
</th></tr></table>

</th></tr></table>

</th></tr>
<tr><th>
Even when you don't have complex markup,
this form of table cells can be convenient
to write content on multiple lines.

</th></tr></table>

</th></tr></table>


See [WikiProcessors](wiki-processors) for more information.

## Comments


Comments can be added to the plain text. These will not be rendered and will not display in any other format than plain text.

<table><tr><th> Wiki Markup </th>
<th> Display 
</th></tr>
<tr><th>```wiki
Nothing to
{{{
#!comment
Your comment for editors here
}}}
see.
```

</th>
<th>
> Nothing to
>
>
> see.

</th></tr></table>

## Miscellaneous

<table><tr><th> Wiki Markup </th>
<th> Display 
</th></tr>
<tr><th>
> Horizontal line:
>
> ```wiki
> Four or more dashes will be replaced 
> by a horizontal line (<HR>)
> ----
> See?
> ```

</th>
<th>
Four or more dashes will be replaced
by a horizontal line (\<HR\>)

---


See?

</th></tr>
<tr><th>
> Two examples of line breaks:
>
> ```wiki
> "macro" style [[BR]] line break
> ```
>
>
> or:
>
> ```wiki
> !WikiCreole style \\ line\\break
> ```

</th>
<th>
"macro" style 
 line break


WikiCreole style 
 line
break

</th></tr>
<tr><td></td>
<td></td></tr></table>