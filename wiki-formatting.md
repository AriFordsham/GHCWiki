# [WikiFormatting](wiki-formatting)

[Font Styles](#FontStyles)[Headings](#Headings)[Subheading](#Subheading)[About  this](#Aboutthis)[Paragraphs](#Paragraphs)[Lists](#Lists)[Definition Lists](#DefinitionLists)[Preformatted Text](#PreformattedText)[Blockquotes](#Blockquotes)[Tables](#Tables)[Links](#Links)[Trac Links](#TracLinks)[Escaping Links and WikiPageNames](#EscapingLinksandWikiPageNames)[Images](#Images)[Macros](#Macros)[Processors](#Processors)[Miscellaneous](#Miscellaneous)


Wiki markup is a core feature in Trac, tightly integrating all the other parts of Trac into a flexible and powerful whole.


Trac has a built in small and powerful wiki rendering engine. This wiki engine implements an ever growing subset of the commands from other popular Wikis,
especially [ MoinMoin](http://moinmoin.wikiwikiweb.de/). 


This page demonstrates the formatting syntax available anywhere [WikiFormatting](wiki-formatting) is allowed.

## Font Styles


The Trac wiki supports the following font styles:

```wiki
 * '''bold'''
 * ''italic''
 * '''''bold italic'''''
 * __underline__
 * {{{monospace}}} or `monospace`
 * ~~strike-through~~
 * ^superscript^ 
 * ,,subscript,,
```


Display:

- **bold**
- *italic*
- ***bold italic***
- underline
- `monospace` or `monospace`
- ~~strike-through~~
- <sup>superscript</sup>
- <sub>subscript</sub>


Note that the `{{{...}}}` and ``...`` commands not only select a monospace font, but also treat their content as verbatim text, meaning that no further wiki processing is done on this text.

## Headings


You can create heading by starting a line with one up to five *equal* characters ("=")
followed by a single space and the headline text. The line should end with a space 
followed by the same number of *=* characters.


Example:

```wiki
= Heading =
== Subheading ==
=== About ''this'' ===
```


Display:

# Heading

## Subheading

### About *this*

## Paragraphs


A new text paragraph is created whenever two blocks of text are separated by one or more empty lines.


A forced line break can also be inserted, using:

```wiki
Line 1[[BR]]Line 2
```


Display:


Line 1
Line 2

## Lists


The wiki supports both ordered/numbered and unordered lists.


Example:

```wiki
 * Item 1
   * Item 1.1
 * Item 2

 1. Item 1
   1. Item 1.1
 1. Item 2
```


Display:

- Item 1

  - Item 1.1
- Item 2

1. Item 1

  1. Item 1.1
1. Item 2


Note that there must be one or more spaces preceding the list item markers, otherwise the list will be treated as a normal paragraph.

## Definition Lists


The wiki also supports definition lists.


Example:

```wiki
 llama::
   some kind of mammal, with hair
 ppython::
   some kind of reptile, without hair
   (can you spot the typo?)
```


Display:

<table><tr><th>llama</th>
<td>
some kind of mammal, with hair
</td></tr>
<tr><th>ppython</th>
<td>
some kind of reptile, without hair
(can you spot the typo?)
</td></tr></table>


Note that you need a space in front of the defined term.

## Preformatted Text


Block containing preformatted text are suitable for source code snippets, notes and examples. Use three *curly braces* wrapped around the text to define a block quote. The curly braces need to be on a separate line.
  
Example:

```wiki
 {{{
  def HelloWorld():
      print "Hello World"
 }}}
```


Display:

```wiki
 def HelloWorld():
     print "Hello World"
```

## Blockquotes


In order to mark a paragraph as blockquote, indent that paragraph with two spaces.


Example:

```wiki
  This text is a quote from someone else.
```


Display:

>
> This text is a quote from someone else.

## Tables


Simple tables can be created like this:

```wiki
||Cell 1||Cell 2||Cell 3||
||Cell 4||Cell 5||Cell 6||
```


Display:

<table><tr><th>Cell 1</th>
<th>Cell 2</th>
<th>Cell 3
</th></tr>
<tr><th>Cell 4</th>
<th>Cell 5</th>
<th>Cell 6
</th></tr></table>


Note that more complex tables can be created using
[reStructuredText](wiki-restructured-text#).

## Links


Hyperlinks are automatically created for [WikiPageNames](wiki-page-names) and URLs. WikiPageLinks can be disabled by prepending an exclamation mark "!" character, such as `!WikiPageLink`.


Example:

```wiki
 TitleIndex, http://www.edgewall.com/, !NotAlink
```


Display:

> [TitleIndex](title-index), [ http://www.edgewall.com/](http://www.edgewall.com/), NotAlink


Links can be given a more descriptive title by writing the link followed by a space and a title and all this inside square brackets.  If the descriptive title is omitted, then the explicit prefix is disguarded, unless the link is an external link. This can be useful for wiki pages not adhering to the [WikiPageNames](wiki-page-names) convention.


Example:

```wiki
 * [http://www.edgewall.com/ Edgewall Software]
 * [wiki:TitleIndex Title Index]
 * [wiki:ISO9000]
```


Display:

- [ Edgewall Software](http://www.edgewall.com/)
- [Title Index](title-index)
- ISO9000?

### Trac Links


Wiki pages can link directly to other parts of the Trac system. Pages can refer to tickets, reports, changesets, milestones, source files and other Wiki pages using the following notations:

```wiki
 * Tickets: #1 or ticket:1
 * Reports: {1} or report:1
 * Changesets: r1, [1] or changeset:1
 * Revision Logs: r1:3, [1:3] or log:branches/0.8-stable#1:3
 * Wiki pages: CamelCase or wiki:CamelCase
 * Milestones: milestone:1.0 or milestone:"End-of-days Release"
 * Files: source:trunk/COPYING
 * Attachments: attachment:"file name.doc"
 * A specific file revision: source:/trunk/COPYING#200
 * A filename with embedded space: source:"/trunk/README FIRST"
```


Display:

- Tickets: [\#1](https://gitlab.haskell.org//ghc/ghc/issues/1) or [ticket:1](https://gitlab.haskell.org//ghc/ghc/issues/1)
- Reports: [{1}](/trac/ghc/report/1) or [report:1](/trac/ghc/report/1)
- Changesets: r1, \[1\] or changeset:1
- Revision Logs: [r1:3](/trac/ghc/log/ghc/?revs=1%3A3), [\[1:3\]](/trac/ghc/log/ghc/?revs=1%3A3) or [log:branches/0.8-stable\#1:3](/trac/ghc/log/ghc/branches/0.8-stable#1:3)
- Wiki pages: [CamelCase](camel-case) or [wiki:CamelCase](camel-case)
- Milestones: milestone:1.0 or milestone:"End-of-days Release"
- Files: source:trunk/COPYING
- Attachments: attachment:"file name.doc"
- A specific file revision: source:/trunk/COPYING\#200
- A filename with embedded space: source:"/trunk/README FIRST"


See [TracLinks](trac-links) for more in-depth information.

## Escaping Links and [WikiPageNames](wiki-page-names)


You may avoid making hyperlinks out of [TracLinks](trac-links) by preceding an expression with a single "!" (exclamation mark).


Example:

```wiki
 !NoHyperLink
 !#42 is not a link
```


Display:

>
> NoHyperLink
> \#42 is not a link

## Images


Urls ending with `.png`, `.gif` or `.jpg` are automatically interpreted as image links, and converted to `<img>` tags.


Example:

```wiki
http://www.edgewall.com/gfx/trac_example_image.png
```


Display:

[ http://www.edgewall.com/gfx/trac_example_image.png](http://www.edgewall.com/gfx/trac_example_image.png)


However, this doesn't give much control over the display mode. This way of inserting images is deprecated in favor of the more powerful `Image` macro (see [WikiMacros](wiki-macros)).

## Macros


Macros are *custom functions* to insert dynamic content in a page.


Example:

```wiki
 [[Timestamp]]
```


Display:

> Timestamp?


See [WikiMacros](wiki-macros) for more information, and a list of installed macros.

## Processors


Trac supports alternative markup formats using [WikiProcessors](wiki-processors). For example, processors are used to write pages in 
[reStructuredText](wiki-restructured-text) or [HTML](wiki-html). 


Example 1:

```wiki
{{{
#!html
<h1 style="text-align: right; color: blue">HTML Test</h1>
}}}
```


Display:

# HTML Test


Example:

```wiki
{{{
#!python
class Test:
    def __init__(self):
        print "Hello World"
if __name__ == '__main__':
   Test()
}}}
```


Display:

```
classTest:def__init__(self):print"Hello World"if__name__=='__main__':
   Test()
```


Perl:

```
my($test)=0;if($test>0){
echo "hello";}
```


See [WikiProcessors](wiki-processors) for more information.

## Miscellaneous


Four or more dashes will be replaced by a horizontal line (\<HR\>)


Example:

```wiki
 ----
```


Display:

---

---


See also: [TracLinks](trac-links), [TracGuide](trac-guide), [WikiHtml](wiki-html), [WikiMacros](wiki-macros), [WikiProcessors](wiki-processors), [TracSyntaxColoring](trac-syntax-coloring).
