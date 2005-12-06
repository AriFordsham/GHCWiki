# Wiki Page Names


Wiki page names are written using [CamelCase](camel-case). Within a wiki text, any word in [CamelCase](camel-case) automatically becomes a hyperlink to the wiki page with that same name.


Page names must follow these rules:

1. The name must consist of **alphabetic characters only**. No digits, spaces, punctuation, or underscores are allowed.
1. A name must have at least two capital letters.
1. The first character must be capitalized.
1. Every capital letter must be followed by one or more lower-case letters. 
1. The use of slash ( / ) is permitted to create a hierarchy inside the wiki.  (See SubWiki and ParentWiki macros in the [ MacroBazaar](http://projects.edgewall.com/trac/wiki/MacroBazaar) which provide a way to list all sub-entries and a link up the hierarchy respectively.)


If you want to create a wiki page that doesn't follow [CamelCase](camel-case) rules you could use the following syntax:

```wiki
[wiki:Wiki_page]
```


This will be rendered as:

> Wiki_page?

---


See also: [WikiNewPage](wiki-new-page), [WikiFormatting](wiki-formatting), [TracWiki](trac-wiki)