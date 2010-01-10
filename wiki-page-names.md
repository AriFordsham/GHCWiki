# Wiki Page Names


Wiki page names commonly use the [CamelCase](camel-case) convention. Within wiki text, any word in [CamelCase](camel-case) automatically becomes a hyperlink to the wiki page with that name.

[CamelCase](camel-case) page names must follow these rules:

1. The name must consist of **alphabetic characters only**. No digits, spaces, punctuation, or underscores are allowed.
1. A name must have at least two capital letters.
1. The first character must be capitalized.
1. Every capital letter must be followed by one or more lower-case letters. 
1. The use of slash ( / ) is permitted in page names (possibly representing a hierarchy).


If you want to create a wiki page that doesn't follow [CamelCase](camel-case) rules you can use the following syntax:

```wiki
 * [wiki:Wiki_page], [wiki:ISO9000]
 * [wiki:"Space Matters"] that page name embeds space characters
 * or simply: ["WikiPageName"]s (!MoinMoin's internal free links style)
```


This will be rendered as:

- Wiki_page?, ISO9000?
- Space Matters? that page name embeds space characters
- or simply: WikiPageName?s (MoinMoin's internal free links style)


Starting with Trac 0.11, it's also possible to link to a specific *version* of a Wiki page, as you would do for a specific version of a file, for example: [WikiStart\@1](wiki-start?version=1).


See also [TracLinks\#QuotingspaceinTracLinks](trac-links#quoting-space-in-traclinks).


Finally, and as exemplified in the line above, one can also append an anchor to a Wiki page name, in order to link to a specific section within that page. The anchor can easily be seen by hovering the mouse over a section heading, then clicking on the ¶ sign that appears at its end. The anchor is usually generated automatically, but it's also possible to specify it explicitly: see [WikiFormatting\#using-explicit-id-in-heading](wiki-formatting#).

---


See also: [WikiNewPage](wiki-new-page), [WikiFormatting](wiki-formatting), [TracWiki](trac-wiki), [TracLinks](trac-links)