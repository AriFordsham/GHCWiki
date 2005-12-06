# Using HTML in Wiki Text


Trac supports inserting HTML into any wiki context, accomplished using the HTML [WikiProcessor](wiki-processors).


HTML support is built-in, and does not require installing any additional packages.

## How to Use HTML


To inform the wiki engine that a block of text should be treated as HTML, use the *html* processor. 


This example should explain:

```wiki
{{{
#!html
<h1 style="text-align: right; color: blue">HTML Test</h1>
}}}
```


Results in:

# HTML Test

## More Information

- [ http://www.w3.org/](http://www.w3.org/) -- World Wide Web Consortium
- [ http://www.w3.org/MarkUp/](http://www.w3.org/MarkUp/) -- HTML Markup Home Page

---


See also:  [WikiProcessors](wiki-processors), [WikiFormatting](wiki-formatting), [WikiRestructuredText](wiki-restructured-text)