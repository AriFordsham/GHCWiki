# Trac Links


As you might have guessed, [TracLinks](trac-links) are a very fundamental feature of Trac.


They allow hyperlinking between Trac entities (tickets, reports, changesets, Wiki
pages, milestones and source files) from anywhere [WikiFormatting](wiki-formatting) is used.

[TracLinks](trac-links) are generally of the form **type:id** (where *id* represents the
number, name or path of the item) though some frequently used kinds of items
also have short-hand notations.


Some examples:

- Tickets: **\#1** or **ticket:1**
- Reports: **{1}** or **report:1**
- Changesets: **r1**, **\[1\]** or **changeset:1**
- Revision log: **r1:3**, **\[1:3\]** or **log:\#1:3**
- Wiki pages: **[CamelCase](camel-case)** or **wiki:CamelCase**
- Milestones: **milestone:1.0**
- Attachment: **attachment:ticket:944:attachment.1073.diff**
- Files: **source:trunk/COPYING**
- A specific file revision: **source:/trunk/COPYING\#200**


Display:

- Tickets: [\#1](https://gitlab.haskell.org//ghc/ghc/issues/1) or [ticket:1](https://gitlab.haskell.org//ghc/ghc/issues/1)
- Reports: [{1}](/trac/ghc/report/1) or [report:1](/trac/ghc/report/1)
- Changesets: r1, \[1\] or changeset:1
- Differences: [r1:3](/trac/ghc/log/ghc/?revs=1%3A3), [\[1:3\]](/trac/ghc/log/ghc/?revs=1%3A3) or [log:\#1:3](/trac/ghc/log/ghc/#1:3)
- Wiki pages: [CamelCase](camel-case) or [wiki:CamelCase](camel-case)
- Milestones: milestone:1.0
- Files: source:trunk/COPYING
- Attachment: attachment:ticket:944:attachment.1073.diff
- A specific file revision: source:/trunk/COPYING\#200

**Note:** The [wiki:CamelCase](camel-case) form is rarely used, but it can be convenient to refer to
pages whose names do not follow [WikiPageNames](wiki-page-names) rules, i.e., single words,
non-alphabetic characters, etc.


Trac links using the full (non-shorthand) notation can also be given a custom
link title like this:

```wiki
[ticket:1 This is a link to ticket number one].
```


Display: [This is a link to ticket number one](https://gitlab.haskell.org//ghc/ghc/issues/1).


If the title is be omitted, only the id (the part after the colon) is displayed:

```wiki
[ticket:1]
```


Display: [1](https://gitlab.haskell.org//ghc/ghc/issues/1)


It might seem a simple enough concept at a glance, but actually allows quite a complex network of information. In practice, it's very intuitive and simple to use, and we've found the "link trail" extremely helpful to better understand what's happening in a project or why a particular change was made.

## attachement: links


The link syntax for attachments is as follows:

- attachment:the_file.txt creates a link to the attachment the_file.txt of the current object
- attachment:wiki:MyPage:the_file.txt creates a link to the attachment the_file.txt of the MyPage wiki page
- attachment:ticket:753:the_file.txt creates a link to the attachment the_file.txt of the ticket 753 attachment:wiki:MyPage:the_file.txt

## source: links


The default behavior for a source:/some/path link is to open the directory browser 
if the path points to a directory and otherwise open the log view. 
It's also possible to link directly to a specific revision of a file like this: source:/some/file\@123 
or like this to link to the latest revision: source:/some/file\@latest.
If the revision is specified, one can even link to a specific line number: source:/some/file\@123\#L10 

## Quoting space in [TracLinks](trac-links)


The usual syntax for quoting space is:

- attachment:'the file.txt' or
- attachment:"the file.txt" 

## Where to use [TracLinks](trac-links)


You can use [TracLinks](trac-links) in:

- Source code (Subversion) commit messages
- Wiki pages
- Full descriptions for tickets, reports and milestones


and any other text fields explicitly marked as supporting [WikiFormatting](wiki-formatting).

## Escaping Links


To prevent parsing of a TracLink, you can escape it by preceding it with a '!' (exclamation mark).

```wiki
 !NoLinkHere.
 ![42] is not a link either.
```


Display:

>
> NoLinkHere.
> \[42\] is not a link either.

---


See also: [WikiFormatting](wiki-formatting), [TracWiki](trac-wiki)