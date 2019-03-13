# Browser Tips

## Add a search engine

- Firefox: Add a new bookmark.
- Google Chrome: Right-click on the address bar and select `Edit Search Engines`, then you can scroll all the way down and add a new search engine entry.

## Finding tickets by number


Add a new bookmark/entry, with

```wiki
Name: [#] GHC ticket
Location/URL: https://gitlab.haskell.org/ghc/ghc/issues/%s
Keyword: #
```


Then typing `# 5129` in the title bar goes to that ticket (note the
space).

## Searching for tickets


Add a new bookmark/entry, with

```wiki
Name: [t] GHC ticket search
Location/URL: https://gitlab.haskell.org/ghc/ghc/issues?scope=all&utf8=%E2%9C%93&state=opened&search=%s
Keyword: t
```

Now typing `t <query>` into the title bar jumps directly to GitLab's issue
search.

Alternatively, you can use Google to perform the search:

```
Location/URL: https://www.google.com/search?q=%s site:gitlab.haskell.org/ghc/ghc/issues
```

## Searching the wiki


Add a new bookmark/entry, with

```wiki
Name: [w] GHC wiki search
Location/URL: http://www.google.com/search?q=%s site:gitlab.haskell.org/ghc/ghc/wikis
Keyword: w
```

## Searching for emails


Add a new bookmark/entry, with

```wiki
Name: [m] GHC mail search
Location/URL: http://www.google.com/search?q=%s (site:http://news.gmane.org/gmane.comp.lang.haskell.cvs.ghc OR site:http://www.haskell.org/pipermail/ghc-devs/ OR site:http://www.haskell.org/pipermail/glasgow-haskell-users/)
Keyword: m
```
