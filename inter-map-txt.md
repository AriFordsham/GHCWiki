# [InterMapTxt](inter-map-txt)

## This is the place for defining [InterWiki](inter-wiki) prefixes


This page was modelled after the [ MeatBall:InterMapTxt](http://www.usemod.com/cgi-bin/mb.pl?InterMapTxt) page.
In addition, an optional comment is allowed after the mapping.


This page is interpreted in a special way by Trac, in order to support
InterWiki links in a flexible and dynamic way.


The code block after the first line separator in this page
will be interpreted as a list of InterWiki specifications:

```wiki
prefix <space> URL [<space> # comment]
```


By using `$1`, `$2`, etc. within the URL, it is possible to create 
[InterWiki](inter-wiki) links which support multiple arguments, e.g. Trac:ticket:40.
The URL itself can be optionally followed by a comment, 
which will subsequently be used for decorating the links 
using that prefix.


New InterWiki links can be created by adding to that list, in real time.
Note however that *deletions* are also taken into account immediately,
so it may be better to use comments for disabling prefixes.



Also note that InterWiki prefixes are case insensitive.


## List of Active Prefixes



<table><tr><th><i>Prefix</i></th>
<th><i>Site</i></th></tr>
<tr><th><a href="http://www.acronymfinder.com/af-query.asp?String=exact&amp;Acronym=RecentChanges">Acronym</a></th>
<th><a href="http://www.acronymfinder.com/af-query.asp?String=exact&amp;Acronym=">http://www.acronymfinder.com/af-query.asp?String=exact&amp;Acronym=</a></th></tr>
<tr><th><a href="http://c2.com/cgi/wiki?FindPage&amp;value=RecentChanges">C2find</a></th>
<th><a href="http://c2.com/cgi/wiki?FindPage&amp;value=">http://c2.com/cgi/wiki?FindPage&amp;value=</a></th></tr>
<tr><th><a href="http://c2.com/cgi/wiki?RecentChanges">c2Wiki</a></th>
<th><a href="http://c2.com/cgi/wiki?">http://c2.com/cgi/wiki?</a></th></tr>
<tr><th><a href="http://www.google.com/search?q=cache:RecentChanges">Cache</a></th>
<th><a href="http://www.google.com/search?q=cache:">http://www.google.com/search?q=cache:</a></th></tr>
<tr><th><a href="http://cheeseshop.python.org/pypi/RecentChanges">CheeseShop</a></th>
<th><a href="http://cheeseshop.python.org/pypi/">Python Package $1 from the Cheese Shop</a></th></tr>
<tr><th><a href="http://search.cpan.org/perldoc?RecentChanges">CPAN</a></th>
<th><a href="http://search.cpan.org/perldoc?">http://search.cpan.org/perldoc?</a></th></tr>
<tr><th><a href="http://bugs.debian.org/RecentChanges">DebianBug</a></th>
<th><a href="http://bugs.debian.org/">http://bugs.debian.org/</a></th></tr>
<tr><th><a href="http://packages.debian.org/RecentChanges">DebianPackage</a></th>
<th><a href="http://packages.debian.org/">http://packages.debian.org/</a></th></tr>
<tr><th><a href="http://www.dict.org/bin/Dict?Database=*&amp;Form=Dict1&amp;Strategy=*&amp;Query=RecentChanges">Dictionary</a></th>
<th><a href="http://www.dict.org/bin/Dict?Database=*&amp;Form=Dict1&amp;Strategy=*&amp;Query=">http://www.dict.org/bin/Dict?Database=*&amp;Form=Dict1&amp;Strategy=*&amp;Query=</a></th></tr>
<tr><th><a href="http://www.google.com/search?q=RecentChanges">Google</a></th>
<th><a href="http://www.google.com/search?q=">http://www.google.com/search?q=</a></th></tr>
<tr><th><a href="http://groups.google.com/group/RecentChanges/msg/">GoogleGroups</a></th>
<th><a href="http://groups.google.com/group/$1/msg/$2">Message $2 in $1 Google Group</a></th></tr>
<tr><th><a href="http://hackage.haskell.org/package/RecentChanges">hackage</a></th>
<th><a href="http://hackage.haskell.org/package/$1">Haskell Package $1 from the HackageDB</a></th></tr>
<tr><th><a href="http://en.wikipedia.org/wiki/ISO_RecentChanges">ISO</a></th>
<th><a href="http://en.wikipedia.org/wiki/ISO_">ISO Standard $1 in Wikipedia</a></th></tr>
<tr><th><a href="http://downlode.org/perl/jargon-redirect.cgi?term=RecentChanges">JargonFile</a></th>
<th><a href="http://downlode.org/perl/jargon-redirect.cgi?term=">http://downlode.org/perl/jargon-redirect.cgi?term=</a></th></tr>
<tr><th><a href="http://www.usemod.com/cgi-bin/mb.pl?RecentChanges">MeatBall</a></th>
<th><a href="http://www.usemod.com/cgi-bin/mb.pl?">http://www.usemod.com/cgi-bin/mb.pl?</a></th></tr>
<tr><th><a href="http://www.selenic.com/mercurial/wiki/index.cgi/RecentChanges">Mercurial</a></th>
<th><a href="http://www.selenic.com/mercurial/wiki/index.cgi/">the wiki for the Mercurial distributed SCM</a></th></tr>
<tr><th><a href="http://sunir.org/apps/meta.pl?RecentChanges">MetaWiki</a></th>
<th><a href="http://sunir.org/apps/meta.pl?">http://sunir.org/apps/meta.pl?</a></th></tr>
<tr><th><a href="http://meta.wikipedia.org/wiki/RecentChanges">MetaWikiPedia</a></th>
<th><a href="http://meta.wikipedia.org/wiki/">http://meta.wikipedia.org/wiki/</a></th></tr>
<tr><th><a href="http://issues.apache.org/jira/browse/MODPYTHON-RecentChanges">MODPYTHON</a></th>
<th><a href="http://issues.apache.org/jira/browse/MODPYTHON-">Issue $1 in mod_python&apos;s JIRA instance</a></th></tr>
<tr><th><a href="http://moinmoin.wikiwikiweb.de/RecentChanges">MoinMoin</a></th>
<th><a href="http://moinmoin.wikiwikiweb.de/">http://moinmoin.wikiwikiweb.de/</a></th></tr>
<tr><th><a href="http://bugs.mysql.com/bug.php?id=RecentChanges">mysql-bugs</a></th>
<th><a href="http://bugs.mysql.com/bug.php?id=">Bug #$1 in MySQL&apos;s bug database</a></th></tr>
<tr><th><a href="http://peak.telecommunity.com/DevCenter/RecentChanges">peak</a></th>
<th><a href="http://peak.telecommunity.com/DevCenter/">$1 in Python Enterprise Application Kit&apos;s Wiki</a></th></tr>
<tr><th><a href="http://www.python.org/peps/pep-RecentChanges.html">PEP</a></th>
<th><a href="http://www.python.org/peps/pep-$1.html">Python Enhancement Proposal</a></th></tr>
<tr><th><a href="https://phabricator.haskell.org/RecentChanges">Phab</a></th>
<th><a href="https://phabricator.haskell.org/$1">https://phabricator.haskell.org/$1</a></th></tr>
<tr><th><a href="http://bugs.python.org/issueRecentChanges">Python-issue</a></th>
<th><a href="http://bugs.python.org/issue$1">Python Issue #$1</a></th></tr>
<tr><th><a href="http://bugs.python.org/issueRecentChanges">PythonBug</a></th>
<th><a href="http://bugs.python.org/issue$1">Python Issue #$1</a></th></tr>
<tr><th><a href="http://tools.ietf.org/html/rfcRecentChanges">RFC</a></th>
<th><a href="http://tools.ietf.org/html/rfc$1">IETF&apos;s RFC $1</a></th></tr>
<tr><th><a href="http://www.sqlite.org/cvstrac/wiki?p=RecentChanges">SQLite</a></th>
<th><a href="http://www.sqlite.org/cvstrac/wiki?p=">http://www.sqlite.org/cvstrac/wiki?p=</a></th></tr>
<tr><th><a href="http://www.orcaware.com/svn/wiki/RecentChanges">SvnWiki</a></th>
<th><a href="http://www.orcaware.com/svn/wiki/">Subversion Wiki</a></th></tr>
<tr><th><a href="http://thread.gmane.org/gmane.comp.version-control.subversion.trac.devel/RecentChanges">trac-dev</a></th>
<th><a href="http://thread.gmane.org/gmane.comp.version-control.subversion.trac.devel/">Message $1 in Trac Development Mailing List</a></th></tr>
<tr><th><a href="http://thread.gmane.org/gmane.comp.version-control.subversion.trac.general/RecentChanges">Trac-ML</a></th>
<th><a href="http://thread.gmane.org/gmane.comp.version-control.subversion.trac.general/">Message $1 in Trac Mailing List</a></th></tr>
<tr><th><a href="http://www.whois.sc/RecentChanges">WhoIs</a></th>
<th><a href="http://www.whois.sc/">http://www.whois.sc/</a></th></tr>
<tr><th><a href="http://clublet.com/c/c/why?RecentChanges">Why</a></th>
<th><a href="http://clublet.com/c/c/why?">http://clublet.com/c/c/why?</a></th></tr>
<tr><th><a href="http://en.wikipedia.org/wiki/RecentChanges">WikiPedia</a></th>
<th><a href="http://en.wikipedia.org/wiki/">http://en.wikipedia.org/wiki/</a></th></tr></table>



---

## Prefix Definitions

```wiki
PEP     http://www.python.org/peps/pep-$1.html    # Python Enhancement Proposal 
PythonBug    http://bugs.python.org/issue$1       # Python Issue #$1
Python-issue http://bugs.python.org/issue$1       # Python Issue #$1

Trac-ML  http://thread.gmane.org/gmane.comp.version-control.subversion.trac.general/ # Message $1 in Trac Mailing List
trac-dev http://thread.gmane.org/gmane.comp.version-control.subversion.trac.devel/   # Message $1 in Trac Development Mailing List

Mercurial http://www.selenic.com/mercurial/wiki/index.cgi/ # the wiki for the Mercurial distributed SCM
RFC       http://tools.ietf.org/html/rfc$1 # IETF's RFC $1
ISO       http://en.wikipedia.org/wiki/ISO_ # ISO Standard $1 in Wikipedia

CheeseShop  http://cheeseshop.python.org/pypi/  # Python Package $1 from the Cheese Shop
SQLite      http://www.sqlite.org/cvstrac/wiki?p= 
mysql-bugs  http://bugs.mysql.com/bug.php?id=  # Bug #$1 in MySQL's bug database
peak        http://peak.telecommunity.com/DevCenter/ # $1 in Python Enterprise Application Kit's Wiki
MODPYTHON   http://issues.apache.org/jira/browse/MODPYTHON- # Issue $1 in mod_python's JIRA instance
SvnWiki     http://www.orcaware.com/svn/wiki/ # Subversion Wiki

# Haskell specific
hackage     http://hackage.haskell.org/package/$1 # Haskell Package $1 from the HackageDB
Phab        https://phabricator.haskell.org/$1

#
# A arbitrary pick of InterWiki prefixes...
#
Acronym          http://www.acronymfinder.com/af-query.asp?String=exact&Acronym=
C2find           http://c2.com/cgi/wiki?FindPage&value=
Cache            http://www.google.com/search?q=cache:
CPAN             http://search.cpan.org/perldoc?
DebianBug        http://bugs.debian.org/
DebianPackage    http://packages.debian.org/
Dictionary       http://www.dict.org/bin/Dict?Database=*&Form=Dict1&Strategy=*&Query=
Google           http://www.google.com/search?q=
GoogleGroups     http://groups.google.com/group/$1/msg/$2        # Message $2 in $1 Google Group
JargonFile       http://downlode.org/perl/jargon-redirect.cgi?term=
MeatBall         http://www.usemod.com/cgi-bin/mb.pl?
MetaWiki         http://sunir.org/apps/meta.pl?
MetaWikiPedia    http://meta.wikipedia.org/wiki/
MoinMoin         http://moinmoin.wikiwikiweb.de/
WhoIs            http://www.whois.sc/
Why              http://clublet.com/c/c/why?
c2Wiki           http://c2.com/cgi/wiki?
WikiPedia        http://en.wikipedia.org/wiki/
```