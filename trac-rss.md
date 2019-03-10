# Using RSS with Trac


Several of the Trac modules support content syndication using the RSS (Really Simple Syndication) XML format.
Using the RSS subscription feature in Trac, you can easily monitor progress of the project, a set of issues or even changes to a single file.


Trac supports RSS feeds in:

- [TracTimeline](trac-timeline) —  Use the RSS feed to **subscribe to project events**.
  Monitor overall project progress in your favorite RSS reader.
- [TracTickets](trac-tickets), [TracReports](trac-reports), and [TracQuery](trac-query) — Allows syndication of report and ticket query results.
  Be notified about important and relevant issue tickets.
- [TracBrowser](trac-browser) and [TracRevisionLog](trac-revision-log) — Syndication of file changes.
  Stay up to date with changes to a specific file or directory.

## How to access RSS data


Anywhere in Trac where RSS is available, you should find a small orange **XML** icon, typically placed at the bottom of the page. Clicking the icon will access the RSS feed for that specific resource.

**Note:** Different modules provide different data in their RSS feeds. Usually, the syndicated information corresponds to the current view. For example, if you click the RSS link on a report page, the feed will be based on that report. It might be explained by thinking of the RSS feeds as an *alternate view of the data currently displayed*.

## Links

- *Specifications:*

  - [ http://blogs.law.harvard.edu/tech/rss](http://blogs.law.harvard.edu/tech/rss) — RSS 2.0 Specification

- *Multi-platform RSS readers:*

  - [ http://www.rssowl.org/](http://www.rssowl.org/) — Open source, Eclipse-based, RSS reader for Linux, Mac and Windows systems that supports https and authenticated feeds.

- *Linux/BSD/\*n\*x systems:*

  - [ http://pim.kde.org/users.php](http://pim.kde.org/users.php) — [ KDE](http://kde.org) RSS Reader for Linux/BSD/\*n\*x systems
  - [ http://liferea.sourceforge.net/](http://liferea.sourceforge.net/) — Open source GTK2 RSS Reader for Linux
  - [ http://akregator.sourceforge.net/](http://akregator.sourceforge.net/) — Open source KDE RSS Reader (part of KDE-PIM)

- *Mac OS X systems:*

  - [ http://ranchero.com/netnewswire/](http://ranchero.com/netnewswire/) — An excellent RSS reader for Mac OS X (has both free and pay versions)
  - [ http://www.utsire.com/shrook/](http://www.utsire.com/shrook/) — An RSS reader for Max OS X that supports https (even with self signed certificates) and authenticated feeds.
  - [ http://vienna-rss.sourceforge.net/](http://vienna-rss.sourceforge.net/) — Open source Feed Reader for Mac OS X with smart folders support
  - [ http://www.mesadynamics.com/Tickershock.html](http://www.mesadynamics.com/Tickershock.html) — Non-intrusive "news ticker" style RSS reader for Mac OS X

- *Windows systems:*

  - [ http://www.rssreader.com/](http://www.rssreader.com/) — Free and powerful RSS Reader for Windows
  - [ http://www.sharpreader.net/](http://www.sharpreader.net/) — A free RSS Reader written in .NET for Windows

- *Firefox:*

  - [ http://www.mozilla.org/products/firefox/](http://www.mozilla.org/products/firefox/) — Mozilla Firefox supports [ live bookmarks](http://www.mozilla.org/products/firefox/live-bookmarks.html) using RSS
  - [ http://sage.mozdev.org](http://sage.mozdev.org) — Sage RSS and Atom feed aggregator for Mozilla Firefox
  - [ http://www.wizzrss.com/Welcome.php](http://www.wizzrss.com/Welcome.php) — WizzRSS Feed Reader for Firefox

---


See also: [TracGuide](trac-guide), [TracTimeline](trac-timeline), [TracReports](trac-reports), [TracBrowser](trac-browser)