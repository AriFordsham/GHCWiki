# The Trac Roadmap


The roadmap provides a view on the [ticket system](trac-tickets) that helps planning and managing the future development of a project.

## The Roadmap View


Basically, the roadmap is just a list of future milestones. You can add a description to milestones (using [WikiFormatting](wiki-formatting)) describing main objectives, for example. In addition, tickets targeted for a milestone are aggregated, and the ratio between active and resolved tickets is displayed as a milestone progress bar.  It is possible to further customise the ticket grouping? and have multiple ticket statuses shown on the progress bar.

## The Milestone View


It is possible to drill down into this simple statistic by viewing the individual milestone pages. By default, the active/resolved ratio will be grouped and displayed by component. You can also regroup the status by other criteria, such as ticket owner or severity. Ticket numbers are linked to [custom queries](trac-query) listing corresponding tickets.

## Roadmap Administration


It is possible to add, modify and remove milestones using either [TracAdmin](trac-admin) or the web interface. 

**Note:** Milestone descriptions can currently only be edited from the web interface. With appropriate permissions, you'll see buttons for milestone management on the roadmap and milestone pages.

## iCalendar Support


The Roadmap supports the [ iCalendar](http://www.ietf.org/rfc/rfc2445.txt) format to keep track of planned milestones and related tickets from your favorite calendar software. Calendar applications supporting the iCalendar specification include [ Apple iCal](http://www.apple.com/ical/) for Mac OS X and the cross-platform [ Mozilla Calendar](http://www.mozilla.org/projects/calendar/), and [ Chandler](http://chandlerproject.org). [ Korganizer](http://kontact.kde.org/korganizer/) (the calendar application of the [ KDE](http://www.kde.org/) project) and [ Evolution](http://www.novell.com/de-de/products/desktop/features/evolution.html) also support iCalendar.


To subscribe to the roadmap, copy the iCalendar link from the roadmap (found at the bottom of the page) and choose the "Subscribe to remote calendar" action (or similar) of your calendar application, and insert the URL just copied.

**Note:** For tickets to be included in the calendar as tasks, you need to be logged in when copying the link. You will only see tickets assigned to yourself, and associated with a milestone.


More information about iCalendar can be found at [ Wikipedia](http://en.wikipedia.org/wiki/ICalendar).

---


See also: TracRoadmapCustomGroups, [TracTickets](trac-tickets), [TracReports](trac-reports), [TracQuery](trac-query), [TracGuide](trac-guide)