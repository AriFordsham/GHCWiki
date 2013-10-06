# Trac Navigation


Starting with Trac 0.11, it is now possible to customize the main and meta navigation entries in some basic ways.


The new `[mainnav]` and `[metanav]` configuration sections can now be used to customize the text and link used for the navigation items, or even to disable them.  The `mainnav` and `metanav` options in the `[trac]` configuration section can also be used to change the order.

### `[mainnav]`

`[mainnav]` corresponds to the **main navigation bar**, the one containing entries such as *Wiki*, *Timeline*, *Roadmap*, *Browse Source* and so on. This navigation bar is meant to access the default page of the main modules enabled in Trac that are accessible for the current user.

**Example**


In the following example, we rename the link to the Wiki start "Home", and make the "View Tickets" entry link to a specific report.  The second example (below) also hides the "Help/Guide" link.


Relevant excerpt from the [TracIni](trac-ini):

```wiki
[mainnav]
wiki.label = Home
tickets.href = /report/24
```

### `[metanav]`

`[metanav]` corresponds to the **meta navigation bar**, by default positioned above the main navigation bar and below the *Search* box. It contains the *Log in*, *Logout*, *Help/Guide* etc. entries. This navigation bar is meant to access some global information about the Trac project and the current user.


There is one special entry in the  `[metanav]` section: `logout.redirect` is the page the user sees after hitting the logout button. 

** Example **

```wiki
[metanav]
help = disabled
logout.redirect = wiki/Logout
```

### Notes


Possible URL formats (for `.href` or `.redirect`):

<table><tr><th>**config**</th>
<th>**redirect to**</th></tr>
<tr><th>`wiki/Logout`</th>
<th>`/projects/env/wiki/Logout`</th></tr>
<tr><th>`http://hostname/`</th>
<th>`http://hostname/`</th></tr>
<tr><th>`/projects`</th>
<th>`/projects`</th></tr></table>

### `[trac]`


The `mainnav` and `metanav` options in the `[trac]` configuration section control the order in which the navigation items are displayed (left to right).  This can be useful with plugins that add navigation items.

** Example **


In the following example, we change the order to prioritise the ticket related items further left.


Relevant excerpt from the [TracIni](trac-ini):

```wiki
[trac]
mainnav = wiki,tickets,newticket,timeline,roadmap,browser,search,admin
```


The default order and item names can be viewed in the [trac section of TracIni](trac-ini#).

### Context Navigation


Note that it is still not possible to customize the **contextual navigation bar**, i.e. the one usually placed below the main navigation bar.

---


See also: [TracInterfaceCustomization](trac-interface-customization), and the [ TracHacks:NavAddPlugin](http://trac-hacks.org/wiki/NavAddPlugin) or [ TracHacks:MenusPlugin](http://trac-hacks.org/wiki/MenusPlugin) (still needed for adding entries)
