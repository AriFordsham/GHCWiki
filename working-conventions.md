# Working on GHC


GHC is a BSD-licensed open-source project, and we welcome your help in making it better. This page and the side bar on the left have pointers to information you'll need.

- [How to fix a bug in GHC](working-conventions/fixing-bugs).

- [How to add a new feature to GHC](working-conventions/adding-features).

- [ How to propose a change to the libraries](http://haskell.org/haskellwiki/Library_submissions)

## Working conventions

- **Using Git**: Our conventions and some useful tips for using git are here: [Using Git](working-conventions/git), and information about our submodule setup is in [WorkingConventions/Git/Submodules](working-conventions/git/submodules).

- **Using Phabricator**: we use Phabricator as a code review tool; here are [our Phabricator guidance notes](phabricator).

- **Releases and branches**: Our conventions for making releases and how the branches are managed: [Releases](working-conventions/releases)

- **Using the Bug Tracker**: see [Using the Bug Tracker](working-conventions/bug-tracker)

- **Coding style**: When you are editing GHC's source code, please follow our coding guidelines:

  - [Coding style in the compiler](commentary/coding-style)
  - [Coding style in the runtime system](commentary/rts/conventions)

- **Licensing**: make sure you are familiar with GHC's [Licensing](licensing).  Unless you say otherwise, we will assume that if you submit a contribution to GHC, then you intend to supply it to us under the same license as the existing code. However, we do not ask for copyright attribution; you retain copyright on any contributions you make, so feel free to add your copyright to the top of any file in which you make non-trivial changes.

## Tips and Tricks

- To have an easier time looking up tickets and searching trac, use [the browser tips page](browser-tips) to make your search and lookups for Trac tickets substantially easier.

- If you use Emacs, see [Emacs](emacs) for some useful stuff to put in your `.emacs` file.

- If you have lots of Haskell installations, you may find Edsko's blog post [ Comprehensive Haskell Sandboxes](http://www.edsko.net/2013/02/10/comprehensive-haskell-sandboxes/) useful.
