# Contributing to GHC


GHC is a BSD-licensed open-source project, and we welcome your help in making it better. This page and the side bar on the right have pointers to information you'll need.

- [Information for newcomers](contributing#newcomers-to-ghc). This the first stop for those people who say, "I want to contribute to GHC, but I don't know quite where to begin." Begin here.

- [How to contribute a patch to GHC](/Contributing-a-Patch). For [adding features](working-conventions/adding-features), there are a few extra steps to follow.

- [How to propose a change to the libraries](http://haskell.org/haskellwiki/Library_submissions)

## Working conventions

- **Using Git**: read [how to use git with GHC](working-conventions/git). Information about our submodule setup is in [WorkingConventions/Git/Submodules](working-conventions/git/submodules), and some useful Git tricks are in [WorkingConventions/Git/Tricks](working-conventions/git/tricks).

- **GitLab conventions**:
   - [Label usage](gitlab/labels)
   - [Issue tracker conventions](gitlab/issues)
   - [Merge request conventions](gitlab/merge-requests)

- **Releases and branches**: Our conventions for making releases and how the branches are managed: [Releases](working-conventions/releases)

- **Useful tools**: [Various tools](working-conventions/useful-tools) which exist to make working on GHC more pleasant.

- **Coding style**: When you are editing GHC's source code, please follow our coding guidelines:

  - [Coding style in the compiler](commentary/coding-style)
  - [Coding style in the runtime system](commentary/rts/conventions)

- **Licensing**: make sure you are familiar with GHC's [Licensing](licensing).  Unless you say otherwise, we will assume that if you submit a contribution to GHC, then you intend to supply it to us under the same license as the existing code. However, we do not ask for copyright attribution; you retain copyright on any contributions you make, so feel free to add your copyright to the top of any file in which you make non-trivial changes.

- **For boot libraries**: GHC ships with a number of [boot libraries](commentary/libraries/version-history) maintained outside of GHC itself. Maintainers of such libraries should read [WorkingConventions/BootLibraries](working-conventions/boot-libraries) for guidance on how to maintain their libraries.

## Tips and Tricks

- [Loading GHC into GHCi](building/in-ghci) can provide a more iterative development experience. 

- To have an easier time looking up tickets and searching trac, use [the browser tips page](browser-tips) to make your search and lookups for Trac tickets substantially easier.

- If you use Emacs, see [Emacs](emacs) for some useful stuff to put in your `.emacs` file.

- If you have lots of Haskell installations, you may find Edsko's blog post [Comprehensive Haskell Sandboxes](http://www.edsko.net/2013/02/10/comprehensive-haskell-sandboxes/) useful.

## Newcomers to GHC


While the [building guide](building), [working conventions](working-conventions), [commentary](commentary) and [debugging](debugging) pages (always linked from the right sidebar) have great information that can come in handy while you're working on your first, or first several patches, this section is intended to have the details you will need to get rolling.


If you have any questions along the way don't hesitate to reach out to the community. There are people on the [mailing lists and IRC](mailing-lists-and-irc) who will gladly help you (although you may need to be patient). Don't forget that all GHC developers are still learning; your question is never too silly to ask.

### First steps

- See [Building/QuickStart](building/quick-start) to get started building GHC. Expect it all to take roughly between 30 minutes and a couple of hours, depending on your CPU speed, and the number of jobs you can run in parallel. Note that [building older versions of GHC may require having an older version of GHC on your path](https://ghc.haskell.org/trac/ghc/wiki/Building/Preparation/Tools).

- While you are waiting for your build to finish, orient yourself to the general architecture of GHC. This [article](http://www.aosabook.org/en/ghc.html) is written by two of the chief architects of GHC, Simon Marlow and Simon Peyton-Jones, is excellent and current (2012).

- After a successful build, you should have your brand new compiler in `./inplace/bin/ghc-stage2`. (GHCi is launched with `./inplace/bin/ghc-stage2 --interactive`). Try it out.

### Finding a ticket



Now that you can build GHC, let's get hacking. But first, you'll need to identify a goal. GHC's Trac tickets are a great place to find starting points. You are encouraged to ask for a starting point on IRC or the `ghc-devs` [mailing list](mailing-lists-and-irc). There someone familiar with the process can help you find a ticket that matches your expertise and help you when you get stuck.

If you want to get a taste for possible starting tasks, below is a list of tickets that appear to be "low-hanging fruit" -- things that might be reasonable for a newcomer to GHC hacking. Of course, we can't ever be sure of how hard a task is before doing it, so apologies if one of these is too hard.

You can add tickets to this list by giving them the ~newcomer label.


### Advice

- Read up on the steps you are expected to take for [contributing a patch to GHC](/Contributing-a-Patch).

- Need help? You can email the [ghc-devs](http://www.haskell.org/mailman/listinfo/ghc-devs) list, or ask on IRC in `#ghc`.

- Don't get scared. GHC is a big codebase, but it makes sense when you stare at it long enough!

- Don't hesitate to ask questions. We have all been beginners at some point and understand that diving in to GHC can be a challenge. Asking questions will help you make better use of your hacking time.

- Be forewarned that many pages on the GHC Wiki are somewhat out-of-date. Always check the last modification date. Email `ghc-devs` if you're not sure.

- You may want to look at these "how it went for me" blog posts.

  - [Hacking on GHC (is not that hard)](http://rawgit.com/gibiansky/4c54f767bf21a6954b23/raw/67c62c5555f40c6fb67b124307725df168201361/exp.html) by Andrew Gibiansky
  - [Contributing to GHC](http://anniecherkaev.com/projects/contributing-to-ghc) by Annie Cherkaev
  - [Contributing to GHC via Phabricator](https://medium.com/@zw3rk/contributing-to-ghc-290653b63147) by Moritz Angermann

- There is a blog post series by Stephen Diehl that provides an overview of many important data structures and contains links to other sources of information: [Dive into GHC](http://www.stephendiehl.com/posts/ghc_01.html)


Happy hacking!
