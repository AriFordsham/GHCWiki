# GHC Status


This page summarises the state of play on GHC

## Releases


Here are our [release](working-conventions/releases) plans for

- [GHC 7.12.1](status/ghc-7.12.1).
- [GHC 7.10.1](status/ghc-7.10.1).


We release GHC on multiple platforms; the [platforms page](platforms) gives details.


For fun: the release plans for previous releases:

- [GHC 7.8.4](status/ghc-7.8.4)
- [GHC 7.8.3](status/ghc-7.8.3)
- [GHC 7.8.1](status/ghc-7.8)
- [GHC 6.12](status/ghc-6.12)
- [GHC 6.10](status/ghc-6.10)

## Automated builds and performance testing


We have several automated ways of monitoring GHC.  Each has its own detailed description page.

- [Harbormaster](phabricator/harbormaster) is a part of [Phabricator](phabricator), which builds all [ GHC commits](https://phabricator.haskell.org/diffusion/GHC/history/) and incoming patches for testing.
- [The GHC builders](builder-summary) build GHC every night on multiple platforms.
- [Travis](travis) also watches the repository for new commits (any branch) and validates them. [](https://travis-ci.org/ghc/ghc/builds)
- [ Our performance dashboard](http://ghcspeed-nomeata.rhcloud.com) (experimental) monitors changes in the performance of GHC itself, and of programs compiled by GHC, with a per-commit granularity.
- [ Haskell.org server status page](http://status.haskell.org/)

## Tickets and patches

- [ Ticket statistics](https://ghc.haskell.org/trac/ghc/ticketstats): gives a graph of our ticket statistics over time 
- [Summary of open tickets](/trac/ghc/wiki/Status/Tickets), listed by component.
- [Status/SLPJ-Tickets](status/slpj-tickets) is a curation of interesting tickets by SPJ
- The yearly [GHC bug sweep](bug-sweep) attends to lost and forgotten tickets.

- [ A list of all Phabricator patches](https://phabricator.haskell.org/differential/query/dUJ4ndtfSChZ/)
- [ A list of all Phabricator patches that have been accepted](https://phabricator.haskell.org/differential/query/5LIb9B9n_08b/)


GHC's Trac is also used by the [ Haskell Core Libraries Committee](http://www.haskell.org/haskellwiki/Core_Libraries_Committee) to track progress on changes to the [ core libraries](http://www.haskell.org/haskellwiki/Library_submissions#The_Core_Libraries):

- [ Active Core Libraries tickets](https://ghc.haskell.org/trac/ghc/query?status=infoneeded&status=merge&status=new&status=patch&status=upstream&component=Core+Libraries&col=id&col=summary&col=component&col=status&col=type&col=priority&col=milestone&order=priority) (these tickets have "Component" set to "Core Libraries")

## Biannual status reports


Here are biannual GHC status reports, published in the [ Haskell Communities and Activities Report](http://haskell.org/communities/)

- [GHC status October 2014](status/oct14)
- [GHC status May 2014](status/may14)
- [GHC status October 2013](status/oct13)
- [GHC status May 2013](status/may13)
- [GHC status October 2012](status/oct12)
- [GHC status May 2012](status/may12)
- [GHC status October 2011](status/oct11)
- [GHC status May 2011](status/may11)
- [GHC status October 2010](status/oct10)
- [GHC status April 2010](status/apr10)
- [GHC status October 2009](status/oct09)
- [GHC status May 2009](status/may09)
- [GHC status October 2008](status/october08)
- [GHC status May 2008](status/may08)
- [GHC status November 2007](status/nov07)
- [GHC status April 2007](status/april07)
- [GHC status October 2006](status/october06)