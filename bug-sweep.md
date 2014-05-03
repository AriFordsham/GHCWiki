# GHC bug sweep


In the tradition of the [ Ubuntu 5-a-day](https://wiki.ubuntu.com/5-A-Day), 
the GHC Bug Sweep is a scheme in a similar vein.  The
main aim is simple: we want to make sure that no ticket is
forgotten.  So, what we plan to do is to look at every ticket in the
database in sequence, starting from the oldest, and try to make some
progress on the ticket.  In this way we'll ensure that we keep the
amount of cruft in the ticket database down to a minimum, keep the database healthy, and keep everything moving.


The bug sweep is a way that virtually anyone can contribute to GHC, in
as small or large a way as you like.  Whenever you have a spare minute
or two, claim a ticket and look at it (see below for how to claim a
ticket).  You don't necessarily have to fix the bug or implement the
feature: all you have to do is make *some* progress on the ticket.
Here are some things to check:

- **All tickets**: 

  - Check for duplicates (Google search with "site:hackage.haskell.org" is usually better than using Trac's search).
  - Tidy up the description: add markup if necessary, link to related information and/or other tickets
  - Check that the ticket is categorised correctly, including

    - the title is a good summary of the bug
    - platform/OS are correct
    - component is correct
    - it is on the correct milestone (developers only)
    - the "difficulty" is a reasonable estimate (developers only)
  - If the ticket has a patch 

    - (developers only) review the patch
    - if it looks ready to go, add a comment to the ticket to say so.

- **Bugs**:

  - Check that the bug hasn't already been fixed.
  - Set the new "Type of Failure" field
  - If the bug has some reproduction instructions, try them out with a recent GHC and see if the bug still happens.  If the results are different, update the ticket to include that information.
  - Add the program to the [testsuite](building/running-tests#adding-a-new-test). 
  - If the bug does not have repro instructions, ask the submitter for more details.
  - Check that there is still value in having the ticket open.  If we cannot make progress without feedback from the submitter, and a long time has elapsed (e.g. 6 months), then we should close the bug.

- **Feature requests** and **tasks**: 

  - check that it hasn't already been done.
  - link to related feature requests


Here are some [more details on how we use the bug tracker](working-conventions/bug-tracker).


Note: you don't have to do *all* of the things on the list.  Doing *any* of them is good.  The main thing is that every ticket gets at least looked at.


For many tickets there may be nothing to do: if so, just proceed to the next ticket.  You might not be
able to reproduce the bug (because you don't have access to the right
platform, for instance).  In that case just add a comment to the
ticket to note that the bug needs to be reproduced with an up to date
GHC, and hopefully someone else will pick it up.


How do we track which tickets have been looked at in the sweep yet?
Below is a list: just [ edit this page](http://hackage.haskell.org/trac/ghc/wiki/BugSweep?action=edit), remove a bug from the list, and
look at it.  When the list is empty, we'll take a new snapshot of the
database and start again.

## The tickets (bugs, tasks, feature requests, the lot)

[ Edit this page](http://hackage.haskell.org/trac/ghc/wiki/BugSweep?action=edit) to remove a ticket from the following list.  You don't have to take the one at the top, but the top is as good a place to start as any:

- [\#2607](https://gitlab.haskell.org//ghc/ghc/issues/2607)
- [\#2625](https://gitlab.haskell.org//ghc/ghc/issues/2625)
- [\#2628](https://gitlab.haskell.org//ghc/ghc/issues/2628)
- [\#2630](https://gitlab.haskell.org//ghc/ghc/issues/2630)
- [\#2640](https://gitlab.haskell.org//ghc/ghc/issues/2640)
- [\#2641](https://gitlab.haskell.org//ghc/ghc/issues/2641)
- [\#2642](https://gitlab.haskell.org//ghc/ghc/issues/2642)
- [\#2648](https://gitlab.haskell.org//ghc/ghc/issues/2648)
- [\#2708](https://gitlab.haskell.org//ghc/ghc/issues/2708)
- [\#2710](https://gitlab.haskell.org//ghc/ghc/issues/2710)
- [\#2721](https://gitlab.haskell.org//ghc/ghc/issues/2721)
- [\#2725](https://gitlab.haskell.org//ghc/ghc/issues/2725)
- [\#2731](https://gitlab.haskell.org//ghc/ghc/issues/2731)
- [\#2737](https://gitlab.haskell.org//ghc/ghc/issues/2737)
- [\#2742](https://gitlab.haskell.org//ghc/ghc/issues/2742)
- [\#2776](https://gitlab.haskell.org//ghc/ghc/issues/2776)
- [\#2786](https://gitlab.haskell.org//ghc/ghc/issues/2786)
- [\#2803](https://gitlab.haskell.org//ghc/ghc/issues/2803)
- [\#2805](https://gitlab.haskell.org//ghc/ghc/issues/2805)
- [\#2836](https://gitlab.haskell.org//ghc/ghc/issues/2836)
- [\#2867](https://gitlab.haskell.org//ghc/ghc/issues/2867)
- [\#2893](https://gitlab.haskell.org//ghc/ghc/issues/2893)
- [\#2895](https://gitlab.haskell.org//ghc/ghc/issues/2895)
- [\#2896](https://gitlab.haskell.org//ghc/ghc/issues/2896)
- [\#2917](https://gitlab.haskell.org//ghc/ghc/issues/2917)
- [\#2926](https://gitlab.haskell.org//ghc/ghc/issues/2926)
- [\#2933](https://gitlab.haskell.org//ghc/ghc/issues/2933)
- [\#2940](https://gitlab.haskell.org//ghc/ghc/issues/2940)
- [\#2945](https://gitlab.haskell.org//ghc/ghc/issues/2945)
- [\#2946](https://gitlab.haskell.org//ghc/ghc/issues/2946)
- [\#2950](https://gitlab.haskell.org//ghc/ghc/issues/2950)
- [\#2968](https://gitlab.haskell.org//ghc/ghc/issues/2968)
- [\#2986](https://gitlab.haskell.org//ghc/ghc/issues/2986)
- [\#2988](https://gitlab.haskell.org//ghc/ghc/issues/2988)
- [\#2991](https://gitlab.haskell.org//ghc/ghc/issues/2991)
- [\#3000](https://gitlab.haskell.org//ghc/ghc/issues/3000)
- [\#3003](https://gitlab.haskell.org//ghc/ghc/issues/3003)
- [\#3021](https://gitlab.haskell.org//ghc/ghc/issues/3021)
- [\#3024](https://gitlab.haskell.org//ghc/ghc/issues/3024)
- [\#3032](https://gitlab.haskell.org//ghc/ghc/issues/3032)
- [\#3034](https://gitlab.haskell.org//ghc/ghc/issues/3034)
- [\#3048](https://gitlab.haskell.org//ghc/ghc/issues/3048)
- [\#3052](https://gitlab.haskell.org//ghc/ghc/issues/3052)
- [\#3055](https://gitlab.haskell.org//ghc/ghc/issues/3055)
- [\#3061](https://gitlab.haskell.org//ghc/ghc/issues/3061)
- [\#3065](https://gitlab.haskell.org//ghc/ghc/issues/3065)
- [\#3070](https://gitlab.haskell.org//ghc/ghc/issues/3070)
- [\#3073](https://gitlab.haskell.org//ghc/ghc/issues/3073)
- [\#3081](https://gitlab.haskell.org//ghc/ghc/issues/3081)
- [\#3085](https://gitlab.haskell.org//ghc/ghc/issues/3085)
- [\#3107](https://gitlab.haskell.org//ghc/ghc/issues/3107)
- [\#3122](https://gitlab.haskell.org//ghc/ghc/issues/3122)
- [\#3123](https://gitlab.haskell.org//ghc/ghc/issues/3123)
- [\#3134](https://gitlab.haskell.org//ghc/ghc/issues/3134)
- [\#3138](https://gitlab.haskell.org//ghc/ghc/issues/3138)
- [\#3140](https://gitlab.haskell.org//ghc/ghc/issues/3140)
- [\#3178](https://gitlab.haskell.org//ghc/ghc/issues/3178)
- [\#3184](https://gitlab.haskell.org//ghc/ghc/issues/3184)
- [\#3191](https://gitlab.haskell.org//ghc/ghc/issues/3191)
- [\#3192](https://gitlab.haskell.org//ghc/ghc/issues/3192)
- [\#3205](https://gitlab.haskell.org//ghc/ghc/issues/3205)
- [\#3215](https://gitlab.haskell.org//ghc/ghc/issues/3215)
- [\#3217](https://gitlab.haskell.org//ghc/ghc/issues/3217)
- [\#3231](https://gitlab.haskell.org//ghc/ghc/issues/3231)
- [\#3238](https://gitlab.haskell.org//ghc/ghc/issues/3238)
- [\#3251](https://gitlab.haskell.org//ghc/ghc/issues/3251)
- [\#3266](https://gitlab.haskell.org//ghc/ghc/issues/3266)
- [\#3282](https://gitlab.haskell.org//ghc/ghc/issues/3282)
- [\#3283](https://gitlab.haskell.org//ghc/ghc/issues/3283)
- [\#3314](https://gitlab.haskell.org//ghc/ghc/issues/3314)
- [\#3321](https://gitlab.haskell.org//ghc/ghc/issues/3321)
- [\#3351](https://gitlab.haskell.org//ghc/ghc/issues/3351)
- [\#3353](https://gitlab.haskell.org//ghc/ghc/issues/3353)
- [\#3354](https://gitlab.haskell.org//ghc/ghc/issues/3354)
- [\#3355](https://gitlab.haskell.org//ghc/ghc/issues/3355)
- [\#3372](https://gitlab.haskell.org//ghc/ghc/issues/3372)
- [\#3373](https://gitlab.haskell.org//ghc/ghc/issues/3373)
- [\#3376](https://gitlab.haskell.org//ghc/ghc/issues/3376)
- [\#3379](https://gitlab.haskell.org//ghc/ghc/issues/3379)
- [\#3384](https://gitlab.haskell.org//ghc/ghc/issues/3384)
- [\#3427](https://gitlab.haskell.org//ghc/ghc/issues/3427)
- [\#3447](https://gitlab.haskell.org//ghc/ghc/issues/3447)
- [\#3452](https://gitlab.haskell.org//ghc/ghc/issues/3452)
- [\#3458](https://gitlab.haskell.org//ghc/ghc/issues/3458)
- [\#3462](https://gitlab.haskell.org//ghc/ghc/issues/3462)
- [\#3464](https://gitlab.haskell.org//ghc/ghc/issues/3464)
- [\#3470](https://gitlab.haskell.org//ghc/ghc/issues/3470)
- [\#3483](https://gitlab.haskell.org//ghc/ghc/issues/3483)
- [\#3490](https://gitlab.haskell.org//ghc/ghc/issues/3490)
- [\#3509](https://gitlab.haskell.org//ghc/ghc/issues/3509)
- [\#3511](https://gitlab.haskell.org//ghc/ghc/issues/3511)
- [\#3517](https://gitlab.haskell.org//ghc/ghc/issues/3517)
- [\#3533](https://gitlab.haskell.org//ghc/ghc/issues/3533)
- [\#3541](https://gitlab.haskell.org//ghc/ghc/issues/3541)
- [\#3543](https://gitlab.haskell.org//ghc/ghc/issues/3543)
- [\#3545](https://gitlab.haskell.org//ghc/ghc/issues/3545)
- [\#3547](https://gitlab.haskell.org//ghc/ghc/issues/3547)
- [\#3549](https://gitlab.haskell.org//ghc/ghc/issues/3549)
- [\#3557](https://gitlab.haskell.org//ghc/ghc/issues/3557)
- [\#3559](https://gitlab.haskell.org//ghc/ghc/issues/3559)
- [\#3569](https://gitlab.haskell.org//ghc/ghc/issues/3569)
- [\#3571](https://gitlab.haskell.org//ghc/ghc/issues/3571)
- [\#3575](https://gitlab.haskell.org//ghc/ghc/issues/3575)
- [\#3577](https://gitlab.haskell.org//ghc/ghc/issues/3577)
- [\#3583](https://gitlab.haskell.org//ghc/ghc/issues/3583)
- [\#3588](https://gitlab.haskell.org//ghc/ghc/issues/3588)
- [\#3601](https://gitlab.haskell.org//ghc/ghc/issues/3601)
- [\#3606](https://gitlab.haskell.org//ghc/ghc/issues/3606)
- [\#3615](https://gitlab.haskell.org//ghc/ghc/issues/3615)
- [\#3619](https://gitlab.haskell.org//ghc/ghc/issues/3619)
- [\#3620](https://gitlab.haskell.org//ghc/ghc/issues/3620)
- [\#3625](https://gitlab.haskell.org//ghc/ghc/issues/3625)
- [\#3628](https://gitlab.haskell.org//ghc/ghc/issues/3628)
- [\#3632](https://gitlab.haskell.org//ghc/ghc/issues/3632)
- [\#3645](https://gitlab.haskell.org//ghc/ghc/issues/3645)
- [\#3646](https://gitlab.haskell.org//ghc/ghc/issues/3646)
- [\#3649](https://gitlab.haskell.org//ghc/ghc/issues/3649)
- [\#3654](https://gitlab.haskell.org//ghc/ghc/issues/3654)
- [\#3658](https://gitlab.haskell.org//ghc/ghc/issues/3658)