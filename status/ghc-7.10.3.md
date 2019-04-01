# GHC plans for 7.10.3


GHC 7.10.3 will be released in early November 2015. We will do so if (but only if!) we have documented cases of "show-stoppers" in 7.10.2.  Namely, cases from users where
 

- You are unable to use 7.10.2 because of some bug
- There is no reasonable workaround, so you are truly stuck
- We know how to fix it
- The fix is not too disruptive; i.e. does not risk introducing a raft of new bugs


So please tell us if your problem falls in to this category, saying a bit about why it is important to you, and why you can't work around it.  It's worth attaching the details to the relevant ticket, add it to the manual list just below, and send a heads-up email to ghc-devs to draw attention to it.


Here's a list of the tickets that appear to fall into this show-stopping category:

- #10528, #10829, #10745 (simplification on LHS and RHS of rules)
- #10726, #10795 ([Ruben Moor email](https://mail.haskell.org/pipermail/glasgow-haskell-users/2015-September/026035.html), [ Moritz Drexl email](https://mail.haskell.org/pipermail/glasgow-haskell-users/2015-September/026049.html))
- #10568, #10672 ([Luke Iannini email](https://mail.haskell.org/pipermail/ghc-devs/2015-September/009973.html))
- #10777 (Windows): some pressure to get these patches into 7.10.3, eg same Moritz Drexl email above


Possible (not world ending, but probably not easy to workaround and breaks):

- #9238 (Levent Erkok): mentioned [reddit](https://www.reddit.com/r/haskell/comments/3kwsu4/ghcdev_anyone_need_ghc_7103) as a [ bug for SBV](https://github.com/LeventErkok/sbv/issues/138#issuecomment-139804285)


See the %7.10.3 milestone.


## Tickets slated for 7.10.3

See the %7.10.3 milestone.


