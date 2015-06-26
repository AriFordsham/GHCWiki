# Current Status of Template Haskell


This page tracks bug reports and other issues affecting Template Haskell. It is intended to be up-to-date. If, as you're reading this, you see something that's no longer true, **please change it**. (We can always look through the history to get old stuff back.)

[ Richard Eisenberg](https://www.cis.upenn.edu/~eir) is serving as the TH czar as of June 2015.

## Active TH redesigns


This section of the page is for links to other wiki pages discussing design changes to Template Haskell. Please put new pages in `TemplateHaskell/Design/YourNewFeature`.

**Old TH redesigns:** See [TemplateHaskell](template-haskell).

## TH tickets suitable for newcomers


New to hacking on GHC? Try one of these:

No results

## Patched TH tickets:


These are presumably waiting for review.

<table><tr><th>Ticket (Ticket query: status: patch, component: Template+Haskell, max: 0, col: id, col: differential, col: milestone, col: summary, col: priority, col: owner, order: id)</th>
<th>Differential Rev(s) (Ticket query: status: patch, component: Template+Haskell, max: 0, col: id, col: differential, col: milestone, col: summary, col: priority, col: owner, order: differential)</th>
<th>Milestone (Ticket query: status: patch, component: Template+Haskell, max: 0, col: id, col: differential, col: milestone, col: summary, col: priority, col: owner, order: milestone)</th>
<th>Summary (Ticket query: status: patch, component: Template+Haskell, max: 0, col: id, col: differential, col: milestone, col: summary, col: priority, col: owner, order: summary)</th>
<th>Priority (Ticket query: status: patch, component: Template+Haskell, max: 0, col: id, col: differential, col: milestone, col: summary, col: priority, col: owner, desc: 1, order: priority)</th>
<th>Owner (Ticket query: status: patch, component: Template+Haskell, max: 0, col: id, col: differential, col: milestone, col: summary, col: priority, col: owner, order: owner)</th></tr>
<tr><th>[\#12778](https://gitlab.haskell.org//ghc/ghc/issues/12778)</th>
<th>[ Phab:D3003](https://phabricator.haskell.org/D3003)</th>
<th></th>
<th>[Expose variables bound in quotations to reify](https://gitlab.haskell.org//ghc/ghc/issues/12778)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#10391](https://gitlab.haskell.org//ghc/ghc/issues/10391)</th>
<th>[ Phab:D4925](https://phabricator.haskell.org/D4925)</th>
<th></th>
<th>[Ability to get export list of TH reified module](https://gitlab.haskell.org//ghc/ghc/issues/10391)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th>mgsloan</th></tr></table>

## Open TH tickets:


If you know of a ticket not listed here, please set its "Category" to be "Template Haskell" and it should show up.

<table><tr><th>Ticket (Ticket query: status: new, component: Template+Haskell, max: 0, col: id, col: milestone, col: summary, col: priority, col: owner, order: id)</th>
<th>Milestone (Ticket query: status: new, component: Template+Haskell, max: 0, col: id, col: milestone, col: summary, col: priority, col: owner, order: milestone)</th>
<th>Summary (Ticket query: status: new, component: Template+Haskell, max: 0, col: id, col: milestone, col: summary, col: priority, col: owner, order: summary)</th>
<th>Priority (Ticket query: status: new, component: Template+Haskell, max: 0, col: id, col: milestone, col: summary, col: priority, col: owner, desc: 1, order: priority)</th>
<th>Owner (Ticket query: status: new, component: Template+Haskell, max: 0, col: id, col: milestone, col: summary, col: priority, col: owner, order: owner)</th></tr>
<tr><th>[\#10946](https://gitlab.haskell.org//ghc/ghc/issues/10946)</th>
<th>[8.10.1](/trac/ghc/milestone/8.10.1)</th>
<th>[Typed hole inside typed Template Haskell bracket causes panic](https://gitlab.haskell.org//ghc/ghc/issues/10946)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      high
                    </th>
<th></th></tr>
<tr><th>[\#1444](https://gitlab.haskell.org//ghc/ghc/issues/1444)</th>
<th>[⊥](/trac/ghc/milestone/%E2%8A%A5)</th>
<th>[Template Haskell: add proper support for qualified names in non-splicing applications](https://gitlab.haskell.org//ghc/ghc/issues/1444)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#1475](https://gitlab.haskell.org//ghc/ghc/issues/1475)</th>
<th>[⊥](/trac/ghc/milestone/%E2%8A%A5)</th>
<th>[Adding imports and exports with Template Haskell](https://gitlab.haskell.org//ghc/ghc/issues/1475)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#1800](https://gitlab.haskell.org//ghc/ghc/issues/1800)</th>
<th>[⊥](/trac/ghc/milestone/%E2%8A%A5)</th>
<th>[Template Haskell support for running functions defined in the same  module](https://gitlab.haskell.org//ghc/ghc/issues/1800)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#1831](https://gitlab.haskell.org//ghc/ghc/issues/1831)</th>
<th>[⊥](/trac/ghc/milestone/%E2%8A%A5)</th>
<th>[reify never provides the declaration of variables](https://gitlab.haskell.org//ghc/ghc/issues/1831)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#2041](https://gitlab.haskell.org//ghc/ghc/issues/2041)</th>
<th>[⊥](/trac/ghc/milestone/%E2%8A%A5)</th>
<th>[Allow splicing in concrete syntax](https://gitlab.haskell.org//ghc/ghc/issues/2041)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#4372](https://gitlab.haskell.org//ghc/ghc/issues/4372)</th>
<th></th>
<th>[Accept expressions in left-hand side of quasiquotations](https://gitlab.haskell.org//ghc/ghc/issues/4372)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#5416](https://gitlab.haskell.org//ghc/ghc/issues/5416)</th>
<th>[⊥](/trac/ghc/milestone/%E2%8A%A5)</th>
<th>[Local modules and Template Haskell declaration splices](https://gitlab.haskell.org//ghc/ghc/issues/5416)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#5463](https://gitlab.haskell.org//ghc/ghc/issues/5463)</th>
<th></th>
<th>[SPECIALISE pragmas generated from Template Haskell are ignored](https://gitlab.haskell.org//ghc/ghc/issues/5463)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#5467](https://gitlab.haskell.org//ghc/ghc/issues/5467)</th>
<th></th>
<th>[Template Haskell: support for Haddock comments](https://gitlab.haskell.org//ghc/ghc/issues/5467)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#6089](https://gitlab.haskell.org//ghc/ghc/issues/6089)</th>
<th></th>
<th>[Allow declaration splices inside declaration brackets](https://gitlab.haskell.org//ghc/ghc/issues/6089)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>simonpj</th></tr>
<tr><th>[\#7066](https://gitlab.haskell.org//ghc/ghc/issues/7066)</th>
<th></th>
<th>[isInstance does not work for compound types](https://gitlab.haskell.org//ghc/ghc/issues/7066)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>simonpj</th></tr>
<tr><th>[\#7141](https://gitlab.haskell.org//ghc/ghc/issues/7141)</th>
<th></th>
<th>[Inlining the single method of a class can shadow rules](https://gitlab.haskell.org//ghc/ghc/issues/7141)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>simonpj</th></tr>
<tr><th>[\#7277](https://gitlab.haskell.org//ghc/ghc/issues/7277)</th>
<th></th>
<th>[Recompilation check fails for TH unless functions are inlined](https://gitlab.haskell.org//ghc/ghc/issues/7277)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#7808](https://gitlab.haskell.org//ghc/ghc/issues/7808)</th>
<th></th>
<th>[data families and TH names do not mix well (e.g. cannot use TH deriving)](https://gitlab.haskell.org//ghc/ghc/issues/7808)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8398](https://gitlab.haskell.org//ghc/ghc/issues/8398)</th>
<th></th>
<th>[reify module list in TH](https://gitlab.haskell.org//ghc/ghc/issues/8398)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8426](https://gitlab.haskell.org//ghc/ghc/issues/8426)</th>
<th></th>
<th>[one-shot compilation + TH doesn't see instances that is seen in batch mode](https://gitlab.haskell.org//ghc/ghc/issues/8426)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8489](https://gitlab.haskell.org//ghc/ghc/issues/8489)</th>
<th></th>
<th>[clean up dependency and usages handling in interface files](https://gitlab.haskell.org//ghc/ghc/issues/8489)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>mgsloan</th></tr>
<tr><th>[\#8510](https://gitlab.haskell.org//ghc/ghc/issues/8510)</th>
<th></th>
<th>[Clear up what extensions are needed at a Template Haskell splice site](https://gitlab.haskell.org//ghc/ghc/issues/8510)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#8679](https://gitlab.haskell.org//ghc/ghc/issues/8679)</th>
<th></th>
<th>[Extend FunD data constructor with information about type signature](https://gitlab.haskell.org//ghc/ghc/issues/8679)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9693](https://gitlab.haskell.org//ghc/ghc/issues/9693)</th>
<th></th>
<th>[Reloading GHCi with Template Haskell names can panic GHC](https://gitlab.haskell.org//ghc/ghc/issues/9693)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#9699](https://gitlab.haskell.org//ghc/ghc/issues/9699)</th>
<th></th>
<th>[TH function to list names in scope](https://gitlab.haskell.org//ghc/ghc/issues/9699)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#10271](https://gitlab.haskell.org//ghc/ghc/issues/10271)</th>
<th></th>
<th>[Typed Template Haskell splice difficulty when resolving overloading](https://gitlab.haskell.org//ghc/ghc/issues/10271)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#10330](https://gitlab.haskell.org//ghc/ghc/issues/10330)</th>
<th></th>
<th>[Better Template Haskell error message locations](https://gitlab.haskell.org//ghc/ghc/issues/10330)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#10385](https://gitlab.haskell.org//ghc/ghc/issues/10385)</th>
<th></th>
<th>[Annotation restriction is not respected while generating Annotation via TH](https://gitlab.haskell.org//ghc/ghc/issues/10385)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>alanz</th></tr>
<tr><th>[\#10572](https://gitlab.haskell.org//ghc/ghc/issues/10572)</th>
<th></th>
<th>[Type signatures are not implicitly quantified over TH type variables](https://gitlab.haskell.org//ghc/ghc/issues/10572)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#10599](https://gitlab.haskell.org//ghc/ghc/issues/10599)</th>
<th>[8.10.1](/trac/ghc/milestone/8.10.1)</th>
<th>[Template Haskell doesn't allow \`newName "type"\`](https://gitlab.haskell.org//ghc/ghc/issues/10599)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>goldfire</th></tr>
<tr><th>[\#10701](https://gitlab.haskell.org//ghc/ghc/issues/10701)</th>
<th></th>
<th>[-fth-dec-file uses qualified names from hidden modules](https://gitlab.haskell.org//ghc/ghc/issues/10701)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#10702](https://gitlab.haskell.org//ghc/ghc/issues/10702)</th>
<th></th>
<th>[-fth-dec-file uses qualified names in binding positions](https://gitlab.haskell.org//ghc/ghc/issues/10702)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#10707](https://gitlab.haskell.org//ghc/ghc/issues/10707)</th>
<th></th>
<th>[-fth-dec-file outputs invalid case clauses](https://gitlab.haskell.org//ghc/ghc/issues/10707)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>bollmann</th></tr>
<tr><th>[\#10842](https://gitlab.haskell.org//ghc/ghc/issues/10842)</th>
<th></th>
<th>["Reactive" Template Haskell](https://gitlab.haskell.org//ghc/ghc/issues/10842)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#10853](https://gitlab.haskell.org//ghc/ghc/issues/10853)</th>
<th>[8.10.1](/trac/ghc/milestone/8.10.1)</th>
<th>[Refine addTopDecls](https://gitlab.haskell.org//ghc/ghc/issues/10853)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#11078](https://gitlab.haskell.org//ghc/ghc/issues/11078)</th>
<th></th>
<th>[Access to module renaming with reifyModule, in TemplateHaskell](https://gitlab.haskell.org//ghc/ghc/issues/11078)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#11081](https://gitlab.haskell.org//ghc/ghc/issues/11081)</th>
<th>[⊥](/trac/ghc/milestone/%E2%8A%A5)</th>
<th>[Implement Introspective Template Haskell](https://gitlab.haskell.org//ghc/ghc/issues/11081)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#11251](https://gitlab.haskell.org//ghc/ghc/issues/11251)</th>
<th></th>
<th>[isInstance does not work on Typeable with base-4.8 anymore](https://gitlab.haskell.org//ghc/ghc/issues/11251)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>goldfire</th></tr>
<tr><th>[\#11584](https://gitlab.haskell.org//ghc/ghc/issues/11584)</th>
<th></th>
<th>[\[Template Haskell\] Language.Haskell.TH.Syntax.hs contains misleading comment](https://gitlab.haskell.org//ghc/ghc/issues/11584)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>bollmann</th></tr>
<tr><th>[\#11593](https://gitlab.haskell.org//ghc/ghc/issues/11593)</th>
<th></th>
<th>[Template Haskell: Add a way to get names that are neither capturable nor capturing.](https://gitlab.haskell.org//ghc/ghc/issues/11593)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#11812](https://gitlab.haskell.org//ghc/ghc/issues/11812)</th>
<th></th>
<th>[Template Haskell can induce non-unique Uniques](https://gitlab.haskell.org//ghc/ghc/issues/11812)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#12073](https://gitlab.haskell.org//ghc/ghc/issues/12073)</th>
<th></th>
<th>[Missing instance of MonadFix for Q](https://gitlab.haskell.org//ghc/ghc/issues/12073)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#12249](https://gitlab.haskell.org//ghc/ghc/issues/12249)</th>
<th></th>
<th>[Template Haskell top level scoping error](https://gitlab.haskell.org//ghc/ghc/issues/12249)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#12410](https://gitlab.haskell.org//ghc/ghc/issues/12410)</th>
<th></th>
<th>[Somehow detect splicing in ghci](https://gitlab.haskell.org//ghc/ghc/issues/12410)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#12451](https://gitlab.haskell.org//ghc/ghc/issues/12451)</th>
<th></th>
<th>[TemplateHaskell and Data.Typeable - tcIfaceGlobal (local): not found](https://gitlab.haskell.org//ghc/ghc/issues/12451)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#12452](https://gitlab.haskell.org//ghc/ghc/issues/12452)</th>
<th></th>
<th>[TemplateHaskell - variables in top level splices and loading modules.](https://gitlab.haskell.org//ghc/ghc/issues/12452)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#12561](https://gitlab.haskell.org//ghc/ghc/issues/12561)</th>
<th></th>
<th>[Scope extrusion in Template Haskell](https://gitlab.haskell.org//ghc/ghc/issues/12561)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#12818](https://gitlab.haskell.org//ghc/ghc/issues/12818)</th>
<th></th>
<th>[Allow reify to find top-level bindings in later declaration groups](https://gitlab.haskell.org//ghc/ghc/issues/12818)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#13054](https://gitlab.haskell.org//ghc/ghc/issues/13054)</th>
<th></th>
<th>[Generating unique names with template haskell](https://gitlab.haskell.org//ghc/ghc/issues/13054)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#13269](https://gitlab.haskell.org//ghc/ghc/issues/13269)</th>
<th></th>
<th>[Changes in foreign code used in TH do not trigger recompilation](https://gitlab.haskell.org//ghc/ghc/issues/13269)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#13587](https://gitlab.haskell.org//ghc/ghc/issues/13587)</th>
<th></th>
<th>[addTopDecls fails with typed Template Haskell](https://gitlab.haskell.org//ghc/ghc/issues/13587)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#13728](https://gitlab.haskell.org//ghc/ghc/issues/13728)</th>
<th></th>
<th>[Clarify the difference between NameL and NameU in Template Haskell](https://gitlab.haskell.org//ghc/ghc/issues/13728)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#14030](https://gitlab.haskell.org//ghc/ghc/issues/14030)</th>
<th>[8.10.1](/trac/ghc/milestone/8.10.1)</th>
<th>[Implement the "Derive Lift instances for data types in template-haskell" proposal](https://gitlab.haskell.org//ghc/ghc/issues/14030)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>RyanGlScott</th></tr>
<tr><th>[\#14032](https://gitlab.haskell.org//ghc/ghc/issues/14032)</th>
<th></th>
<th>[Can't splice TH quote with infix declaration for name in two different namespaces](https://gitlab.haskell.org//ghc/ghc/issues/14032)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th>RyanGlScott</th></tr>
<tr><th>[\#14212](https://gitlab.haskell.org//ghc/ghc/issues/14212)</th>
<th></th>
<th>[Give better error message with non-supported Backpack/TH use](https://gitlab.haskell.org//ghc/ghc/issues/14212)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#14318](https://gitlab.haskell.org//ghc/ghc/issues/14318)</th>
<th></th>
<th>[TH shadowing bind statement triggers -Wunused-matches](https://gitlab.haskell.org//ghc/ghc/issues/14318)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#14474](https://gitlab.haskell.org//ghc/ghc/issues/14474)</th>
<th></th>
<th>[reify RHS of "value" variable](https://gitlab.haskell.org//ghc/ghc/issues/14474)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#14571](https://gitlab.haskell.org//ghc/ghc/issues/14571)</th>
<th></th>
<th>[RFE: Make template-haskell cabal package require a specific version of GHC](https://gitlab.haskell.org//ghc/ghc/issues/14571)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#14623](https://gitlab.haskell.org//ghc/ghc/issues/14623)</th>
<th></th>
<th>[Allow qAddDependentFile on directories](https://gitlab.haskell.org//ghc/ghc/issues/14623)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#14838](https://gitlab.haskell.org//ghc/ghc/issues/14838)</th>
<th>[8.10.1](/trac/ghc/milestone/8.10.1)</th>
<th>[missing "incomplete-patterns" warning for TH-generated functions](https://gitlab.haskell.org//ghc/ghc/issues/14838)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15167](https://gitlab.haskell.org//ghc/ghc/issues/15167)</th>
<th>[8.10.1](/trac/ghc/milestone/8.10.1)</th>
<th>[DerivClause list is not populated for (TyConI (DataD ...))](https://gitlab.haskell.org//ghc/ghc/issues/15167)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15270](https://gitlab.haskell.org//ghc/ghc/issues/15270)</th>
<th>[8.10.1](/trac/ghc/milestone/8.10.1)</th>
<th>[TH doesn't verify name types during conversion](https://gitlab.haskell.org//ghc/ghc/issues/15270)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15356](https://gitlab.haskell.org//ghc/ghc/issues/15356)</th>
<th>[8.10.1](/trac/ghc/milestone/8.10.1)</th>
<th>[Template Haskell should turn off RebindableSyntax in quotes](https://gitlab.haskell.org//ghc/ghc/issues/15356)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15433](https://gitlab.haskell.org//ghc/ghc/issues/15433)</th>
<th>[8.10.1](/trac/ghc/milestone/8.10.1)</th>
<th>[Internal error with PartialTypeSignatures and TH](https://gitlab.haskell.org//ghc/ghc/issues/15433)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15437](https://gitlab.haskell.org//ghc/ghc/issues/15437)</th>
<th>[8.10.1](/trac/ghc/milestone/8.10.1)</th>
<th>[Internal error when applying a scoped type variable inside a typed expression quotation](https://gitlab.haskell.org//ghc/ghc/issues/15437)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15464](https://gitlab.haskell.org//ghc/ghc/issues/15464)</th>
<th>[8.10.1](/trac/ghc/milestone/8.10.1)</th>
<th>[Template Haskell creates System names when it shouldn't](https://gitlab.haskell.org//ghc/ghc/issues/15464)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15533](https://gitlab.haskell.org//ghc/ghc/issues/15533)</th>
<th>[8.6.1](/trac/ghc/milestone/8.6.1)</th>
<th>[Access the number of bits in the target machine's Int type at compile time](https://gitlab.haskell.org//ghc/ghc/issues/15533)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15760](https://gitlab.haskell.org//ghc/ghc/issues/15760)</th>
<th></th>
<th>[Preserve parens in TH](https://gitlab.haskell.org//ghc/ghc/issues/15760)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15824](https://gitlab.haskell.org//ghc/ghc/issues/15824)</th>
<th></th>
<th>[Prefix/infix distinction in TemplateHaskell types is lost](https://gitlab.haskell.org//ghc/ghc/issues/15824)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15833](https://gitlab.haskell.org//ghc/ghc/issues/15833)</th>
<th></th>
<th>[Typed template haskell quote fails to typecheck when spliced due to an ambiguous type variable](https://gitlab.haskell.org//ghc/ghc/issues/15833)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15863](https://gitlab.haskell.org//ghc/ghc/issues/15863)</th>
<th></th>
<th>[Splcing a type class method selects the wrong instance](https://gitlab.haskell.org//ghc/ghc/issues/15863)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#15865](https://gitlab.haskell.org//ghc/ghc/issues/15865)</th>
<th></th>
<th>[Typed template haskell and implicit parameters lead to incorrect results](https://gitlab.haskell.org//ghc/ghc/issues/15865)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#16176](https://gitlab.haskell.org//ghc/ghc/issues/16176)</th>
<th></th>
<th>[Let-insertion for template haskell](https://gitlab.haskell.org//ghc/ghc/issues/16176)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#16177](https://gitlab.haskell.org//ghc/ghc/issues/16177)</th>
<th></th>
<th>[Rename Q (TExp a) to Code a](https://gitlab.haskell.org//ghc/ghc/issues/16177)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#16178](https://gitlab.haskell.org//ghc/ghc/issues/16178)</th>
<th></th>
<th>[Brackets and splices should be overloaded like the static keyword](https://gitlab.haskell.org//ghc/ghc/issues/16178)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#16300](https://gitlab.haskell.org//ghc/ghc/issues/16300)</th>
<th></th>
<th>[Make TH always reify data types with explicit return kinds](https://gitlab.haskell.org//ghc/ghc/issues/16300)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#16396](https://gitlab.haskell.org//ghc/ghc/issues/16396)</th>
<th></th>
<th>[TH doesn't preserve \`forall\`](https://gitlab.haskell.org//ghc/ghc/issues/16396)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      normal
                    </th>
<th></th></tr>
<tr><th>[\#4222](https://gitlab.haskell.org//ghc/ghc/issues/4222)</th>
<th></th>
<th>[Template Haskell lets you reify supposedly-abstract data types](https://gitlab.haskell.org//ghc/ghc/issues/4222)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th></th></tr>
<tr><th>[\#5016](https://gitlab.haskell.org//ghc/ghc/issues/5016)</th>
<th></th>
<th>[Make Template Haskell: -ddump-splices generate executable code](https://gitlab.haskell.org//ghc/ghc/issues/5016)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th></th></tr>
<tr><th>[\#5959](https://gitlab.haskell.org//ghc/ghc/issues/5959)</th>
<th></th>
<th>[Top level splice in Template Haskell has over-ambitious lexical scope?](https://gitlab.haskell.org//ghc/ghc/issues/5959)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th></th></tr>
<tr><th>[\#10331](https://gitlab.haskell.org//ghc/ghc/issues/10331)</th>
<th></th>
<th>[Accept HsSyn in splices and generate it in quotes (ghc-api)](https://gitlab.haskell.org//ghc/ghc/issues/10331)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th></th></tr>
<tr><th>[\#11377](https://gitlab.haskell.org//ghc/ghc/issues/11377)</th>
<th></th>
<th>[Template Haskell only imports](https://gitlab.haskell.org//ghc/ghc/issues/11377)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th></th></tr>
<tr><th>[\#12034](https://gitlab.haskell.org//ghc/ghc/issues/12034)</th>
<th></th>
<th>[Template Haskell + hs-boot = Not in scope during type checking, but it passed the renamer](https://gitlab.haskell.org//ghc/ghc/issues/12034)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th></th></tr>
<tr><th>[\#13551](https://gitlab.haskell.org//ghc/ghc/issues/13551)</th>
<th></th>
<th>[Support DEPRECATED and WARNING pragmas on Template Haskell](https://gitlab.haskell.org//ghc/ghc/issues/13551)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      low
                    </th>
<th></th></tr>
<tr><th>[\#1012](https://gitlab.haskell.org//ghc/ghc/issues/1012)</th>
<th></th>
<th>[ghc panic with mutually recursive modules and template haskell](https://gitlab.haskell.org//ghc/ghc/issues/1012)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      lowest
                    </th>
<th></th></tr>
<tr><th>[\#2340](https://gitlab.haskell.org//ghc/ghc/issues/2340)</th>
<th></th>
<th>[Improve Template Haskell error recovery](https://gitlab.haskell.org//ghc/ghc/issues/2340)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      lowest
                    </th>
<th></th></tr>
<tr><th>[\#3355](https://gitlab.haskell.org//ghc/ghc/issues/3355)</th>
<th></th>
<th>[Refactor Template Haskell syntax conversions](https://gitlab.haskell.org//ghc/ghc/issues/3355)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      lowest
                    </th>
<th></th></tr>
<tr><th>[\#11378](https://gitlab.haskell.org//ghc/ghc/issues/11378)</th>
<th>[⊥](/trac/ghc/milestone/%E2%8A%A5)</th>
<th>[Use the compiler that built ghc for dynamic code loading, for cross-compiling](https://gitlab.haskell.org//ghc/ghc/issues/11378)</th>
<th>
                      
                      
                      
                      
                      
                      
                      
                      
                      lowest
                    </th>
<th></th></tr></table>