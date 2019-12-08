(I was also `heisenbug` in the Trac)

Agenda:

- Pointer tagging for big families (#14373)
  - [ ] add to wiki
  - [ ] perf implications
  - [ ] cleanup patches (strictness?, better `partition`)
  - [ ] figure out why the CI had an allocation bump (which went away)
- Reusing memory congruent objects in STG (#13861)