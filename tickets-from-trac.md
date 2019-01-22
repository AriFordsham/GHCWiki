
All tickets, their comments, and attachments previously present on [Trac](https://ghc.haskell.org/trac/ghc/) have been migrated to GitLab. During this import authorship, formatting, and insofar as is possible, metadata was preserved. Some Trac metadata fields have no analogue in GitLab. Changes in these fields are represented in GitLab by collapsed-by-default *Trac metadata* blocks.

In particular, fields have been translated as follows:

| Trac field       | GitLab representation |
| :--------------- | :-------------------- |
| Title            | Ticket title          |
| Description      | Ticket description    |
| Reported by      | Ticket author         |
| Owned by         | Ticket owner          |
| Priority         | Ticket weight         |
| Milestone        | [Milestone](https://gitlab.haskell.org/ghc/ghc/milestones) |
| Component        | Metadata block        |
| Version          | Metadata block        |
| Keywords         | Tags (selectively)    |
| Cc               | Metadata block        |
| Operating System | Metadata block        |
| Architecture     | Metadata block        |
| Type of failure  | Metadata block        |
| Test Case        | Metadata block        |
| Blocked by       | Metadata block        |
| Blocking         | Metadata block        |
| Related tickets  | Related tickets       |
| Differential Revs | Metadata block       |
| Wiki page         | Metadata block       |
| Attachments       | Either attachments or [Snippets](https://gitlab.haskell.org/ghc/ghc/snippets) |
| State             | Ticket state and tags |
