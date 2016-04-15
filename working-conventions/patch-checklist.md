# Checklist for submitted patches


This is the beginning of a checklist to check for patches submitted for merge.

- Includes regression test?

- Does patch add a user-visible command-line flag?

  - Document in users guide (`doc/user_guide`) and `utils/mkUserGuidePart/Options/`

- Introduces new syntax?

  - Document in `doc/user_guide/glasgow_exts.rst`
  - Document in `doc/user_guide/*-relnotes.rst`
  - Add Template Haskell support

- Can build with last two releases of GHC?
