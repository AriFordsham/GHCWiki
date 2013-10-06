# Emacs tips and tricks


Most of us use Emacs for GHC development. This page lists various Emacs configuration tips and tricks used by GHC developers in hope that they will be useful to others. Each entry includes a short description, instruction how to enable given setting (if it contains only a block of lisp code it means you need to add it to your `.emacs` configuration file) and a more detailed commentary

# General

## Automatically remove trailing whitespaces on save

**Description**: Automatically removes trailing whitespaces when file is saved.
**How to enable**:

```wiki
(add-hook 'before-save-hook 'delete-trailing-whitespace)
```

**Comment**: Currently source code of GHC contains lots of trailing whitespaces, which means that **this setting is dangerous**. It will remove ALL trailing whitespaces in every file that you edit, which means you might have one or two lines changed by you and a hundred lines automatically changed by removing trailing whitespaces. This will require you to separate whitespaces into a separate commit by using `git add -i`. This is tedious, so be warned.

# Haskell-specific

# GHC-specific

**Description**: 
**How to enable**:
**Comment**:
