# Git for Darcs Users

## darcs record


Git supports interactive recording very similar to darcs.

```wiki
git add -p
```


or

```wiki
git add -i
```


The main difference is that Git does not automatically commit the changes.  You have to do that manually using

```wiki
git commit [-m "commit message"]
```


If you do not supply a commit message, it will open your default editor.  If you want to abort the commit, use an empty commit message.

**Tip**: If you want to see the diff when you edit the commit message, use

```wiki
git commit -v
```

### darcs record -a

```wiki
git commit -a
```


This will add and commit all (not ignored) files.

## darcs changes

```wiki
git log
```

### darcs changes --last \<N\>

```wiki
git log -n <N>
```

### darcs changes --summary

```wiki
git log --stat
```


See `git log --help` for a lot of extra options, to refine the output.

## darcs whatsnew

```wiki
git status
```

## darcs diff

```wiki
git diff
```

TODO describe diff commands for index

## darcs revert

```wiki
git reset --hard
```

**Note**: `git reset` only resets the staged files, i.e., the things added with `git add`.

## darcs unrecord


...

## darcs annotate

```wiki
git blame
```

# General Notes

## The Index

## Suggested Workflow