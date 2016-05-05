# Guidelines for boot library maintainers


Maintaining a GHC [boot library](commentary/libraries) is generally quite similar to any other well-maintained Haskell library. However, we do have a few specific requests,

- Follow the [ Package Versioning Policy](https://wiki.haskell.org/Package_versioning_policy)

- Avoid non-fast-forward changes to tracked branches in the repository followed by GHC

- Use annotated and signed tags for releases

## Git tags

`git push` doesn't push tags by default, you have to push the tag explicitly via `git push origin v1.2.3.4` or `git push --tags` (be careful though, the later command pushes \*all\* local tags)


For tagging the release in Git **annotated Git tags** shall be used. These are tags which contain a bit more metadata (e.g. creation date, tagger name, a tag message) than the "lightweight" tags created by `git tag` by default (see [ git-tag(1)](https://git-scm.com/docs/git-tag) for details). To create a annotated tag use the `--annotate` (`-a`) flag, e.g.

```wiki
git tag -a -s v1.2.3.4
```


Note how here we also used the `--sign` (`-s`) flag, which requests that Git attach a GPG signature to the tag. It is highly recommended that GHC boot libraries use signed releases.


Like with `git commit`, the above command will drop you into a text editor to compose a "tag message". You may also use the usual `-m` flag to specify a message in the command-line.


Here's [ an example](https://git.haskell.org/packages/deepseq.git/tag/c32a156c8dafaea05e91563afe2f72ad3590f57b) of a signed Git tag object.
