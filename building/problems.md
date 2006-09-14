# Common build problems

## General builds

### GCC 4 issues


It has been observed on Gentoo systems that GCC 4 may fail, complaining about there being no `-nopie` option. Try using GCC 3.

## Building from the Darcs tree

<table><tr><th>**Problem**</th>
<td>```wiki
Distribution/Compat/FilePath.hs:2: error: Cabal/Distribution/Compat/FilePath.hs: No such file or directory
make[1]: *** [depend] Error 1
make: *** [stage1] Error 1
```

</td></tr>
<tr><th>**Possible Solution**</th>
<td>
Be sure you have run `sh darcs-all get` to get all necessary packages. Don't forget to run `autoreconf` again after you pull in new packages.
</td></tr></table>