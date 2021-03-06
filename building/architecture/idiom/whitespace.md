# Idiom: whitespace


make has a rather ad-hoc approach to whitespace. Most of the time it ignores it, e.g.

```wiki
FOO = bar
```


sets `FOO` to `"bar"`, not `" bar"`. However, sometimes whitespace is significant,
and calling macros is one example. For example, we used to have a call

```wiki
$(call all-target, $$($1_$2_INPLACE))
```


and this passed `" $$($1_$2_INPLACE)"` as the argument to `all-target`. This in turn generated

```wiki
.PHONY: all_ inplace/bin/ghc-asm
```


which caused an infinite loop, as make continually thought that `ghc-asm` was out-of-date, rebuilt it,
reinvoked make, and then thought it was out of date again.


The moral of the story is, avoid white space unless you're sure it'll be OK!
