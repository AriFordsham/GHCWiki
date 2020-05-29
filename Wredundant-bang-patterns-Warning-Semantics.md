`-Wredundant-bang-patterns` is an extra sanity check that warns when bang pattern is unnecessary. Usually this is the case when the value being pattern matched against is already in WHNF. Of course there are a few tricky cases, most notably dealing with values of unlifted types.

### Motivation
The programmer might expect that `!` performs a deep evaluation or just hopes that random addition of `!` to some arguments may improve speed or memory consumption. This results in useless source clutter.

##### Algebraic Constructors
Always warn if we are pattern matching against Data Constructor - since the act of pattern matching evaluates the value to WHNF already.

However, do check if this is a newtype. If it is - don't warn but perform the check on its argument.

##### Lists

Always warn about bang unless `RebindableSyntax` is ON. In that case we don't know what is going on so do not bother.

##### Tuples

Always warn for tuples - lifted and unlifted.

##### "Strict Bindings"
A bang at the top level of a let or where binding makes the binding strict, regardless of the pattern. In that case we only warn if the value is unlifted, which means it is alw Numbays strict.

`-Wunbanged-strict-patterns` requires us to write banged, unlifted bindings sometimes. For example:

```
let !(I# x) = 5 in ...
```

That bang is required by `-Wunbanged-strict-patterns`. In that case we don't warn.

##### Numeric Patterns
Consider:
```
data One = One deriving Show

instance Eq One where
  _ == _ = True   -- NB: lazy

instance Num One where
  _ + _ = One
  _ - _ = One
  _ * _ = One
  abs _ = One
  signum _ = One
  fromInteger _ = One
  negate _ = One

foo :: One -> One
foo 5 = One

```
Evaluating `foo undefined` yields `One`. Putting a bang on the `5` pattern causes `foo undefined` to throw an exception instead. Thus, we don't warn about bangs.

##### Variable Patterns
Only warn if the type of pattern-matched value is unlifted.

```
foo :: Int -> ()
foo !i = ()      -- should warn

bar :: Int# -> ()
bar !i = ()      -- no warning!
```

##### Wild-card Patterns
Similar to variable patterns, only warn in case of unlifted type.

##### Sum Patterns
Always warn (because ... ?)

##### Other cases when we never warn
We never warn for:
* Lazy patterns
* View Patterns
* Splice Patterns
* Numeric Patterns