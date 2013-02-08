# Proposed changes to arrow notation


This page describes a couple of proposed changes to the notation enabled with -XArrows.
([current documentation](http://www.haskell.org/ghc/docs/latest/html/users_guide/arrow-notation.html))

## Changing the types of arrow operators


Currently, the type of each argument of an operator (and its result) is required have the form

```wiki
a (...(e,t1), ... tn) t
```


where `e` is a type variable shared by all these types, but the arrow types `a` can vary.  The User's Guide has these examples:

```wiki
ArrowPlus a => (<+>) :: a e c -> a e c -> a e c
untilA :: ArrowChoice a => a e () -> a e Bool -> a e ()
handleA :: ... => a e c -> a (e,Ex) c -> a e c
bracketA :: ... => a e b -> a (e,b) c -> a (e,c) d -> a e d
runReader :: ... => a e c -> a' (e,State) c
runState :: ... => a e c -> a' (e,State) (c,State)
bind :: Arrow a => a e b -> a (e,b) c -> a e c
bind_ :: Arrow a => a e b -> a e c -> a e c
cond :: ArrowChoice a => a e b -> a e b -> a (e,Bool) b
```


The problem is that to work out how many `ti`s there are, the type checker needs to be able to determine whether a type is a pair type or this Skolem variable `e`, and this can't be done with GHC's new constraint-based type system.


The plan is to re-arrange the shapes of the argument and result types to

```wiki
a (e, (t1, ... (tn, ())...)) t
```


For the above examples, the new types will be

```wiki
ArrowPlus a => (<+>) :: a (e,()) c -> a (e,()) c -> a (e,()) c
untilA :: ArrowChoice a => a (e,()) () -> a (e,()) Bool -> a (e,()) ()
handleA :: ... => a (e,()) c -> a (e,(Ex,())) c -> a (e,()) c
bracketA :: ... => a (e,()) b -> a (e,(b,())) c -> a (e,(c,())) d -> a (e,()) d
runReader :: ... => a (e,()) c -> a' (e,(State,())) c
runState :: ... => a (e,()) c -> a' (e,(State,())) (c,State)
bind :: Arrow a => a (e,()) b -> a (e,(b,())) c -> a (e,()) c
bind_ :: Arrow a => a (e,()) b -> a (e,()) c -> a (e,()) c
cond :: ArrowChoice a => a (e,()) b -> a (e,()) b -> a (e,(Bool,())) b
```


Now in the cases of `(<+>)`, `untilA` and `bind`, the new types are specializations of the old, so those operators will still work, but the others will need to be re-defined with the new types.

## Generalizing the types of commands


The translation of many of the different varieties of command does not require the full `Arrow` class, but rather just

```wiki
premap :: Arrow a => (b -> b') -> a b' c -> a b c
premap f g = arr f >>> g
```


So the proposal is to introduce a superclass of `Arrow` with just this:

```wiki
class PreArrow a where
    premap :: Arrow a => (b -> b') -> a b' c -> a b c
```


and require that class instead of `Arrow` for the types of those constructs. ([ libraries proposal](http://thread.gmane.org/gmane.comp.lang.haskell.libraries/17609))


This shouldn't break any code that uses arrows, but will require rewriting of instances of `Arrow`.
