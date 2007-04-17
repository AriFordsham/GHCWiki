## Closure conversion as part of vectorisation

**TODO** describe the treatment of higher-order functions and closure conversion here. The relevant paper is [ http://www.cse.unsw.edu.au/\~chak/papers/LCK06.html](http://www.cse.unsw.edu.au/~chak/papers/LCK06.html). The approach is described in more detail in [ http://opus.kobv.de/tuberlin/volltexte/2006/1286/](http://opus.kobv.de/tuberlin/volltexte/2006/1286/).

### Closure-converted types as indexed-types


After some brainstorming, Roman and I (= Manuel) came to the conclusion that we can save ourselves a lot of bookkeeping if we can represent closure-converted types by indexed types (i.e., keeping track of which original types correspond to which converted types).  The idea is to use a class

```wiki
class CC a where
  data CConv a        -- closure converted 'a'
  to :: a -> CConv a
  fr :: CConv a -> a
```


The most interesting instance is that for functions, which reads

```wiki
data Clo a b = forall e. Clo (c -> a -> b) e

class (CC a, CC b) => CC (a -> b) where
  data CConv (a -> b) = CCArrow (Clo a b)
  to f = Clo (\_ -> f) ()
  fr (Clo f e) = f e
```

### From the Skype discussion, 16 Mar 07


For each function `f::ty`, create a closure-converted function `fc::ty'`, where `ty'` is the closure-converted verion of `ty`.
Optimisation: make `fc=f`, if the code for `fc` will be identical to that of `f`.  


For each data type `T`, create a closure-converted data type `Tc`, whose constructors use `Clo` instead of `->`.  Optimisation: if `Tc` is identical to `T`, don't create a new data type.
