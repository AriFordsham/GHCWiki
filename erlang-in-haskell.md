# Distributed Haskell Processes (or Erlang in Haskell)


Haskell is great at shared-memory concurrency, but we do not yet 
have a good story for distributed systems that need:

- Disjoint address spaces
- Message passing rather than shared memory
- Relatively long latencies on messages
- Possibility of failure (computers or links going down)


The current fashion for "cloud computing" also needs a similar
computational model.


I admire Erlang, which was designed for exactly this combination
of circumstances.  Which leads to the following question: could
we take the best of Erlang and embed it as a DSL into Haskell?
This page summarises a possible design.

## Processes


I call the underlying monad `Erlang` in honour of Erlang, although
many details will differ.

```wiki
newtype Erlang a
instance Monad Erlang
liftIO :: IO a -> Erlang a    -- You can do IO in the Erlang monad
```


Creating a new process:

```wiki
spawn   :: Closure (Erlang ()) -> Erlang Pid
spawnAt :: Location -> Closure (Erlang ()) -> Erlang Pid
-- Problem: must not capture any free varaibles

self :: Erlang Pid
```


Here `Closure` is some kind of specification of what you want the
remote process to do.  More on that anon.


Processes communicate (only) over typed channels.

```wiki
newChan :: Erlang (Send a, Recv a)
send    :: Serialisable a => Send a -> a -> Erlang ()
receive :: Serialisable a => Recv a -> Erlang a

class Serialisable a where
  serialise :: a -> Bytestring
```


Using typed channels is different to Erlang, where messages are send
to the process.

## Monitoring processes


One of Erlang's most distinctive strength is that one process A can
"monitor" another B, and be told if B dies.


A process can exit, specifying a reason for doing so. Moreover,
a process can shoot down another process, again specifying a reason:

```wiki
exit :: ExitReason -> Erlang ()
  -- Exit this process
  -- Like an exception... can be caught
catch :: Erlang a -> (ExitReason -> Erlang a) -> Erlang a

sendExit :: Pid -> ExitReason -> Erlang ()
  -- Send another process an exit signal with specified reason
  -- In Erlang the recipient cannot catch this

data ExitReason 
  = Normal 
  | Kill 
  | Killed 
  | Exception Exception    -- Extensible?
```


This "shooting down" is done by a special kind of message,
called an **exit signal**.  An exit signal carries an `ExitReason`.


A process can "link" to another process:

```wiki
link :: Pid -> Erlang ()
  -- Link this process to another one
  -- Exception if the process is dead
  -- If the other process dies or terminates normally, 
  --   you get sent an exit signal
  -- Linking is symmetric

monitor :: Pid -> Erlang ()
  -- If the other process dies or terminates normally, 
  --   you get sent a message
  -- Monitoring is not symmetric
  -- Can be implemented in terms of link (by spawning another process)

isProcessAlive :: Pid -> Erlang Bool
```


Note that if you link to a process and it dies, *you* get an exit
signal.
A process has a "trap-exit status", which controls what happens
if you receive an exit signal.

```wiki
trapExit :: Maybe (Send ExitReason) -> Erlang ()
```


When a process receives an exit signal, it is processed as follows

- If reason = `Kill` =\> process dies, sending an exit signal with reason `Killed` to all linked processes

- Otherwise, if the process has `TrapExit` = `Just ch`, then exit-signal is turned into a message, and sent to `ch`

- Otherwise, if reason = `Normal`, do nothing

- Otherwise, the process dies sending `ExitSignal(same-reason)` to linked processes


Typically "worker processes" will have `TrapExit` off 
and "system processes" will have them on, so that they can
respond to the death of their linked processes.

## Process registry


The process registry lets you register processes so that others can find them.
There is one process register per node, and one global registry. The Erlang
API is something like this:

```wiki
getRegistry :: Erlang Pid

register :: String -> Pid -> Erlang ()
  -- Exception if name [or Pid] is already registered

unregister :: String -> Erlang ()
  -- Does not require that you are the guy!
  -- Exception if not registered
  -- A process that terminates is unregisted automatically

whereIs :: String -> Erlang (Maybe Pid)

registered :: Erlang [String]
  -- All registered names
```


My sense is that it'd be better to implement the registry in Haskell. Then all 
we'd need is a way to find the local registry process.

## Closures


We need some way to specify a thunk to send to another node.
Basically just a pair of a code pointer and some free variables:

```wiki
data Closure a where
  Clo :: Serialisable v => (v -#> a) -> v -> Clo a

eval :: Closure a -> a
eval (Clo fun args) = fun ## args
```


The type `(v -#> a)` is intended to be the type of "pure functions"; that is,
a function that

- Has no free variables; is pure code
- Can be represented simply a code pointer


One possible implementation is:

```wiki
type (v -#> a) = String
## :: (v -#> a) -> v -> a
## fun arg = ...lookup the function, do dynamic type check....
```


A more interesting possibilty is to provide direct language support.

- A type `(v -#> a)`
- A intro form `(\# v. a)`
- An elim form `(f ## v)`


Plus perhaps some support for building values of type `Closure a` from
a term of type `a`.

```wiki
   spawn <e> 
means
   spawn (Clo (\#(a,b,c) -> e) (a,b,c))
     -- where a,b,c are the free vars of e
```


Random notes 

```wiki
f x = let g = <\y = x+y> in
        ...<Clo (\#g -> ...) g >...


dmap :: Clo (a->b) -> [(a,Place)] -> Erlang [b]
dmap (| f |) = mapM (\(x,p) -> do { (s,r) <- newChan
                            ; spawnAt p (| send s (f x) |)
                            ; receive r })

  \(| f |) -> e
  \c -> #define f = (c ##) in e
  \(Clo f' x) -> #define f (f' # x) in e

[#| e |#]
<e>  ==   $(cloify [|e|])  -- Good try

G|vtop |- 
--------------------
G |- \#x -> e : (a -#> b)
```